//! Functionality for running the MUSE2 simulation across milestone years.
use crate::asset::{Asset, AssetPool, AssetRef};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessMap;
use crate::simulation::prices::{Prices, calculate_prices};
use crate::timeit::{DispatchTimerContext, get_dispatch_time, get_investment_time};
use crate::units::Capacity;
use anyhow::{Context, Result};
use context_manager;
use log::info;
use std::path::Path;
use std::rc::Rc;

pub mod optimisation;
use optimisation::{DispatchRun, FlowMap};
pub mod investment;
use investment::perform_agent_investment;
pub mod market;
pub mod prices;
pub use prices::PriceMap;

/// Run the simulation.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `output_path` - The folder to which output files will be written
/// * `debug_model` - Whether to write additional information (e.g. duals) to output files
pub fn run(model: &Model, output_path: &Path, debug_model: bool) -> Result<()> {
    let mut writer = DataWriter::create(output_path, &model.model_path, debug_model)?;
    let mut user_assets = model.user_assets.clone();
    let mut asset_pool = AssetPool::new(); // active assets

    // Iterate over milestone years
    let mut year_iter = model.iter_years().peekable();
    let year = year_iter.next().unwrap(); // Unwrap is safe: model must contain at least one milestone year

    info!("Milestone year: {year}");

    // Commission assets for base year
    let new_assets = asset_pool.commission_new(year, &mut user_assets);

    // Write assets to file
    writer.write_assets(new_assets)?;
    writer.write_asset_capacities(year, &asset_pool)?;

    // Gather candidates for the next year, if any
    let next_year = year_iter.peek().copied();
    let mut candidates = candidate_assets_for_next_year(
        &model.processes,
        next_year,
        model.parameters.candidate_asset_capacity,
    );

    // Run dispatch optimisation
    info!("Running dispatch optimisation...");
    let (mut prices, flow_map) =
        run_dispatch_for_year(model, &asset_pool, &candidates, year, &mut writer)?;

    // Write results of dispatch optimisation to file
    writer.write_flows(year, &flow_map)?;
    writer.write_prices(year, &prices.market)?;

    while let Some(year) = year_iter.next() {
        info!("Milestone year: {year}");

        // Decommission assets whose lifetime has passed
        asset_pool.decommission_old(year);

        // Commission user-defined assets for this year
        let mut new_assets = asset_pool.commission_new(year, &mut user_assets).to_vec();

        // Take all the active assets as a list of existing assets
        let existing_assets = asset_pool.take();

        // Iterative loop to "iron out" prices via repeated investment and dispatch
        let mut ironing_out_iter = 0;
        let selected_assets: Vec<AssetRef> = loop {
            // Add context to the writer
            writer.set_debug_context(format!("ironing out iteration {ironing_out_iter}"));

            // Perform agent investment
            info!("Running agent investment...");
            let selected_assets =
                perform_agent_investment(model, year, &existing_assets, &prices, &mut writer)
                    .context("Agent investment failed")?;

            // Run dispatch optimisation to get updated prices for the next iteration
            info!("Running dispatch optimisation...");
            let (new_prices, ..) =
                run_dispatch_for_year(model, &selected_assets, &candidates, year, &mut writer)?;

            // Check if prices have converged using time slice-weighted averages
            let prices_stable = prices.market.within_tolerance_weighted(
                &new_prices.market,
                model.parameters.price_tolerance,
                &model.time_slice_info,
            );

            // Update prices for the next iteration
            prices = new_prices;

            // Clear writer context
            writer.clear_debug_context();

            // Break early if prices have converged
            if prices_stable {
                info!("Prices converged after {} iterations", ironing_out_iter + 1);
                break selected_assets;
            }

            // Break if max iterations reached
            ironing_out_iter += 1;
            if ironing_out_iter == model.parameters.max_ironing_out_iterations {
                info!(
                    "Max ironing out iterations ({}) reached",
                    model.parameters.max_ironing_out_iterations
                );
                break selected_assets;
            }
        };

        // Add selected_assets to the active pool, receiving the newly commissioned ones
        let newly_selected = asset_pool.extend(selected_assets);
        new_assets.extend_from_slice(newly_selected);

        // Decommission unused assets
        asset_pool.mothball_unretained(existing_assets, year);
        asset_pool.decommission_mothballed(year, model.parameters.mothball_years);

        // Write newly commissioned assets
        writer.write_assets(&new_assets)?;
        writer.write_asset_capacities(year, &asset_pool)?;

        // Gather candidates for the next year, if any
        let next_year = year_iter.peek().copied();
        candidates = candidate_assets_for_next_year(
            &model.processes,
            next_year,
            model.parameters.candidate_asset_capacity,
        );

        // Run dispatch optimisation
        info!("Running final dispatch optimisation for year {year}...");
        let (new_prices, flow_map) =
            run_dispatch_for_year(model, &asset_pool, &candidates, year, &mut writer)?;

        // Write results of dispatch optimisation to file
        writer.write_flows(year, &flow_map)?;
        writer.write_prices(year, &new_prices.market)?;

        // Prices for the next year
        prices = new_prices;
    }

    writer.flush()?;

    info!(
        "--- Total time spent in investment steps: {:.1}ms",
        get_investment_time()
    );
    info!(
        "--- Total time spent in dispatch steps: {:.1}ms",
        get_dispatch_time()
    );

    Ok(())
}

// Run dispatch to get flows and prices for a milestone year
#[context_manager::wrap(DispatchTimerContext)]
fn run_dispatch_for_year(
    model: &Model,
    assets: &[AssetRef],
    candidates: &[AssetRef],
    year: u32,
    writer: &mut DataWriter,
) -> Result<(Prices, FlowMap)> {
    // Run dispatch optimisation with existing assets only, if there are any. If not, then assume no
    // flows (i.e. all are zero)
    let (solution_existing, flow_map) = if assets.is_empty() {
        (None, FlowMap::default())
    } else {
        let solution =
            DispatchRun::new(model, assets, year).run("final without candidates", writer)?;
        let flow_map = solution.create_flow_map();
        (Some(solution), flow_map)
    };

    // Perform a separate dispatch run with both existing assets and candidates, if there are any,
    // to get shadow prices. If not, use the existing solution alone.
    let solution_with_candidates = if candidates.is_empty() {
        None
    } else {
        Some(
            DispatchRun::new(model, assets, year)
                .with_candidates(candidates)
                .run("final with candidates", writer)?,
        )
    };

    // Calculate prices from the appropriate solution(s). If there were candidates, use the solution
    // that includes them; otherwise use the existing solution alone. If there were no assets at
    // all, return empty prices.
    let prices = match (
        solution_existing.as_ref(),
        solution_with_candidates.as_ref(),
    ) {
        (None, None) => Prices::default(),
        (Some(existing), None) => calculate_prices(model, existing, existing, year)?,
        (Some(existing), Some(with_candidates)) => {
            calculate_prices(model, existing, with_candidates, year)?
        }
        (None, Some(with_candidates)) => {
            calculate_prices(model, with_candidates, with_candidates, year)?
        }
    };

    Ok((prices, flow_map))
}

/// Create candidate assets for all potential processes in a specified year
fn candidate_assets_for_next_year(
    processes: &ProcessMap,
    next_year: Option<u32>,
    candidate_asset_capacity: Capacity,
) -> Vec<AssetRef> {
    let mut candidates = Vec::new();
    let Some(next_year) = next_year else {
        return candidates;
    };

    for process in processes
        .values()
        .filter(move |process| process.active_for_year(next_year))
    {
        for region_id in &process.regions {
            candidates.push(
                Asset::new_candidate_for_dispatch(
                    Rc::clone(process),
                    region_id.clone(),
                    candidate_asset_capacity,
                    next_year,
                )
                .unwrap()
                .into(),
            );
        }
    }

    candidates
}
