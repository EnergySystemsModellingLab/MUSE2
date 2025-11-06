//! Functionality for running the MUSE2 simulation.
use crate::asset::{Asset, AssetPool, AssetRef};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessMap;
use crate::simulation::prices::calculate_prices;
use crate::units::Capacity;
use anyhow::{Context, Result};
use log::info;
use std::path::Path;
use std::rc::Rc;

pub mod optimisation;
use optimisation::{DispatchRun, FlowMap};
pub mod investment;
use investment::perform_agent_investment;
pub mod prices;
pub use prices::CommodityPrices;

/// Run the simulation.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `assets` - The asset pool
/// * `output_path` - The folder to which output files will be written
/// * `debug_model` - Whether to write additional information (e.g. duals) to output files
pub fn run(
    model: &Model,
    mut assets: AssetPool,
    output_path: &Path,
    debug_model: bool,
) -> Result<()> {
    let mut writer = DataWriter::create(output_path, &model.model_path, debug_model)?;

    // Iterate over milestone years
    let mut year_iter = model.iter_years().peekable();
    let year = year_iter.next().unwrap(); // NB: There will be at least one year

    info!("Milestone year: {year}");

    // Commission assets for base year
    assets.update_for_year(year);

    // Write assets to file
    writer.write_assets(assets.iter_all())?;

    // Gather candidates for the next year, if any
    let next_year = year_iter.peek().copied();
    let mut candidates = candidate_assets_for_next_year(
        &model.processes,
        next_year,
        model.parameters.candidate_asset_capacity,
    );

    // Run dispatch optimisation
    info!("Running dispatch optimisation...");
    let (flow_map, mut prices) =
        run_dispatch_for_year(model, assets.as_slice(), &candidates, year, &mut writer)?;

    // Write results of dispatch optimisation to file
    writer.write_flows(year, &flow_map)?;
    writer.write_prices(year, &prices)?;

    while let Some(year) = year_iter.next() {
        info!("Milestone year: {year}");

        // Commission new assets and decommission those whose lifetime has passed. We do this
        // *before* agent investment, to prevent agents from selecting assets that are being
        // decommissioned in this milestone year.
        assets.update_for_year(year);

        // Take all the active assets as a list of existing assets
        let existing_assets = assets.take();

        // Ironing out loop
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
            let (_flow_map, new_prices) =
                run_dispatch_for_year(model, &selected_assets, &candidates, year, &mut writer)?;

            // Check if prices have converged using time slice-weighted averages
            let prices_stable = prices.within_tolerance_weighted(
                &new_prices,
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

        // Add selected_assets to the active pool
        assets.extend(selected_assets);

        // Decommission unused assets
        assets.decommission_if_not_active(existing_assets, year);

        // Write assets
        writer.write_assets(assets.iter_all())?;

        // Gather candidates for the next year, if any
        let next_year = year_iter.peek().copied();
        candidates = candidate_assets_for_next_year(
            &model.processes,
            next_year,
            model.parameters.candidate_asset_capacity,
        );

        // Run dispatch optimisation
        info!("Running final dispatch optimisation for year {year}...");
        let (flow_map, new_prices) =
            run_dispatch_for_year(model, assets.as_slice(), &candidates, year, &mut writer)?;

        // Write results of dispatch optimisation to file
        writer.write_flows(year, &flow_map)?;
        writer.write_prices(year, &new_prices)?;

        // Prices for the next year
        prices = new_prices;
    }

    writer.flush()?;

    Ok(())
}

// Run dispatch to get flows and prices for a milestone year
fn run_dispatch_for_year(
    model: &Model,
    assets: &[AssetRef],
    candidates: &[AssetRef],
    year: u32,
    writer: &mut DataWriter,
) -> Result<(FlowMap, CommodityPrices)> {
    // Run dispatch optimisation with existing assets only, if there are any. If not, then assume no
    // flows (i.e. all are zero)
    let (solution_existing, flow_map) = (!assets.is_empty())
        .then(|| -> Result<_> {
            let solution =
                DispatchRun::new(model, assets, year).run("final without candidates", writer)?;
            let flow_map = solution.create_flow_map();

            Ok((Some(solution), flow_map))
        })
        .transpose()?
        .unwrap_or_default();

    // Perform a separate dispatch run with both existing assets and candidates, if there are any,
    // to get prices. If not, use the previous solution.
    let solution_for_prices = (!candidates.is_empty())
        .then(|| {
            DispatchRun::new(model, assets, year)
                .with_candidates(candidates)
                .run("final with candidates", writer)
        })
        .transpose()?
        .or(solution_existing);

    // If there were either existing or candidate assets, we can calculate prices.
    // If not, return empty maps.
    let prices = solution_for_prices
        .map(|solution| calculate_prices(model, &solution))
        .unwrap_or_default();

    Ok((flow_map, prices))
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
                Asset::new_candidate(
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
