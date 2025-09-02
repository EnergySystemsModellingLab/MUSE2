//! Functionality for running the MUSE 2.0 simulation.
use crate::asset::{Asset, AssetPool, AssetRef};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessMap;
use crate::simulation::prices::{calculate_prices_and_reduced_costs, ReducedCosts};
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
    model: Model,
    mut assets: AssetPool,
    output_path: &Path,
    debug_model: bool,
) -> Result<()> {
    let mut writer = DataWriter::create(output_path, &model.model_path, debug_model)?;

    // Iterate over milestone years
    let mut year_iter = model.iter_years().peekable();
    let year = year_iter.next().unwrap(); // NB: There will be at least one year

    info!("Milestone year: {year}");

    // There shouldn't be assets already commissioned, but let's do this just in case
    assets.decommission_old(year);

    // Commission assets for base year
    assets.commission_new(year);

    // Write assets to file
    writer.write_assets(assets.iter_all())?;

    // Gather candidates for the next year
    let next_year = year_iter.peek().copied().unwrap(); // should be at least one more year
    let mut candidates = candidate_assets_for_year(
        &model.processes,
        next_year,
        model.parameters.candidate_asset_capacity,
    );

    // Run dispatch optimisation
    info!("Running dispatch optimisation...");
    let (flow_map, mut prices, mut reduced_costs) =
        run_dispatch_for_year(&model, assets.as_slice(), &candidates, year, &mut writer)?;

    // Write results of dispatch optimisation to file
    writer.write_flows(year, &flow_map)?;
    writer.write_prices(year, &prices)?;
    writer.write_debug_reduced_costs(year, &reduced_costs)?;

    while let Some(year) = year_iter.next() {
        info!("Milestone year: {year}");

        // Decommission assets whose lifetime has passed. We do this *before* agent investment, to
        // prevent agents from selecting assets that are being decommissioned in this milestone
        // year.
        assets.decommission_old(year);

        // Commission pre-defined assets for this year
        assets.commission_new(year);

        // Take all the active assets as a list of existing assets
        let existing_assets = assets.take();

        // Ironing out loop
        let mut ironing_out_iter = 0;
        let selected_assets: Vec<AssetRef> = loop {
            // Add context to the writer
            writer.set_debug_context(format!("ironing out iteration {ironing_out_iter}"));

            // Perform agent investment
            info!("Running agent investment...");
            let selected_assets = perform_agent_investment(
                &model,
                year,
                &existing_assets,
                &prices,
                &reduced_costs,
                &mut writer,
            )
            .context("Agent investment failed")?;

            // We need to add candidates from all existing_assets that aren't in selected_assets as
            // these may be re-chosen in the next iteration
            let candidates_for_existing = existing_assets
                .iter()
                .filter(|asset| !selected_assets.contains(asset))
                .map(|asset| asset.as_candidate(Some(model.parameters.candidate_asset_capacity)))
                .collect();

            // Run dispatch optimisation to get updated reduced costs and prices for the next
            // iteration
            info!("Running dispatch optimisation...");
            let (_flow_map, new_prices, new_reduced_costs) = run_dispatch_for_year(
                &model,
                &selected_assets,
                &[candidates.clone(), candidates_for_existing].concat(),
                year,
                &mut writer,
            )?;

            // Check if prices have converged
            let prices_stable =
                prices.within_tolerance(&new_prices, model.parameters.price_tolerance);

            // Update prices and reduced costs for the next iteration
            prices = new_prices;
            reduced_costs = new_reduced_costs;

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
        candidates = next_year
            .map(|next_year| {
                candidate_assets_for_year(
                    &model.processes,
                    next_year,
                    model.parameters.candidate_asset_capacity,
                )
            })
            .unwrap_or_default();

        // Run dispatch optimisation
        info!("Running final dispatch optimisation for year {year}...");
        let (flow_map, new_prices, new_reduced_costs) =
            run_dispatch_for_year(&model, assets.as_slice(), &candidates, year, &mut writer)?;

        // Write results of dispatch optimisation to file
        writer.write_flows(year, &flow_map)?;
        writer.write_prices(year, &new_prices)?;
        writer.write_debug_reduced_costs(year, &new_reduced_costs)?;

        // Reduced cost and prices for the next year
        reduced_costs = new_reduced_costs;
        prices = new_prices;
    }

    writer.flush()?;

    Ok(())
}

// Run dispatch to get flows, prices and reduced costs for a milestone year
fn run_dispatch_for_year(
    model: &Model,
    assets: &[AssetRef],
    candidates: &[AssetRef],
    year: u32,
    writer: &mut DataWriter,
) -> Result<(FlowMap, CommodityPrices, ReducedCosts)> {
    // Dispatch optimisation with existing assets only
    let solution_existing =
        DispatchRun::new(model, assets, year).run("final without candidates", writer)?;
    let flow_map = solution_existing.create_flow_map();

    // Perform a separate dispatch run with existing assets and candidates (if there are any)
    let solution = if candidates.is_empty() {
        solution_existing
    } else {
        DispatchRun::new(model, assets, year)
            .with_candidates(candidates)
            .run("final with candidates", writer)?
    };

    // Calculate commodity prices and asset reduced costs
    let (prices, reduced_costs) =
        calculate_prices_and_reduced_costs(model, &solution, assets, year);

    Ok((flow_map, prices, reduced_costs))
}

/// Create candidate assets for all potential processes in a specified year
fn candidate_assets_for_year(
    processes: &ProcessMap,
    year: u32,
    candidate_asset_capacity: Capacity,
) -> Vec<AssetRef> {
    let mut candidates = Vec::new();
    for process in processes
        .values()
        .filter(move |process| process.active_for_year(year))
    {
        for region_id in process.regions.iter() {
            candidates.push(
                Asset::new_candidate(
                    Rc::clone(process),
                    region_id.clone(),
                    candidate_asset_capacity,
                    year,
                )
                .unwrap()
                .into(),
            );
        }
    }

    candidates
}
