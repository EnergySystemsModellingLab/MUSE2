//! Functionality for running the MUSE2 simulation across milestone years.
use crate::asset::{Asset, AssetPool, AssetRef};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessMap;
use crate::simulation::prices::{Prices, calculate_prices};
use crate::units::Capacity;
use anyhow::{Context, Result};
use log::info;
use std::path::Path;
use std::sync::Arc;

pub mod activity;
pub mod investment;
use investment::{AgentRunMode, run_agents_for_year};
pub mod market;
pub mod prices;
pub use prices::PriceMap;

/// Run the simulation.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `output_path` - The folder to which output files will be written
/// * `debug_model` - Whether to write additional agent optimisation information to output files
#[allow(clippy::too_many_lines)]
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

    info!("Running agent dispatch...");
    writer.set_debug_context("base year agent dispatch".to_string());
    let base_year_selection = run_agents_for_year(
        model,
        year,
        &asset_pool,
        AgentRunMode::Dispatch,
        &Prices::default(),
        &mut writer,
    )
    .context("Agent dispatch failed")?;
    writer.clear_debug_context();

    let mut prices = calculate_prices(
        model,
        &base_year_selection.activity,
        &candidates,
        &Prices::default(),
        year,
    )?;
    writer.write_flows(year, &base_year_selection.flow_map())?;
    writer.write_prices(year, &prices.market)?;

    while let Some(year) = year_iter.next() {
        info!("Milestone year: {year}");

        // Decommission assets whose lifetime has passed
        asset_pool.decommission_old(year);

        // Commission user-defined assets for this year
        let mut new_assets = asset_pool.commission_new(year, &mut user_assets).to_vec();

        // Take all the active assets as a list of existing assets
        let existing_assets = asset_pool.take();

        // Iterative loop to "iron out" prices via repeated investment and price calculation
        let mut ironing_out_iter = 0;
        let mut selection = loop {
            // Add context to the writer
            writer.set_debug_context(format!("ironing out iteration {ironing_out_iter}"));

            // Perform agent investment
            info!("Running agent investment...");
            let selection = run_agents_for_year(
                model,
                year,
                &existing_assets,
                AgentRunMode::Invest,
                &prices,
                &mut writer,
            )
            .context("Agent investment failed")?;

            let new_prices =
                calculate_prices(model, &selection.activity, &candidates, &prices, year)?;

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
                break selection;
            }

            // Break if max iterations reached
            ironing_out_iter += 1;
            if ironing_out_iter == model.parameters.max_ironing_out_iterations {
                info!(
                    "Max ironing out iterations ({}) reached",
                    model.parameters.max_ironing_out_iterations
                );
                break selection;
            }
        };

        // Commission selected assets and re-key their activities to the commissioned assets
        let newly_selected = selection.commission(&mut asset_pool);
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

        let new_prices = calculate_prices(model, &selection.activity, &candidates, &prices, year)?;

        writer.write_flows(year, &selection.flow_map())?;
        writer.write_prices(year, &new_prices.market)?;

        // Prices for the next year
        prices = new_prices;
    }

    writer.flush()?;

    Ok(())
}

/// Create candidate assets for all potential processes in a specified year
pub fn candidate_assets_for_next_year(
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
                    Arc::clone(process),
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
