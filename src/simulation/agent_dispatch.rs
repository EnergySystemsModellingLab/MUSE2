//! The `agent_dispatch` simulation protocol.
//!
//! Agents choose capacity for each milestone year, and the activities determined by their own
//! optimisations are used directly: they give the flows used to propagate demand between markets,
//! the prices calculated in each ironing-out iteration, and the final flows and prices for the
//! year. No system-wide dispatch optimisation is performed.
//!
//! In the base year, agents dispatch the committed assets. In subsequent years, agents invest, and
//! this is repeated with updated prices until prices converge ("ironing out").
use crate::asset::AssetPool;
use crate::commodity::CommodityID;
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::activity::{FlowMap, Selection};
use crate::simulation::candidate_assets_for_next_year;
use crate::simulation::investment::{
    AgentRunMode, AllDemandMap, FlowEstimator, run_agents_for_year,
};
use crate::simulation::market::MarketSet;
use crate::simulation::prices::{Prices, calculate_prices};
use anyhow::{Context, Result};
use log::info;

/// Run the simulation using the `agent_dispatch` protocol.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `writer` - The data writer to which results are written
#[allow(clippy::too_many_lines)]
pub fn run(model: &Model, writer: &mut DataWriter) -> Result<()> {
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

    // Agents dispatch the committed assets to get flows and prices for the base year. There are no
    // previous prices in the base year, so cyclic markets are seeded with zero.
    info!("Running agent dispatch...");
    writer.set_debug_context("base year agent dispatch".to_string());
    let base_year_selection = run_agents_for_year(
        model,
        year,
        &asset_pool,
        AgentRunMode::Dispatch,
        &Prices::default(),
        &mut AgentActivities,
        writer,
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
                &mut AgentActivities,
                writer,
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

    Ok(())
}

/// Takes the flows of assets selected by agents directly from the activities determined during
/// asset selection.
struct AgentActivities;

impl FlowEstimator for AgentActivities {
    fn flows_after_market_set(
        &mut self,
        _market_set: &MarketSet,
        _settled_markets: &[(CommodityID, RegionID)],
        _all_selected: &Selection,
        new_selection: &Selection,
        _writer: &mut DataWriter,
    ) -> Result<FlowMap> {
        Ok(new_selection.flow_map())
    }

    fn flows_after_cycle_market(
        &mut self,
        _cycle: &[(CommodityID, RegionID)],
        _market: &(CommodityID, RegionID),
        _pass: u32,
        _net_demand: &AllDemandMap,
        selection: &Selection,
        _writer: &mut DataWriter,
    ) -> Result<FlowMap> {
        Ok(selection.flow_map())
    }
}
