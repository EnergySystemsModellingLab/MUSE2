//! The `system_dispatch` simulation protocol.
//!
//! Agents choose capacity for each milestone year; commodity flows and prices are then obtained
//! from a system-wide, cost-minimising dispatch optimisation.
//!
//! In the base year, the assets are dispatched with and without candidate assets to get flows and
//! prices. In subsequent years, agents invest, with partial dispatch runs used to propagate demand
//! between markets. This is repeated with updated prices until prices converge ("ironing out"),
//! before a final dispatch of the resulting asset pool gives the flows and prices for the year.
use crate::asset::{AssetPool, AssetRef};
use crate::commodity::CommodityID;
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::activity::{FlowMap, Selection};
use crate::simulation::candidate_assets_for_next_year;
use crate::simulation::investment::{
    AgentRunMode, AllDemandMap, FlowEstimator, collect_preset_demands_for_year, run_agents_for_year,
};
use crate::simulation::market::MarketSet;
use crate::simulation::prices::{PriceMap, Prices, calculate_prices};
use crate::timeit::DispatchTimer;
use anyhow::{Context, Result};
use context_manager;
use itertools::Itertools;
use log::{debug, info};
use std::slice;

mod debug;
pub mod optimisation;
use debug::create_debug_files;
use optimisation::{DispatchRun, Solution};

/// Run the simulation using the `system_dispatch` protocol.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `writer` - The data writer to which results are written
#[allow(clippy::too_many_lines)]
pub fn run(model: &Model, writer: &mut DataWriter) -> Result<()> {
    if writer.debug_enabled() {
        create_debug_files(writer)?;
    }

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
    // There are no previous prices in the base year, so cyclic markets are seeded with zero
    let (mut prices, mut shadow_prices, flow_map) = run_dispatch_for_year(
        model,
        &asset_pool,
        &candidates,
        &Prices::default(),
        year,
        writer,
    )?;

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
            // NB: activities from asset selection are not currently used
            let mut flow_estimator = PartialDispatch::new(model, year, &shadow_prices);
            let selected_assets = run_agents_for_year(
                model,
                year,
                &existing_assets,
                AgentRunMode::Invest,
                &prices,
                &mut flow_estimator,
                writer,
            )
            .context("Agent investment failed")?
            .assets;

            // Run dispatch optimisation to get updated prices for the next iteration
            info!("Running dispatch optimisation...");
            let (new_prices, new_shadow_prices, ..) =
                run_dispatch_for_year(model, &selected_assets, &candidates, &prices, year, writer)?;

            // Check if prices have converged using time slice-weighted averages
            let prices_stable = prices.market.within_tolerance_weighted(
                &new_prices.market,
                model.parameters.price_tolerance,
                &model.time_slice_info,
            );

            // Update prices for the next iteration
            prices = new_prices;
            shadow_prices = new_shadow_prices;

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
        let (new_prices, new_shadow_prices, flow_map) =
            run_dispatch_for_year(model, &asset_pool, &candidates, &prices, year, writer)?;

        // Write results of dispatch optimisation to file
        writer.write_flows(year, &flow_map)?;
        writer.write_prices(year, &new_prices.market)?;

        // Prices for the next year
        prices = new_prices;
        shadow_prices = new_shadow_prices;
    }

    Ok(())
}

/// Run dispatch to get flows and prices for a milestone year.
///
/// Tries to get commodity flows from a dispatch run which just includes existing assets and prices
/// from a run with both existing and candidate assets. If either flows or prices cannot be
/// calculated, empty maps are returned.
///
/// Returns the prices, the shadow prices from the run with candidates, and the flows of the
/// existing assets.
///
/// `previous_prices` (from the previous year or ironing-out iteration) are used to seed price
/// calculations for cyclic markets.
///
/// Fully mothballed assets or mothballed tranches thereof are excluded.
#[context_manager::wrap(DispatchTimer)]
fn run_dispatch_for_year(
    model: &Model,
    assets: &[AssetRef],
    candidates: &[AssetRef],
    previous_prices: &Prices,
    year: u32,
    writer: &mut DataWriter,
) -> Result<(Prices, PriceMap, FlowMap)> {
    // Sanity checks
    debug_assert!(assets.iter().all(|asset| !asset.is_candidate()));
    debug_assert!(candidates.iter().all(|asset| asset.is_candidate()));

    let market_demands = collect_preset_demands_for_year(&model.commodities, year);

    // Run dispatch optimisation with existing assets only, if there are any. If not, then assume no
    // flows (i.e. all are zero)
    let (solution_existing, flow_map) = if assets.is_empty() {
        (None, FlowMap::default())
    } else {
        let solution = DispatchRun::new(model, assets, year, &market_demands)
            .run("final without candidates", writer)?;
        let flow_map = solution.create_flow_map();
        (Some(solution), flow_map)
    };

    // Perform a separate dispatch run with both existing assets and candidates, if there are any,
    // to get shadow prices. If not, use the existing solution alone.
    let solution_with_candidates = if candidates.is_empty() {
        None
    } else {
        Some(
            DispatchRun::new(model, assets, year, &market_demands)
                .with_candidates(candidates)
                .run("final with candidates", writer)?,
        )
    };

    // Calculate prices from the appropriate solution(s). If there were candidates, use the solution
    // that includes them; otherwise use the existing solution alone. If there were no assets at
    // all, return empty prices.
    let (prices, shadow_prices) = match (
        solution_existing.as_ref(),
        solution_with_candidates.as_ref(),
    ) {
        (None, None) => (Prices::default(), PriceMap::default()),
        (Some(existing), None) => calculate_prices_from_solutions(
            model,
            existing,
            existing,
            candidates,
            previous_prices,
            year,
        )?,
        (Some(existing), Some(with_candidates)) => calculate_prices_from_solutions(
            model,
            existing,
            with_candidates,
            candidates,
            previous_prices,
            year,
        )?,
        (None, Some(with_candidates)) => calculate_prices_from_solutions(
            model,
            with_candidates,
            with_candidates,
            candidates,
            previous_prices,
            year,
        )?,
    };

    Ok((prices, shadow_prices, flow_map))
}

/// Estimates the flows of assets selected by agents using partial dispatch runs.
///
/// Each run dispatches only the assets selected so far and balances only the markets which agents
/// have already settled.
struct PartialDispatch<'a> {
    /// The model
    model: &'a Model,
    /// The milestone year
    year: u32,
    /// Preset demands for the year
    preset_demands: AllDemandMap,
    /// Shadow prices used for markets without a balance constraint
    shadow_prices: &'a PriceMap,
}

impl<'a> PartialDispatch<'a> {
    fn new(model: &'a Model, year: u32, shadow_prices: &'a PriceMap) -> Self {
        Self {
            model,
            year,
            preset_demands: collect_preset_demands_for_year(&model.commodities, year),
            shadow_prices,
        }
    }
}

impl FlowEstimator for PartialDispatch<'_> {
    fn flows_after_market_set(
        &mut self,
        market_set: &MarketSet,
        settled_markets: &[(CommodityID, RegionID)],
        all_selected: &Selection,
        _new_selection: &Selection,
        writer: &mut DataWriter,
    ) -> Result<FlowMap> {
        debug!("Running post-investment dispatch for '{market_set}'");

        // As upstream markets by definition will not yet have producers, we explicitly set
        // their prices using external values so that they don't appear free
        let solution = DispatchRun::new(
            self.model,
            &all_selected.assets,
            self.year,
            &self.preset_demands,
        )
        .without_commodity_constraints()
        .with_market_balance_subset(settled_markets)
        .with_input_prices(self.shadow_prices)
        .run(&format!("post {market_set} investment"), writer)?;

        Ok(solution.create_flow_map())
    }

    fn flows_after_cycle_market(
        &mut self,
        cycle: &[(CommodityID, RegionID)],
        market: &(CommodityID, RegionID),
        pass: u32,
        net_demand: &AllDemandMap,
        selection: &Selection,
        writer: &mut DataWriter,
    ) -> Result<FlowMap> {
        let markets_str = cycle.iter().map(|(c, r)| format!("{c}|{r}")).join(", ");
        let (commodity_id, region_id) = market;
        let description =
            format!("cycle ({markets_str}) post {commodity_id}|{region_id} investment pass {pass}");

        debug!("Running {description}");
        let solution = DispatchRun::new(self.model, &selection.assets, self.year, net_demand)
            .without_commodity_constraints()
            .with_market_balance_subset(slice::from_ref(market))
            .with_input_prices(self.shadow_prices)
            .run(&description, writer)
            .with_context(|| format!("Dispatch failed for cycle ({markets_str})"))?;
        debug!("Completed {description}");

        Ok(solution.create_flow_map())
    }
}

/// Calculate commodity prices from the solutions to dispatch optimisations.
///
/// This extracts the activities of existing assets from `solution_without_candidates` and the
/// shadow prices from `solution_with_candidates`, then calls [`calculate_prices`].
///
/// # Returns
///
/// The calculated prices, along with the shadow prices from `solution_with_candidates`.
///
/// # Arguments
///
/// * `model` - The model
/// * `solution_without_candidates` - Solution to dispatch optimisation without candidates
/// * `solution_with_candidates` - Solution to dispatch optimisation with candidates
/// * `candidates` - The candidate assets included in `solution_with_candidates`, if any
/// * `previous_prices` - Prices from the previous year or ironing-out iteration, used to seed
///   price calculations for cyclic markets
/// * `year` - The year for which prices are being calculated
pub fn calculate_prices_from_solutions(
    model: &Model,
    solution_without_candidates: &Solution,
    solution_with_candidates: &Solution,
    candidates: &[AssetRef],
    previous_prices: &Prices,
    year: u32,
) -> Result<(Prices, PriceMap)> {
    let prices = calculate_prices(
        model,
        &solution_without_candidates.activity_map_for_existing(),
        candidates,
        previous_prices,
        year,
    )?;

    // Shadow prices come from the solution _with_ candidates, so that we can get them for
    // commodities not produced/consumed by any existing assets.
    let shadow_prices =
        PriceMap::from_iter(solution_with_candidates.iter_commodity_balance_duals());
    Ok((prices, shadow_prices))
}
