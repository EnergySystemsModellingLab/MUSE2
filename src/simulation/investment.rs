//! Code for performing agent investment.
use super::optimisation::{DispatchRun, FlowMap};
use crate::agent::Agent;
use crate::asset::AssetRef;
use crate::commodity::{Commodity, CommodityID, CommodityMap};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::prices::Prices;
use crate::time_slice::TimeSliceSelection;
use crate::timeit::InvestmentTimer;
use crate::units::{Capacity, Flow};
use anyhow::Result;
use context_manager;
use indexmap::IndexMap;
use itertools::Itertools;
use log::debug;
use std::collections::HashMap;

pub mod appraisal;
use appraisal::coefficients::{
    calculate_activity_coefficients_for_assets, calculate_capacity_coefficients_for_assets,
};
use appraisal::optimisation::perform_optimisation;

/// A map of demand across time-slice selections for a specific market
pub type DemandMap = IndexMap<TimeSliceSelection, Flow>;

/// Demand for a given combination of commodity, region and time-slice selection
pub type AllDemandMap = IndexMap<(CommodityID, RegionID, TimeSliceSelection), Flow>;

/// Perform agent investment to determine capacity investment of new assets for next milestone year.
///
/// # Arguments
///
/// * `model` - The model
/// * `year` - Current milestone year
/// * `existing_assets` - The asset pool (commissioned and otherwise)
/// * `prices` - Commodity prices calculated in the previous full system dispatch
/// * `writer` - Data writer
///
/// # Returns
///
/// The assets selected (including retained commissioned assets) for the given planning `year` or an
/// error.
#[context_manager::wrap(InvestmentTimer)]
pub fn perform_agent_investment(
    model: &Model,
    year: u32,
    existing_assets: &[AssetRef],
    prices: &Prices,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    // Initialise net demand map
    let preset_demands = collect_preset_demands_for_year(&model.commodities, year);
    let mut net_demand = preset_demands.clone();

    // Keep a list of all the assets selected
    // This includes Commissioned assets that are selected for retention, and new Ready assets
    let mut all_selected_assets = Vec::new();

    let investment_order = &model.investment_order[&year];
    debug!(
        "Investment order for year '{year}': {}",
        investment_order.iter().join(" -> ")
    );

    // Keep track of the markets that have been seen so far. This will be used to apply
    // balance constraints in the dispatch optimisation - we only apply balance constraints for
    // markets that have been seen so far.
    let mut seen_markets = Vec::new();

    // Iterate over market sets in the investment order for this year
    for market_set in investment_order {
        // Select assets for this market set
        let selected_assets = market_set.select_assets(
            model,
            year,
            &net_demand,
            existing_assets,
            prices,
            &seen_markets,
            &all_selected_assets,
            writer,
        )?;

        // Update our list of seen markets
        for market in market_set.iter_markets() {
            seen_markets.push(market.clone());
        }

        // If no assets have been selected, skip dispatch optimisation
        // **TODO**: this probably means there's no demand for the market, which we could
        // presumably preempt
        if selected_assets.is_empty() {
            debug!("No assets selected for '{market_set}'");
            continue;
        }

        // Add the selected assets to the list of all selected assets
        all_selected_assets.extend(selected_assets.iter().cloned());

        // Perform dispatch optimisation with assets that have been selected so far
        // **TODO**: presumably we only need to do this for selected_assets, as assets added in
        // previous iterations should not change
        debug!("Running post-investment dispatch for '{market_set}'");

        // As upstream markets by definition will not yet have producers, we explicitly set
        // their prices using external values so that they don't appear free
        let solution = DispatchRun::new(model, &all_selected_assets, year, &preset_demands)
            .without_commodity_constraints()
            .with_market_balance_subset(&seen_markets)
            .with_input_prices(&prices.shadow)
            .run(&format!("post {market_set} investment"), writer)?;

        // Update demand map with flows from newly added assets
        update_net_demand_map(
            &mut net_demand,
            &solution.create_flow_map(),
            &selected_assets,
        );
    }

    Ok(all_selected_assets)
}

/// Collect the preset commodity demands for a given year into a map of commodity, region and
/// time slice selection to demand.
///
/// Demand for each commodity is stored at its natural time-slice selection level, matching the
/// balance level at which the investment appraisal operates.
pub fn collect_preset_demands_for_year(commodities: &CommodityMap, year: u32) -> AllDemandMap {
    let mut demand_map = AllDemandMap::new();
    for (commodity_id, commodity) in commodities {
        for ((region_id, data_year, time_slice_selection), demand) in &commodity.demand {
            if *data_year != year {
                continue;
            }
            demand_map.insert(
                (
                    commodity_id.clone(),
                    region_id.clone(),
                    time_slice_selection.clone(),
                ),
                *demand,
            );
        }
    }
    demand_map
}

/// Update net demand map with flows from a set of assets
///
/// Non-primary output flows are ignored. This way, demand profiles aren't affected by production
/// of side-products from other assets. The result is that all commodity demands must be met by
/// assets with that commodity as their primary output. Effectively, agents do not see production of
/// side-products from other assets when making investment decisions.
///
/// TODO: this is a very flawed approach. The proper solution might be for agents to consider
/// multiple commodities simultaneously, but that would require substantial work to implement.
pub fn update_net_demand_map(demand: &mut AllDemandMap, flows: &FlowMap, assets: &[AssetRef]) {
    for ((asset, commodity_id, time_slice), flow) in flows {
        if assets.contains(asset) {
            // Only consider input flows and output flows from the primary output commodity
            // (excluding secondary outputs)
            if (flow < &Flow(0.0))
                || asset
                    .primary_output()
                    .is_some_and(|p| &p.commodity.id == commodity_id)
            {
                let level = asset
                    .get_flow(commodity_id)
                    .unwrap()
                    .commodity
                    .time_slice_level;
                let selection = level.containing_selection(time_slice);
                let key = (commodity_id.clone(), asset.region_id().clone(), selection);
                // Note: we use the negative of the flow as input flows are negative in the flow map.
                let value = demand
                    .entry(key)
                    .and_modify(|value| *value -= *flow)
                    .or_insert(-*flow);

                if *value < Flow(0.0) {
                    *value = Flow(0.0);
                }
            }
        }
    }
}

/// Get the best assets for meeting demand for the given commodity
#[allow(clippy::too_many_arguments)]
pub fn select_best_assets(
    model: &Model,
    opt_assets: &[AssetRef],
    agent_addition_limits: &HashMap<ProcessID, Capacity>,
    agent_total_limits: &HashMap<ProcessID, Capacity>,
    commodity: &Commodity,
    agent: &Agent,
    _region_id: &RegionID,
    prices: &Prices,
    demand: &DemandMap,
    year: u32,
    _writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    // Calculate activity coefficients for all asset options according to the agent's objective
    let activity_coefficients =
        calculate_activity_coefficients_for_assets(model, opt_assets, prices, year);

    // Calculate capacity coefficients for all asset options according to the agent's objective
    let capacity_coefficients = calculate_capacity_coefficients_for_assets(opt_assets);

    // Perform optimisation
    let results = perform_optimisation(
        model,
        opt_assets,
        commodity,
        &activity_coefficients,
        &capacity_coefficients,
        agent_addition_limits,
        agent_total_limits,
        demand,
    )?;

    let mut selected_assets = Vec::new();
    for (asset, result) in results.capacities {
        // Solver noise can give tiny negative values
        let capacity = asset
            .capacity()
            .with_total_rounded_up(result.max(Capacity(0.0)));
        if capacity.is_zero() {
            continue; // existing assets left out here are mothballed by the pool
        }

        // Selected assets are unmothballed. This must happen before `set_capacity`.
        let mut selected = asset.with_no_mothballed_capacity();
        let inner = selected.make_mut();
        if inner.is_candidate() {
            inner.select_candidate_for_investment(agent.id.clone());
        }
        inner.set_capacity(capacity);
        selected_assets.push(selected);
    }

    Ok(selected_assets)
}
