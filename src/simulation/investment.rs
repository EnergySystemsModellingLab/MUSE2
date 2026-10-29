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
    mut opt_assets: Vec<AssetRef>,
    agent_addition_limits: &HashMap<ProcessID, Capacity>,
    agent_total_limits: &HashMap<ProcessID, Capacity>,
    commodity: &Commodity,
    agent: &Agent,
    region_id: &RegionID,
    prices: &Prices,
    demand: &DemandMap,
    year: u32,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    let objective_type = &agent.objectives[&year];

    // Calculate activity coefficients for all asset options according to the agent's objective
    let activity_coefficients =
        calculate_activity_coefficients_for_assets(model, &opt_assets, prices, year);

    // Calculate capacity coefficients for all asset options according to the agent's objective
    let capacity_coefficients = calculate_capacity_coefficients_for_assets(&opt_assets);

    // Perform optimisation
    let results = perform_optimisation(
        model,
        &opt_assets,
        commodity,
        &activity_coefficients,
        &capacity_coefficients,
        agent_addition_limits,
        agent_total_limits,
        demand,
    )?;

    // // Convert Candidate assets to Ready
    // // At this point we also assign the agent ID to the asset
    // for asset in &mut selected_assets {
    //     if asset.is_candidate() {
    //         asset
    //             .make_mut()
    //             .select_candidate_for_investment(agent.id.clone());
    //     }
    // }

    Ok(opt_assets) // PLACEHOLDER
}

/// Prepare existing assets for reappraisal.
///
/// Assets are replaced in `assets` with an asset representing a single tranche, as they are
/// appraised one tranche at a time. Returns a map from the asset to its original number of tranches.
fn prepare_commissioned_assets_for_retention(assets: &mut [AssetRef]) -> HashMap<AssetRef, u32> {
    let mut available_retention_tranches = HashMap::new();

    for asset in assets.iter_mut().filter(|asset| asset.is_commissioned()) {
        let num_tranches = asset.num_tranches();

        // Replace with single tranche as we appraise one tranche at a time
        *asset = asset.clone().as_single_tranche();

        // Store remaining tranches
        available_retention_tranches.insert(asset.clone(), num_tranches);
    }

    available_retention_tranches
}

/// Check whether there is any remaining demand that is unmet in any time slice
fn is_any_remaining_demand(demand: &DemandMap, absolute_tolerance: Flow) -> bool {
    demand.values().any(|flow| *flow > absolute_tolerance)
}

/// Remove assets that exceed the provided process limits for one complete tranche.
fn remove_assets_exceeding_agent_limits(
    opt_assets: &mut Vec<AssetRef>,
    remaining_agent_limits: &HashMap<ProcessID, Capacity>,
    only_candidates: bool,
) {
    opt_assets.retain(|asset| {
        (only_candidates && !asset.is_candidate())
            || remaining_agent_limits
                .get(asset.process_id())
                .is_none_or(|limit| *limit >= asset.total_capacity())
    });
}

// Update remaining process capacity limit with the capacity of a selected asset
fn subtract_capacity_from_remaining_limit(
    best_asset: &AssetRef,
    remaining_agent_limits: &mut HashMap<ProcessID, Capacity>,
) {
    if let Some(remaining_capacity) = remaining_agent_limits.get_mut(best_asset.process_id()) {
        *remaining_capacity -= best_asset.total_capacity();
        assert!(
            *remaining_capacity >= Capacity(0.0),
            "Remaining Capacity has fallen below zero"
        );
    }
}

/// Update the remaining investment options and selection state.
///
/// If the asset is a candidate, its one-tranche capacity is subtracted from
/// `remaining_agent_addition_limits` (if applicable) to ensure that process addition limits are not
/// exceeded. If the asset is commissioned, one tranche is subtracted from
/// `available_retention_tranches` to ensure that retention does not invent new capacity.
///
/// # Arguments
///
/// * `best_asset` - The asset that has been selected as the best option in this round
/// * `opt_assets` - The list of remaining asset options to be considered in future rounds
/// * `remaining_agent_addition_limits` - The remaining agent addition limits for processes
/// * `remaining_agent_total_limit` - The remaining capacity for processes
/// * `available_retention_tranches` - The commissioned tranches available for retention
fn update_selection_state(
    best_asset: &AssetRef,
    opt_assets: &mut Vec<AssetRef>,
    remaining_agent_addition_limits: &mut HashMap<ProcessID, Capacity>,
    remaining_agent_total_limits: &mut HashMap<ProcessID, Capacity>,
    available_retention_tranches: &mut HashMap<AssetRef, u32>,
) {
    // Subtract asset capacity from the total capacity limit, if applicable.
    subtract_capacity_from_remaining_limit(best_asset, remaining_agent_total_limits);

    // Update the remaining agent addition limit for the selected asset, if applicable, and remove it
    // from the options if the limit is exhausted.
    if best_asset.is_candidate() {
        // Candidate assets: remove capacity from the investment limit, if applicable.
        subtract_capacity_from_remaining_limit(best_asset, remaining_agent_addition_limits);
    } else {
        // Commissioned assets: we've appraised a single tranche, so remove one tranche from the
        // available retention count for this asset.
        let remaining = available_retention_tranches.get_mut(best_asset).unwrap();
        *remaining = remaining.saturating_sub(1);

        // If all tranches have been selected, remove the asset from the investment options.
        if *remaining == 0 {
            let old_idx = opt_assets
                .iter()
                .position(|asset| *asset == *best_asset)
                .unwrap();
            opt_assets.swap_remove(old_idx);
            available_retention_tranches.remove(best_asset);
        }
    }
}

/// Record a selected asset. Candidate selections represent one tranche; repeated selections of the
/// same candidate increase the resulting asset's number of tranches while preserving its tranche
/// size.
///
/// # Arguments
///
/// * `best_asset` - The asset that has been selected as the best option in this round
/// * `best_assets` - The list of assets that have been selected so far
fn record_asset_selection(best_asset: AssetRef, best_assets: &mut Vec<AssetRef>) {
    assert!(
        best_asset.is_commissioned() || best_asset.is_candidate(),
        "Invalid asset type"
    );

    // Add the selected asset to the list of best assets, or add one tranche if it's already there.
    if let Some(existing_asset) = best_assets.iter_mut().find(|asset| **asset == best_asset) {
        // If the asset is already selected, add the additional required tranche
        existing_asset
            .make_mut()
            .increase_capacity(best_asset.capacity());
    } else {
        // Otherwise add it to the list of best assets. Selected assets are unmothballed.
        best_assets.push(best_asset.with_no_mothballed_capacity());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{
        agent_id, asset, process, process_activity_limits_map, process_flows_map, svd_commodity,
        time_slice, time_slice_info, time_slice_info2,
    };
    use crate::process::{ActivityLimits, FlowType, Process, ProcessFlow};
    use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceSelection};
    use crate::units::Dimensionless;
    use crate::units::{Flow, FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use rstest::{fixture, rstest};

    use std::sync::Arc;

    #[rstest]
    fn get_demand_limiting_capacity_works(
        time_slice: TimeSliceID,
        time_slice_info: TimeSliceInfo,
        svd_commodity: Commodity,
        mut process: Process,
    ) {
        // Add flows for the process using the existing commodity fixture
        let commodity_rc = Arc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Arc::clone(&commodity_rc),
            coeff: FlowPerActivity(2.0), // 2 units of flow per unit of activity
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        let process_flows_map = process_flows_map(process.regions.clone(), Arc::new(process_flows));
        process.flows = process_flows_map;

        // Create asset with the configured process
        let asset = asset(process);

        // Create demand map - demand of 10.0 for our time slice
        let demand = indexmap! { TimeSliceSelection::Single(time_slice.clone()) => Flow(10.0)};

        // Call the function
        let result = get_demand_limiting_capacity(&time_slice_info, &asset, &commodity_rc, &demand);

        // Expected calculation:
        // max_flow_per_cap = activity_per_capacity_limit (1.0) * coeff (2.0) = 2.0
        // required_capacity = demand (10.0) / max_flow_per_cap (2.0) = 5.0
        assert_eq!(result, Capacity(5.0));
    }

    #[rstest]
    fn get_demand_limiting_capacity_multiple_time_slices(
        time_slice_info2: TimeSliceInfo,
        svd_commodity: Commodity,
        mut process: Process,
    ) {
        let (time_slice1, time_slice2) =
            time_slice_info2.time_slices.keys().collect_tuple().unwrap();

        // Add flows for the process using the existing commodity fixture
        let commodity_rc = Arc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Arc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0), // 1 unit of flow per unit of activity
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        let process_flows_map = process_flows_map(process.regions.clone(), Arc::new(process_flows));
        process.flows = process_flows_map;

        // Add activity limits for the process
        let mut limits = ActivityLimits::new_with_full_availability(&time_slice_info2);
        limits.add_time_slice_limit(time_slice1.clone(), Dimensionless(0.0)..=Dimensionless(0.2));
        limits.add_time_slice_limit(time_slice2.clone(), Dimensionless(0.0)..=Dimensionless(0.0));
        let limits_map = process_activity_limits_map(process.regions.clone(), limits);
        process.activity_limits = limits_map;

        // Create asset with the configured process
        let asset = asset(process);

        // Create demand map with different demands for each time slice
        let demand = indexmap! {
            TimeSliceSelection::Single(time_slice1.clone()) => Flow(4.0), // Requires capacity of 4.0/0.2 = 20.0
            TimeSliceSelection::Single(time_slice2.clone()) => Flow(3.0), // Would require infinite capacity, but should be skipped
        };

        // Call the function
        let result =
            get_demand_limiting_capacity(&time_slice_info2, &asset, &commodity_rc, &demand);

        // Expected: maximum of the capacity requirements across time slices (excluding zero limit)
        // Time slice 1: demand (4.0) / (activity_limit (0.2) * coeff (1.0)) = 20.0
        // Time slice 2: skipped due to zero activity limit
        // Maximum = 20.0
        assert_eq!(result, Capacity(20.0));
    }

    #[rstest]
    fn get_demand_limiting_capacity_uses_coarser_limits(
        time_slice_info2: TimeSliceInfo,
        svd_commodity: Commodity,
        mut process: Process,
    ) {
        let (time_slice1, time_slice2) =
            time_slice_info2.time_slices.keys().collect_tuple().unwrap();

        // Configure a 1:1 activity-to-flow relationship.
        let commodity_rc = Arc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Arc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        process.flows = process_flows_map(process.regions.clone(), Arc::new(process_flows));

        // Fine-grained limits imply a capacity requirement of 5:
        //   TS1: 5 / 1 = 5
        //   TS2: 5 / 1 = 5
        //
        // The annual limit implies:
        //   (5 + 5) / 0.5 = 20
        //
        // The function should return the larger value.
        let limits = HashMap::from([
            (
                TimeSliceSelection::Single(time_slice1.clone()),
                Dimensionless(0.0)..=Dimensionless(1.0),
            ),
            (
                TimeSliceSelection::Single(time_slice2.clone()),
                Dimensionless(0.0)..=Dimensionless(1.0),
            ),
            (
                TimeSliceSelection::Annual,
                Dimensionless(0.0)..=Dimensionless(0.5),
            ),
        ]);

        process.activity_limits = process_activity_limits_map(
            process.regions.clone(),
            ActivityLimits::new_from_limits(&limits, &time_slice_info2).unwrap(),
        );

        let asset = asset(process);

        let demand = indexmap! {
            TimeSliceSelection::Single(time_slice1.clone()) => Flow(5.0),
            TimeSliceSelection::Single(time_slice2.clone()) => Flow(5.0),
        };

        let result =
            get_demand_limiting_capacity(&time_slice_info2, &asset, &commodity_rc, &demand);

        assert_eq!(result, Capacity(20.0));
    }

    #[rstest]
    #[case(Flow(10.0), Dimensionless(1.0), Capacity(10.0))] // normal: demand / (limit * coeff) = 10 / (1 * 1) = 10
    #[case(Flow(0.0), Dimensionless(1.0), Capacity(0.0))] // zero demand → zero capacity
    #[case(Flow(10.0), Dimensionless(0.5), Capacity(20.0))] // activity limit < 1: 10 / (0.5 * 1) = 20
    #[case(Flow(10.0), Dimensionless(0.0), Capacity(0.0))] // activity limit = 0 → zero capacity
    fn calculate_asset_capacity_scale_works(
        time_slice: TimeSliceID,
        time_slice_info: TimeSliceInfo,
        svd_commodity: Commodity,
        mut process: Process,
        #[case] demand_value: Flow,
        #[case] activity_limit: Dimensionless,
        #[case] expected: Capacity,
    ) {
        let commodity_rc = Arc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Arc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        process.flows = process_flows_map(
            process.regions.clone(),
            Arc::new(indexmap! { commodity_rc.id.clone() => process_flow }),
        );

        let mut limits = ActivityLimits::new_with_full_availability(&time_slice_info);
        limits.add_time_slice_limit(time_slice.clone(), Dimensionless(0.0)..=activity_limit);
        process.activity_limits = process_activity_limits_map(process.regions.clone(), limits);

        let asset = asset(process);
        let demand = indexmap! { TimeSliceSelection::Single(time_slice)=> demand_value };
        assert_eq!(
            calculate_candidate_asset_capacity_scale(&asset, &commodity_rc, &demand),
            expected
        );
    }

    #[rstest]
    fn subtract_capacity_from_remaining_limit_works(asset: Asset) {
        let mut remaining_agent_limits =
            HashMap::from([(asset.process_id().clone(), Capacity(10.0))]);
        subtract_capacity_from_remaining_limit(
            &AssetRef::from(asset.clone()),
            &mut remaining_agent_limits,
        );

        assert_eq!(
            remaining_agent_limits[asset.process_id()].clone(),
            Capacity(8.0)
        );
    }

    #[fixture]
    fn commissioned_asset(agent_id: AgentID, asset: Asset) -> Asset {
        Asset::new_commissioned(
            agent_id,
            Arc::new(asset.process().clone()),
            asset.region_id().clone(),
            asset.capacity(),
            asset.commission_year(),
        )
        .unwrap()
    }

    #[fixture]
    fn candidate_asset(asset: Asset) -> Asset {
        Asset::new_candidate(
            Arc::new(asset.process().clone()),
            asset.region_id().clone(),
            asset.total_capacity(),
            asset.commission_year(),
        )
        .unwrap()
    }

    #[rstest]
    fn remove_assets_exceeding_agent_limits_works(
        commissioned_asset: Asset,
        candidate_asset: Asset,
    ) {
        let mut opt_assets = vec![
            commissioned_asset.clone().into(),
            candidate_asset.clone().into(),
        ];
        let limits = HashMap::from([(commissioned_asset.process_id().clone(), Capacity(1.0))]);

        // Check only the candidate asset is removed when `only_candidates` is true
        remove_assets_exceeding_agent_limits(&mut opt_assets, &limits, true);
        assert_eq!(opt_assets.len(), 1);
        assert_eq!(opt_assets[0], commissioned_asset.clone().into());

        // Check all assets are removed when `only_candidates` is false
        opt_assets.push(candidate_asset.clone().into());
        remove_assets_exceeding_agent_limits(&mut opt_assets, &limits, false);
        assert!(opt_assets.is_empty());
    }

    #[rstest]
    #[case(false)]
    #[case(true)]
    fn update_selection_state_works(
        commissioned_asset: Asset,
        candidate_asset: Asset,
        #[case] commissioned: bool,
    ) {
        let best_asset: AssetRef = if commissioned {
            commissioned_asset.clone().into()
        } else {
            candidate_asset.clone().into()
        };
        let mut opt_assets = vec![best_asset.clone()];
        let mut addition_limits = HashMap::from([(best_asset.process_id().clone(), Capacity(2.0))]);
        let mut total_limits = HashMap::from([(best_asset.process_id().clone(), Capacity(2.0))]);
        let mut retention_tranches = HashMap::from([(best_asset.clone(), 1)]);

        update_selection_state(
            &best_asset,
            &mut opt_assets,
            &mut addition_limits,
            &mut total_limits,
            &mut retention_tranches,
        );

        if commissioned {
            // Expect total_limits to reduce and retention_tranches to be removed
            assert_eq!(total_limits[best_asset.process_id()], Capacity(0.0));
            assert!(!retention_tranches.contains_key(&best_asset));

            // Expect addition_limits to remain unchanged
            assert_eq!(addition_limits[best_asset.process_id()], Capacity(2.0));
        } else {
            // Expect total_limits and addition_limits to reduce
            assert_eq!(total_limits[best_asset.process_id()], Capacity(0.0));
            assert_eq!(addition_limits[best_asset.process_id()], Capacity(0.0));

            // Expect retention_tranches to remain unchanged
            assert_eq!(retention_tranches[&best_asset], 1);
        }
    }
}
