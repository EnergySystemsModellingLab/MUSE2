//! Code for performing agent investment.
use super::activity::{FlowMap, Selection};
use crate::asset::{Asset, AssetRef};
use crate::commodity::{Commodity, CommodityID, CommodityMap};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::Process;
use crate::region::RegionID;
use crate::simulation::market::MarketSet;
use crate::simulation::prices::Prices;
use crate::time_slice::{TimeSliceInfo, TimeSliceLevel, TimeSliceSelection};
use crate::timeit::InvestmentTimer;
use crate::units::{ActivityPerCapacity, Capacity, Flow, FlowPerCapacity};
use anyhow::Result;
use context_manager;
use indexmap::IndexMap;
use itertools::Itertools;
use log::debug;
use std::collections::HashMap;
use std::sync::Arc;
use strum::IntoEnumIterator;

pub mod competitive;
pub mod portfolio;

/// A map of demand across time-slice selections for a specific market
pub type DemandMap = IndexMap<TimeSliceSelection, Flow>;

/// How agents are run for a milestone year
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AgentRunMode {
    /// Agents dispatch their existing assets as committed assets, without investment options
    Dispatch,
    /// Agents choose which existing assets to retain and which candidates to invest in
    Invest,
}

/// Determines the commodity flows of assets selected by agents.
///
/// These flows are used to update the net demand seen by markets later in the investment order.
/// How they are obtained depends on the simulation protocol.
pub trait FlowEstimator {
    /// Called after agents have been run for a top-level market set (only if something was
    /// selected).
    ///
    /// `settled_markets` contains all markets processed so far, including those in `market_set`.
    /// `all_selected` is the cumulative selection including `new_selection`.
    fn flows_after_market_set(
        &mut self,
        market_set: &MarketSet,
        settled_markets: &[(CommodityID, RegionID)],
        all_selected: &Selection,
        new_selection: &Selection,
        writer: &mut DataWriter,
    ) -> Result<FlowMap>;

    /// Called after agents have been run for one market inside a cycle (only if something was
    /// selected).
    ///
    /// `pass` is the selection pass within the cycle (1 or 2) and `net_demand` is the net demand
    /// for that pass.
    fn flows_after_cycle_market(
        &mut self,
        cycle: &[(CommodityID, RegionID)],
        market: &(CommodityID, RegionID),
        pass: u32,
        net_demand: &AllDemandMap,
        selection: &Selection,
        writer: &mut DataWriter,
    ) -> Result<FlowMap>;
}

/// The assets and processes available to an agent to meet its share of demand for one market
#[derive(Default)]
pub struct AssetOptions {
    /// Assets with fixed capacity: they are dispatched but cannot be expanded, reduced or mothballed
    pub committed: Vec<AssetRef>,
    /// Existing assets which may be retained in full, in part or not at all
    pub existing: Vec<AssetRef>,
    /// Processes from which the algorithm may create candidate assets
    pub processes: Vec<Arc<Process>>,
}

/// Demand for a given combination of commodity, region and time-slice selection
pub type AllDemandMap = IndexMap<(CommodityID, RegionID, TimeSliceSelection), Flow>;

/// Run agents for a milestone year, either dispatching committed assets or choosing investments.
///
/// # Arguments
///
/// * `model` - The model
/// * `year` - Milestone year being processed
/// * `assets` - The asset pool (commissioned and otherwise)
/// * `mode` - Whether agents dispatch committed assets or make investment decisions
/// * `prices` - Commodity prices calculated in the previous dispatch
/// * `flow_estimator` - Determines the flows of selected assets, to propagate demand upstream
/// * `writer` - Data writer
///
/// # Returns
///
/// The assets returned by the agents for the given `year`, along with their activities, or an
/// error.
#[context_manager::wrap(InvestmentTimer)]
pub fn run_agents_for_year(
    model: &Model,
    year: u32,
    assets: &[AssetRef],
    mode: AgentRunMode,
    prices: &Prices,
    flow_estimator: &mut dyn FlowEstimator,
    writer: &mut DataWriter,
) -> Result<Selection> {
    // Initialise net demand map
    let mut net_demand = collect_preset_demands_for_year(&model.commodities, year);

    // Keep track of all the assets selected
    // This includes Commissioned assets that are selected for retention, and new Ready assets
    let mut all_selected = Selection::default();

    let investment_order = &model.investment_order[&year];
    debug!(
        "Investment order for year '{year}': {}",
        investment_order.iter().join(" -> ")
    );

    // Keep track of the markets that have been settled so far, i.e. those for which agents have
    // already been run. Flow estimators may use this to restrict their calculations.
    let mut settled_markets = Vec::new();

    // Iterate over market sets in the investment order for this year
    for market_set in investment_order {
        // Select assets for this market set
        let selection = market_set.run_agents(
            model,
            year,
            mode,
            &net_demand,
            assets,
            prices,
            flow_estimator,
            writer,
        )?;

        // Update our list of settled markets
        for market in market_set.iter_markets() {
            settled_markets.push(market.clone());
        }

        // If no assets have been selected, skip flow estimation
        // **TODO**: this probably means there's no demand for the market, which we could
        // presumably preempt
        if selection.is_empty() {
            debug!("No assets selected for '{market_set}'");
            continue;
        }

        // Add the selected assets to the list of all selected assets
        let new_selection = selection;
        all_selected.extend(new_selection.clone());

        // Determine the flows of the assets selected so far
        // **TODO**: presumably we only need to do this for selected_assets, as assets added in
        // previous iterations should not change
        let flows = flow_estimator.flows_after_market_set(
            market_set,
            &settled_markets,
            &all_selected,
            &new_selection,
            writer,
        )?;

        // Update demand map with flows from newly added assets
        update_net_demand_map(&mut net_demand, &flows, &new_selection.assets);
    }

    Ok(all_selected)
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
                // Net demand may become negative, representing a surplus.
                demand
                    .entry(key)
                    .and_modify(|value| *value -= *flow)
                    .or_insert(-*flow);
            }
        }
    }
}

/// Returns the minimum installed capacity required for `asset` to satisfy the demand that it can
/// potentially serve, accounting for its activity constraints.
///
/// The returned value is the maximum capacity requirement implied by any time-slice selection,
/// since constraints at coarser aggregation levels (e.g. seasonal or annual limits) can require
/// more capacity than constraints at the finest time-slice level.
///
/// Demand is evaluated using the commodity's balance level. Demand within a balance bucket is
/// treated as fungible: if the asset is capable of operating in any constituent time slice of a
/// bucket, then all demand in that bucket is considered serviceable by the asset.
///
/// Selections whose maximum supply is zero are ignored. Such selections would otherwise imply an
/// infinite capacity requirement and therefore provide no useful lower bound.
pub fn get_demand_limiting_capacity(
    time_slice_info: &TimeSliceInfo,
    asset: &Asset,
    commodity: &Commodity,
    demand: &DemandMap,
) -> Capacity {
    let coeff = asset.get_flow(&commodity.id).unwrap().coeff;
    let mut capacity = Capacity(0.0);
    let mut demand_cache: HashMap<_, Flow> = HashMap::new();

    // Calculate demand-limiting capacity at each timeslice level and take the max.
    for level in TimeSliceLevel::iter() {
        for selection in time_slice_info.iter_selections_at_level(level) {
            // Maximum supply within this selection according to the asset's activity limits.
            let max_supply_for_selection = *asset
                .get_activity_per_capacity_limits_for_selection(&selection)
                .end()
                * coeff;

            // Selections with zero supply would imply infinite demand-limiting capacity,
            // so they do not contribute to the maximum.
            if max_supply_for_selection == FlowPerCapacity(0.0) {
                continue;
            }

            // Serviceable demand within this selection.
            //
            // Demand is effectively grouped into balance buckets at the commodity's
            // balance level. A balance bucket contributes if:
            //   1. The bucket is contained within this selection, and
            //   2. The asset can operate in at least one constituent timeslice
            //      within that bucket.
            //
            // Demand within a balance bucket is fungible, so if the asset can serve
            // any timeslice in the bucket, all demand in that bucket is considered
            // serviceable.
            let demand_selection_level = level.max(commodity.time_slice_level);
            let demand_selection = selection
                .containing_selection_at_level(demand_selection_level)
                .unwrap();
            let serviceable_demand_for_selection = *demand_cache
                .entry(demand_selection.clone())
                .or_insert_with(|| {
                    demand_selection
                        .iter_at_level(time_slice_info, commodity.time_slice_level)
                        .unwrap()
                        .filter(|(bucket, _)| {
                            bucket.iter(time_slice_info).any(|(ts, _)| {
                                *asset.get_activity_per_capacity_limits(ts).end()
                                    > ActivityPerCapacity(0.0)
                            })
                        })
                        .map(|(bucket, _)| demand[&bucket])
                        .sum()
                });

            // Calculate demand-limiting capacity for this selection and take the
            // maximum across all selections.
            capacity = capacity.max(serviceable_demand_for_selection / max_supply_for_selection);
        }
    }

    capacity
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{
        asset, process, process_activity_limits_map, process_flows_map, svd_commodity, time_slice,
        time_slice_info, time_slice_info2,
    };
    use crate::process::{ActivityLimits, FlowType, Process, ProcessFlow};
    use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceSelection};
    use crate::units::Dimensionless;
    use crate::units::{Flow, FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use rstest::rstest;

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
}
