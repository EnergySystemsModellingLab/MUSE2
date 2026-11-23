//! Code for running agent optimisations.
use super::activity::{ActivityMap, FlowMap, create_flow_map};
use super::dispatch::DispatchRun;
use crate::asset::AssetRef;
use crate::commodity::{CommodityID, CommodityMap};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::Process;
use crate::region::RegionID;
use crate::simulation::prices::Prices;
use crate::time_slice::TimeSliceSelection;
use crate::timeit::InvestmentTimer;
use crate::units::Flow;
use anyhow::Result;
use context_manager;
use indexmap::IndexMap;
use itertools::Itertools;
use log::debug;
use std::collections::HashMap;
use std::sync::Arc;

pub mod competitive;
pub mod population;
pub mod portfolio;

/// A map of demand across time-slice selections for a specific market
pub type DemandMap = IndexMap<TimeSliceSelection, Flow>;

/// The assets selected by agents, along with the activities determined during asset selection
#[derive(Clone, Default)]
pub struct Selection {
    /// The selected assets (including retained commissioned assets)
    pub assets: Vec<AssetRef>,
    /// The activity of each selected asset in each time slice
    pub activity: ActivityMap,
}

impl Selection {
    /// Create a [`Selection`] from the results of an asset selection optimisation.
    ///
    /// `selected` pairs each asset as it appears in `activity` with the asset that was ultimately
    /// selected. These may compare unequal (e.g. candidates are assigned an agent on selection), so
    /// activities are re-keyed to the selected assets. Activities of assets which were not selected
    /// are discarded.
    pub fn from_optimisation(selected: Vec<(AssetRef, AssetRef)>, activity: &ActivityMap) -> Self {
        let activity = {
            let selected_by_original: HashMap<_, _> =
                selected.iter().map(|(old, new)| (old, new)).collect();
            activity
                .iter()
                .filter_map(|((asset, time_slice), activity)| {
                    let asset = selected_by_original.get(asset)?;
                    Some((((*asset).clone(), time_slice.clone()), *activity))
                })
                .collect::<ActivityMap>()
        };
        debug_assert!(
            selected
                .iter()
                .all(|(_, new)| activity.keys().any(|(asset, _)| asset == new)),
            "All selected assets must have activities"
        );
        let assets = selected.into_iter().map(|(_, new)| new).collect();

        Self { assets, activity }
    }

    /// Whether no assets were selected
    pub fn is_empty(&self) -> bool {
        self.assets.is_empty()
    }

    /// Add the assets and activities from another [`Selection`]
    pub fn extend(&mut self, other: Selection) {
        debug_assert!(
            other
                .assets
                .iter()
                .all(|asset| !self.assets.contains(asset)),
            "Assets cannot be selected more than once"
        );
        self.assets.extend(other.assets);
        self.activity.extend(other.activity);
    }

    /// Retain only the assets (and their activities) for which `predicate` returns true
    pub fn retain<F>(&mut self, predicate: F)
    where
        F: Fn(&AssetRef) -> bool,
    {
        self.assets.retain(&predicate);
        self.activity.retain(|(asset, _), _| predicate(asset));
    }

    /// Create a map of commodity flows from the activities of the selected assets
    pub fn flow_map(&self) -> FlowMap {
        create_flow_map(
            self.activity
                .iter()
                .map(|((asset, time_slice), activity)| (asset, time_slice, *activity)),
        )
    }
}

/// How agents are run for a milestone year
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AgentRunMode {
    /// Agents dispatch their existing assets as committed assets, without investment options
    Dispatch,
    /// Agents choose which existing assets to retain and which candidates to invest in
    Invest,
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
/// * `prices` - Commodity prices calculated in the previous full system dispatch
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
    writer: &mut DataWriter,
) -> Result<Selection> {
    // Initialise net demand map
    let preset_demands = collect_preset_demands_for_year(&model.commodities, year);
    let mut net_demand = preset_demands.clone();

    // Keep track of all the assets selected
    // This includes Commissioned assets that are selected for retention, and new Ready assets
    let mut all_selected = Selection::default();

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
        let selection =
            market_set.run_agents(model, year, mode, &net_demand, assets, prices, writer)?;

        // Update our list of seen markets
        for market in market_set.iter_markets() {
            seen_markets.push(market.clone());
        }

        // If no assets have been selected, skip dispatch optimisation
        // **TODO**: this probably means there's no demand for the market, which we could
        // presumably preempt
        if selection.is_empty() {
            debug!("No assets selected for '{market_set}'");
            continue;
        }

        // Add the selected assets to the list of all selected assets
        let selected_assets = selection.assets.clone();
        all_selected.extend(selection);

        // Perform dispatch optimisation with assets that have been selected so far
        // **TODO**: presumably we only need to do this for selected_assets, as assets added in
        // previous iterations should not change
        debug!("Running post-investment dispatch for '{market_set}'");

        // As upstream markets by definition will not yet have producers, we explicitly set
        // their prices using external values so that they don't appear free
        let solution = DispatchRun::new(model, &all_selected.assets, year, &preset_demands)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::Asset;
    use crate::commodity::Commodity;
    use crate::fixture::{agent_id, process, process_flows_map, svd_commodity, time_slice};
    use crate::process::{FlowType, Process, ProcessFlow};
    use crate::time_slice::TimeSliceID;
    use crate::units::{Activity, Capacity, Flow, FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use rstest::rstest;

    use std::sync::Arc;

    fn candidate(process: &Process, commission_year: u32) -> AssetRef {
        Asset::new_candidate(
            Arc::new(process.clone()),
            "GBR".into(),
            Capacity(1.0),
            commission_year,
        )
        .unwrap()
        .into()
    }

    #[rstest]
    fn selection_from_optimisation_rekeys_activity(process: Process, time_slice: TimeSliceID) {
        let selected_candidate = candidate(&process, 2015);
        let unselected_candidate = candidate(&process, 2016);
        let mut selected_asset = selected_candidate.clone();
        selected_asset
            .make_mut()
            .select_candidate_for_investment(agent_id());
        assert_ne!(selected_candidate, selected_asset);

        let activity = indexmap! {
            (selected_candidate.clone(), time_slice.clone()) => Activity(1.0),
            (unselected_candidate, time_slice.clone()) => Activity(2.0),
        };
        let selection = Selection::from_optimisation(
            vec![(selected_candidate, selected_asset.clone())],
            &activity,
        );

        assert_eq!(selection.assets, [selected_asset.clone()]);
        assert_eq!(
            selection.activity,
            indexmap! { (selected_asset, time_slice) => Activity(1.0) }
        );
    }

    #[rstest]
    fn selection_retain_filters_assets_and_activity(process: Process, time_slice: TimeSliceID) {
        let asset1 = candidate(&process, 2015);
        let asset2 = candidate(&process, 2016);
        let mut selection = Selection {
            assets: vec![asset1.clone(), asset2.clone()],
            activity: indexmap! {
                (asset1.clone(), time_slice.clone()) => Activity(1.0),
                (asset2.clone(), time_slice.clone()) => Activity(2.0),
            },
        };

        selection.retain(|asset| *asset == asset2);

        assert_eq!(selection.assets, std::slice::from_ref(&asset2));
        assert_eq!(
            selection.activity,
            indexmap! { (asset2, time_slice) => Activity(2.0) }
        );
    }

    #[rstest]
    fn selection_flow_map_multiplies_activity_by_coeff(
        mut process: Process,
        svd_commodity: Commodity,
        time_slice: TimeSliceID,
    ) {
        let commodity = Arc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Arc::clone(&commodity),
            coeff: FlowPerActivity(2.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        process.flows = process_flows_map(
            process.regions.clone(),
            Arc::new(indexmap! { commodity.id.clone() => process_flow }),
        );
        let asset = candidate(&process, 2015);
        let selection = Selection {
            assets: vec![asset.clone()],
            activity: indexmap! { (asset.clone(), time_slice.clone()) => Activity(3.0) },
        };

        assert_eq!(
            selection.flow_map(),
            indexmap! { (asset, commodity.id.clone(), time_slice) => Flow(6.0) }
        );
    }
}
