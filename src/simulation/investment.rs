//! Code for performing agent investment.
use super::optimisation::{DispatchRun, FlowMap};
use crate::agent::{Agent, AgentID};
use crate::asset::{Asset, AssetCapacity, AssetIterator, AssetRef, AssetState};
use crate::commodity::{Commodity, CommodityID, CommodityMap};
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::prices::Prices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceLevel, TimeSliceSelection};
use crate::units::{ActivityPerCapacity, Capacity, Dimensionless, Flow, FlowPerCapacity};
use anyhow::{Context, Result, ensure};
use indexmap::IndexMap;
use itertools::{Itertools, chain};
use log::{debug, warn};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use strum::IntoEnumIterator;

pub mod appraisal;
use appraisal::coefficients::calculate_coefficients_for_assets;
use appraisal::{
    AppraisalOutput, appraise_investment, count_equal_and_best_appraisal_outputs,
    sort_and_filter_appraisal_outputs,
};

/// A map of demand across time slices for a specific market
type DemandMap = IndexMap<TimeSliceID, Flow>;

/// Demand for a given combination of commodity, region and time slice
type AllDemandMap = IndexMap<(CommodityID, RegionID, TimeSliceID), Flow>;

/// Represents a set of markets which are invested in together.
#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub enum InvestmentSet {
    /// Assets are selected for a single market using `select_assets_for_single_market`
    Single((CommodityID, RegionID)),
    /// Assets are selected for a group of markets which forms a cycle.
    /// Experimental: handled by `select_assets_for_cycle` and guarded by the broken options
    /// parameter.
    Cycle(Vec<(CommodityID, RegionID)>),
    /// Assets are selected for a layer of independent `InvestmentSet`s
    Layer(Vec<InvestmentSet>),
}

impl InvestmentSet {
    /// Recursively iterate over all markets contained in this `InvestmentSet`.
    pub fn iter_markets<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = &'a (CommodityID, RegionID)> + 'a> {
        match self {
            InvestmentSet::Single(market) => Box::new(std::iter::once(market)),
            InvestmentSet::Cycle(markets) => Box::new(markets.iter()),
            InvestmentSet::Layer(set) => Box::new(set.iter().flat_map(|s| s.iter_markets())),
        }
    }

    /// Selects assets for this investment set variant and passes through the shared
    /// context needed by single-market, cycle, or layered selection.
    ///
    /// # Arguments
    ///
    /// * `model` – Simulation model supplying parameters, processes, and dispatch.
    /// * `year` – Planning year being solved.
    /// * `demand` – Net demand profiles available to all markets before selection.
    /// * `existing_assets` – Assets already commissioned in the system.
    /// * `prices` – Commodity price assumptions to use when valuing investments.
    /// * `seen_markets` – Markets for which investments have already been settled.
    /// * `previously_selected_assets` – Assets chosen in earlier investment sets.
    /// * `writer` – Data sink used to log optimisation artefacts.
    #[allow(clippy::too_many_arguments)]
    fn select_assets(
        &self,
        model: &Model,
        year: u32,
        demand: &AllDemandMap,
        existing_assets: &[AssetRef],
        prices: &Prices,
        seen_markets: &[(CommodityID, RegionID)],
        previously_selected_assets: &[AssetRef],
        writer: &mut DataWriter,
    ) -> Result<Vec<AssetRef>> {
        match self {
            InvestmentSet::Single((commodity_id, region_id)) => select_assets_for_single_market(
                model,
                commodity_id,
                region_id,
                year,
                demand,
                existing_assets,
                prices,
                writer,
            ),
            InvestmentSet::Cycle(markets) => {
                debug!("Starting investment for cycle '{self}'");
                select_assets_for_cycle(
                    model,
                    markets,
                    year,
                    demand,
                    existing_assets,
                    prices,
                    seen_markets,
                    previously_selected_assets,
                    writer,
                )
                .with_context(|| {
                    format!(
                        "Investments failed for market set {self} with cyclical dependencies. \
                         Please note that the investment algorithm is currently experimental for \
                         models with circular commodity dependencies and may not be able to find \
                         a solution in all cases."
                    )
                })
            }
            InvestmentSet::Layer(investment_sets) => {
                debug!("Starting asset selection for layer '{self}'");
                let mut all_assets = Vec::new();
                for investment_set in investment_sets {
                    let assets = investment_set.select_assets(
                        model,
                        year,
                        demand,
                        existing_assets,
                        prices,
                        seen_markets,
                        previously_selected_assets,
                        writer,
                    )?;
                    all_assets.extend(assets);
                }
                debug!("Completed asset selection for layer '{self}'");
                Ok(all_assets)
            }
        }
    }
}

impl Display for InvestmentSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvestmentSet::Single((commodity_id, region_id)) => {
                write!(f, "{commodity_id}|{region_id}")
            }
            InvestmentSet::Cycle(markets) => {
                write!(
                    f,
                    "({})",
                    markets.iter().map(|(c, r)| format!("{c}|{r}")).join(", ")
                )
            }
            InvestmentSet::Layer(ids) => {
                write!(f, "[{}]", ids.iter().join(", "))
            }
        }
    }
}

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
pub fn perform_agent_investment(
    model: &Model,
    year: u32,
    existing_assets: &[AssetRef],
    prices: &Prices,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    // Initialise net demand map
    let mut net_demand =
        flatten_preset_demands_for_year(&model.commodities, &model.time_slice_info, year);

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

    // Iterate over investment sets in the investment order for this year
    for investment_set in investment_order {
        // Select assets for this investment set
        let selected_assets = investment_set.select_assets(
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
        for market in investment_set.iter_markets() {
            seen_markets.push(market.clone());
        }

        // If no assets have been selected, skip dispatch optimisation
        // **TODO**: this probably means there's no demand for the market, which we could
        // presumably preempt
        if selected_assets.is_empty() {
            debug!("No assets selected for '{investment_set}'");
            continue;
        }

        // Add the selected assets to the list of all selected assets
        all_selected_assets.extend(selected_assets.iter().cloned());

        // Perform dispatch optimisation with assets that have been selected so far
        // **TODO**: presumably we only need to do this for selected_assets, as assets added in
        // previous iterations should not change
        debug!("Running post-investment dispatch for '{investment_set}'");

        // As upstream markets by definition will not yet have producers, we explicitly set
        // their prices using external values so that they don't appear free
        let solution = DispatchRun::new(model, &all_selected_assets, year)
            .with_market_balance_subset(&seen_markets)
            .with_input_prices(&prices.shadow)
            .run(&format!("post {investment_set} investment"), writer)?;

        // Update demand map with flows from newly added assets
        update_net_demand_map(
            &mut net_demand,
            &solution.create_flow_map(),
            &selected_assets,
        );
    }

    Ok(all_selected_assets)
}

/// Select assets for a single market in a given year
///
/// Returns a list of assets that are selected for investment for this market in this year.
#[allow(clippy::too_many_arguments)]
fn select_assets_for_single_market(
    model: &Model,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    year: u32,
    demand: &AllDemandMap,
    existing_assets: &[AssetRef],
    prices: &Prices,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    let commodity = &model.commodities[commodity_id];

    let mut selected_assets = Vec::new();
    for (agent, commodity_portion) in
        get_responsible_agents(model.agents.values(), commodity_id, region_id, year)
    {
        debug!(
            "Running asset selection for agent '{}' in market '{}|{}'",
            &agent.id, commodity_id, region_id
        );

        // Get demand portion for this market for this agent in this year
        let demand_portion_for_market = get_demand_portion_for_market(
            &model.time_slice_info,
            demand,
            commodity_id,
            region_id,
            commodity_portion,
        );

        // Existing and candidate assets from which to choose
        let opt_assets = get_asset_options(
            existing_assets,
            &demand_portion_for_market,
            agent,
            commodity,
            region_id,
            year,
            model.parameters.capacity_limit_factor,
        )
        .collect::<Vec<_>>();

        // Calculate investment limits for candidate assets
        let investment_limits =
            collect_investment_limits_for_candidates(&opt_assets, commodity_portion);

        // Choose assets from among existing pool and candidates
        let best_assets = select_best_assets(
            model,
            opt_assets,
            investment_limits,
            commodity,
            agent,
            region_id,
            prices,
            demand_portion_for_market,
            year,
            writer,
        )?;
        selected_assets.extend(best_assets);
    }

    Ok(selected_assets)
}

/// Iterates through the a pre-ordered set of markets forming a cycle, selecting assets for each
/// market in turn.
///
/// Dispatch optimisation is performed after each market is visited to rebalance demand.
/// While dispatching, newly selected (`Ready`) assets are given flexible capacity (bounded by
/// `capacity_margin`) so small demand shifts caused by later markets can be absorbed. After all
/// markets have been visited once, the final set of assets is returned, applying any capacity
/// adjustments from the final full-system dispatch optimisation.
///
/// Dispatch may fail at any point if new demands are encountered for previously visited markets,
/// and the `capacity_margin` is not sufficient to absorb the demand shift. At this point, the
/// simulation is terminated with an error prompting the user to increase the `capacity_margin`.
/// A longer-term solution (TODO) may be to trigger re-investment for the affected markets. Other
/// yet-to-implement features may also help to stabilise the cycle, such as capacity growth limits.
#[allow(clippy::too_many_arguments)]
fn select_assets_for_cycle(
    model: &Model,
    markets: &[(CommodityID, RegionID)],
    year: u32,
    demand: &AllDemandMap,
    existing_assets: &[AssetRef],
    prices: &Prices,
    seen_markets: &[(CommodityID, RegionID)],
    previously_selected_assets: &[AssetRef],
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    // Precompute a joined string for logging
    let markets_str = markets.iter().map(|(c, r)| format!("{c}|{r}")).join(", ");

    // Iterate over the markets to select assets
    let mut current_demand = demand.clone();
    let mut assets_for_cycle = IndexMap::new();
    let mut last_solution = None;
    for (idx, (commodity_id, region_id)) in markets.iter().enumerate() {
        // Select assets for this market
        let assets = select_assets_for_single_market(
            model,
            commodity_id,
            region_id,
            year,
            &current_demand,
            existing_assets,
            prices,
            writer,
        )?;
        assets_for_cycle.insert((commodity_id.clone(), region_id.clone()), assets);

        // Assemble full list of assets for dispatch (previously selected + all chosen so far)
        let mut all_assets = previously_selected_assets.to_vec();
        let assets_for_cycle_flat: Vec<_> = assets_for_cycle
            .values()
            .flat_map(|v| v.iter().cloned())
            .collect();
        all_assets.extend_from_slice(&assets_for_cycle_flat);

        // We balance all previously seen markets plus all cycle markets up to and including this one
        let mut markets_to_balance = seen_markets.to_vec();
        markets_to_balance.extend_from_slice(&markets[0..=idx]);

        // We allow all `Ready` state assets to have flexible capacity
        let flexible_capacity_assets: Vec<_> = assets_for_cycle_flat
            .iter()
            .filter(|asset| matches!(asset.state(), AssetState::Ready { .. }))
            .cloned()
            .collect();

        // Retrieve installable capacity limits for flexible capacity assets.
        let mut agent_share_cache = HashMap::new();
        let capacity_limits = flexible_capacity_assets
            .iter()
            .filter_map(|asset| {
                let agent_id = asset.agent_id().unwrap();
                let commodity_id = asset.primary_output_commodity().unwrap();
                let agent_share = *agent_share_cache
                    .entry((agent_id, commodity_id))
                    .or_insert_with(|| {
                        model.agents[agent_id].commodity_portions[&(commodity_id.clone(), year)]
                    });
                asset
                    .max_installable_capacity(agent_share)
                    .map(|max_capacity| (asset.clone(), max_capacity))
            })
            .collect::<HashMap<_, _>>();

        // Run dispatch
        let solution = DispatchRun::new(model, &all_assets, year)
            .with_market_balance_subset(&markets_to_balance)
            .with_flexible_capacity_assets(
                &flexible_capacity_assets,
                Some(&capacity_limits),
                // Gives newly selected cycle assets limited capacity wiggle-room; existing assets stay fixed.
                model.parameters.capacity_margin,
            )
            .run(
                &format!("cycle ({markets_str}) post {commodity_id}|{region_id} investment"),
                writer,
            )
            .with_context(|| {
                format!(
                    "Cycle balancing failed for cycle ({markets_str}), capacity_margin: {}. \
                     Try increasing the capacity_margin.",
                    model.parameters.capacity_margin
                )
            })?;

        // Calculate new net demand map with all assets selected so far
        current_demand.clone_from(demand);
        update_net_demand_map(
            &mut current_demand,
            &solution.create_flow_map(),
            &assets_for_cycle_flat,
        );
        last_solution = Some(solution);
    }

    // Finally, update flexible capacity assets based on the final solution
    let mut all_cycle_assets: Vec<_> = assets_for_cycle.into_values().flatten().collect();
    if let Some(solution) = last_solution {
        let new_capacities: HashMap<_, _> = solution.iter_capacity().collect();
        for asset in &mut all_cycle_assets {
            if let Some(new_capacity) = new_capacities.get(asset) {
                debug!(
                    "Capacity of asset '{}' modified during cycle balancing ({} to {})",
                    asset.process_id(),
                    asset.total_capacity(),
                    new_capacity.total_capacity()
                );
                asset.make_mut().set_capacity(*new_capacity);
            }
        }
    }

    Ok(all_cycle_assets)
}

/// Flatten the preset commodity demands for a given year into a map of commodity, region and
/// time slice to demand.
///
/// Since demands for some commodities may be specified at a coarser time slice level, we need to
/// distribute these demands over all time slices. Note: the way that we do this distribution is
/// irrelevant, as demands will only be balanced to the appropriate level, but we still need to do
/// this for the solver to work.
///
/// **TODO**: these assumptions may need to be revisited, e.g. when we come to storage technologies
fn flatten_preset_demands_for_year(
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
    year: u32,
) -> AllDemandMap {
    let mut demand_map = AllDemandMap::new();
    for (commodity_id, commodity) in commodities {
        for ((region_id, data_year, time_slice_selection), demand) in &commodity.demand {
            if *data_year != year {
                continue;
            }

            // We split the demand equally over all time slices in the selection
            // NOTE: since demands will only be balanced to the time slice level of the commodity
            // it doesn't matter how we do this distribution, only the total matters.
            #[allow(clippy::cast_precision_loss)]
            let n_time_slices = time_slice_selection.iter(time_slice_info).count() as f64;
            let demand_per_slice = *demand / Dimensionless(n_time_slices);
            for (time_slice, _) in time_slice_selection.iter(time_slice_info) {
                demand_map.insert(
                    (commodity_id.clone(), region_id.clone(), time_slice.clone()),
                    demand_per_slice,
                );
            }
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
fn update_net_demand_map(demand: &mut AllDemandMap, flows: &FlowMap, assets: &[AssetRef]) {
    for ((asset, commodity_id, time_slice), flow) in flows {
        if assets.contains(asset) {
            let key = (
                commodity_id.clone(),
                asset.region_id().clone(),
                time_slice.clone(),
            );

            // Only consider input flows and output flows from the primary output commodity
            // (excluding secondary outputs)
            if (flow < &Flow(0.0))
                || asset
                    .primary_output()
                    .is_some_and(|p| &p.commodity.id == commodity_id)
            {
                // Note: we use the negative of the flow as input flows are negative in the flow map.
                demand
                    .entry(key)
                    .and_modify(|value| *value -= *flow)
                    .or_insert(-*flow);
            }
        }
    }
}

/// Get a portion of the demand profile for this market
fn get_demand_portion_for_market(
    time_slice_info: &TimeSliceInfo,
    demand: &AllDemandMap,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    commodity_portion: Dimensionless,
) -> DemandMap {
    time_slice_info
        .iter_ids()
        .map(|time_slice| {
            (
                time_slice.clone(),
                commodity_portion
                    * *demand
                        .get(&(commodity_id.clone(), region_id.clone(), time_slice.clone()))
                        .unwrap_or(&Flow(0.0)),
            )
        })
        .collect()
}

/// Get the agents responsible for a given market in a given year along with the commodity
/// portion for which they are responsible
fn get_responsible_agents<'a, I>(
    agents: I,
    commodity_id: &'a CommodityID,
    region_id: &'a RegionID,
    year: u32,
) -> impl Iterator<Item = (&'a Agent, Dimensionless)>
where
    I: Iterator<Item = &'a Agent>,
{
    agents.filter_map(move |agent| {
        if !agent.regions.contains(region_id) {
            return None;
        }
        let portion = agent
            .commodity_portions
            .get(&(commodity_id.clone(), year))?;

        Some((agent, *portion))
    })
}

/// Calculates a characteristic capacity scale for a candidate asset.
///
/// The returned value is the capacity that would satisfy the total annual demand assuming the asset
/// operates at its maximum annual activity for the entire year. It ignores finer-grained activity
/// constraints and temporal variations in demand.
///
/// This value is later scaled by `capacity_limit_factor` to set capacities for candidate assets in
/// each round of investments.
///
/// If the asset has zero maximum annual supply, zero capacity is returned. This indicates that the
/// asset is non-feasible, and will be excluded from consideration by `select_best_assets`.
fn calculate_candidate_asset_capacity_scale(
    asset: &Asset,
    commodity: &Commodity,
    demand: &DemandMap,
) -> Capacity {
    let coeff = asset.get_flow(&commodity.id).unwrap().coeff;
    let max_annual_supply_per_capacity = *asset
        .get_activity_per_capacity_limits_for_selection(&TimeSliceSelection::Annual)
        .end()
        * coeff;
    if max_annual_supply_per_capacity < FlowPerCapacity::EPSILON {
        return Capacity(0.0);
    }
    let annual_demand = demand.values().copied().sum::<Flow>();
    annual_demand / max_annual_supply_per_capacity
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
fn get_demand_limiting_capacity(
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
                        .map(|(bucket, _)| {
                            bucket
                                .iter(time_slice_info)
                                .map(|(ts, _)| demand[ts])
                                .sum::<Flow>()
                        })
                        .sum()
                });

            // Calculate demand-limiting capacity for this selection and take the
            // maximum across all selections.
            capacity = capacity.max(serviceable_demand_for_selection / max_supply_for_selection);
        }
    }

    capacity
}

/// Get options from existing and potential assets for the given parameters
fn get_asset_options<'a>(
    all_existing_assets: &'a [AssetRef],
    demand: &'a DemandMap,
    agent: &'a Agent,
    commodity: &'a Commodity,
    region_id: &'a RegionID,
    year: u32,
    capacity_limit_factor: Dimensionless,
) -> impl Iterator<Item = AssetRef> + 'a {
    // Get existing assets which produce the commodity of interest
    let existing_assets = all_existing_assets
        .iter()
        .filter_agent(&agent.id)
        .filter_region(region_id)
        .filter_primary_producers_of(&commodity.id)
        .cloned();

    // Get candidates assets which produce the commodity of interest
    let candidate_assets = get_candidate_assets(
        demand,
        agent,
        region_id,
        commodity,
        year,
        capacity_limit_factor,
    );

    chain(existing_assets, candidate_assets)
}

/// Get candidate assets which produce a particular commodity for a given agent
///
/// Each candidate is assigned a capacity based on a characteristic capacity scale for the asset.
/// The scale is computed from the total annual demand for the commodity and the asset's maximum
/// annual production per unit capacity (see `calculate_candidate_asset_capacity_scale`), then
/// multiplied by `capacity_limit_factor`.
fn get_candidate_assets<'a>(
    demand: &'a DemandMap,
    agent: &'a Agent,
    region_id: &'a RegionID,
    commodity: &'a Commodity,
    year: u32,
    capacity_limit_factor: Dimensionless,
) -> impl Iterator<Item = AssetRef> + 'a {
    agent
        .iter_search_space(region_id, &commodity.id, year)
        .map(move |process| {
            // Create asset with zero capacity, which will be updated below
            let mut asset =
                Asset::new_candidate(process.clone(), region_id.clone(), Capacity(0.0), year)
                    .unwrap();

            // Set capacity based on demand
            // This will serve as the upper limit when appraising the asset (may later be
            // constrained by process addition limits and demand-limiting capacity)
            let capacity_scale =
                calculate_candidate_asset_capacity_scale(&asset, commodity, demand);
            let asset_capacity = AssetCapacity::from_capacity(capacity_scale, asset.unit_size())
                .apply_limit_factor(capacity_limit_factor);
            asset.set_capacity(asset_capacity);
            asset.into()
        })
}

/// Print debug message if there are multiple equally good outputs
fn log_on_equal_appraisal_outputs(
    outputs: &[AppraisalOutput],
    agent_id: &AgentID,
    commodity_id: &CommodityID,
    region_id: &RegionID,
) {
    if outputs.is_empty() {
        return;
    }

    let num_identical = count_equal_and_best_appraisal_outputs(outputs);

    if num_identical > 0 {
        let asset_details = outputs[..=num_identical]
            .iter()
            .map(|output| {
                let asset = &output.asset;
                format!(
                    "Process ID: '{}' (State: {}{}, Commission year: {})",
                    asset.process_id(),
                    asset.state(),
                    asset
                        .id()
                        .map(|id| format!(", Asset ID: {id}"))
                        .unwrap_or_default(),
                    asset.commission_year()
                )
            })
            .join(", ");
        debug!(
            "Found equally good appraisals for Agent ID: {agent_id}, Commodity: '{commodity_id}', \
            Region: {region_id}. Options: [{asset_details}]. Selecting first option.",
        );
    }
}

/// Investment limits are based on any annual addition limits specified by the process, scaled
/// according to the agent's portion of the commodity demand and the number of years elapsed since
/// the previous milestone year.
fn collect_investment_limits_for_candidates(
    opt_assets: &[AssetRef],
    commodity_portion: Dimensionless,
) -> HashMap<AssetRef, AssetCapacity> {
    opt_assets
        .iter()
        .filter(|asset| !asset.is_commissioned())
        .filter_map(|asset| {
            asset
                .max_installable_capacity(commodity_portion)
                .map(|limit_capacity| (asset.clone(), limit_capacity))
        })
        .collect()
}

/// Get the best assets for meeting demand for the given commodity
#[allow(clippy::too_many_arguments)]
fn select_best_assets(
    model: &Model,
    mut opt_assets: Vec<AssetRef>,
    investment_limits: HashMap<AssetRef, AssetCapacity>,
    commodity: &Commodity,
    agent: &Agent,
    region_id: &RegionID,
    prices: &Prices,
    mut demand: DemandMap,
    year: u32,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    let objective_type = &agent.objectives[&year];
    let mut remaining_candidate_capacity = investment_limits;

    // Calculate coefficients for all asset options according to the agent's objective
    let coefficients =
        calculate_coefficients_for_assets(model, objective_type, &opt_assets, prices, year);

    // Iteratively select the best asset until demand is met
    let mut round = 0;
    let mut best_assets: Vec<AssetRef> = Vec::new();
    while is_any_remaining_demand(
        &demand,
        model.parameters.remaining_demand_absolute_tolerance,
    ) {
        ensure!(
            !opt_assets.is_empty(),
            "Failed to meet demand for commodity '{}' in region '{}' with provided investment \
            options. This may be due to overly restrictive process investment constraints.",
            &commodity.id,
            region_id
        );

        // Since all assets with the same `group_id` are identical, we only need to appraise one
        // from each group.
        let mut seen_groups = HashSet::new();

        // Appraise all options
        let mut outputs_for_opts = Vec::new();
        for asset in &opt_assets {
            // Skip any assets from groups we've already seen
            if let Some(group_id) = asset.group_id()
                && !seen_groups.insert(group_id)
            {
                continue;
            }

            // For candidates, cap the asset's capacity by the current demand-limiting capacity
            // and, where an addition constraint exists, the remaining installable capacity.
            let mut asset = asset.clone();
            if !asset.is_commissioned() {
                let dlc = AssetCapacity::from_capacity(
                    get_demand_limiting_capacity(
                        &model.time_slice_info,
                        &asset,
                        commodity,
                        &demand,
                    ),
                    asset.unit_size(),
                );
                let cap = asset.capacity().min(dlc);
                let max_capacity = remaining_candidate_capacity
                    .get(&asset)
                    .copied()
                    .map_or(cap, |remaining| cap.min(remaining));
                asset.make_mut().set_capacity(max_capacity);
            }

            // Skip assets with zero capacity
            if asset.capacity().total_capacity() <= Capacity(0.0) {
                continue;
            }

            let output = appraise_investment(
                model,
                &asset,
                commodity,
                objective_type,
                &coefficients[&asset],
                &demand,
            )?;
            outputs_for_opts.push(output);
        }

        // Save appraisal results
        writer.write_appraisal_debug_info(
            year,
            &format!("{} {} round {}", &commodity.id, &agent.id, round),
            &outputs_for_opts,
            &demand,
        )?;

        // Sort by investment priority and discard non-feasible options
        let num_nonfeasible = sort_and_filter_appraisal_outputs(&mut outputs_for_opts);

        // If none of the remaining options are feasible, we terminate the loop. We may still be
        // able to meet the full demands with assets selected so far, so we continue anyway with a
        // warning.
        if outputs_for_opts.is_empty() {
            warn!(
                "Investment appraisal completed with unmet demand for commodity '{}', region '{}', \
                year '{}', agent '{}'. {} non-feasible investments were not considered. \
                This unmet demand may still be satisfied during the full system dispatch.",
                &commodity.id, region_id, year, agent.id, num_nonfeasible
            );
            break;
        }

        // Warn if there are multiple equally good assets
        log_on_equal_appraisal_outputs(&outputs_for_opts, &agent.id, &commodity.id, region_id);

        let best_output = outputs_for_opts.into_iter().next().unwrap();

        // Log the selected asset
        debug!(
            "Selected {} asset '{}' (capacity: {})",
            &best_output.asset.state(),
            &best_output.asset.process_id(),
            best_output.asset.capacity().total_capacity()
        );

        // Update the assets and remaining candidate capacity
        update_assets(
            best_output.asset,
            &mut opt_assets,
            &mut remaining_candidate_capacity,
            &mut best_assets,
        );

        demand = best_output.unmet_demand;
        round += 1;
    }

    // Convert Candidate assets to Ready
    // At this point we also assign the agent ID to the asset
    for asset in &mut best_assets {
        if let AssetState::Candidate = asset.state() {
            asset
                .make_mut()
                .select_candidate_for_investment(agent.id.clone());
        }
    }

    Ok(best_assets)
}

/// Check whether there is any remaining demand that is unmet in any time slice
fn is_any_remaining_demand(demand: &DemandMap, absolute_tolerance: Flow) -> bool {
    demand.values().any(|flow| *flow > absolute_tolerance)
}

/// Update capacity of chosen asset, if needed, and update both asset options and chosen assets
fn update_assets(
    best_asset: AssetRef,
    opt_assets: &mut Vec<AssetRef>,
    remaining_candidate_capacity: &mut HashMap<AssetRef, AssetCapacity>,
    best_assets: &mut Vec<AssetRef>,
) {
    match best_asset.state() {
        AssetState::Commissioned { .. } => {
            // Remove this asset from the options
            opt_assets.retain(|asset| *asset != best_asset);
            best_assets.push(best_asset);
        }
        AssetState::Candidate => {
            // Track remaining capacity for assets that have limits
            if let Some(remaining_capacity) = remaining_candidate_capacity.get_mut(&best_asset) {
                *remaining_capacity = *remaining_capacity - best_asset.capacity();

                // If there's no capacity remaining, remove the asset from the options
                if remaining_capacity.total_capacity() <= Capacity(0.0) {
                    let old_idx = opt_assets
                        .iter()
                        .position(|asset| *asset == best_asset)
                        .unwrap();

                    opt_assets.swap_remove(old_idx);
                    remaining_candidate_capacity.remove(&best_asset);
                }
            }

            if let Some(existing_asset) = best_assets.iter_mut().find(|asset| **asset == best_asset)
            {
                // If the asset is already in the list of best assets, add the additional required capacity
                existing_asset
                    .make_mut()
                    .increase_capacity(best_asset.capacity());
            } else {
                // Otherwise add it to the list of best assets
                best_assets.push(best_asset);
            }
        }
        _ => panic!("update_assets should only be called with Commissioned or Candidate assets"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{
        asset, process, process_activity_limits_map, process_flows_map, process_parameter_map,
        region_id, svd_commodity, time_slice, time_slice_info, time_slice_info2,
    };
    use crate::process::{
        ActivityLimits, FlowType, Process, ProcessActivityLimitsMap, ProcessFlow, ProcessFlowsMap,
        ProcessInvestmentConstraint, ProcessInvestmentConstraintsMap, ProcessParameterMap,
    };
    use crate::region::RegionID;
    use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceSelection};
    use crate::units::Dimensionless;
    use crate::units::{ActivityPerCapacity, Flow, FlowPerActivity, MoneyPerFlow};
    use indexmap::{IndexSet, indexmap};
    use rstest::{fixture, rstest};
    use std::rc::Rc;
    use std::slice::from_ref;

    #[rstest]
    fn get_demand_limiting_capacity_works(
        time_slice: TimeSliceID,
        time_slice_info: TimeSliceInfo,
        svd_commodity: Commodity,
        mut process: Process,
    ) {
        // Add flows for the process using the existing commodity fixture
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(2.0), // 2 units of flow per unit of activity
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        let process_flows_map = process_flows_map(process.regions.clone(), Rc::new(process_flows));
        process.flows = process_flows_map;

        // Create asset with the configured process
        let asset = asset(process);

        // Create demand map - demand of 10.0 for our time slice
        let demand = indexmap! { time_slice.clone() => Flow(10.0)};

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
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0), // 1 unit of flow per unit of activity
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        let process_flows_map = process_flows_map(process.regions.clone(), Rc::new(process_flows));
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
            time_slice1.clone() => Flow(4.0), // Requires capacity of 4.0/0.2 = 20.0
            time_slice2.clone() => Flow(3.0), // Would require infinite capacity, but should be skipped
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
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        process.flows = process_flows_map(process.regions.clone(), Rc::new(process_flows));

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
            time_slice1.clone() => Flow(5.0),
            time_slice2.clone() => Flow(5.0),
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
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        process.flows = process_flows_map(
            process.regions.clone(),
            Rc::new(indexmap! { commodity_rc.id.clone() => process_flow }),
        );

        let mut limits = ActivityLimits::new_with_full_availability(&time_slice_info);
        limits.add_time_slice_limit(time_slice.clone(), Dimensionless(0.0)..=activity_limit);
        process.activity_limits = process_activity_limits_map(process.regions.clone(), limits);

        let asset = asset(process);
        let demand = indexmap! { time_slice => demand_value };
        assert_eq!(
            calculate_candidate_asset_capacity_scale(&asset, &commodity_rc, &demand),
            expected
        );
    }

    #[rstest]
    fn collect_investment_limits_for_candidates_empty_list() {
        let result = collect_investment_limits_for_candidates(&[], Dimensionless(1.0));
        assert!(result.is_empty());
    }

    #[fixture]
    fn commissioned_asset(asset: Asset) -> AssetRef {
        asset.into()
    }

    #[fixture]
    fn uncommissioned_asset_without_limit(process: Process, region_id: RegionID) -> AssetRef {
        Asset::new_candidate(Rc::new(process), region_id, Capacity(10.0), 2015)
            .unwrap()
            .into()
    }

    #[fixture]
    fn uncommissioned_asset_with_limit(
        region_id: RegionID,
        process_activity_limits_map: ProcessActivityLimitsMap,
        process_flows_map: ProcessFlowsMap,
        process_parameter_map: ProcessParameterMap,
    ) -> AssetRef {
        let region_ids: IndexSet<RegionID> = [region_id.clone()].into();

        let mut constraints = ProcessInvestmentConstraintsMap::new();

        constraints.insert(
            (region_id.clone(), 2015),
            Rc::new(ProcessInvestmentConstraint {
                addition_limit: Some(Capacity(10.0)),
            }),
        );

        let process = Process {
            id: "constrained_process".into(),
            description: String::new(),
            years: 2010..=2020,
            activity_limits: process_activity_limits_map,
            flows: process_flows_map,
            parameters: process_parameter_map,
            regions: region_ids,
            primary_output: None,
            capacity_to_activity: ActivityPerCapacity(1.0),
            investment_constraints: constraints,
            unit_size: None,
        };

        Asset::new_candidate(Rc::new(process), region_id, Capacity(15.0), 2015)
            .unwrap()
            .into()
    }

    #[rstest]
    fn commissioned_assets_are_excluded(commissioned_asset: AssetRef) {
        let result = collect_investment_limits_for_candidates(
            from_ref(&commissioned_asset),
            Dimensionless(1.0),
        );

        assert!(!result.contains_key(&commissioned_asset));
    }

    #[rstest]
    fn candidate_assets_without_limits_are_excluded(uncommissioned_asset_without_limit: AssetRef) {
        let result = collect_investment_limits_for_candidates(
            from_ref(&uncommissioned_asset_without_limit),
            Dimensionless(1.0),
        );

        assert!(!result.contains_key(&uncommissioned_asset_without_limit));
    }

    #[rstest]
    fn candidate_assets_with_limits_are_included(uncommissioned_asset_with_limit: AssetRef) {
        let result = collect_investment_limits_for_candidates(
            from_ref(&uncommissioned_asset_with_limit),
            Dimensionless(1.0),
        );

        assert!(result.contains_key(&uncommissioned_asset_with_limit));
    }
}
