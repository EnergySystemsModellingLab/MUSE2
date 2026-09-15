//! Code for creating sets of markets.
use super::optimisation::{DispatchRun, FlowMap};
use crate::agent::Agent;
use crate::asset::{Asset, AssetCapacity, AssetIterator, AssetRef};
use crate::commodity::{Commodity, CommodityID};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::{Process, ProcessID};
use crate::region::RegionID;
use crate::simulation::investment::{
    AllDemandMap, DemandMap, calculate_candidate_asset_capacity_scale, select_best_assets,
    update_net_demand_map,
};
use crate::simulation::prices::Prices;
use crate::time_slice::{TimeSliceInfo, TimeSliceLevel};
use crate::units::{Capacity, Dimensionless, Flow};
use anyhow::{Context, Result, bail};
use itertools::{Itertools, chain};
use log::debug;
use std::collections::HashMap;
use std::fmt::Display;

/// Represents a set of markets which are invested in together.
#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub enum MarketSet {
    /// Assets are selected for a single market using [`select_assets_for_single_market`]
    Single((CommodityID, RegionID)),
    /// Assets are selected for a group of markets which forms a cycle.
    /// Experimental: handled by [`select_assets_for_cycle`] and guarded by the broken options
    /// parameter.
    Cycle {
        /// Investment order
        investment_order: Vec<(CommodityID, RegionID)>,
        /// Processes to exclude from the second pass
        excluded_processes: Vec<ProcessID>,
    },
    /// Assets are selected for a layer of independent [`MarketSet`]s
    Layer(Vec<MarketSet>),
}

impl MarketSet {
    /// Recursively iterate over all markets contained in this `MarketSet`.
    pub fn iter_markets<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = &'a (CommodityID, RegionID)> + 'a> {
        match self {
            MarketSet::Single(market) => Box::new(std::iter::once(market)),
            MarketSet::Cycle {
                investment_order, ..
            } => Box::new(investment_order.iter()),
            MarketSet::Layer(set) => Box::new(set.iter().flat_map(|s| s.iter_markets())),
        }
    }

    /// Selects assets for this market set variant and passes through the shared
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
    /// * `previously_selected_assets` – Assets chosen in earlier market sets.
    /// * `writer` – Data sink used to log optimisation artefacts.
    #[allow(clippy::too_many_arguments)]
    pub fn select_assets(
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
            MarketSet::Single((commodity_id, region_id)) => select_assets_for_single_market(
                model,
                commodity_id,
                region_id,
                year,
                demand,
                existing_assets,
                prices,
                &[],
                writer,
            ),
            MarketSet::Cycle { .. } => {
                debug!("Starting investment for cycle '{self}'");
                select_assets_for_cycle(
                    model,
                    self,
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
            MarketSet::Layer(investment_sets) => {
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

impl Display for MarketSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MarketSet::Single((commodity_id, region_id)) => {
                write!(f, "{commodity_id}|{region_id}")
            }
            MarketSet::Cycle {
                investment_order, ..
            } => {
                write!(
                    f,
                    "({})",
                    investment_order
                        .iter()
                        .map(|(c, r)| format!("{c}|{r}"))
                        .join(", ")
                )
            }
            MarketSet::Layer(ids) => {
                write!(f, "[{}]", ids.iter().join(", "))
            }
        }
    }
}

/// Select assets for a single market in a given year
///
/// Returns a list of assets that are selected for investment for this market in this year.
#[allow(clippy::too_many_arguments)]
pub fn select_assets_for_single_market(
    model: &Model,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    year: u32,
    demand: &AllDemandMap,
    existing_assets: &[AssetRef],
    prices: &Prices,
    excluded_processes: &[ProcessID],
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    let commodity = &model.commodities[commodity_id];

    let mut selected_assets = Vec::new();
    for (agent, commodity_portion) in
        get_responsible_agents(model.agents.values(), commodity_id, region_id, year)
    {
        debug!(
            "Running asset selection for agent '{}' in market '{}|{}'",
            agent.id, commodity_id, region_id
        );

        // Get demand portion for this market for this agent in this year
        let demand_portion_for_market = get_demand_portion_for_market(
            &model.time_slice_info,
            demand,
            commodity_id,
            region_id,
            commodity_portion,
            commodity.time_slice_level,
        );

        // Existing and candidate assets from which to choose
        let mut opt_assets = get_asset_options(
            existing_assets,
            &demand_portion_for_market,
            agent,
            commodity,
            region_id,
            year,
            model.parameters.capacity_tranche_fraction,
        )
        .collect::<Vec<_>>();

        // Exclude certain processes from the options
        opt_assets.retain(|asset| !excluded_processes.contains(asset.process_id()));

        // Calculate the agent's share of addition limits for candidate processes
        let agent_addition_limits = collect_agent_limits(
            agent,
            region_id,
            commodity_id,
            year,
            commodity_portion,
            Process::agent_addition_limit,
        );

        // Calculate the agent's share of total capacity limits for all processes
        let agent_total_limits = collect_agent_limits(
            agent,
            region_id,
            commodity_id,
            year,
            commodity_portion,
            Process::agent_total_limit,
        );

        // Choose assets from among existing pool and candidates
        let best_assets = select_best_assets(
            model,
            opt_assets,
            agent_addition_limits,
            agent_total_limits,
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

/// Iterates through a pre-ordered set of markets forming a cycle, selecting assets for each
/// market in turn.
///
/// Dispatch optimisation is performed after each market is visited.
///
/// Dispatch may fail at any point if new demands are encountered for previously visited markets.
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub fn select_assets_for_cycle(
    model: &Model,
    market_set: &MarketSet,
    year: u32,
    demand: &AllDemandMap,
    existing_assets: &[AssetRef],
    prices: &Prices,
    _seen_markets: &[(CommodityID, RegionID)],
    _previously_selected_assets: &[AssetRef],
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    let MarketSet::Cycle {
        investment_order, ..
    } = market_set
    else {
        bail!("expected MarketSet::Cycle");
    };

    // Precompute a joined string for logging
    let markets_str = investment_order
        .iter()
        .map(|(c, r)| format!("{c}|{r}"))
        .join(", ");

    // Feedback processes are excluded from the second pass while their first-pass flows are
    // retained as part of the demand carried into that pass.
    let feedback_processes = model
        .processes
        .iter()
        .filter(|(_, process)| process.feedback_process)
        .map(|(process_id, _)| process_id.clone())
        .collect::<Vec<_>>();

    // STEP 1
    // Iterate over the markets in order1, considering all processes
    let mut net_demand = demand.clone();
    let mut assets_for_first_pass = Vec::new();
    let mut retained_first_pass_assets = Vec::new();
    let mut excluded_flows = FlowMap::new();
    for market in investment_order {
        let (commodity_id, region_id) = market.clone();

        // Select assets for this market
        debug!("Running {commodity_id}|{region_id} selection pass 1");
        let selected_assets = select_assets_for_single_market(
            model,
            &commodity_id,
            &region_id,
            year,
            &net_demand,
            existing_assets,
            prices,
            &[],
            writer,
        )?;
        debug!("Completed {commodity_id}|{region_id} selection pass 1");

        // If no assets have been selected, skip to the next market
        if selected_assets.is_empty() {
            debug!("No assets selected for '{commodity_id}|{region_id}'");
            continue;
        }
        assets_for_first_pass.extend(selected_assets.iter().cloned());
        retained_first_pass_assets.extend(
            selected_assets
                .iter()
                .filter(|asset| feedback_processes.contains(asset.process_id()))
                .cloned(),
        );

        // Run dispatch
        debug!("Running cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 1");
        let solution = DispatchRun::new(model, &selected_assets, year, &net_demand)
            .without_commodity_constraints()
            .with_market_balance_subset(std::slice::from_ref(market))
            .run(
                &format!("cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 1"),
                writer,
            )
            .with_context(|| format!("Dispatch failed for cycle ({markets_str})"))?;
        debug!("Completed cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 1");

        // Update demand map with flows from newly selected assets
        let flows = solution.create_flow_map();
        for ((asset, commodity_id, time_slice), flow) in &flows {
            if feedback_processes.contains(asset.process_id()) {
                excluded_flows
                    .entry((asset.clone(), commodity_id.clone(), time_slice.clone()))
                    .and_modify(|stored_flow| *stored_flow += *flow)
                    .or_insert(*flow);
            }
        }
        update_net_demand_map(
            &mut net_demand,
            &flows,
            &selected_assets,
            &model.commodities,
        );
    }

    // STEP 2
    // Iterate over the markets in order2, excluding feedback processes
    let mut net_demand = demand.clone();
    update_net_demand_map(
        &mut net_demand,
        &excluded_flows,
        &assets_for_first_pass,
        &model.commodities,
    );
    let mut assets_for_second_pass = Vec::new();
    for market in investment_order {
        let (commodity_id, region_id) = market.clone();

        // Select assets for this market
        debug!("Running {commodity_id}|{region_id} selection pass 2");
        let selected_assets = select_assets_for_single_market(
            model,
            &commodity_id,
            &region_id,
            year,
            &net_demand,
            existing_assets,
            prices,
            &feedback_processes,
            writer,
        )?;
        debug!("Completed {commodity_id}|{region_id} selection pass 2");

        // If no assets have been selected, skip to the next market
        if selected_assets.is_empty() {
            debug!("No assets selected for '{commodity_id}|{region_id}'");
            continue;
        }
        assets_for_second_pass.extend(selected_assets.iter().cloned());

        // // Assemble full list of assets for dispatch (previously selected + all chosen so far)
        // let mut all_assets = previously_selected_assets.to_vec();
        // all_assets.extend(assets_for_first_pass.iter().cloned());
        // all_assets.extend(assets_for_second_pass.iter().cloned());

        // // We balance all previously seen markets plus all cycle markets up to and including this one
        // let mut markets_to_balance = seen_markets.to_vec();
        // markets_to_balance.extend_from_slice(&investment_order[0..=idx]);

        // Run dispatch
        debug!("Running cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 2");
        let solution = DispatchRun::new(model, &selected_assets, year, &net_demand)
            .without_commodity_constraints()
            .with_market_balance_subset(std::slice::from_ref(market))
            .run(
                &format!("cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 2"),
                writer,
            )
            .with_context(|| format!("Dispatch failed for cycle ({markets_str})"))?;
        debug!("Completed cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 2");

        // Update demand map with flows from newly selected assets
        update_net_demand_map(
            &mut net_demand,
            &solution.create_flow_map(),
            &selected_assets,
            &model.commodities,
        );
    }

    Ok(retained_first_pass_assets
        .into_iter()
        .chain(assets_for_second_pass)
        .collect())
}

/// Get a portion of the demand profile for this market
pub fn get_demand_portion_for_market(
    time_slice_info: &TimeSliceInfo,
    demand: &AllDemandMap,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    commodity_portion: Dimensionless,
    balance_level: TimeSliceLevel,
) -> DemandMap {
    time_slice_info
        .iter_selections_at_level(balance_level)
        .map(|selection| {
            let demand = *demand
                .get(&(commodity_id.clone(), region_id.clone(), selection.clone()))
                .unwrap_or(&Flow(0.0));
            (selection, commodity_portion * demand)
        })
        .collect()
}

/// Get the agents responsible for a given market in a given year along with the commodity
/// portion for which they are responsible
pub fn get_responsible_agents<'a, I>(
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

/// Get options from existing and potential assets for the given parameters
pub fn get_asset_options<'a>(
    all_existing_assets: &'a [AssetRef],
    demand: &'a DemandMap,
    agent: &'a Agent,
    commodity: &'a Commodity,
    region_id: &'a RegionID,
    year: u32,
    capacity_tranche_fraction: Dimensionless,
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
        capacity_tranche_fraction,
    );

    chain(existing_assets, candidate_assets)
}

/// Get candidate assets which produce a particular commodity for a given agent
///
/// Each candidate represents one investment tranche with a defined capacity.
/// - For processes with a defined `tranche_size`, the capacity is set to `tranche_size`.
/// - For processes without a defined `tranche_size`, the capacity is calculated based on the total
///   demand for the commodity and the asset's maximum annual production per unit capacity
///   (see `calculate_candidate_asset_capacity_scale`), then multiplied by
///   `capacity_tranche_fraction` to infer the tranche size.
fn get_candidate_assets<'a>(
    demand: &'a DemandMap,
    agent: &'a Agent,
    region_id: &'a RegionID,
    commodity: &'a Commodity,
    year: u32,
    capacity_tranche_fraction: Dimensionless,
) -> impl Iterator<Item = AssetRef> + 'a {
    agent
        .iter_search_space(region_id, &commodity.id, year)
        .map(move |process| {
            // Create asset with zero capacity, which will be updated below
            let mut asset =
                Asset::new_candidate(process.clone(), region_id.clone(), Capacity(0.0), year)
                    .unwrap();

            // Set capacity of the candidate for investment appraisal
            let tranche_size = if let Some(tranche_size) = asset.process().tranche_size {
                // For processes with a defined tranche size, take this
                tranche_size
            } else {
                // Otherwise, infer the tranche size from demand and the capacity_tranche_fraction.
                let capacity_scale =
                    calculate_candidate_asset_capacity_scale(&asset, commodity, demand);
                capacity_scale * capacity_tranche_fraction
            };
            // println!("DEMAND: {demand:?}");
            let asset_capacity = AssetCapacity::single(tranche_size);
            asset.set_capacity(asset_capacity);
            asset.into()
        })
}

/// Collects capacity limits for all processes in the agent's search space for a given market.
///
/// Processes without a defined limit are excluded from the returned map. The limit type is
/// determined by `get_agent_limit`, which should be one of [`Process::agent_addition_limit`] (the
/// agent's share of the annual addition limit) or [`Process::agent_total_limit`] (the agent's share
/// of the maximum total installed capacity).
///
/// # Arguments
///
/// * `agent` – Agent whose search space is queried.
/// * `region_id` – Region for which limits are calculated.
/// * `commodity_id` – Commodity for which limits are calculated.
/// * `year` – Milestone year being solved.
/// * `commodity_portion` – Agent's fractional share of commodity demand, used to scale limits.
/// * `get_agent_limit` – Method on [`Process`] that returns the limit value.
pub fn collect_agent_limits(
    agent: &Agent,
    region_id: &RegionID,
    commodity_id: &CommodityID,
    year: u32,
    commodity_portion: Dimensionless,
    get_agent_limit: fn(&Process, &RegionID, u32, Dimensionless) -> Option<Capacity>,
) -> HashMap<ProcessID, Capacity> {
    agent
        .iter_search_space(region_id, commodity_id, year)
        .filter_map(|process| {
            get_agent_limit(process, region_id, year, commodity_portion)
                .map(|limit| (process.id.clone(), limit))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{Agent, AgentCommodityPortionsMap, AgentObjectiveMap, DecisionRule};
    use crate::fixture::{process, region_id};
    use crate::process::{Process, ProcessInvestmentConstraint};
    use crate::region::RegionID;
    use crate::units::{Capacity, Dimensionless};
    use rstest::rstest;
    use std::collections::HashMap;
    use std::sync::Arc;

    fn agent_with_process(
        process: Process,
        region_id: &RegionID,
        commodity_id: &CommodityID,
    ) -> Agent {
        let mut search_space = HashMap::new();
        search_space.insert(
            (commodity_id.clone(), region_id.clone(), 2015),
            Arc::new(vec![Arc::new(process)]),
        );
        Agent {
            id: "agent1".into(),
            description: String::new(),
            commodity_portions: AgentCommodityPortionsMap::new(),
            search_space,
            decision_rule: DecisionRule::Single,
            regions: [region_id.clone()].into(),
            objectives: AgentObjectiveMap::new(),
        }
    }

    #[rstest]
    fn collect_agent_limits_uses_search_space(mut process: Process, region_id: RegionID) {
        process.investment_constraints.insert(
            (region_id.clone(), 2015),
            Arc::new(ProcessInvestmentConstraint {
                addition_limit: Some(Capacity(10.0)),
                total_capacity_limit: Some(Capacity(100.0)),
            }),
        );
        let commodity_id = "commodity".into();
        let process_id = process.id.clone();
        let agent = agent_with_process(process, &region_id, &commodity_id);

        let result = collect_agent_limits(
            &agent,
            &region_id,
            &commodity_id,
            2015,
            Dimensionless(0.5),
            Process::agent_addition_limit,
        );

        assert_eq!(result.get(&process_id), Some(&Capacity(5.0)));

        let result = collect_agent_limits(
            &agent,
            &region_id,
            &commodity_id,
            2015,
            Dimensionless(0.5),
            Process::agent_total_limit,
        );

        assert_eq!(result.get(&process_id), Some(&Capacity(50.0)));
    }

    #[rstest]
    fn collect_agent_limits_excludes_processes_without_limits(
        process: Process,
        region_id: RegionID,
    ) {
        let commodity_id = "commodity".into();
        let process_id = process.id.clone();
        let agent = agent_with_process(process, &region_id, &commodity_id);

        let result = collect_agent_limits(
            &agent,
            &region_id,
            &commodity_id,
            2015,
            Dimensionless(1.0),
            Process::agent_addition_limit,
        );

        assert!(!result.contains_key(&process_id));
    }
}
