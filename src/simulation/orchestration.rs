//! Coordinates agent runs across markets and planning years.
use super::activity::FlowMap;
use super::agent::run_agent;
use super::agent::{AllDemandMap, AssetOptions, DemandMap, Selection};
use super::dispatch::DispatchRun;
use crate::agent::Agent;
use crate::asset::{AssetIterator, AssetRef};
use crate::commodity::{Commodity, CommodityID, CommodityMap};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::{Process, ProcessID};
use crate::region::RegionID;
use crate::simulation::market::MarketSet;
use crate::simulation::prices::Prices;
use crate::time_slice::TimeSliceInfo;
use crate::timeit::InvestmentTimer;
use crate::units::{Capacity, Dimensionless, Flow};
use anyhow::{Context, Result};
use context_manager;
use indexmap::IndexSet;
use itertools::Itertools;
use log::debug;
use std::collections::HashMap;

/// How agents are run for a milestone year
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AgentRunMode {
    /// Agents dispatch their existing assets as committed assets, without investment options
    Dispatch,
    /// Agents choose which existing assets to retain and which candidates to invest in
    Invest,
}

/// Runs agents for a given market set, and passes through the shared context needed by
/// single-market, cycle, or layered runs.
///
/// # Arguments
///
/// * `market_set` – Market set to run.
/// * `model` – Simulation model supplying parameters, processes, and dispatch.
/// * `year` – Planning year being solved.
/// * `mode` – Whether agents dispatch committed assets or make investment decisions.
/// * `demand` – Net demand profiles available to all markets before selection.
/// * `assets` – Assets available to agents in this market.
/// * `prices` – Commodity price assumptions to use when valuing investments.
/// * `writer` – Data sink used to log optimisation artefacts.
///
/// # Returns
///
/// The selected assets along with their activities from the agent runs.
#[allow(clippy::too_many_arguments)]
pub fn run_agents_for_market_set(
    market_set: &MarketSet,
    model: &Model,
    year: u32,
    mode: AgentRunMode,
    demand: &AllDemandMap,
    assets: &[AssetRef],
    prices: &Prices,
    writer: &mut DataWriter,
) -> Result<Selection> {
    match market_set {
        MarketSet::Single((commodity_id, region_id)) => run_agents_for_single_market(
            model,
            commodity_id,
            region_id,
            year,
            mode,
            demand,
            assets,
            prices,
            &IndexSet::new(),
            writer,
        ),
        MarketSet::Cycle {
            markets,
            feedback_processes,
        } => {
            debug!(
                "Starting agent runs for cycle '{market_set}' with feedback processes: [{}]",
                feedback_processes
                    .iter()
                    .map(|(process_id, region_id)| format!("{process_id}|{region_id}"))
                    .join(", ")
            );
            run_agents_for_cycle(
                model,
                markets,
                feedback_processes,
                year,
                mode,
                demand,
                assets,
                prices,
                writer,
            )
            .with_context(|| {
                format!(
                    "Investments failed for market set {market_set} with cyclical dependencies. \
                         Please note that the investment algorithm is currently experimental for \
                         models with circular commodity dependencies and may not be able to find \
                         a solution in all cases."
                )
            })
        }
        MarketSet::Layer(investment_sets) => {
            debug!("Starting agent runs for layer '{market_set}'");
            let mut layer_selection = Selection::default();
            for investment_set in investment_sets {
                let selection = run_agents_for_market_set(
                    investment_set,
                    model,
                    year,
                    mode,
                    demand,
                    assets,
                    prices,
                    writer,
                )?;
                layer_selection.extend(selection);
            }
            debug!("Completed agent runs for layer '{market_set}'");
            Ok(layer_selection)
        }
    }
}

/// Run the responsible agents for a single market in a given year.
///
/// Returns the assets returned by the agents for this market in this year, along with their
/// activities.
#[allow(clippy::too_many_arguments)]
pub fn run_agents_for_single_market(
    model: &Model,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    year: u32,
    mode: AgentRunMode,
    demand: &AllDemandMap,
    assets: &[AssetRef],
    prices: &Prices,
    excluded_processes: &IndexSet<ProcessID>,
    writer: &mut DataWriter,
) -> Result<Selection> {
    let commodity = &model.commodities[commodity_id];

    let mut market_selection = Selection::default();
    for (agent, commodity_portion) in
        get_responsible_agents(model.agents.values(), commodity_id, region_id, year)
    {
        debug!(
            "Running agent '{}' in market '{}|{}'",
            agent.id, commodity_id, region_id
        );

        // Get demand portion for this market for this agent in this year
        let demand_portion_for_market = get_demand_portion_for_market(
            &model.time_slice_info,
            demand,
            commodity,
            region_id,
            commodity_portion,
        );

        // Collect the relevant assets owned by this agent
        let agent_assets: Vec<AssetRef> = assets
            .iter()
            .filter_agent(&agent.id)
            .filter_region(region_id)
            .filter_primary_producers_of(commodity_id)
            .cloned()
            .collect();

        let (options, agent_addition_limits, agent_total_limits) = match mode {
            AgentRunMode::Dispatch => (
                AssetOptions {
                    committed: agent_assets,
                    existing: Vec::new(),
                    processes: Vec::new(),
                },
                HashMap::new(),
                HashMap::new(),
            ),
            AgentRunMode::Invest => {
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

                // Filter available assets and processes before running the agent
                let options = AssetOptions {
                    committed: Vec::new(),
                    existing: agent_assets
                        .iter()
                        .filter(|asset| !excluded_processes.contains(asset.process_id()))
                        .cloned()
                        .collect(),
                    processes: agent
                        .iter_search_space(region_id, commodity_id, year)
                        .filter(|process| !excluded_processes.contains(&process.id))
                        .cloned()
                        .collect(),
                };
                (options, agent_addition_limits, agent_total_limits)
            }
        };

        // Choose assets from among existing pool and candidates
        let selection = run_agent(
            model,
            agent,
            commodity,
            region_id,
            &demand_portion_for_market,
            &options,
            agent_addition_limits,
            agent_total_limits,
            prices,
            year,
            writer.debug_writer(),
        )?;
        market_selection.extend(selection);
    }

    Ok(market_selection)
}

/// Iterates through a pre-ordered set of markets forming a cycle, running agents for each market in
/// turn.
///
/// `feedback_processes` identifies process-region pairs that produce a cycle market and consume a
/// market earlier in the investment order. In `Invest` mode, assets for these pairs are selected
/// in the first pass and retained, then the remaining assets are selected in a second pass that
/// excludes those processes. In `Dispatch` mode, there is no reselection, so all assets from the
/// first pass are returned. A dispatch optimisation after each market updates demand for subsequent
/// markets.
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub fn run_agents_for_cycle(
    model: &Model,
    markets: &[(CommodityID, RegionID)],
    feedback_processes: &IndexSet<(ProcessID, RegionID)>,
    year: u32,
    mode: AgentRunMode,
    demand: &AllDemandMap,
    assets: &[AssetRef],
    prices: &Prices,
    writer: &mut DataWriter,
) -> Result<Selection> {
    // Precompute a joined string for logging
    let markets_str = markets.iter().map(|(c, r)| format!("{c}|{r}")).join(", ");
    let is_feedback_asset = |asset: &AssetRef| {
        feedback_processes.contains(&(asset.process_id().clone(), asset.region_id().clone()))
    };

    // First pass: iterate over the markets in order, considering all processes
    let mut net_demand = demand.clone();
    let mut assets_for_first_pass = Vec::new();
    let mut retained_first_pass = Selection::default();
    let mut excluded_flows = FlowMap::new();
    for market in markets {
        let (commodity_id, region_id) = market.clone();

        // Select assets for this market
        debug!("Running {commodity_id}|{region_id} selection pass 1");
        let selection = run_agents_for_single_market(
            model,
            &commodity_id,
            &region_id,
            year,
            mode,
            &net_demand,
            assets,
            prices,
            &IndexSet::new(),
            writer,
        )?;
        debug!("Completed {commodity_id}|{region_id} selection pass 1");

        // If no assets have been selected, skip to the next market
        if selection.is_empty() {
            debug!("No assets selected for '{commodity_id}|{region_id}'");
            continue;
        }

        if mode == AgentRunMode::Invest {
            assets_for_first_pass.extend(selection.assets.iter().cloned());
        }
        let mut retained = selection.clone();
        retained.retain(|asset| mode == AgentRunMode::Dispatch || is_feedback_asset(asset));
        retained_first_pass.extend(retained);

        // Run dispatch
        debug!("Running cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 1");
        let solution = DispatchRun::new(model, &selection.assets, year, &net_demand)
            .without_commodity_constraints()
            .with_market_balance_subset(std::slice::from_ref(market))
            .with_input_prices(&prices.shadow)
            .run(
                &format!("cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 1"),
                writer,
            )
            .with_context(|| format!("Dispatch failed for cycle ({markets_str})"))?;
        debug!("Completed cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 1");

        // Update demand map with flows from newly selected assets
        let flows = solution.create_flow_map();
        for ((asset, commodity_id, time_slice), flow) in &flows {
            if mode == AgentRunMode::Invest && is_feedback_asset(asset) {
                excluded_flows
                    .entry((asset.clone(), commodity_id.clone(), time_slice.clone()))
                    .and_modify(|stored_flow| *stored_flow += *flow)
                    .or_insert(*flow);
            }
        }
        update_net_demand_map(&mut net_demand, &flows, &selection.assets);
    }

    if mode == AgentRunMode::Dispatch {
        return Ok(retained_first_pass);
    }

    // Second pass: iterate over the markets in the same order, excluding feedback processes, with
    // demand updated to account for the flows of feedback assets retained from the first pass
    let mut net_demand = demand.clone();
    update_net_demand_map(&mut net_demand, &excluded_flows, &assets_for_first_pass);
    let mut second_pass = Selection::default();
    for market in markets {
        let (commodity_id, region_id) = market.clone();
        let feedback_processes: IndexSet<ProcessID> = feedback_processes
            .iter()
            .filter_map(|(p, r)| (*r == region_id).then_some(p.clone()))
            .collect();

        // Select assets for this market
        debug!("Running {commodity_id}|{region_id} selection pass 2");
        let selection = run_agents_for_single_market(
            model,
            &commodity_id,
            &region_id,
            year,
            mode,
            &net_demand,
            assets,
            prices,
            &feedback_processes,
            writer,
        )?;
        debug!("Completed {commodity_id}|{region_id} selection pass 2");

        // If no assets have been selected, skip to the next market
        if selection.is_empty() {
            debug!("No assets selected for '{commodity_id}|{region_id}'");
            continue;
        }

        // Run dispatch
        debug!("Running cycle ({markets_str}) post {commodity_id}|{region_id} investment pass 2");
        let solution = DispatchRun::new(model, &selection.assets, year, &net_demand)
            .without_commodity_constraints()
            .with_market_balance_subset(std::slice::from_ref(market))
            .with_input_prices(&prices.shadow)
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
            &selection.assets,
        );
        second_pass.extend(selection);
    }

    retained_first_pass.extend(second_pass);
    Ok(retained_first_pass)
}

/// Get a portion of the demand profile for this market
///
/// Negative net demand (i.e. a surplus) is treated as zero demand.
pub fn get_demand_portion_for_market(
    time_slice_info: &TimeSliceInfo,
    demand: &AllDemandMap,
    commodity: &Commodity,
    region_id: &RegionID,
    commodity_portion: Dimensionless,
) -> DemandMap {
    time_slice_info
        .iter_selections_at_level(commodity.time_slice_level)
        .map(|ts_selection| {
            let demand_for_selection = *demand
                .get(&(
                    commodity.id.clone(),
                    region_id.clone(),
                    ts_selection.clone(),
                ))
                .unwrap_or(&Flow(0.0));
            (
                ts_selection,
                commodity_portion * demand_for_selection.max(Flow(0.0)),
            )
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
        let selection = run_agents_for_market_set(
            market_set,
            model,
            year,
            mode,
            &net_demand,
            assets,
            prices,
            writer,
        )?;

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
    use crate::agent::{
        Agent, AgentCommodityPortionsMap, AgentObjectiveMap, AgentType, DecisionRule,
    };
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
            agent_type: AgentType::default(),
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
