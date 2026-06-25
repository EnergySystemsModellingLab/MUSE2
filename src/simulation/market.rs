//! Code for creating sets of markets.
use super::optimisation::DispatchRun;
use crate::agent::Agent;
use crate::asset::{Asset, AssetCapacity, AssetIterator, AssetRef, AssetState};
use crate::commodity::{Commodity, CommodityID};
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::investment::{
    AllDemandMap, DemandMap, get_demand_limiting_capacity, select_best_assets,
    update_net_demand_map,
};
use crate::simulation::prices::Prices;
use crate::time_slice::TimeSliceInfo;
use crate::units::{Capacity, Dimensionless, Flow};
use anyhow::{Context, Result};
use indexmap::IndexMap;
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
    /// Experimental: handled by [`select_assets_for_cycle`]. May not work in every case.
    Cycle(Vec<(CommodityID, RegionID)>),
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
            MarketSet::Cycle(markets) => Box::new(markets.iter()),
            MarketSet::Layer(set) => Box::new(set.iter().flat_map(|s| s.iter_markets())),
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
                writer,
            ),
            MarketSet::Cycle(markets) => {
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
            MarketSet::Cycle(markets) => {
                write!(
                    f,
                    "({})",
                    markets.iter().map(|(c, r)| format!("{c}|{r}")).join(", ")
                )
            }
            MarketSet::Layer(ids) => {
                write!(f, "[{}]", ids.iter().join(", "))
            }
        }
    }
}

/// Calculate investment limits for an agent's candidate assets in a given year
///
/// Investment limits are based on demand for the commodity (capacity cannot exceed that needed to
/// meet demand), and any annual addition limits specified by the process (scaled according to the
/// agent's portion of the commodity demand and the number of years elapsed since the previous
/// milestone year).
fn calculate_investment_limits_for_candidates(
    opt_assets: &[AssetRef],
    commodity_portion: Dimensionless,
) -> HashMap<AssetRef, AssetCapacity> {
    // Calculate limits for each candidate asset
    opt_assets
        .iter()
        .filter(|asset| !asset.is_commissioned())
        .map(|asset| {
            // Start off with the demand-limiting capacity (pre-calculated when creating candidate)
            let mut cap = asset.capacity();

            // Cap by the addition limits of the process, if specified
            if let Some(limit_capacity) = asset.max_installable_capacity(commodity_portion) {
                cap = cap.min(limit_capacity);
            }

            (asset.clone(), cap)
        })
        .collect()
}

/// Get candidate assets which produce a particular commodity for a given agent
fn get_candidate_assets<'a>(
    time_slice_info: &'a TimeSliceInfo,
    demand: &'a DemandMap,
    agent: &'a Agent,
    region_id: &'a RegionID,
    commodity: &'a Commodity,
    year: u32,
) -> impl Iterator<Item = AssetRef> + 'a {
    agent
        .iter_search_space(region_id, &commodity.id, year)
        .map(move |process| {
            let mut asset =
                Asset::new_candidate(process.clone(), region_id.clone(), Capacity(0.0), year)
                    .unwrap();

            // Set capacity based on demand
            // This will serve as the upper limit when appraising the asset
            let capacity = get_demand_limiting_capacity(time_slice_info, &asset, commodity, demand);
            let asset_capacity = AssetCapacity::from_capacity(capacity, asset.unit_size());
            asset.set_capacity(asset_capacity);

            asset.into()
        })
}

/// Get options from existing and potential assets for the given parameters
fn get_asset_options<'a>(
    time_slice_info: &'a TimeSliceInfo,
    all_existing_assets: &'a [AssetRef],
    demand: &'a DemandMap,
    agent: &'a Agent,
    commodity: &'a Commodity,
    region_id: &'a RegionID,
    year: u32,
) -> impl Iterator<Item = AssetRef> + 'a {
    // Get existing assets which produce the commodity of interest
    let existing_assets = all_existing_assets
        .iter()
        .filter_agent(&agent.id)
        .filter_region(region_id)
        .filter_primary_producers_of(&commodity.id)
        .cloned();

    // Get candidates assets which produce the commodity of interest
    let candidate_assets =
        get_candidate_assets(time_slice_info, demand, agent, region_id, commodity, year);

    chain(existing_assets, candidate_assets)
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
            &model.time_slice_info,
            existing_assets,
            &demand_portion_for_market,
            agent,
            commodity,
            region_id,
            year,
        )
        .collect::<Vec<_>>();

        // Calculate investment limits for candidate assets
        let investment_limits =
            calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentID;
    use crate::fixture::{
        agent_id, process, process_activity_limits_map, process_flows_map,
        process_investment_constraints, process_parameter_map, region_id,
    };
    use crate::process::{
        Process, ProcessActivityLimitsMap, ProcessFlowsMap, ProcessInvestmentConstraint,
        ProcessInvestmentConstraintsMap, ProcessParameterMap,
    };
    use crate::region::RegionID;
    use crate::units::Dimensionless;
    use crate::units::{ActivityPerCapacity, Capacity};
    use indexmap::IndexSet;
    use rstest::rstest;
    use std::rc::Rc;

    #[rstest]
    fn calculate_investment_limits_for_candidates_empty_list() {
        // Test with empty list of assets
        let opt_assets: Vec<AssetRef> = vec![];
        let commodity_portion = Dimensionless(1.0);

        let result = calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

        assert!(result.is_empty());
    }

    #[rstest]
    fn calculate_investment_limits_for_candidates_commissioned_assets_filtered(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        // Create a mix of commissioned and candidate assets
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);

        // Create commissioned asset - should be filtered out
        let commissioned_asset = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            2015,
        )
        .unwrap();

        // Create candidate asset - should be included
        let candidate_asset =
            Asset::new_candidate(process_rc.clone(), region_id.clone(), capacity, 2015).unwrap();

        let candidate_asset_ref = AssetRef::from(candidate_asset);
        let opt_assets = vec![
            AssetRef::from(commissioned_asset),
            candidate_asset_ref.clone(),
        ];
        let commodity_portion = Dimensionless(1.0);

        let result = calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

        // Only the candidate asset should be in the result
        assert_eq!(result.len(), 1);
        assert!(result.contains_key(&candidate_asset_ref));
    }

    #[rstest]
    fn calculate_investment_limits_for_candidates_no_investment_constraints(
        process: Process,
        region_id: RegionID,
    ) {
        // Create candidate asset without investment constraints
        let process_rc = Rc::new(process);
        let capacity = Capacity(15.0);

        let candidate_asset = Asset::new_candidate(process_rc, region_id, capacity, 2015).unwrap();

        let opt_assets = vec![AssetRef::from(candidate_asset.clone())];
        let commodity_portion = Dimensionless(0.8);

        let result = calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

        // Should return the asset's original capacity since no constraints apply
        assert_eq!(result.len(), 1);
        let asset_ref = AssetRef::from(candidate_asset);
        assert_eq!(result[&asset_ref], AssetCapacity::Continuous(capacity));
    }

    #[rstest]
    // Asset capacity higher than constraint -> limited by constraint
    #[case(Capacity(15.0), Capacity(10.0))]
    // Asset capacity lower than constraint -> limited by asset capacity
    #[case(Capacity(5.0), Capacity(5.0))]
    fn calculate_investment_limits_for_candidates_with_constraints(
        region_id: RegionID,
        process_activity_limits_map: ProcessActivityLimitsMap,
        process_flows_map: ProcessFlowsMap,
        process_parameter_map: ProcessParameterMap,
        #[case] asset_capacity: Capacity,
        #[case] expected_limit: Capacity,
    ) {
        let region_ids: IndexSet<RegionID> = [region_id.clone()].into();

        // Add investment constraint with addition limit
        let constraint = ProcessInvestmentConstraint {
            addition_limit: Some(Capacity(10.0)),
        };
        let mut constraints = ProcessInvestmentConstraintsMap::new();
        constraints.insert((region_id.clone(), 2015), Rc::new(constraint));

        let process = Process {
            id: "constrained_process".into(),
            description: "Process with constraints".into(),
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

        let process_rc = Rc::new(process);

        let candidate_asset =
            Asset::new_candidate(process_rc, region_id, asset_capacity, 2015).unwrap();

        let opt_assets = vec![AssetRef::from(candidate_asset.clone())];
        let commodity_portion = Dimensionless(1.0);

        let result = calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

        // Should be limited by the minimum of asset capacity and constraint
        assert_eq!(result.len(), 1);
        let asset_ref = AssetRef::from(candidate_asset);
        assert_eq!(
            result[&asset_ref],
            AssetCapacity::Continuous(expected_limit)
        );
    }

    #[rstest]
    fn calculate_investment_limits_for_candidates_multiple_assets(
        region_id: RegionID,
        process_activity_limits_map: ProcessActivityLimitsMap,
        process_flows_map: ProcessFlowsMap,
        process_parameter_map: ProcessParameterMap,
    ) {
        let region_ids: IndexSet<RegionID> = [region_id.clone()].into();

        // Create first process with constraints
        let constraint1 = ProcessInvestmentConstraint {
            addition_limit: Some(Capacity(12.0)),
        };
        let mut constraints1 = ProcessInvestmentConstraintsMap::new();
        constraints1.insert((region_id.clone(), 2015), Rc::new(constraint1));

        let process1 = Process {
            id: "process1".into(),
            description: "First process".into(),
            years: 2010..=2020,
            activity_limits: process_activity_limits_map.clone(),
            flows: process_flows_map.clone(),
            parameters: process_parameter_map.clone(),
            regions: region_ids.clone(),
            primary_output: None,
            capacity_to_activity: ActivityPerCapacity(1.0),
            investment_constraints: constraints1,
            unit_size: None,
        };

        // Create second process without constraints
        let process2 = Process {
            id: "process2".into(),
            description: "Second process".into(),
            years: 2010..=2020,
            activity_limits: process_activity_limits_map,
            flows: process_flows_map,
            parameters: process_parameter_map,
            regions: region_ids,
            primary_output: None,
            capacity_to_activity: ActivityPerCapacity(1.0),
            investment_constraints: process_investment_constraints(),
            unit_size: None,
        };

        let process1_rc = Rc::new(process1);
        let process2_rc = Rc::new(process2);

        let candidate1 =
            Asset::new_candidate(process1_rc, region_id.clone(), Capacity(20.0), 2015).unwrap();

        let candidate2 = Asset::new_candidate(process2_rc, region_id, Capacity(8.0), 2015).unwrap();

        let opt_assets = vec![
            AssetRef::from(candidate1.clone()),
            AssetRef::from(candidate2.clone()),
        ];
        let commodity_portion = Dimensionless(0.75);

        let result = calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

        // Should have both assets in result
        assert_eq!(result.len(), 2);

        // First asset should be limited by constraint: 12.0 * 0.75 = 9.0
        let asset1_ref = AssetRef::from(candidate1);
        assert_eq!(
            result[&asset1_ref],
            AssetCapacity::Continuous(Capacity(9.0))
        );

        // Second asset should use its original capacity (no constraints)
        let asset2_ref = AssetRef::from(candidate2);
        assert_eq!(
            result[&asset2_ref],
            AssetCapacity::Continuous(Capacity(8.0))
        );
    }
    #[rstest]
    fn calculate_investment_limits_for_candidates_discrete_capacity(
        region_id: RegionID,
        process_activity_limits_map: crate::process::ProcessActivityLimitsMap,
        process_flows_map: crate::process::ProcessFlowsMap,
        process_parameter_map: crate::process::ProcessParameterMap,
    ) {
        let region_ids: IndexSet<RegionID> = [region_id.clone()].into();

        // Add investment constraint
        let constraint = ProcessInvestmentConstraint {
            addition_limit: Some(Capacity(35.0)), // Enough for 3.5 units at 10.0 each
        };
        let mut constraints = ProcessInvestmentConstraintsMap::new();
        constraints.insert((region_id.clone(), 2015), Rc::new(constraint));

        let process = Process {
            id: "discrete_process".into(),
            description: "Process with discrete units".into(),
            years: 2010..=2020,
            activity_limits: process_activity_limits_map,
            flows: process_flows_map,
            parameters: process_parameter_map,
            regions: region_ids,
            primary_output: None,
            capacity_to_activity: ActivityPerCapacity(1.0),
            investment_constraints: constraints,
            unit_size: Some(Capacity(10.0)), // Discrete units of 10.0 capacity each
        };

        let process_rc = Rc::new(process);
        let capacity = Capacity(50.0); // 5 units at 10.0 each

        let candidate_asset = Asset::new_candidate(process_rc, region_id, capacity, 2015).unwrap();

        let opt_assets = vec![AssetRef::from(candidate_asset.clone())];
        let commodity_portion = Dimensionless(1.0);

        let result = calculate_investment_limits_for_candidates(&opt_assets, commodity_portion);

        // Should be limited by constraint and rounded down to whole units
        // Constraint: 35.0, divided by unit size 10.0 = 3.5 -> floor to 3 units = 30.0
        assert_eq!(result.len(), 1);
        let asset_ref = AssetRef::from(candidate_asset);
        assert_eq!(
            result[&asset_ref],
            AssetCapacity::Discrete(3, Capacity(10.0))
        );
        assert_eq!(result[&asset_ref].total_capacity(), Capacity(30.0));
    }
}
