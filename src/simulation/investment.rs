//! Code for performing agent investment.
use super::optimisation::{DispatchRun, FlowMap};
use super::prices::ReducedCosts;
use crate::agent::Agent;
use crate::asset::{Asset, AssetIterator, AssetRef, AssetState};
use crate::commodity::{Commodity, CommodityID, CommodityMap};
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::CommodityPrices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Capacity, Dimensionless, Flow, FlowPerCapacity, MoneyPerFlow};
use anyhow::{Result, ensure};
use indexmap::IndexMap;
use itertools::{chain, iproduct};
use log::debug;
use std::collections::HashMap;

pub mod appraisal;
use appraisal::appraise_investment;

/// A map of demand across time slices for a specific commodity and region
type DemandMap = IndexMap<TimeSliceID, Flow>;

/// Demand for a given combination of commodity, region and time slice
type AllDemandMap = IndexMap<(CommodityID, RegionID, TimeSliceID), Flow>;

/// Perform agent investment to determine capacity investment of new assets for next milestone year.
///
/// # Arguments
///
/// * `model` - The model
/// * `year` - Current milestone year
/// * `assets` - The asset pool
/// * `prices` - Commodity prices
/// * `reduced_costs` - Reduced costs for assets
/// * `writer` - Data writer
pub fn perform_agent_investment(
    model: &Model,
    year: u32,
    existing_assets: &[AssetRef],
    prices: &CommodityPrices,
    reduced_costs: &ReducedCosts,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    // Initialise demand map
    let mut demand =
        flatten_preset_demands_for_year(&model.commodities, &model.time_slice_info, year);

    // Keep a list of all the assets selected
    // This includes Commissioned assets that are selected for retention, and new Selected assets
    let mut all_selected_assets = Vec::new();

    for region_id in model.iter_regions() {
        let cur_commodities = &model.commodity_order[&(region_id.clone(), year)];

        // Prices to be used for input flows for commodities not produced in dispatch run
        let mut external_prices =
            get_prices_for_commodities(prices, &model.time_slice_info, region_id, cur_commodities);
        let mut seen_commodities = Vec::new();
        for commodity_id in cur_commodities {
            seen_commodities.push(commodity_id.clone());
            let commodity = &model.commodities[commodity_id];

            // Remove prices for already-seen commodities. Commodities which are produced by at
            // least one asset in the dispatch run will have prices produced endogenously (via the
            // commodity balance constraints), but commodities for which investment has not yet been
            // performed will, by definition, not have any producers. For these, we provide prices
            // from the previous dispatch run otherwise they will appear to be free to the model.
            for time_slice in model.time_slice_info.iter_ids() {
                external_prices.remove(&(
                    commodity_id.clone(),
                    region_id.clone(),
                    time_slice.clone(),
                ));
            }

            // List of assets selected/retained for this region/commodity
            let mut selected_assets = Vec::new();

            for (agent, commodity_portion) in
                get_responsible_agents(model.agents.values(), commodity_id, region_id, year)
            {
                debug!(
                    "Running investment for agent '{}' with commodity '{}' in region '{}'",
                    &agent.id, commodity_id, region_id
                );

                // Get demand portion for this commodity for this agent in this region/year
                let demand_portion_for_commodity = get_demand_portion_for_commodity(
                    &model.time_slice_info,
                    &demand,
                    commodity_id,
                    region_id,
                    commodity_portion,
                );

                // Existing and candidate assets from which to choose
                let opt_assets = get_asset_options(
                    &model.time_slice_info,
                    existing_assets,
                    &demand_portion_for_commodity,
                    agent,
                    commodity,
                    region_id,
                    year,
                )
                .collect();

                // Choose assets from among existing pool and candidates
                let best_assets = select_best_assets(
                    model,
                    opt_assets,
                    commodity,
                    agent,
                    reduced_costs,
                    demand_portion_for_commodity,
                    year,
                    writer,
                )?;
                selected_assets.extend(best_assets);
            }

            // If no assets have been selected for this region/commodity, skip dispatch optimisation
            // **TODO**: this probably means there's no demand for the commodity, which we could
            // presumably preempt
            if selected_assets.is_empty() {
                continue;
            }

            // Add the selected assets to the list of all selected assets
            all_selected_assets.extend(selected_assets.clone());

            // Perform dispatch optimisation with assets that have been selected so far
            // **TODO**: presumably we only need to do this for selected_assets, as assets added in
            // previous iterations should not change
            debug!(
                "Running post-investment dispatch for commodity '{commodity_id}' in region '{region_id}'"
            );

            // As upstream commodities by definition will not yet have producers, we explicitly set
            // their prices using previous values so that they don't appear free
            let solution = DispatchRun::new(model, &all_selected_assets, year)
                .with_commodity_subset(&seen_commodities)
                .with_input_prices(&external_prices)
                .run(
                    &format!("post {commodity_id}/{region_id} investment"),
                    writer,
                )?;

            // Update demand map with flows from newly added assets
            update_demand_map(&mut demand, &solution.create_flow_map(), &selected_assets);
        }
    }

    Ok(all_selected_assets)
}

/// Flatten the preset commodity demands for a given year into a map of commodity, region and
/// time slice to demand.
///
/// Since demands for some commodities may be specified at a coarser timeslice level, we need to
/// distribute these demands over all timeslices. Note: the way that we do this distribution is
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

            // We split the demand equally over all timeslices in the selection
            // NOTE: since demands will only be balanced to the timeslice level of the commodity
            // it doesn't matter how we do this distribution, only the total matters.
            #[allow(clippy::cast_precision_loss)]
            let n_timeslices = time_slice_selection.iter(time_slice_info).count() as f64;
            let demand_per_slice = *demand / Dimensionless(n_timeslices);
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

/// Update demand map with flows from a set of assets
fn update_demand_map(demand: &mut AllDemandMap, flows: &FlowMap, assets: &[AssetRef]) {
    for ((asset, commodity_id, time_slice), flow) in flows {
        if assets.contains(asset) {
            let key = (
                commodity_id.clone(),
                asset.region_id().clone(),
                time_slice.clone(),
            );

            // Note: we use the negative of the flow as input flows are negative in the flow map.
            demand
                .entry(key)
                .and_modify(|value| *value -= *flow)
                .or_insert(-*flow);
        }
    }
}

/// Get a portion of the demand profile for this commodity and region
fn get_demand_portion_for_commodity(
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

/// Get the agents responsible for a given commodity in a given year along with the commodity
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

/// Get the maximum required capacity across time slices
fn get_demand_limiting_capacity(
    time_slice_info: &TimeSliceInfo,
    asset: &Asset,
    commodity: &Commodity,
    demand: &DemandMap,
) -> Capacity {
    let coeff = asset.get_flow(&commodity.id).unwrap().coeff;
    let mut capacity = Capacity(0.0);

    for time_slice_selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level)
    {
        let demand_for_selection: Flow = time_slice_selection
            .iter(time_slice_info)
            .map(|(time_slice, _)| demand[time_slice])
            .sum();

        // Calculate max capacity required for this time slice selection
        // For commodities with a coarse timeslice level, we have to allow the possibility that all
        // of the demand gets served by production in a single timeslice
        for (time_slice, _) in time_slice_selection.iter(time_slice_info) {
            let max_flow_per_cap =
                *asset.get_activity_per_capacity_limits(time_slice).end() * coeff;
            if max_flow_per_cap != FlowPerCapacity(0.0) {
                capacity = capacity.max(demand_for_selection / max_flow_per_cap);
            }
        }
    }

    capacity
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
        .iter_possible_producers_of(region_id, &commodity.id, year)
        .map(move |process| {
            let mut asset =
                Asset::new_candidate(process.clone(), region_id.clone(), Capacity(0.0), year)
                    .unwrap();
            asset.set_capacity(get_demand_limiting_capacity(
                time_slice_info,
                &asset,
                commodity,
                demand,
            ));

            asset.into()
        })
}

/// Get a map of prices for a subset of commodities
fn get_prices_for_commodities(
    prices: &CommodityPrices,
    time_slice_info: &TimeSliceInfo,
    region_id: &RegionID,
    commodities: &[CommodityID],
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow> {
    iproduct!(commodities.iter(), time_slice_info.iter_ids())
        .map(|(commodity_id, time_slice)| {
            let price = prices.get(commodity_id, region_id, time_slice).unwrap();
            (
                (commodity_id.clone(), region_id.clone(), time_slice.clone()),
                price,
            )
        })
        .collect()
}

/// Get the best assets for meeting demand for the given commodity
#[allow(clippy::too_many_arguments)]
fn select_best_assets(
    model: &Model,
    mut opt_assets: Vec<AssetRef>,
    commodity: &Commodity,
    agent: &Agent,
    reduced_costs: &ReducedCosts,
    mut demand: DemandMap,
    year: u32,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    let mut best_assets: Vec<AssetRef> = Vec::new();

    let mut remaining_candidate_capacity = HashMap::from_iter(
        opt_assets
            .iter()
            .filter(|asset| !asset.is_commissioned())
            .map(|asset| (asset.clone(), asset.capacity())),
    );

    let mut round = 0;
    while is_any_remaining_demand(&demand) {
        ensure!(
            !opt_assets.is_empty(),
            "Failed to meet demand for commodity '{}' with provided assets",
            &commodity.id
        );

        // Appraise all options
        let mut outputs_for_opts = Vec::new();
        for asset in &opt_assets {
            let max_capacity = (!asset.is_commissioned()).then(|| {
                let max_capacity = model.parameters.capacity_limit_factor * asset.capacity();
                let remaining_capacity = remaining_candidate_capacity[asset];
                max_capacity.min(remaining_capacity)
            });

            let output = appraise_investment(
                model,
                asset,
                max_capacity,
                commodity,
                &agent.objectives[&year],
                reduced_costs,
                &demand,
            )?;

            // Store the appraisal results if the capacity is positive. If capacity is zero,
            // this means the asset is infeasible for investment. This can happen if the asset has
            // zero activity limits for all time slices with demand. This can also happen due to a
            // known issue with the NPV objective, for which we do not currently have a solution
            // (see https://github.com/EnergySystemsModellingLab/MUSE_2.0/issues/716).
            if output.capacity > Capacity(0.0) {
                outputs_for_opts.push(output);
            } else {
                debug!(
                    "Skipping candidate '{}' with zero capacity",
                    asset.process_id()
                );
            }
        }

        // Make sure there are some options to consider
        ensure!(
            !outputs_for_opts.is_empty(),
            "No feasible investment options for commodity '{}'",
            &commodity.id
        );

        // Save appraisal results
        writer.write_appraisal_debug_info(
            year,
            &format!("{} {} round {}", &commodity.id, &agent.id, round),
            &outputs_for_opts,
        )?;

        // Select the best investment option
        let best_output = outputs_for_opts
            .into_iter()
            .min_by(|a, b| a.metric.partial_cmp(&b.metric).unwrap())
            .unwrap();

        // Log the selected asset
        debug!(
            "Selected {} asset '{}' (capacity: {})",
            &best_output.asset.state(),
            &best_output.asset.process_id(),
            best_output.capacity
        );

        // Update the assets
        update_assets(
            best_output.asset,
            best_output.capacity,
            &mut opt_assets,
            &mut remaining_candidate_capacity,
            &mut best_assets,
        );

        demand = best_output.unmet_demand;
        round += 1;
    }

    // Convert Candidate assets to Selected
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
fn is_any_remaining_demand(demand: &DemandMap) -> bool {
    demand.values().any(|flow| *flow > Flow(0.0))
}

/// Update capacity of chosen asset, if needed, and update both asset options and chosen assets
fn update_assets(
    mut best_asset: AssetRef,
    capacity: Capacity,
    opt_assets: &mut Vec<AssetRef>,
    remaining_candidate_capacity: &mut HashMap<AssetRef, Capacity>,
    best_assets: &mut Vec<AssetRef>,
) {
    match best_asset.state() {
        AssetState::Commissioned { .. } => {
            // Remove this asset from the options
            opt_assets.retain(|asset| *asset != best_asset);
            best_assets.push(best_asset);
        }
        AssetState::Candidate => {
            // Remove this capacity from the available remaining capacity for this asset
            let remaining_capacity = remaining_candidate_capacity.get_mut(&best_asset).unwrap();
            *remaining_capacity -= capacity;

            // If there's no capacity remaining, remove the asset from the options
            if *remaining_capacity <= Capacity(0.0) {
                let old_idx = opt_assets
                    .iter()
                    .position(|asset| *asset == best_asset)
                    .unwrap();
                opt_assets.swap_remove(old_idx);
                remaining_candidate_capacity.remove(&best_asset);
            }

            if let Some(existing_asset) = best_assets.iter_mut().find(|asset| **asset == best_asset)
            {
                // If the asset is already in the list of best assets, add the additional required capacity
                existing_asset.make_mut().increase_capacity(capacity);
            } else {
                // Otherwise, update the capacity of the chosen asset and add it to the list of best assets
                best_asset.make_mut().set_capacity(capacity);
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
        asset, process, process_parameter_map, region_id, svd_commodity, time_slice,
        time_slice_info, time_slice_info2,
    };
    use crate::process::{FlowType, ProcessFlow};
    use crate::region::RegionID;
    use crate::time_slice::{TimeSliceID, TimeSliceInfo};
    use crate::units::{Dimensionless, Flow, FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use itertools::Itertools;
    use rstest::rstest;
    use std::rc::Rc;

    #[rstest]
    fn test_get_demand_limiting_capacity(
        time_slice: TimeSliceID,
        region_id: RegionID,
        time_slice_info: TimeSliceInfo,
        svd_commodity: Commodity,
    ) {
        // Create a process flow using the existing commodity fixture
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(2.0), // 2 units of flow per unit of activity
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        // Create a process with the flows and activity limits
        let mut process = process(
            [region_id.clone()].into_iter().collect(),
            process_parameter_map([region_id.clone()].into_iter().collect()),
        );

        // Add the flow to the process
        process.flows.insert(
            (region_id.clone(), 2015), // Using default commission year from fixture
            [(commodity_rc.id.clone(), process_flow)]
                .into_iter()
                .collect(),
        );

        // Add activity limits
        process.activity_limits.insert(
            (region_id.clone(), 2015, time_slice.clone()),
            Dimensionless(0.0)..=Dimensionless(1.0),
        );

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
    fn test_get_demand_limiting_capacity_multiple_time_slices(
        time_slice_info2: TimeSliceInfo,
        svd_commodity: Commodity,
        region_id: RegionID,
    ) {
        // Create time slices from the fixture (day and night)
        let (time_slice1, time_slice2) =
            time_slice_info2.time_slices.keys().collect_tuple().unwrap();

        // Create a process flow using the existing commodity fixture
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0), // 1 unit of flow per unit of activity
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        // Create a process with the flows and activity limits
        let mut process = process(
            [region_id.clone()].into_iter().collect(),
            process_parameter_map([region_id.clone()].into_iter().collect()),
        );

        // Add the flow to the process
        process.flows.insert(
            (region_id.clone(), 2015), // Using default commission year from fixture
            [(commodity_rc.id.clone(), process_flow)]
                .into_iter()
                .collect(),
        );

        // Add activity limits for both time slices with different limits
        process.activity_limits.insert(
            (region_id.clone(), 2015, time_slice1.clone()),
            Dimensionless(0.0)..=Dimensionless(2.0), // Higher limit for day
        );
        process.activity_limits.insert(
            (region_id.clone(), 2015, time_slice2.clone()),
            Dimensionless(0.0)..=Dimensionless(0.0), // Zero limit for night - should be skipped
        );

        // Create asset with the configured process
        let asset = asset(process);

        // Create demand map with different demands for each time slice
        let demand = indexmap! {
            time_slice1.clone() => Flow(4.0), // Requires capacity of 4.0/2.0 = 2.0
            time_slice2.clone() => Flow(3.0), // Would require infinite capacity, but should be skipped
        };

        // Call the function
        let result =
            get_demand_limiting_capacity(&time_slice_info2, &asset, &commodity_rc, &demand);

        // Expected: maximum of the capacity requirements across time slices (excluding zero limit)
        // Time slice 1: demand (4.0) / (activity_limit (2.0) * coeff (1.0)) = 2.0
        // Time slice 2: skipped due to zero activity limit
        // Maximum = 2.0
        assert_eq!(result, Capacity(2.0));
    }
}
