//! The competitive asset selection algorithm.
//!
//! Candidate and existing assets compete in rounds: each round every option is appraised with a
//! mini dispatch optimisation, and the best option according to the agent's objective is selected.
mod coefficients;
mod constraints;
mod costs;
mod metrics;
mod optimisation;
mod output;

use crate::agent::{Agent, AgentID};
use crate::asset::{Asset, AssetCapacity, AssetIterator, AssetRef};
use crate::commodity::{Commodity, CommodityID};
use crate::model::Model;
use crate::output::DebugDataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::investment::DemandMap;
use crate::simulation::prices::Prices;
use crate::time_slice::TimeSliceSelection;
use crate::units::{Capacity, Dimensionless, Flow, FlowPerCapacity};
use anyhow::{Result, ensure};
use coefficients::calculate_coefficients_for_assets;
use indexmap::IndexSet;
use itertools::{Itertools, chain};
use log::{debug, warn};
use metrics::{
    AppraisalOutput, appraise_investment, count_equal_and_best_appraisal_outputs,
    sort_and_filter_appraisal_outputs,
};
use output::write_debug_info;
use rayon::prelude::*;
use std::collections::HashMap;

/// Select assets for an agent to meet its share of demand for a commodity in a region.
///
/// Existing assets of the agent which produce the commodity compete with candidate assets from the
/// agent's search space. Processes in `excluded_processes` are not considered.
///
/// # Arguments
///
/// * `model` - The model
/// * `agent` - The agent selecting assets
/// * `commodity` - The commodity for which demand must be met
/// * `region_id` - The region
/// * `demand` - The agent's share of demand for the commodity in the region
/// * `existing_assets` - The asset pool (commissioned and otherwise)
/// * `excluded_processes` - Processes which may not be selected
/// * `agent_addition_limits` - The agent's share of addition limits for candidate processes
/// * `agent_total_limits` - The agent's share of total capacity limits for processes
/// * `prices` - Commodity prices from the previous full system dispatch
/// * `year` - Current milestone year
/// * `debug_writer` - Optional debug writer
///
/// # Returns
///
/// The assets selected (including retained commissioned assets), or an error.
#[allow(clippy::too_many_arguments)]
pub fn select_assets(
    model: &Model,
    agent: &Agent,
    commodity: &Commodity,
    region_id: &RegionID,
    demand: &DemandMap,
    existing_assets: &[AssetRef],
    excluded_processes: &IndexSet<ProcessID>,
    agent_addition_limits: &HashMap<ProcessID, Capacity>,
    agent_total_limits: &HashMap<ProcessID, Capacity>,
    prices: &Prices,
    year: u32,
    debug_writer: Option<&mut DebugDataWriter>,
) -> Result<Vec<AssetRef>> {
    // Existing and candidate assets from which to choose
    let mut opt_assets = get_asset_options(
        existing_assets,
        demand,
        agent,
        commodity,
        region_id,
        year,
        model.parameters.capacity_tranche_fraction,
    )
    .collect::<Vec<_>>();

    // Exclude certain processes from the options
    opt_assets.retain(|asset| !excluded_processes.contains(asset.process_id()));

    select_best_assets(
        model,
        opt_assets,
        agent_addition_limits.clone(),
        agent_total_limits.clone(),
        commodity,
        agent,
        region_id,
        prices,
        demand.clone(),
        year,
        debug_writer,
    )
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
            let asset_capacity = AssetCapacity::single(tranche_size);
            asset.set_capacity(asset_capacity);
            asset.into()
        })
}

/// Calculates a characteristic capacity scale for a candidate asset.
///
/// The returned value is the capacity that would satisfy the total annual demand assuming the asset
/// operates at its maximum annual activity for the entire year. It ignores finer-grained activity
/// constraints and temporal variations in demand.
///
/// For processes that do not define a tranche size, this value is later scaled by
/// `capacity_tranche_fraction` to set the capacity of one candidate investment tranche.
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

/// Get the best assets for meeting demand for the given commodity
#[allow(clippy::too_many_arguments)]
pub fn select_best_assets(
    model: &Model,
    mut opt_assets: Vec<AssetRef>,
    agent_addition_limits: HashMap<ProcessID, Capacity>,
    agent_total_limits: HashMap<ProcessID, Capacity>,
    commodity: &Commodity,
    agent: &Agent,
    region_id: &RegionID,
    prices: &Prices,
    mut demand: DemandMap,
    year: u32,
    mut debug_writer: Option<&mut DebugDataWriter>,
) -> Result<Vec<AssetRef>> {
    let objective_type = agent.objectives[&year];

    // Remaining addition limits for candidate processes
    // Initialised as the full agent addition limits, and reduced as candidate assets are selected
    let mut remaining_agent_addition_limits = agent_addition_limits;

    // Remaining total capacity for all assets
    // Initialised as full agent total limits, and reduced as each asset is selection
    let mut remaining_agent_total_limits = agent_total_limits;

    // Store commissioned tranches available for retention and replace assets with single tranches
    let mut available_retention_tranches =
        prepare_commissioned_assets_for_retention(&mut opt_assets);

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
        // Remove assets that would exceed the remaining limits for their processes from the options
        // The addition limit applies only to candidate assets, the total limit applies to all assets
        remove_assets_exceeding_agent_limits(
            &mut opt_assets,
            &remaining_agent_addition_limits,
            true,
        );
        remove_assets_exceeding_agent_limits(&mut opt_assets, &remaining_agent_total_limits, false);
        ensure!(
            !opt_assets.is_empty(),
            "Failed to meet demand for commodity '{}' in region '{}' with provided investment \
            options. This may be due to overly restrictive process investment constraints.",
            commodity.id,
            region_id
        );
        // Appraise all options in parallel: each asset's appraisal is independent (all shared
        // state is read-only within this block), so we can safely use Rayon here.
        // Each HiGHS solve inside `appraise_investment` is configured to use only one thread
        // (via `parallel="off"`) to avoid over-subscription.
        let mut outputs: Vec<AppraisalOutput> = opt_assets
            .par_iter()
            .map(|asset| -> Result<Option<AppraisalOutput>> {
                // Skip assets with zero capacity
                if asset.total_capacity() <= Capacity(0.0) {
                    return Ok(None);
                }

                Ok(Some(appraise_investment(
                    model,
                    asset,
                    commodity,
                    objective_type,
                    &coefficients[asset],
                    &demand,
                )?))
            })
            .collect::<Result<Vec<_>>>()? // propagate any solver error
            .into_iter()
            .flatten()
            .collect();

        // Save appraisal results
        write_debug_info(
            debug_writer.as_deref_mut(),
            year,
            &format!("{} {} round {}", commodity.id, agent.id, round),
            &outputs,
            &demand,
            commodity.time_slice_level,
        )?;

        // Sort by investment priority and discard non-feasible options
        let num_nonfeasible = sort_and_filter_appraisal_outputs(&mut outputs);

        // If none of the remaining options are feasible, we terminate the loop. We may still be
        // able to meet the full demands with assets selected so far, so we continue anyway with a
        // warning.
        if outputs.is_empty() {
            warn!(
                "Investment appraisal completed with unmet demand for commodity '{}', region '{}', \
                year '{}', agent '{}'. {} non-feasible investments were not considered. \
                This unmet demand may still be satisfied during the full system dispatch.",
                commodity.id, region_id, year, agent.id, num_nonfeasible
            );
            break;
        }

        // Warn if there are multiple equally good assets
        log_on_equal_appraisal_outputs(&outputs, &agent.id, &commodity.id, region_id);

        let best_output = outputs.into_iter().next().unwrap();

        // Log the selected asset
        debug!(
            "Selected {} asset '{}' (capacity: {})",
            best_output.asset.state(),
            best_output.asset.process_id(),
            best_output.asset.total_capacity()
        );

        // Update the remaining selection state
        update_selection_state(
            &best_output.asset,
            &mut opt_assets,
            &mut remaining_agent_addition_limits,
            &mut remaining_agent_total_limits,
            &mut available_retention_tranches,
        );

        // Record the selected asset
        record_asset_selection(best_output.asset, &mut best_assets);

        demand = best_output.unmet_demand;
        round += 1;
    }

    // Convert Candidate assets to Ready
    // At this point we also assign the agent ID to the asset
    for asset in &mut best_assets {
        if asset.is_candidate() {
            asset
                .make_mut()
                .select_candidate_for_investment(agent.id.clone());
        }
    }

    Ok(best_assets)
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
    use crate::fixture::{
        agent_id, asset, process, process_activity_limits_map, process_flows_map, svd_commodity,
        time_slice, time_slice_info,
    };
    use crate::process::{ActivityLimits, FlowType, Process, ProcessFlow};
    use crate::time_slice::{TimeSliceID, TimeSliceInfo};
    use crate::units::{FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use rstest::{fixture, rstest};
    use std::sync::Arc;

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
        assert_eq!(opt_assets, []);
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
