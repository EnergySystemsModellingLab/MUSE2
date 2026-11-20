//! The population agent algorithm.
//!
//! Each option is appraised once against the full demand profile. Feasible options are then
//! selected in merit order, scaling their fitted profiles to the remaining demand.
mod constraints;
mod costs;
mod metrics;
mod optimisation;
mod output;

use crate::agent::{Agent, AgentID};
use crate::asset::{Asset, AssetCapacity, AssetRef};
use crate::commodity::{Commodity, CommodityID};
use crate::model::Model;
use crate::output::DebugDataWriter;
use crate::process::{Process, ProcessID};
use crate::region::RegionID;
use crate::simulation::PriceMap;
use crate::simulation::activity::ActivityMap;
use crate::simulation::investment::Selection;
use crate::simulation::investment::{AssetOptions, DemandMap};
use crate::simulation::prices::Prices;
use crate::units::{Activity, Capacity, Dimensionless, Flow};
use anyhow::{Result, anyhow};
use itertools::{Itertools, chain};
use log::debug;
use metrics::{AppraisalOutput, count_equal_and_best_appraisal_outputs, sort_appraisal_outputs};
use optimisation::perform_optimisation;
use output::write_debug_info;
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;

/// Run the population algorithm for an agent to meet its share of demand for a commodity.
///
/// Committed assets are merit-ordered first and selected unchanged, up to their active capacity.
/// Existing assets and candidates are then merit-ordered together to meet any remaining demand.
///
/// # Arguments
///
/// * `model` - The model
/// * `agent` - The agent selecting assets
/// * `commodity` - The commodity for which demand must be met
/// * `region_id` - The region
/// * `demand` - The agent's share of demand for the commodity in the region
/// * `options` - Committed assets, existing assets and processes available to the agent
/// * `agent_addition_limits` - Addition limits defined for candidate processes for this agent
/// * `agent_total_limits` - Total capacity limits defined for existing and candidate assets for
///   this agent
/// * `prices` - Market prices used to calculate input costs
/// * `year` - Current milestone year
/// * `debug_writer` - Optional debug writer
///
/// # Returns
///
/// The selected assets and their activities, or an error if demand cannot be met.
#[allow(clippy::too_many_arguments)]
pub fn run_agent(
    model: &Model,
    agent: &Agent,
    commodity: &Commodity,
    region_id: &RegionID,
    demand: &DemandMap,
    options: &AssetOptions,
    agent_addition_limits: HashMap<ProcessID, Capacity>,
    agent_total_limits: HashMap<ProcessID, Capacity>,
    prices: &Prices,
    year: u32,
    debug_writer: Option<&mut DebugDataWriter>,
) -> Result<Selection> {
    // Existing assets retain their current capacity as the maximum retention amount. Candidate
    // capacity is assembled as in portfolio: discrete when the process has a tranche size and
    // continuous otherwise.
    let opt_assets = chain(
        options.existing.iter().cloned(),
        get_candidate_assets(&options.processes, region_id, year),
    )
    .collect::<Vec<_>>();

    select_best_assets(
        model,
        &options.committed,
        &opt_assets,
        agent_addition_limits,
        agent_total_limits,
        commodity,
        agent,
        region_id,
        prices,
        demand,
        year,
        debug_writer,
    )
}

/// Construct candidates using the same continuous/discrete capacity setup as portfolio.
///
/// A candidate with a tranche size starts as one tranche; the optimisation determines its
/// capacity and selection later rounds it down to a whole number of tranches. Candidates without a
/// tranche size start with zero continuous capacity. No demand-derived candidate capacity scale is
/// needed because capacity is a free variable during appraisal.
///
/// # Arguments
///
/// * `processes` - Processes available in the agent's search space
/// * `region_id` - The region in which the candidate is located
/// * `year` - The candidate's commission year
pub fn get_candidate_assets<'a>(
    processes: &'a [Arc<Process>],
    region_id: &'a RegionID,
    year: u32,
) -> impl Iterator<Item = AssetRef> + 'a {
    processes.iter().map(move |process| {
        let tranche_size = process.tranche_size.unwrap_or(Capacity(0.0));
        let mut asset =
            Asset::new_candidate(process.clone(), region_id.clone(), tranche_size, year).unwrap();
        if process.tranche_size.is_none() {
            asset.set_capacity(AssetCapacity::continuous(Capacity(0.0)));
        }
        asset.into()
    })
}

/// Select committed assets first, then existing and candidate assets in merit order.
///
/// Every option is appraised once against the original demand profile. Since a feasible appraisal
/// reproduces that profile exactly, selecting a fraction of its capacity supplies the same fraction
/// of every demand selection; the remaining demand therefore retains the original shape.
///
/// Committed assets are ordered by metric among themselves and used up to active capacity. Then
/// existing assets and candidates compete together. Candidate additions are bounded by addition
/// and total limits; existing retention is bounded by the asset's capacity and total limits.
///
/// # Returns
///
/// The committed assets and the assets needed to meet demand, along with their selected activities.
/// Returns an error if the available profiles and limits cannot meet demand.
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
pub fn select_best_assets(
    model: &Model,
    committed_assets: &[AssetRef],
    opt_assets: &[AssetRef],
    agent_addition_limits: HashMap<ProcessID, Capacity>,
    agent_total_limits: HashMap<ProcessID, Capacity>,
    commodity: &Commodity,
    agent: &Agent,
    region_id: &RegionID,
    prices: &Prices,
    demand: &DemandMap,
    year: u32,
    debug_writer: Option<&mut DebugDataWriter>,
) -> Result<Selection> {
    let has_options = !opt_assets.is_empty();
    let mut remaining_demand = demand.clone();
    let mut selected = committed_assets
        .iter()
        .map(|asset| (asset.clone(), asset.clone()))
        .collect::<Vec<_>>();
    let mut activities = ActivityMap::new();
    for asset in committed_assets {
        // Committed assets remain selected even when they are not dispatched. Give each one
        // explicit zero activities so the returned selection contains a complete activity map.
        for time_slice in model.time_slice_info.iter_ids() {
            activities.insert((asset.clone(), time_slice.clone()), Activity(0.0));
        }
    }

    // With no material demand, committed assets are still returned unchanged; there is no profile
    // to appraise and no existing asset or candidate needs to be retained or built.
    if !is_any_remaining_demand(
        &remaining_demand,
        model.parameters.remaining_demand_absolute_tolerance,
    ) {
        return Ok(Selection::from_optimisation(selected, &activities));
    }

    // Appraise each option against the same full profile exactly once. Committed costs are sunk,
    // while candidates include annualised capital costs.
    let mut committed_appraisals = appraise_assets(
        model,
        committed_assets,
        commodity,
        &prices.market,
        demand,
        year,
    )?;
    let mut option_appraisals =
        appraise_assets(model, opt_assets, commodity, &prices.market, demand, year)?;
    if let Some(writer) = debug_writer {
        // Write the complete appraisal sets once rather than repeating them after each selection.
        let run_description = format!("{} {} {}", commodity.id, region_id, agent.id);
        write_debug_info(
            writer,
            year,
            &run_description,
            &committed_appraisals,
            demand,
            commodity.time_slice_level,
        )?;
        write_debug_info(
            writer,
            year,
            &run_description,
            &option_appraisals,
            demand,
            commodity.time_slice_level,
        )?;
    }

    sort_appraisal_outputs(&mut committed_appraisals);
    sort_appraisal_outputs(&mut option_appraisals);
    log_on_equal_appraisal_outputs(&committed_appraisals, &agent.id, &commodity.id, region_id);
    log_on_equal_appraisal_outputs(&option_appraisals, &agent.id, &commodity.id, region_id);

    // Committed assets are a separate first stage. Their installed capacity is fixed, but their
    // activity is scaled to the portion of the full demand profile they can cover.
    for appraisal in committed_appraisals {
        if !is_any_remaining_demand(
            &remaining_demand,
            model.parameters.remaining_demand_absolute_tolerance,
        ) {
            break;
        }
        let selected_capacity = capacity_for_remaining_demand(
            appraisal.capacity,
            remaining_demand_fraction(demand, &remaining_demand),
        )
        .min(appraisal.asset.active_capacity());
        apply_appraisal(
            &appraisal,
            selected_capacity,
            demand,
            &mut remaining_demand,
            &mut activities,
        );
        debug!(
            "Selected committed asset '{}' (capacity used: {selected_capacity})",
            appraisal.asset.process_id()
        );
    }

    // Candidates and existing assets now compete by the same cost metric. Limits are applied to
    // selected capacity, not to the full-demand capacity used for appraisal.
    let mut remaining_addition_limits = agent_addition_limits;
    let mut remaining_total_limits = agent_total_limits;
    for appraisal in option_appraisals {
        if !is_any_remaining_demand(
            &remaining_demand,
            model.parameters.remaining_demand_absolute_tolerance,
        ) {
            break;
        }

        let requested_capacity = capacity_for_remaining_demand(
            appraisal.capacity,
            remaining_demand_fraction(demand, &remaining_demand),
        );
        let mut capacity_limit = requested_capacity;
        if !appraisal.asset.is_candidate() {
            // Existing assets may be retained or mothballed, but never expanded.
            capacity_limit = capacity_limit.min(appraisal.asset.total_capacity());
        }
        if let Some(limit) = remaining_total_limits.get(appraisal.asset.process_id()) {
            capacity_limit = capacity_limit.min(*limit);
        }
        if appraisal.asset.is_candidate()
            && let Some(limit) = remaining_addition_limits.get(appraisal.asset.process_id())
        {
            capacity_limit = capacity_limit.min(*limit);
        }

        // A tranche-sized asset must not be rounded up past the demand or any remaining limit.
        let selected_capacity =
            round_capacity_down(appraisal.asset.capacity(), capacity_limit).total_capacity();
        if selected_capacity <= Capacity(0.0) {
            continue;
        }

        let selected_asset = selected_asset(&appraisal.asset, selected_capacity, agent);
        selected.push((appraisal.asset.clone(), selected_asset));
        // The fitted profile is linear in capacity, so use the selected capacity to update both
        // activities and every selection of the remaining demand map.
        apply_appraisal(
            &appraisal,
            selected_capacity,
            demand,
            &mut remaining_demand,
            &mut activities,
        );

        subtract_capacity_limit(
            appraisal.asset.process_id(),
            selected_capacity,
            &mut remaining_total_limits,
        );
        if appraisal.asset.is_candidate() {
            subtract_capacity_limit(
                appraisal.asset.process_id(),
                selected_capacity,
                &mut remaining_addition_limits,
            );
        }
        debug!(
            "Selected {} asset '{}' (capacity: {selected_capacity})",
            appraisal.asset.state(),
            appraisal.asset.process_id()
        );
    }

    if is_any_remaining_demand(
        &remaining_demand,
        model.parameters.remaining_demand_absolute_tolerance,
    ) {
        if !has_options {
            return Err(no_options_error(false, agent, commodity, region_id, year));
        }
        return Err(no_options_error(
            has_options,
            agent,
            commodity,
            region_id,
            year,
        ));
    }

    Ok(Selection::from_optimisation(selected, &activities))
}

fn appraise_assets(
    model: &Model,
    assets: &[AssetRef],
    commodity: &Commodity,
    market_prices: &PriceMap,
    demand: &DemandMap,
    year: u32,
) -> Result<Vec<AppraisalOutput>> {
    // Appraisals are independent and can run in parallel. Infeasible profiles are expected and
    // omitted; configuration and other solver errors propagate.
    let results = assets
        .par_iter()
        .map(|asset| -> Result<Option<AppraisalOutput>> {
            let Some(results) =
                perform_optimisation(model, asset, commodity, market_prices, year, demand)?
            else {
                return Ok(None);
            };
            if results.capacity <= Capacity(0.0) {
                return Ok(None);
            }
            Ok(Some(AppraisalOutput::new(
                asset.clone(),
                results.capacity,
                &results.activity,
                results.metric,
            )))
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(results.into_iter().flatten().collect())
}

fn selected_asset(asset: &AssetRef, capacity: Capacity, agent: &Agent) -> AssetRef {
    // Retained commissioned assets are unmothballed before reducing their capacity. Candidates are
    // marked ready for investment before their selected tranche count is assigned.
    let mut selected_asset = asset.clone().with_no_mothballed_capacity();
    let capacity = round_capacity_down(asset.capacity(), capacity);
    let inner = selected_asset.make_mut();
    if inner.is_candidate() {
        inner.select_candidate_for_investment(agent.id.clone());
    }
    inner.set_capacity(capacity);
    selected_asset
}

fn apply_appraisal(
    appraisal: &AppraisalOutput,
    capacity: Capacity,
    original_demand: &DemandMap,
    remaining_demand: &mut DemandMap,
    activities: &mut ActivityMap,
) {
    if capacity <= Capacity(0.0) {
        return;
    }
    // The appraisal matches original demand at `appraisal.capacity`; scaling capacity scales its
    // activity and supply profile by the same factor.
    let fraction = Dimensionless((capacity / appraisal.capacity).value());
    for (time_slice, activity_per_capacity) in &appraisal.activity_per_capacity {
        *activities
            .entry((appraisal.asset.clone(), time_slice.clone()))
            .or_insert(Activity(0.0)) += *activity_per_capacity * capacity;
    }
    for (selection, original_flow) in original_demand {
        let flow = (remaining_demand[selection] - *original_flow * fraction).max(Flow(0.0));
        remaining_demand.insert(selection.clone(), flow);
    }
}

/// Scale the capacity required for the full profile to the current residual demand.
fn capacity_for_remaining_demand(
    full_demand_capacity: Capacity,
    remaining_fraction: Dimensionless,
) -> Capacity {
    full_demand_capacity * remaining_fraction
}

/// Calculate the residual demand as a fraction of the original profile.
///
/// Every selected option supplies a scalar multiple of the original profile, so the ratio of
/// total flows is also the ratio for each time-slice selection.
fn remaining_demand_fraction(original: &DemandMap, remaining: &DemandMap) -> Dimensionless {
    let original_total: Flow = original.values().copied().sum();
    if original_total <= Flow(0.0) {
        return Dimensionless(0.0);
    }
    let remaining_total: Flow = remaining.values().copied().sum();
    (remaining_total / original_total).min(Dimensionless(1.0))
}

/// Represent a selected capacity using the asset's capacity type, rounding discrete capacity down.
///
/// The returned capacity never exceeds the amount available from demand or investment limits.
fn round_capacity_down(capacity_type: AssetCapacity, capacity: Capacity) -> AssetCapacity {
    match capacity_type {
        AssetCapacity::Continuous(_) => AssetCapacity::continuous(capacity.max(Capacity(0.0))),
        AssetCapacity::Discrete { tranche_size, .. } if tranche_size > Capacity(0.0) => {
            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
            let num_tranches = (capacity / tranche_size).value().floor() as u32;
            AssetCapacity::discrete(num_tranches, tranche_size)
        }
        AssetCapacity::Discrete { tranche_size, .. } => AssetCapacity::discrete(0, tranche_size),
    }
}

/// Reduce a process limit by selected capacity, if that process has a configured limit.
fn subtract_capacity_limit(
    process_id: &ProcessID,
    capacity: Capacity,
    limits: &mut HashMap<ProcessID, Capacity>,
) {
    if let Some(limit) = limits.get_mut(process_id) {
        *limit = (*limit - capacity).max(Capacity(0.0));
    }
}

/// Check whether any demand selection remains above the model's absolute tolerance.
fn is_any_remaining_demand(demand: &DemandMap, absolute_tolerance: Flow) -> bool {
    demand.values().any(|flow| *flow > absolute_tolerance)
}

/// Log when multiple options share the same best metric and fallback ordering.
fn log_on_equal_appraisal_outputs(
    outputs: &[AppraisalOutput],
    agent_id: &AgentID,
    commodity_id: &CommodityID,
    region_id: &RegionID,
) {
    let num_identical = count_equal_and_best_appraisal_outputs(outputs);
    if num_identical == 0 {
        return;
    }

    let asset_details = outputs[..=num_identical]
        .iter()
        .map(|output| {
            let asset = &output.asset;
            format!(
                "Process ID: '{}' (State: {}{}, Commission year: {})",
                asset.process_id(),
                asset.state(),
                asset.id().map_or_default(|id| format!(", Asset ID: {id}")),
                asset.commission_year()
            )
        })
        .join(", ");
    debug!(
        "Found equally good appraisals for Agent ID: {agent_id}, Commodity: '{commodity_id}', \
        Region: {region_id}. Options: [{asset_details}]. Selecting first option.",
    );
}

/// Create the error returned when available options and limits cannot meet demand.
fn no_options_error(
    has_options: bool,
    agent: &Agent,
    commodity: &Commodity,
    region_id: &RegionID,
    year: u32,
) -> anyhow::Error {
    if has_options {
        anyhow!(
            "Failed to meet demand for commodity '{}' in region '{region_id}' with provided \
            investment options. This may be due to overly restrictive process investment \
            constraints.",
            commodity.id
        )
    } else {
        anyhow!(
            "Agent '{}' cannot meet its demand for commodity '{}' in region '{region_id}' in \
            year {year} with its existing assets",
            agent.id,
            commodity.id
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentID;
    use crate::commodity::CommodityID;
    use crate::fixture::build_patched_simple_tempdir;
    use crate::fixture::process;
    use crate::input::load_model;
    use crate::simulation::investment::AssetOptions;
    use crate::simulation::prices::Prices;
    use crate::units::Activity;
    use float_cmp::assert_approx_eq;
    use rstest::rstest;

    #[rstest]
    #[case(None, AssetCapacity::continuous(Capacity(0.0)))]
    #[case(Some(Capacity(3.0)), AssetCapacity::single(Capacity(3.0)))]
    fn candidate_assets_use_portfolio_capacity_setup(
        mut process: Process,
        #[case] tranche_size: Option<Capacity>,
        #[case] expected_capacity: AssetCapacity,
    ) {
        process.tranche_size = tranche_size;
        let candidates = get_candidate_assets(&[Arc::new(process)], &RegionID::from("GBR"), 2020)
            .collect::<Vec<_>>();

        assert_eq!(candidates.len(), 1);
        assert_eq!(candidates[0].capacity(), expected_capacity);
    }

    #[rstest]
    #[case(
        AssetCapacity::continuous(Capacity(5.0)),
        Capacity(2.5),
        AssetCapacity::continuous(Capacity(2.5))
    )]
    #[case(
        AssetCapacity::single(Capacity(2.0)),
        Capacity(5.9),
        AssetCapacity::discrete(2, Capacity(2.0))
    )]
    #[case(
        AssetCapacity::single(Capacity(2.0)),
        Capacity(1.9),
        AssetCapacity::discrete(0, Capacity(2.0))
    )]
    fn capacity_is_rounded_down(
        #[case] capacity_type: AssetCapacity,
        #[case] capacity: Capacity,
        #[case] expected: AssetCapacity,
    ) {
        assert_eq!(round_capacity_down(capacity_type, capacity), expected);
    }

    #[test]
    fn run_agent_supplies_the_full_demand_profile() {
        let model_dir = build_patched_simple_tempdir(Vec::new());
        let model = load_model(model_dir.path()).unwrap();
        let year = model.parameters.milestone_years[0];
        let agent = &model.agents[&AgentID::from("A0_RES")];
        let commodity = &model.commodities[&CommodityID::from("RSHEAT")];
        let region_id = RegionID::from("GBR");
        let process = agent
            .iter_search_space(&region_id, &commodity.id, year)
            .next()
            .unwrap()
            .clone();
        let candidate = get_candidate_assets(std::slice::from_ref(&process), &region_id, year)
            .next()
            .unwrap();
        let flow_coefficient = candidate.get_flow(&commodity.id).unwrap().coeff;
        let demand: DemandMap = model
            .time_slice_info
            .iter_selections_at_level(commodity.time_slice_level)
            .map(|selection| {
                let maximum_activity = *candidate
                    .get_activity_per_capacity_limits_for_selection(&selection)
                    .end();
                (
                    selection,
                    maximum_activity * Capacity(1.0) * flow_coefficient,
                )
            })
            .collect();
        let options = AssetOptions {
            processes: vec![process],
            ..AssetOptions::default()
        };
        let selection = run_agent(
            &model,
            agent,
            commodity,
            &region_id,
            &demand,
            &options,
            HashMap::new(),
            HashMap::new(),
            &Prices::default(),
            year,
            None,
        )
        .unwrap();

        assert_eq!(selection.assets.len(), 1);
        let selected_asset = &selection.assets[0];
        for (time_selection, required_flow) in &demand {
            let supplied_flow: Flow = time_selection
                .iter(&model.time_slice_info)
                .map(|(time_slice, _)| {
                    selection
                        .activity
                        .get(&(selected_asset.clone(), time_slice.clone()))
                        .copied()
                        .unwrap_or(Activity(0.0))
                        * flow_coefficient
                })
                .sum();
            assert_approx_eq!(f64, supplied_flow.value(), required_flow.value());
        }
    }
}
