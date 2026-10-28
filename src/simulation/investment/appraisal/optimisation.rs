//! Optimisation problem for investment tools.
use super::DemandMap;
use super::ObjectiveCoefficients;
use super::constraints::{
    add_activity_constraints, add_demand_constraints, add_equal_utilisation_constraints,
    add_total_capacity_constraints, add_utilisation_peak_constraints,
};
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::process::ProcessID;
use crate::simulation::investment::record_asset_selection;
use crate::simulation::optimisation::ModelError;
use crate::simulation::optimisation::apply_highs_options_from_toml;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Capacity, Flow};
use anyhow::{Context, Result};
use highs::{RowProblem as Problem, Sense};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;

/// A decision variable in the optimisation
///
/// This alias represents a column created in the `highs` solver. Callers rely on the order
/// in which columns are added to the problem when extracting solution values.
pub type Variable = highs::Col;

/// Map containing optimisation results and coefficients
pub struct ResultsMap {
    /// Activity variables in each time slice
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// Remaining unmet demand per time slice, computed post-solve
    pub unmet_demand: DemandMap,
}

/// Adds capacity variables to the problem, one per asset.
///
/// For now these are fixed to the (active) capacity of each asset
fn add_capacity_vars(
    problem: &mut Problem,
    assets: &[AssetRef],
    addition_limits: &HashMap<ProcessID, Capacity>,
) -> IndexMap<AssetRef, Variable> {
    assets
        .iter()
        .map(|asset| {
            let capacity = asset.active_capacity().value();

            // Upper bound to apply once capacity is no longer fixed. Candidates are
            // limited by the process addition limit, if any. Existing assets are capped to their
            // current total capacity
            let _upper_bound = if asset.is_candidate() {
                addition_limits.get(asset.process_id()).map(Capacity::value)
            } else {
                Some(capacity)
            };

            // TODO: when capacity is no longer fixed, use `0.0..=upper`
            // (or `0.0..` if `_upper_bound` is `None`)

            // Zero cost coefficient: capacity is not yet part of the objective
            let var = problem.add_column(0.0, capacity..=capacity);
            (asset.clone(), var)
        })
        .collect()
}

/// Adds activity variables to the problem, one per time slice.
///
/// Returns a map from time slice to the corresponding decision variable.
fn add_activity_vars(
    problem: &mut Problem,
    assets: &[AssetRef],
    coefficients: &HashMap<AssetRef, Arc<ObjectiveCoefficients>>,
) -> IndexMap<(AssetRef, TimeSliceID), Variable> {
    let mut activity_vars = IndexMap::new();
    for asset in assets {
        for (time_slice, cost) in &coefficients[asset].activity_coefficients {
            let var = problem.add_column(cost.value(), 0.0..);
            activity_vars.insert((asset.clone(), time_slice.clone()), var);
        }
    }
    activity_vars
}

/// Adds constraints to the problem.
#[allow(clippy::too_many_arguments)]
fn add_constraints(
    problem: &mut Problem,
    model: &Model,
    assets: &[AssetRef],
    commodity: &Commodity,
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
    capacity_vars: &IndexMap<AssetRef, Variable>,
    total_limits: &HashMap<ProcessID, Capacity>,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
) {
    add_total_capacity_constraints(problem, assets, capacity_vars, total_limits);
    add_activity_constraints(
        problem,
        assets,
        activity_vars,
        capacity_vars,
        time_slice_info,
    );
    add_utilisation_peak_constraints(problem, model, assets, activity_vars);
    add_demand_constraints(
        problem,
        assets,
        commodity,
        time_slice_info,
        demand,
        activity_vars,
    );
    add_equal_utilisation_constraints(problem, activity_vars, time_slice_info, assets);
}

/// Computes remaining unmet demand per time-slice selection after a solve.
///
/// For each time-slice selection at the commodity's balance level, the selection-level residual
/// (`demand - supply`, clamped to zero) is stored directly. Downstream consumers operate at the
/// selection level (e.g. the next round's demand constraint and `is_any_remaining_demand`), so
/// there is no need to distribute values across individual time slices.
fn compute_unmet_demand(
    demand: &DemandMap,
    assets: &[AssetRef],
    activity: &IndexMap<(AssetRef, TimeSliceID), Activity>,
    commodity: &Commodity,
    time_slice_info: &TimeSliceInfo,
) -> DemandMap {
    time_slice_info
        .iter_selections_at_level(commodity.time_slice_level)
        .map(|ts_selection| {
            let demand_for_selection = demand[&ts_selection];

            // Calculate supply for selection: sum output over all assets over all time slices in
            // the selection
            let supply_for_selection: Flow = assets
                .iter()
                .flat_map(|asset| {
                    let coefficient = asset.get_flow(&commodity.id).unwrap().coeff;

                    ts_selection
                        .iter(time_slice_info)
                        .map(move |(time_slice, _)| {
                            let value = activity[&(asset.clone(), time_slice.clone())];
                            value * coefficient
                        })
                })
                .sum();

            let unmet = (demand_for_selection - supply_for_selection).max(Flow(0.0));
            (ts_selection, unmet)
        })
        .collect()
}

/// Performs optimisation for an asset, given the coefficients and demand.
pub fn perform_optimisation(
    model: &Model,
    selected_assets: &[AssetRef],
    asset: &AssetRef,
    commodity: &Commodity,
    coefficients: &HashMap<AssetRef, Arc<ObjectiveCoefficients>>,
    addition_limits: &HashMap<ProcessID, Capacity>,
    total_limits: &HashMap<ProcessID, Capacity>,
    demand: &DemandMap,
) -> Result<ResultsMap> {
    // Record hypothetical asset selection
    let mut assets = selected_assets.to_vec();
    record_asset_selection(asset.clone(), &mut assets);

    // Create problem
    let mut problem = Problem::default();
    let activity_vars = add_activity_vars(&mut problem, &assets, coefficients);
    let capacity_vars = add_capacity_vars(&mut problem, &assets, addition_limits); // must come after activity vars

    // Add constraints
    add_constraints(
        &mut problem,
        model,
        &assets,
        commodity,
        &activity_vars,
        &capacity_vars,
        total_limits,
        demand,
        &model.time_slice_info,
    );

    // Solve model
    let mut highs_model = problem.optimise(Sense::Maximise);
    apply_highs_options_from_toml(&mut highs_model, &model.parameters.highs.appraisal_options)
        .context("Failed to apply custom HiGHS options to appraisal optimisation")?;
    // Enforce single-threaded solving: when appraisals run in parallel via Rayon each HiGHS
    // instance must not spawn additional worker threads. Setting `parallel="off"` disables
    // HiGHS's concurrent simplex strategy without touching the global thread-pool scheduler
    // (setting `threads=N` would fail if the scheduler was already initialised on this thread
    // by a previous solve with a different count).
    highs_model.set_option("parallel", "off");
    let solution = solve_optimal(highs_model)
        .map_err(ModelError::into_anyhow)?
        .get_solution();

    // Extract asset activities and calculate unmet demand
    let solution_values = solution.columns();
    let all_activity = activity_vars
        .keys()
        .zip(solution_values.iter())
        .map(|(key, &value)| (key.clone(), Activity::new(value)))
        .collect();
    let unmet_demand = compute_unmet_demand(
        demand,
        &assets,
        &all_activity,
        commodity,
        &model.time_slice_info,
    );

    // Collect activity for the asset of interest
    // TODO: this should be marginal activity
    let activity = all_activity
        .iter()
        .filter(|&((a, _ts), _value)| a == asset)
        .map(|((_a, ts), value)| (ts.clone(), *value))
        .collect();

    Ok(ResultsMap {
        activity,
        unmet_demand,
    })
}
