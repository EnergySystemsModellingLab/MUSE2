//! Optimisation problem for investment tools.
use super::DemandMap;
use super::constraints::{
    add_activity_constraints, add_demand_constraints, add_equal_utilisation_constraints,
    add_total_capacity_constraints, add_utilisation_peak_constraints,
};
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::process::ProcessID;
use crate::simulation::optimisation::ModelError;
use crate::simulation::optimisation::apply_highs_options_from_toml;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Capacity, MoneyPerActivity, MoneyPerCapacity};
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
    /// Capacities
    pub capacities: IndexMap<AssetRef, Capacity>,
    /// Activity variables in each time slice
    pub activities: IndexMap<(AssetRef, TimeSliceID), Activity>,
}

/// Adds capacity variables to the problem, one per asset.
///
/// Each variable is continuous and represents the asset's absolute capacity, with a cost
/// coefficient taken from `coefficients`. Bounds are `0..=upper`, where `upper` is:
/// - for candidates (at most one per process), the process addition limit, if any (otherwise
///   unbounded)
/// - for existing assets, their total capacity, including any mothballed capacity (they can be
///   reduced, but not increased)
fn add_capacity_vars(
    problem: &mut Problem,
    assets: &[AssetRef],
    coefficients: &HashMap<AssetRef, MoneyPerCapacity>,
    addition_limits: &HashMap<ProcessID, Capacity>,
) -> IndexMap<AssetRef, Variable> {
    assets
        .iter()
        .map(|asset| {
            let upper_bound = if asset.is_candidate() {
                addition_limits.get(asset.process_id()).copied()
            } else {
                Some(asset.total_capacity())
            };

            let cost = coefficients[asset].value();
            let var = match upper_bound {
                Some(upper) => problem.add_column(cost, 0.0..=upper.value()),
                None => problem.add_column(cost, 0.0..),
            };
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
    coefficients: &HashMap<AssetRef, Arc<IndexMap<TimeSliceID, MoneyPerActivity>>>,
) -> IndexMap<(AssetRef, TimeSliceID), Variable> {
    let mut activity_vars = IndexMap::new();
    for asset in assets {
        for (time_slice, cost) in coefficients[asset].iter() {
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

/// Performs optimisation for an asset, given the coefficients and demand.
#[allow(clippy::too_many_arguments)]
pub fn perform_optimisation(
    model: &Model,
    assets: &[AssetRef],
    commodity: &Commodity,
    activity_coefficients: &HashMap<AssetRef, Arc<IndexMap<TimeSliceID, MoneyPerActivity>>>,
    capacity_coefficients: &HashMap<AssetRef, MoneyPerCapacity>,
    addition_limits: &HashMap<ProcessID, Capacity>,
    total_limits: &HashMap<ProcessID, Capacity>,
    demand: &DemandMap,
) -> Result<ResultsMap> {
    // Create problem
    let mut problem = Problem::default();
    let activity_vars = add_activity_vars(&mut problem, assets, activity_coefficients);
    let capacity_vars =
        add_capacity_vars(&mut problem, assets, capacity_coefficients, addition_limits); // must come after activity vars

    // Add constraints
    add_constraints(
        &mut problem,
        model,
        assets,
        commodity,
        &activity_vars,
        &capacity_vars,
        total_limits,
        demand,
        &model.time_slice_info,
    );

    // Solve model
    let mut highs_model = problem.optimise(Sense::Minimise);
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

    // Extract asset activities and capacities. Columns are in the order they were added to the
    // problem: activity variables, then capacity variables.
    let solution_values = solution.columns();
    let (activity_values, remaining_values) = solution_values.split_at(activity_vars.len());
    let capacity_values = &remaining_values[..capacity_vars.len()];

    let activities: IndexMap<_, _> = activity_vars
        .keys()
        .zip(activity_values)
        .map(|(key, &value)| (key.clone(), Activity::new(value)))
        .collect();
    let capacities: IndexMap<_, _> = capacity_vars
        .keys()
        .zip(capacity_values)
        .map(|(asset, &value)| (asset.clone(), Capacity::new(value)))
        .collect();

    Ok(ResultsMap {
        capacities,
        activities,
    })
}
