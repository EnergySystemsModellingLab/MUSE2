//! Optimisation problem for investment tools.
use super::DemandMap;
use super::ObjectiveCoefficients;
use super::constraints::{add_activity_constraints, add_demand_constraints};
use crate::asset::AssetCapacity;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::simulation::optimisation::ModelError;
use crate::simulation::optimisation::apply_highs_options_from_toml;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Flow};
use anyhow::{Context, Result};
use highs::{RowProblem as Problem, Sense};
use indexmap::IndexMap;

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

/// Adds activity variables to the problem, one per time slice.
///
/// Returns a map from time slice to the corresponding decision variable.
fn add_activity_vars(
    problem: &mut Problem,
    cost_coefficients: &ObjectiveCoefficients,
) -> IndexMap<TimeSliceID, Variable> {
    cost_coefficients
        .activity_coefficients
        .iter()
        .map(|(time_slice, cost)| {
            let var = problem.add_column(cost.value(), 0.0..);
            (time_slice.clone(), var)
        })
        .collect()
}

/// Adds constraints to the problem.
fn add_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    max_capacity: AssetCapacity,
    commodity: &Commodity,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
) {
    add_activity_constraints(problem, asset, max_capacity, activity_vars, time_slice_info);
    add_demand_constraints(
        problem,
        asset,
        commodity,
        time_slice_info,
        demand,
        activity_vars,
    );
}

/// Computes remaining unmet demand per time-slice selection after a solve.
///
/// For each time-slice selection at the commodity's balance level, the selection-level residual
/// (`demand - supply`, clamped to zero) is stored directly. Downstream consumers operate at the
/// selection level (e.g. the next round's demand constraint and `is_any_remaining_demand`), so
/// there is no need to distribute values across individual time slices.
fn compute_unmet_demand(
    demand: &DemandMap,
    activity: &IndexMap<TimeSliceID, Activity>,
    commodity: &Commodity,
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
) -> DemandMap {
    let flow_coeff = asset.get_flow(&commodity.id).unwrap().coeff;
    time_slice_info
        .iter_selections_at_level(commodity.time_slice_level)
        .map(|ts_selection| {
            let demand_for_selection = demand[&ts_selection];
            let supply_for_selection: Flow = ts_selection
                .iter(time_slice_info)
                .map(|(ts, _)| activity[ts] * flow_coeff)
                .sum();
            let unmet = (demand_for_selection - supply_for_selection).max(Flow(0.0));
            (ts_selection, unmet)
        })
        .collect()
}

/// Performs optimisation for an asset, given the coefficients and demand.
///
/// Will either maximise or minimise the objective function, depending on the `sense` parameter.
/// The optimisation will use continuous or integer capacity variables depending on whether the
/// asset has a defined unit size.
pub fn perform_optimisation(
    model: &Model,
    asset: &AssetRef,
    max_capacity: AssetCapacity,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
) -> Result<ResultsMap> {
    // Create problem and add variables
    let mut problem = Problem::default();
    let activity_vars = add_activity_vars(&mut problem, coefficients);

    // Add constraints
    add_constraints(
        &mut problem,
        asset,
        max_capacity,
        commodity,
        &activity_vars,
        demand,
        &model.time_slice_info,
    );

    // Solve model
    let mut highs_model = problem.optimise(Sense::Maximise);
    apply_highs_options_from_toml(&mut highs_model, &model.parameters.highs.appraisal_options)
        .context("Failed to apply custom HiGHS options to appraisal optimisation")?;
    let solution = solve_optimal(highs_model)
        .map_err(ModelError::into_anyhow)?
        .get_solution();
    let solution_values = solution.columns();
    let activity = activity_vars
        .keys()
        .zip(solution_values.iter())
        .map(|(time_slice, &value)| (time_slice.clone(), Activity::new(value)))
        .collect();
    let unmet_demand =
        compute_unmet_demand(demand, &activity, commodity, asset, &model.time_slice_info);
    Ok(ResultsMap {
        activity,
        unmet_demand,
    })
}
