//! Optimisation problem for investment tools.
use super::DemandMap;
use super::ObjectiveCoefficients;
use super::constraints::{
    add_activity_constraints, add_capacity_constraint, add_demand_constraints,
};
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Capacity, Flow};
use anyhow::Result;
use highs::{RowProblem as Problem, Sense};
use indexmap::IndexMap;

/// A decision variable in the optimisation
pub type Variable = highs::Col;

/// Map storing variables for the optimisation problem
struct VariableMap {
    /// Capacity variable
    pub capacity_var: Variable,
    /// Activity variables in each time slice
    pub activity_vars: IndexMap<TimeSliceID, Variable>,
    // Unmet demand variables
    pub unmet_demand_vars: IndexMap<TimeSliceID, Variable>,
}

/// Map containing optimisation results and coefficients
pub struct ResultsMap {
    /// Capacity variable
    pub capacity: Capacity,
    /// Activity variables in each time slice
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// Unmet demand variables
    pub unmet_demand: DemandMap,
}

fn add_variables_to_problem(
    problem: &mut Problem,
    cost_coefficients: &ObjectiveCoefficients,
) -> VariableMap {
    // Create capacity variable
    let capacity_var = problem.add_column(cost_coefficients.capacity_coefficient.value(), 0.0..);

    // Create activity variables
    let mut activity_vars = IndexMap::new();
    for (time_slice, cost) in &cost_coefficients.activity_coefficients {
        let var = problem.add_column(cost.value(), 0.0..);
        activity_vars.insert(time_slice.clone(), var);
    }

    // Create unmet demand variables
    // One per time slice, all of which use the same coefficient
    let mut unmet_demand_vars = IndexMap::new();
    for time_slice in cost_coefficients.activity_coefficients.keys() {
        let var = problem.add_column(cost_coefficients.unmet_demand_coefficient.value(), 0.0..);
        unmet_demand_vars.insert(time_slice.clone(), var);
    }

    VariableMap {
        capacity_var,
        activity_vars,
        unmet_demand_vars,
    }
}

/// Adds constraints to the problem.
fn add_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    variables: &VariableMap,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
) {
    add_capacity_constraint(problem, asset, max_capacity, variables.capacity_var);
    add_activity_constraints(
        problem,
        asset,
        variables.capacity_var,
        &variables.activity_vars,
    );
    add_demand_constraints(
        problem,
        asset,
        commodity,
        time_slice_info,
        demand,
        &variables.activity_vars,
        &variables.unmet_demand_vars,
    );
}

/// Performs optimisation for an asset, given the coefficients and demand.
///
/// Will either maximise or minimise the objective function, depending on the `sense` parameter.
#[allow(clippy::too_many_arguments)]
pub fn perform_optimisation(
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
    sense: Sense,
) -> Result<ResultsMap> {
    // Set up problem
    let mut problem = Problem::default();
    let variables = add_variables_to_problem(&mut problem, coefficients);

    // Add constraints
    add_constraints(
        &mut problem,
        asset,
        max_capacity,
        commodity,
        &variables,
        demand,
        time_slice_info,
    );

    // Solve model
    let solution = solve_optimal(problem.optimise(sense))?.get_solution();
    let solution_values = solution.columns();
    Ok(ResultsMap {
        capacity: Capacity::new(solution_values[0]),
        activity: variables
            .activity_vars
            .keys()
            .zip(solution_values[1..].iter())
            .map(|(time_slice, &value)| (time_slice.clone(), Activity::new(value)))
            .collect(),
        unmet_demand: variables
            .unmet_demand_vars
            .keys()
            .zip(solution_values[variables.activity_vars.len() + 1..].iter())
            .map(|(time_slice, &value)| (time_slice.clone(), Flow::new(value)))
            .collect(),
    })
}
