//! Optimisation problem for investment tools.
use super::DemandMap;
use super::ObjectiveCoefficients;
use super::constraints::{
    add_activity_constraints, add_capacity_constraint, add_demand_constraints,
};
use crate::asset::{AssetCapacity, AssetRef};
use crate::commodity::Commodity;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Capacity, Flow};
use anyhow::Result;
use highs::{RowProblem as Problem, Sense};
use indexmap::IndexMap;

/// A decision variable in the optimisation
///
/// This alias represents a column created in the `highs` solver. Callers rely on the order
/// in which columns are added to the problem when extracting solution values.
pub type Variable = highs::Col;

/// Map storing variables for the optimisation problem
struct VariableMap {
    /// Capacity variable.
    ///
    /// This represents absolute capacity for indivisible assets and number of units for
    /// divisible assets.
    capacity_var: Variable,
    /// Activity variables in each time slice
    activity_vars: IndexMap<TimeSliceID, Variable>,
    /// Unmet demand variables
    unmet_demand_vars: IndexMap<TimeSliceID, Variable>,
}

impl VariableMap {
    /// Creates a new variable map by adding variables to the optimisation problem.
    ///
    /// # Arguments
    /// * `problem` - The optimisation problem to add variables to
    /// * `cost_coefficients` - Objective function coefficients for each variable
    ///
    /// # Returns
    /// A new `VariableMap` containing all created decision variables
    fn add_to_problem(
        problem: &mut Problem,
        cost_coefficients: &ObjectiveCoefficients,
        capacity_unit_size: Option<Capacity>,
    ) -> Self {
        // Create capacity variable with its associated cost
        let capacity_coefficient = cost_coefficients.capacity_coefficient.value();
        let capacity_var = match capacity_unit_size {
            Some(unit_size) => {
                // Divisible asset: capacity variable represents number of units
                problem.add_integer_column(capacity_coefficient * unit_size.value(), 0.0..)
            }
            None => {
                // Indivisible asset: capacity variable represents total capacity
                problem.add_column(capacity_coefficient, 0.0..)
            }
        };

        // Create activity variables for each time slice
        let mut activity_vars = IndexMap::new();
        for (time_slice, cost) in &cost_coefficients.activity_coefficients {
            let var = problem.add_column(cost.value(), 0.0..);
            activity_vars.insert(time_slice.clone(), var);
        }

        // Create unmet demand variables for each time slice
        let mut unmet_demand_vars = IndexMap::new();
        for time_slice in cost_coefficients.activity_coefficients.keys() {
            let var = problem.add_column(cost_coefficients.unmet_demand_coefficient.value(), 0.0..);
            unmet_demand_vars.insert(time_slice.clone(), var);
        }

        Self {
            capacity_var,
            activity_vars,
            unmet_demand_vars,
        }
    }
}

/// Map containing optimisation results and coefficients
pub struct ResultsMap {
    /// Capacity variable
    pub capacity: AssetCapacity,
    /// Activity variables in each time slice
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// Unmet demand variables
    pub unmet_demand: DemandMap,
}

/// Adds constraints to the problem.
fn add_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    max_capacity: Option<AssetCapacity>,
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
        time_slice_info,
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
/// The optimisation will use continuous or integer capacity variables depending on whether the
/// asset has a defined unit size.
pub fn perform_optimisation(
    asset: &AssetRef,
    max_capacity: Option<AssetCapacity>,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
    sense: Sense,
) -> Result<ResultsMap> {
    // Create problem and add variables
    let mut problem = Problem::default();
    let variables = VariableMap::add_to_problem(&mut problem, coefficients, asset.unit_size());

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
        // If the asset has a defined unit size, the capacity variable represents number of units,
        // otherwise it represents absolute capacity
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        capacity: match asset.unit_size() {
            Some(unit_size) => {
                AssetCapacity::Discrete(solution_values[0].round() as u32, unit_size)
            }
            None => AssetCapacity::Continuous(Capacity::new(solution_values[0])),
        },
        // The mapping below assumes the column ordering documented on `VariableMap::add_to_problem`:
        // index 0 = capacity, next `n` entries = activities (in the same key order as
        // `cost_coefficients.activity_coefficients`), remaining entries = unmet demand.
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
