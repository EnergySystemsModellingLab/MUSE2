//! Optimisation problem for investment tools.
use super::DemandMap;
use super::constraints::{add_activity_constraints, add_demand_constraints};
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::simulation::optimisation::ModelError;
use crate::simulation::optimisation::apply_highs_options_from_toml;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Dimensionless, Flow, MoneyPerActivity};
use anyhow::{Context, Result};
use highs::{RowProblem as Problem, Sense};
use indexmap::IndexMap;

/// A decision variable in the optimisation
///
/// This alias represents a column created in the `highs` solver. Callers rely on the order
/// in which columns are added to the problem when extracting solution values.
pub type Variable = highs::Col;

/// The result of optimising the dispatch of a candidate investment.
pub struct AppraisalOptimisation {
    /// Activity variables in each time slice
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// Remaining unmet demand per time slice, computed post-solve
    pub unmet_demand: DemandMap,
}

impl AppraisalOptimisation {
    /// Returns whether the asset has positive activity in at least one time slice.
    pub fn has_activity(&self) -> bool {
        self.activity
            .values()
            .any(|activity| *activity != Activity(0.0))
    }
}

/// Adds activity variables to the problem, one per time slice.
///
/// Returns a map from time slice to the corresponding decision variable.
fn add_activity_vars(
    problem: &mut Problem,
    activity_coefficients: &IndexMap<TimeSliceID, MoneyPerActivity>,
) -> IndexMap<TimeSliceID, Variable> {
    activity_coefficients
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
    commodity: &Commodity,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
) {
    add_activity_constraints(problem, asset, activity_vars, time_slice_info);
    add_demand_constraints(
        problem,
        asset,
        commodity,
        time_slice_info,
        demand,
        activity_vars,
    );
}

/// Computes remaining unmet demand per time slice after a solve.
///
/// For each time-slice selection at the commodity's balance level, the selection-level residual
/// (`demand_total - supply_total`, clamped to zero) is divided equally across the time slices in
/// the selection.
///
/// The exact per-time-slice distribution is arbitrary: all downstream consumers sum values back up
/// to the selection level before using them (e.g. the next round's demand constraint, and
/// `is_any_remaining_demand`), so only the selection-level total needs to be correct.
fn compute_unmet_demand(
    demand: &DemandMap,
    activity: &IndexMap<TimeSliceID, Activity>,
    commodity: &Commodity,
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
) -> DemandMap {
    let mut unmet_demand = DemandMap::new();
    let flow_coeff = asset.get_flow(&commodity.id).unwrap().coeff;
    for ts_selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level) {
        let time_slices: Vec<_> = ts_selection.iter(time_slice_info).collect();
        let demand_for_selection: Flow = time_slices.iter().map(|(ts, _)| demand[*ts]).sum();
        let supply_for_selection: Flow = time_slices
            .iter()
            .map(|(ts, _)| activity[*ts] * flow_coeff)
            .sum();

        #[allow(clippy::cast_precision_loss)]
        let unmet_per_slice = (demand_for_selection - supply_for_selection).max(Flow(0.0))
            / Dimensionless(time_slices.len() as f64);
        for (time_slice, _) in &time_slices {
            unmet_demand.insert((*time_slice).clone(), unmet_per_slice);
        }
    }
    unmet_demand
}

/// Performs optimisation for an asset, given the coefficients and demand.
pub fn perform_optimisation(
    model: &Model,
    asset: &AssetRef,
    commodity: &Commodity,
    activity_coefficients: &IndexMap<TimeSliceID, MoneyPerActivity>,
    demand: &DemandMap,
) -> Result<AppraisalOptimisation> {
    // Create problem and add variables
    let mut problem = Problem::default();
    let activity_vars = add_activity_vars(&mut problem, activity_coefficients);

    // Add constraints
    add_constraints(
        &mut problem,
        asset,
        commodity,
        &activity_vars,
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
    let solution_values = solution.columns();
    let activity = activity_vars
        .keys()
        .zip(solution_values.iter())
        .map(|(time_slice, &value)| (time_slice.clone(), Activity::new(value)))
        .collect();
    let unmet_demand =
        compute_unmet_demand(demand, &activity, commodity, asset, &model.time_slice_info);
    Ok(AppraisalOptimisation {
        activity,
        unmet_demand,
    })
}
