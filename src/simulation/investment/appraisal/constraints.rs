//! Constraints for the optimisation problem.
use super::DemandMap;
use super::optimisation::Variable;
use crate::asset::{AssetRef, AssetState};
use crate::commodity::Commodity;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Capacity, Flow};
use highs::RowProblem as Problem;
use indexmap::IndexMap;

/// Adds a capacity constraint to the problem.
///
/// The behaviour depends on whether the asset is commissioned or a candidate:
/// - For a commissioned asset, the capacity is fixed.
/// - For a candidate asset, the capacity is variable between zero and an upper bound.
pub fn add_capacity_constraint(
    problem: &mut Problem,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    capacity_var: Variable,
) {
    let capacity = max_capacity.unwrap_or(asset.capacity());
    let bounds = match asset.state() {
        AssetState::Commissioned { .. } => {
            // Fixed capacity for commissioned assets
            capacity.value()..=capacity.value()
        }
        AssetState::Candidate => {
            // Variable capacity between 0 and max for candidate assets
            0.0..=capacity.value()
        }
        _ => panic!(
            "add_capacity_constraint should only be called with Commissioned or Candidate assets"
        ),
    };
    problem.add_row(bounds, [(capacity_var, 1.0)]);
}

/// Adds activity constraints to the problem.
///
/// Constrains the activity variables to be within the asset's activity limits.
///
/// The behaviour depends on whether the asset is commissioned or a candidate:
/// - For an commissioned asset, the activity limits have fixed bounds based on the asset's (fixed)
///   capacity.
/// - For a candidate asset, the activity limits depend on the capacity of the asset, which is
///   itself variable. The constraints are therefore applied to both the capacity and activity
///   variables. We need separate constraints for the upper and lower bounds.
pub fn add_activity_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    capacity_var: Variable,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
) {
    match asset.state() {
        AssetState::Commissioned { .. } => {
            add_activity_constraints_for_existing(problem, asset, activity_vars, time_slice_info);
        }
        AssetState::Candidate => {
            add_activity_constraints_for_candidate(
                problem,
                asset,
                capacity_var,
                activity_vars,
                time_slice_info,
            );
        }
        _ => panic!(
            "add_activity_constraints should only be called with Commissioned or Candidate assets"
        ),
    }
}

fn add_activity_constraints_for_existing(
    problem: &mut Problem,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
) {
    for (time_slice, var) in activity_vars {
        let limits = asset.get_activity_limits(time_slice);
        let duration = time_slice_info.time_slices[time_slice];
        let limits = (*limits.start() * duration).value()..=(*limits.end() * duration).value();
        problem.add_row(limits, [(*var, 1.0)]);
    }
}

fn add_activity_constraints_for_candidate(
    problem: &mut Problem,
    asset: &AssetRef,
    capacity_var: Variable,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
) {
    for (time_slice, activity_var) in activity_vars {
        let limits = asset.get_activity_per_capacity_limits(time_slice);
        let duration = time_slice_info.time_slices[time_slice];
        let lower_limit = (*limits.start() * duration).value();
        let upper_limit = (*limits.end() * duration).value();

        // Upper bound: activity ≤ capacity * upper_limit
        problem.add_row(..=0.0, [(*activity_var, 1.0), (capacity_var, -upper_limit)]);

        // Lower bound: activity ≥ capacity * lower_limit
        problem.add_row(..=0.0, [(*activity_var, -1.0), (capacity_var, lower_limit)]);
    }
}

/// Adds demand constraints to the problem.
///
/// Constrains supply to be less than or equal to demand, which adapts based on the commodity's
/// balance level.
pub fn add_demand_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    commodity: &Commodity,
    time_slice_info: &TimeSliceInfo,
    demand: &DemandMap,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    unmet_demand_vars: &IndexMap<TimeSliceID, Variable>,
) {
    for ts_selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level) {
        let mut demand_for_ts_selection = Flow(0.0);
        let mut terms = Vec::new();
        for (time_slice, _) in ts_selection.iter(time_slice_info) {
            demand_for_ts_selection += demand[time_slice];
            let flow_coeff = asset.get_flow(&commodity.id).unwrap().coeff;
            terms.push((activity_vars[time_slice], flow_coeff.value()));
            terms.push((unmet_demand_vars[time_slice], 1.0));
        }
        problem.add_row(
            demand_for_ts_selection.value()..=demand_for_ts_selection.value(),
            terms,
        );
    }
}
