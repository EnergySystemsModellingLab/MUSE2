//! Constraints for the optimisation problem.
use super::DemandMap;
use super::optimisation::Variable;
use crate::asset::{AssetRef, AssetState};
use crate::commodity::Commodity;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Capacity, Flow};
use highs::RowProblem as Problem;
use indexmap::IndexMap;
use itertools::Itertools;

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
    for (ts_selection, limits) in asset.iter_activity_limits(time_slice_info) {
        let limits = limits.start().value()..=limits.end().value();
        let terms = ts_selection
            .iter(time_slice_info)
            .map(|(time_slice, _)| {
                let var = *activity_vars.get(time_slice).unwrap();
                (var, 1.0)
            })
            .collect_vec();
        problem.add_row(limits, &terms);
    }
}

fn add_activity_constraints_for_candidate(
    problem: &mut Problem,
    asset: &AssetRef,
    capacity_var: Variable,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
) {
    for (ts_selection, limits) in asset.iter_activity_limits(time_slice_info) {
        let upper_limit = limits.end().value();
        let lower_limit = limits.start().value();

        let mut terms_upper = vec![(capacity_var, -upper_limit)];
        let mut terms_lower = vec![(capacity_var, lower_limit)];
        for (time_slice, _) in ts_selection.iter(time_slice_info) {
            let var = *activity_vars.get(time_slice).unwrap();
            terms_upper.push((var, 1.0));
            terms_lower.push((var, -1.0));
        }

        // Upper bound: activity ≤ capacity * upper_limit
        problem.add_row(..=0.0, &terms_upper);

        // Lower bound: activity ≥ capacity * lower_limit
        problem.add_row(..=0.0, &terms_lower);
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
