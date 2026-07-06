//! Constraints for the optimisation problem.
use super::DemandMap;
use super::optimisation::Variable;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::Flow;
use highs::RowProblem as Problem;
use indexmap::IndexMap;

/// Adds activity constraints to the problem.
///
/// Constrains the activity variables to be within the asset's activity limits.
///
/// The asset's per-capacity activity limits are scaled by the asset's capacity to give
/// absolute bounds, and a single bounded constraint is added per time-slice selection covering the
/// sum of activity in that selection.
pub fn add_activity_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
) {
    let capacity = asset.capacity().total_capacity();
    for (ts_selection, limits) in asset.iter_activity_per_capacity_limits() {
        let limits = (capacity * *limits.start()).value()..=(capacity * *limits.end()).value();

        // Collect activity terms for the time slices in this selection
        let terms = ts_selection
            .iter(time_slice_info)
            .map(|(time_slice, _)| (*activity_vars.get(time_slice).unwrap(), 1.0))
            .collect::<Vec<_>>();

        // Constraint: sum of activities in selection within limits
        problem.add_row(limits, &terms);
    }
}

/// Adds demand constraints to the problem.
///
/// Constrains supply to be less than or equal to demand. One inequality constraint is added per
/// time-slice selection at the commodity's balance level, capping the sum of activity (scaled by
/// flow coefficients) to the total demand for that selection.
pub fn add_demand_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    commodity: &Commodity,
    time_slice_info: &TimeSliceInfo,
    demand: &DemandMap,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
) {
    for ts_selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level) {
        let mut demand_for_ts_selection = Flow(0.0);
        let mut terms = Vec::new();
        for (time_slice, _) in ts_selection.iter(time_slice_info) {
            demand_for_ts_selection += demand[time_slice];
            let flow_coeff = asset.get_flow(&commodity.id).unwrap().coeff;
            terms.push((activity_vars[time_slice], flow_coeff.value()));
        }
        problem.add_row(0.0..=demand_for_ts_selection.value(), terms);
    }
}
