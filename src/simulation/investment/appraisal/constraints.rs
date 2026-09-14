//! Constraints for the optimisation problem.
use super::DemandMap;
use super::optimisation::Variable;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::time_slice::{Season, TimeSliceID, TimeSliceInfo, TimeSliceSelection};
use crate::units::{Flow, MoneyPerCapacityPerYear, Year};
use highs::{Model as HighsModel, RowProblem as Problem};
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
    for (ts_selection, limits) in asset.iter_activity_limits() {
        let limits = limits.start().value()..=limits.end().value();

        // Collect activity terms for the time slices in this selection
        let terms = ts_selection
            .iter(time_slice_info)
            .map(|(time_slice, _)| (activity_vars[time_slice], 1.0))
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
    let flow_coeff = asset.get_flow(&commodity.id).unwrap().coeff;
    for ts_selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level) {
        let mut demand_for_ts_selection = Flow(0.0);
        let mut terms = Vec::new();
        for (time_slice, _) in ts_selection.iter(time_slice_info) {
            demand_for_ts_selection += demand[time_slice];
            terms.push((activity_vars[time_slice], flow_coeff.value()));
        }
        problem.add_row(0.0..=demand_for_ts_selection.value(), terms);
    }
}

/// Add seasonal and annual utilisation peak constraints to the problem.
///
/// This is almost identical to equivalent constraints in the full-system dispatch.
pub fn add_utilisation_peak_constraints(
    problem: &mut HighsModel,
    model: &Model,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
) {
    let has_seasonal_penalty =
        model.parameters.seasonal_utilisation_penalty > MoneyPerCapacityPerYear(0.0);
    let has_annual_penalty =
        model.parameters.annual_utilisation_penalty > MoneyPerCapacityPerYear(0.0);

    // If neither penalties are applied, we don't need to add any variables and constraints
    if !has_seasonal_penalty && !has_annual_penalty {
        return;
    }

    // So long as either penalty is applied, we need to add seasonal peak variables and constraints
    let seasonal_peak_vars = add_seasonal_peak_variables(problem, model);
    add_seasonal_peak_constraints(
        problem,
        asset,
        activity_vars,
        &model.time_slice_info,
        &seasonal_peak_vars,
    );

    // If the annual penalty is applied, we also need to add an annual peak variable and constraints
    if has_annual_penalty {
        let annual_peak_var = add_annual_peak_variable(problem, model);
        add_annual_peak_constraints(
            problem,
            &model.time_slice_info,
            annual_peak_var,
            &seasonal_peak_vars,
        );
    }
}

/// Add seasonal peak variables to the problem.
fn add_seasonal_peak_variables(
    problem: &mut HighsModel,
    model: &Model,
) -> IndexMap<Season, highs::Col> {
    let mut seasonal_peak_vars = IndexMap::new();
    for (season, duration) in &model.time_slice_info.seasons {
        // Scale penalty by season duration
        let col_factor = (model.parameters.seasonal_utilisation_penalty * *duration).value();
        let variable = problem.add_col(col_factor, 0.0.., []);
        seasonal_peak_vars.insert(season.clone(), variable);
    }
    seasonal_peak_vars
}

/// Add annual peak variable to the problem.
fn add_annual_peak_variable(problem: &mut HighsModel, model: &Model) -> highs::Col {
    // Penalty is applied over the whole year, so scale by 1 year
    let col_factor = (model.parameters.annual_utilisation_penalty * Year(1.0)).value();
    problem.add_col(col_factor, 0.0.., [])
}

/// Add constraints linking seasonal peak variables to activity variables for each (asset, season) pair.
fn add_seasonal_peak_constraints(
    problem: &mut HighsModel,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
    seasonal_peak_vars: &IndexMap<Season, highs::Col>,
) {
    let activity_per_capacity = asset.process().capacity_to_activity;
    for (season, &peak_variable) in seasonal_peak_vars {
        for (time_slice, ts_length) in
            TimeSliceSelection::Season(season.clone()).iter(time_slice_info)
        {
            let time_slice_fraction = ts_length / Year(1.0);
            let activity_per_capacity_in_time_slice = activity_per_capacity * time_slice_fraction;
            let capacity_required_per_activity = 1.0 / activity_per_capacity_in_time_slice.value();

            // One unit of capacity supports `activity_per_capacity_in_time_slice` activity in
            // this time slice. The peak variable therefore measures the capacity required by
            // the activity in the time slice.
            problem.add_row(
                0.0..,
                [
                    (peak_variable, 1.0),
                    (activity_vars[time_slice], -capacity_required_per_activity),
                ],
            );
        }
    }
}

/// Add constraints linking seasonal peak variables to annual peak variables for each asset.
fn add_annual_peak_constraints(
    problem: &mut HighsModel,
    time_slice_info: &TimeSliceInfo,
    annual_peak_var: highs::Col,
    seasonal_peak_vars: &IndexMap<Season, highs::Col>,
) {
    for season in time_slice_info.seasons.keys() {
        problem.add_row(
            0.0..,
            [(annual_peak_var, 1.0), (seasonal_peak_vars[season], -1.0)],
        );
    }
}
