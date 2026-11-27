//! Constraints for fitting an asset to the demand profile.
use super::optimisation::Variable;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::simulation::agent::DemandMap;
use crate::time_slice::{Season, TimeSliceID, TimeSliceInfo, TimeSliceSelection};
use crate::units::{MoneyPerCapacityPerYear, Year};
use highs::RowProblem as Problem;
use indexmap::IndexMap;

/// Constrain activity to the asset's availability limits, scaled by the capacity variable.
///
/// For each time-slice selection, the sum of activity is bounded below and above by the asset's
/// per-capacity activity limits multiplied by the fitted capacity.
pub fn add_activity_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    capacity_var: Variable,
    time_slice_info: &TimeSliceInfo,
) {
    for (selection, limits) in asset.iter_activity_per_capacity_limits() {
        let activity_terms = selection
            .iter(time_slice_info)
            .map(|(time_slice, _)| (activity_vars[time_slice], 1.0))
            .collect::<Vec<_>>();

        let mut upper_terms = activity_terms.clone();
        upper_terms.push((capacity_var, -limits.end().value()));
        problem.add_row(..=0.0, upper_terms);

        let mut lower_terms = activity_terms;
        lower_terms.push((capacity_var, -limits.start().value()));
        problem.add_row(0.0.., lower_terms);
    }
}

/// Require supply to equal demand at every selection of the commodity's balance level.
///
/// A separate equality is added for each selection, so a feasible option reproduces the demand
/// profile rather than merely meeting its total annual volume.
pub fn add_demand_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    commodity: &Commodity,
    time_slice_info: &TimeSliceInfo,
    demand: &DemandMap,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
) {
    let flow_coefficient = asset.get_flow(&commodity.id).unwrap().coeff.value();
    for selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level) {
        let terms = selection
            .iter(time_slice_info)
            .map(|(time_slice, _)| (activity_vars[time_slice], flow_coefficient))
            .collect::<Vec<_>>();
        let required = demand[&selection].value();
        problem.add_row(required..=required, terms);
    }
}

/// Add seasonal and annual peak-utilisation cost variables used by portfolio.
///
/// Peak variables carry their configured penalty in the minimisation objective and are constrained
/// to be at least the capacity required by activity in each time slice. Annual peak is constrained
/// to be at least each seasonal peak. If neither penalty is configured, no peak variables or rows
/// are needed.
pub fn add_utilisation_peak_constraints(
    problem: &mut Problem,
    model: &Model,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
) {
    let seasonal_penalty =
        model.parameters.seasonal_utilisation_penalty > MoneyPerCapacityPerYear(0.0);
    let annual_penalty = model.parameters.annual_utilisation_penalty > MoneyPerCapacityPerYear(0.0);
    if !seasonal_penalty && !annual_penalty {
        return;
    }

    // Annual peak is linked through the seasonal peaks, so seasonal variables are also needed when
    // only the annual penalty is configured.
    let seasonal_peak_vars = add_seasonal_peak_variables(problem, model);
    add_seasonal_peak_constraints(
        problem,
        asset,
        activity_vars,
        &model.time_slice_info,
        &seasonal_peak_vars,
    );
    if annual_penalty {
        let annual_peak_var = add_annual_peak_variable(problem, model);
        add_annual_peak_constraints(
            problem,
            &model.time_slice_info,
            annual_peak_var,
            &seasonal_peak_vars,
        );
    }
}

/// Add one non-negative seasonal peak-capacity variable per season.
fn add_seasonal_peak_variables(problem: &mut Problem, model: &Model) -> IndexMap<Season, Variable> {
    model
        .time_slice_info
        .seasons
        .iter()
        .map(|(season, duration)| {
            let cost = model.parameters.seasonal_utilisation_penalty * *duration;
            (season.clone(), problem.add_column(cost.value(), 0.0..))
        })
        .collect()
}

/// Add a non-negative annual peak-capacity variable.
fn add_annual_peak_variable(problem: &mut Problem, model: &Model) -> Variable {
    let cost = model.parameters.annual_utilisation_penalty * Year(1.0);
    problem.add_column(cost.value(), 0.0..)
}

/// Link each seasonal peak to the capacity required by activity in each of its time slices.
fn add_seasonal_peak_constraints(
    problem: &mut Problem,
    asset: &AssetRef,
    activity_vars: &IndexMap<TimeSliceID, Variable>,
    time_slice_info: &TimeSliceInfo,
    seasonal_peak_vars: &IndexMap<Season, Variable>,
) {
    let activity_per_capacity = asset.process().capacity_to_activity;
    for (season, &peak_var) in seasonal_peak_vars {
        for (time_slice, duration) in
            TimeSliceSelection::Season(season.clone()).iter(time_slice_info)
        {
            // Convert the process's annual activity-per-capacity rate to the duration of this
            // time slice. The peak variable then bounds the capacity needed for its activity.
            let activity_per_capacity_in_slice = activity_per_capacity * (duration / Year(1.0));
            let capacity_per_activity = 1.0 / activity_per_capacity_in_slice.value();
            problem.add_row(
                0.0..,
                [
                    (peak_var, 1.0),
                    (activity_vars[time_slice], -capacity_per_activity),
                ],
            );
        }
    }
}

/// Constrain the annual peak to be at least every seasonal peak.
fn add_annual_peak_constraints(
    problem: &mut Problem,
    time_slice_info: &TimeSliceInfo,
    annual_peak_var: Variable,
    seasonal_peak_vars: &IndexMap<Season, Variable>,
) {
    for season in time_slice_info.seasons.keys() {
        problem.add_row(
            0.0..,
            [(annual_peak_var, 1.0), (seasonal_peak_vars[season], -1.0)],
        );
    }
}
