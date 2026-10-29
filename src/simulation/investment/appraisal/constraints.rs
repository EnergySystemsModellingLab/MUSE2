//! Constraints for the optimisation problem.
use super::DemandMap;
use super::optimisation::Variable;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::process::ProcessID;
use crate::simulation::optimisation::group_dispatch_equivalent_assets;
use crate::time_slice::{Season, TimeSliceID, TimeSliceInfo, TimeSliceSelection};
use crate::units::{Capacity, MoneyPerCapacityPerYear, Year};
use highs::RowProblem as Problem;
use indexmap::IndexMap;
use std::collections::HashMap;

/// Adds total capacity limit constraints to the problem.
///
/// For each limited process, the sum of capacity variables across all of its assets (candidate
/// and commissioned) must not exceed the agent's total capacity limit for that process.
pub fn add_total_capacity_constraints(
    problem: &mut Problem,
    assets: &[AssetRef],
    capacity_vars: &IndexMap<AssetRef, Variable>,
    total_limits: &HashMap<ProcessID, Capacity>,
) {
    // IndexMap keeps row order deterministic
    let mut terms_by_process: IndexMap<&ProcessID, Vec<(Variable, f64)>> = IndexMap::new();
    for asset in assets {
        if total_limits.contains_key(asset.process_id()) {
            terms_by_process
                .entry(asset.process_id())
                .or_default()
                .push((capacity_vars[asset], 1.0));
        }
    }

    for (process_id, terms) in terms_by_process {
        problem.add_row(..=total_limits[process_id].value(), terms);
    }
}

/// Adds activity constraints to the problem.
///
/// Constrains the activity variables to be within the asset's per-capacity activity limits,
/// scaled by the asset's capacity variable. Two rows (upper and lower bound) are added per
/// time-slice selection.
pub fn add_activity_constraints(
    problem: &mut Problem,
    assets: &[AssetRef],
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
    capacity_vars: &IndexMap<AssetRef, Variable>,
    time_slice_info: &TimeSliceInfo,
) {
    for asset in assets {
        let capacity_var = capacity_vars[asset];

        for (ts_selection, limits) in asset.iter_activity_per_capacity_limits() {
            let activity_terms = ts_selection
                .iter(time_slice_info)
                .map(|(time_slice, _)| (activity_vars[&(asset.clone(), time_slice.clone())], 1.0))
                .collect::<Vec<_>>();

            // Upper bound: sum(activity) - capacity * upper_limit_per_capacity ≤ 0
            let mut upper_terms = activity_terms.clone();
            upper_terms.push((capacity_var, -limits.end().value()));
            problem.add_row(..=0.0, &upper_terms);

            // Lower bound: sum(activity) - capacity * lower_limit_per_capacity ≥ 0
            let mut lower_terms = activity_terms;
            lower_terms.push((capacity_var, -limits.start().value()));
            problem.add_row(0.0.., &lower_terms);
        }
    }
}

/// Adds demand constraints to the problem.
///
/// Constrains supply to be greater than or equal to demand. One inequality constraint is added per
/// time-slice selection at the commodity's balance level, requiring the sum of activity (scaled by
/// flow coefficients) to be at least the total demand for that selection.
pub fn add_demand_constraints(
    problem: &mut Problem,
    assets: &[AssetRef],
    commodity: &Commodity,
    time_slice_info: &TimeSliceInfo,
    demand: &DemandMap,
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
) {
    // Create a constraint for each ts-selection
    for ts_selection in time_slice_info.iter_selections_at_level(commodity.time_slice_level) {
        // Collect terms: all assets in all time slices in the selection
        let terms = assets
            .iter()
            .flat_map(|asset| {
                let coefficient = asset.get_flow(&commodity.id).unwrap().coeff.value();

                ts_selection
                    .iter(time_slice_info)
                    .map(move |(time_slice, _)| {
                        (
                            activity_vars[&(asset.clone(), time_slice.clone())],
                            coefficient,
                        )
                    })
            })
            .collect::<Vec<_>>();

        // Create the constraint
        problem.add_row(demand[&ts_selection].value().., terms);
    }
}

/// Add seasonal and annual utilisation peak constraints to the problem.
///
/// These are identical to the equivalent constraints in the full-system dispatch.
pub fn add_utilisation_peak_constraints(
    problem: &mut Problem,
    model: &Model,
    assets: &[AssetRef],
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
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
    let seasonal_peak_vars = add_seasonal_peak_variables(problem, model, assets);
    add_seasonal_peak_constraints(
        problem,
        activity_vars,
        &model.time_slice_info,
        &seasonal_peak_vars,
    );

    // If the annual penalty is applied, we also need to add annual peak variables and constraints
    if has_annual_penalty {
        let annual_peak_vars = add_annual_peak_variables(problem, model, assets);
        add_annual_peak_constraints(
            problem,
            &model.time_slice_info,
            &annual_peak_vars,
            &seasonal_peak_vars,
        );
    }
}

/// Add seasonal peak variables to the problem for each (asset, season) pair.
fn add_seasonal_peak_variables(
    problem: &mut Problem,
    model: &Model,
    assets: &[AssetRef],
) -> IndexMap<(AssetRef, Season), Variable> {
    let mut seasonal_peak_vars = IndexMap::new();
    for asset in assets {
        for (season, duration) in &model.time_slice_info.seasons {
            // Scale penalty by season duration
            let col_factor = model.parameters.seasonal_utilisation_penalty * *duration;
            let variable = problem.add_column(col_factor.value(), 0.0..);
            seasonal_peak_vars.insert((asset.clone(), season.clone()), variable);
        }
    }
    seasonal_peak_vars
}

/// Add annual peak variables to the problem for each asset.
fn add_annual_peak_variables(
    problem: &mut Problem,
    model: &Model,
    assets: &[AssetRef],
) -> IndexMap<AssetRef, Variable> {
    // Penalty is applied over the whole year, so scale by 1 year
    let col_factor = model.parameters.annual_utilisation_penalty * Year(1.0);
    assets
        .iter()
        .map(|asset| {
            let variable = problem.add_column(col_factor.value(), 0.0..);
            (asset.clone(), variable)
        })
        .collect()
}

/// Add constraints linking seasonal peak variables to activity variables for each (asset, season) pair.
fn add_seasonal_peak_constraints(
    problem: &mut Problem,
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
    time_slice_info: &TimeSliceInfo,
    seasonal_peak_vars: &IndexMap<(AssetRef, Season), Variable>,
) {
    for ((asset, season), &peak_variable) in seasonal_peak_vars {
        let activity_per_capacity = asset.process().capacity_to_activity;
        let season_selection = TimeSliceSelection::Season(season.clone());
        for (time_slice, ts_length) in season_selection.iter(time_slice_info) {
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
                    (
                        activity_vars[&(asset.clone(), time_slice.clone())],
                        -capacity_required_per_activity,
                    ),
                ],
            );
        }
    }
}

/// Add constraints linking seasonal peak variables to annual peak variables for each asset.
fn add_annual_peak_constraints(
    problem: &mut Problem,
    time_slice_info: &TimeSliceInfo,
    annual_peak_vars: &IndexMap<AssetRef, Variable>,
    seasonal_peak_vars: &IndexMap<(AssetRef, Season), Variable>,
) {
    for (asset, &annual_peak_variable) in annual_peak_vars {
        for season in time_slice_info.seasons.keys() {
            let seasonal_peak_variable = seasonal_peak_vars
                .get(&(asset.clone(), season.clone()))
                .expect("Missing seasonal peak variable for annual peak constraint");
            problem.add_row(
                0.0..,
                [(annual_peak_variable, 1.0), (*seasonal_peak_variable, -1.0)],
            );
        }
    }
}

/// Add constraints requiring dispatch-equivalent assets to have equal utilisation in each time
/// slice.
///
/// Flexible-capacity assets are excluded because their maximum activity depends on a decision
/// variable. The constraints added here are not included in [`ConstraintKeys`], as their duals
/// are not currently used.
pub fn add_equal_utilisation_constraints(
    problem: &mut Problem,
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
    time_slice_info: &TimeSliceInfo,
    assets: &[AssetRef],
) {
    let asset_groups = group_dispatch_equivalent_assets(assets.iter());

    // For each group of assets, add constraints to force equal utilisation in each time slice
    // This is done by anchoring each asset to the first asset in the group (-> (n-1) constraints
    // for a group of n assets)
    for assets in asset_groups {
        let Some((reference_asset, others)) = assets.split_first() else {
            continue;
        };

        let reference_max = reference_asset.max_activity().value();

        for asset in others {
            let asset_max = asset.max_activity().value();

            for time_slice in time_slice_info.iter_ids() {
                // Constraint: (act_a * max_b) - (act_b * max_a) = 0
                problem.add_row(
                    0.0..=0.0,
                    [
                        (
                            activity_vars[&((*asset).clone(), time_slice.clone())],
                            reference_max,
                        ),
                        (
                            activity_vars[&((*reference_asset).clone(), time_slice.clone())],
                            -asset_max,
                        ),
                    ],
                );
            }
        }
    }
}
