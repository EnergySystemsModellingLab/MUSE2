//! Optimisation problem for investment tools.
use super::DemandMap;
use super::ObjectiveCoefficients;
use super::constraints::{
    add_activity_constraints, add_demand_constraints, add_equal_utilisation_constraints,
    add_utilisation_peak_constraints,
};
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::simulation::investment::record_asset_selection;
use crate::simulation::optimisation::ModelError;
use crate::simulation::optimisation::apply_highs_options_from_toml;
use crate::simulation::optimisation::group_dispatch_equivalent_assets;
use crate::simulation::optimisation::solve_optimal;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Flow};
use anyhow::{Context, Result};
use highs::{RowProblem as Problem, Sense};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;

/// A decision variable in the optimisation
///
/// This alias represents a column created in the `highs` solver. Callers rely on the order
/// in which columns are added to the problem when extracting solution values.
pub type Variable = highs::Col;

/// Map containing optimisation results and coefficients
pub struct ResultsMap {
    /// Activity variables in each time slice for the asset of interest
    pub asset_activity: IndexMap<TimeSliceID, Activity>,
    /// Remaining unmet demand per time slice, computed post-solve
    pub unmet_demand: DemandMap,
    /// Full solution to the optimisation (activities of all assets)
    pub solution: IndexMap<(AssetRef, TimeSliceID), Activity>,
}

/// Adds activity variables to the problem, one per time slice.
///
/// Returns a map from time slice to the corresponding decision variable.
fn add_activity_vars(
    problem: &mut Problem,
    assets: &[AssetRef],
    coefficients: &HashMap<AssetRef, Arc<ObjectiveCoefficients>>,
) -> IndexMap<(AssetRef, TimeSliceID), Variable> {
    let mut activity_vars = IndexMap::new();
    for asset in assets {
        for (time_slice, cost) in &coefficients[asset].activity_coefficients {
            let var = problem.add_column(cost.value(), 0.0..);
            activity_vars.insert((asset.clone(), time_slice.clone()), var);
        }
    }
    activity_vars
}

/// Adds constraints to the problem.
fn add_constraints(
    problem: &mut Problem,
    model: &Model,
    assets: &[AssetRef],
    commodity: &Commodity,
    activity_vars: &IndexMap<(AssetRef, TimeSliceID), Variable>,
    demand: &DemandMap,
    time_slice_info: &TimeSliceInfo,
) {
    add_activity_constraints(problem, assets, activity_vars, time_slice_info);
    add_utilisation_peak_constraints(problem, model, assets, activity_vars);
    add_demand_constraints(
        problem,
        assets,
        commodity,
        time_slice_info,
        demand,
        activity_vars,
    );
    add_equal_utilisation_constraints(problem, activity_vars, time_slice_info, assets);
}

/// Computes remaining unmet demand per time-slice selection after a solve.
///
/// For each time-slice selection at the commodity's balance level, the selection-level residual
/// (`demand - supply`, clamped to zero) is stored directly. Downstream consumers operate at the
/// selection level (e.g. the next round's demand constraint and `is_any_remaining_demand`), so
/// there is no need to distribute values across individual time slices.
fn compute_unmet_demand(
    demand: &DemandMap,
    assets: &[AssetRef],
    activity: &IndexMap<(AssetRef, TimeSliceID), Activity>,
    commodity: &Commodity,
    time_slice_info: &TimeSliceInfo,
) -> DemandMap {
    time_slice_info
        .iter_selections_at_level(commodity.time_slice_level)
        .map(|ts_selection| {
            let demand_for_selection = demand[&ts_selection];

            // Calculate supply for selection: sum output over all assets over all time slices in
            // the selection
            let supply_for_selection: Flow = assets
                .iter()
                .flat_map(|asset| {
                    let coefficient = asset.get_flow(&commodity.id).unwrap().coeff;

                    ts_selection
                        .iter(time_slice_info)
                        .map(move |(time_slice, _)| {
                            let value = activity[&(asset.clone(), time_slice.clone())];
                            value * coefficient
                        })
                })
                .sum();

            let unmet = (demand_for_selection - supply_for_selection).max(Flow(0.0));
            (ts_selection, unmet)
        })
        .collect()
}

/// Performs optimisation for an asset, given the coefficients and demand.
pub fn perform_optimisation(
    model: &Model,
    selected_assets: &[AssetRef],
    asset: &AssetRef,
    commodity: &Commodity,
    coefficients: &HashMap<AssetRef, Arc<ObjectiveCoefficients>>,
    demand: &DemandMap,
    previous_solution: &IndexMap<(AssetRef, TimeSliceID), Activity>,
) -> Result<ResultsMap> {
    // Record hypothetical asset selection
    let mut assets = selected_assets.to_vec();
    record_asset_selection(asset.clone(), &mut assets);

    // Create problem
    let mut problem = Problem::default();
    let activity_vars = add_activity_vars(&mut problem, &assets, coefficients);

    // Add constraints
    add_constraints(
        &mut problem,
        model,
        &assets,
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

    // Extract asset activities and calculate unmet demand
    let solution_values = solution.columns();
    let all_activity = activity_vars
        .keys()
        .zip(solution_values.iter())
        .map(|(key, &value)| (key.clone(), Activity::new(value)))
        .collect();
    let unmet_demand = compute_unmet_demand(
        demand,
        &assets,
        &all_activity,
        commodity,
        &model.time_slice_info,
    );

    // Collect equivalence group for the asset of interest
    let equivalence_groups = group_dispatch_equivalent_assets(assets.iter());
    let asset_group = equivalence_groups
        .iter()
        .find(|group| group.contains(&asset))
        .expect("Asset of interest must be present in an equivalence group");

    // Calculate marginal activity for this asset equivalence group - we attribute this all to the
    // candidate tranche
    let marginal_activity: IndexMap<_, _> = all_activity
        .iter()
        .filter(|((asset, _ts), _value)| asset_group.contains(&asset))
        .fold(
            IndexMap::new(),
            |mut marginal, ((candidate, ts), current_value)| {
                let previous_value = previous_solution
                    .get(&(candidate.clone(), ts.clone()))
                    .copied()
                    .unwrap_or(Activity(0.0));

                *marginal.entry(ts.clone()).or_insert(Activity(0.0)) +=
                    *current_value - previous_value;
                marginal
            },
        );

    // Note, there are some cases where marginal activity in certain timeslices (and overall) can be
    // negative (i.e. where adding a tranche to the equivalence group actually decreases activity).
    // Two known cases where this can happen:
    //
    // For annually/seasonally balanced commodities, where increasing capacity hits a binding demand
    // constraint which then gives the asset freedom to choose the best timeslices to meet demand.
    // Before the demand constraint is met, the asset will serve using all profitable timeslices.
    // Once the demand constraint is met, not all timeslices need to be active to meet demand, and
    // the asset will prioritise the _most_ profitable timeslices, at the expense of less profitable
    // timeslices which will see their activities fall. In these circumstances individual timeslices
    // can have negative marginal activity, but marginal activity in each balanced time slice
    // selection (and the year as a whole) cannot be negative (but may be zero). If marginal
    // activity for the year is zero then LCOX is undefined and the tranche will not be considered,
    // even if it would be beneficial for the asset as a whole to add that tranche. This is
    // something we should consider.
    //
    // A second scenario is where there are two different (non-equivalent) assets with identical
    // marginal cost (despite having different VOC and/or flows), and where moving activity between
    // the assets isn't prevented by constraints (e.g. activity constraints or utilisation peak
    // constraints). In this case there will be two identical corner solutions, and adding a tranche
    // to asset A may inadvertently cause it to switch to the equally good solution where asset B
    // wins, hence resulting in a decrease in activity (the opposite can also happen where adding a
    // tranche to asset B switches it to a solution where asset B wins, resulting in artificially
    // inflated marginal activity). For example, in the simple example for 2021 investments RELCHP
    // and RGASPB have identical marginal cost in autumn.night and autumn.evening. This is a bigger
    // problem, going back to the issues of degeneracy and non-deterministic results, and something
    // we should try to understand better and address at the dispatch stage (which ultimately sets
    // the shadow prices that determine marginal costs). In the case of investment appraisal, this
    // can artificially decrease/increase marginal activities resulting in nonsensical LCOX. This is
    // a problem. It may help to seed each model with the previous solution, to reduce the risk of
    // flip-flopping, but the underlying issue also needs to be addressed.

    Ok(ResultsMap {
        asset_activity: marginal_activity,
        unmet_demand,
        solution: all_activity,
    })
}
