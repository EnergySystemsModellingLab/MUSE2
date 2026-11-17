//! Full-demand profile optimisation for one population-agent option.
//!
//! Each option is optimised independently with a free capacity variable. The resulting activity
//! profile therefore either matches every demand selection exactly or is infeasible; the caller
//! later scales the profile to the residual demand and applies the asset's capacity limits.
use super::constraints::{
    add_activity_constraints, add_demand_constraints, add_utilisation_peak_constraints,
};
use super::costs::annual_fixed_cost;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::optimisation::{ModelError, apply_highs_options_from_toml, solve_optimal};
use crate::simulation::PriceMap;
use crate::simulation::investment::DemandMap;
use crate::time_slice::TimeSliceID;
use crate::time_slice::TimeSliceInfo;
use crate::units::{Activity, ActivityPerCapacity, Capacity};
use anyhow::{Context, Result};
use highs::{HighsModelStatus, RowProblem as Problem, Sense};
use indexmap::IndexMap;

/// A decision variable in the optimisation.
///
/// The order in which variables are added matters because solution columns are extracted by index.
pub type Variable = highs::Col;

/// The solved capacity, activity profile and objective value for one full-demand fit.
pub struct ResultsMap {
    /// Capacity required to supply the complete demand profile.
    pub capacity: Capacity,
    /// Activity in each time slice that supplies the complete demand profile.
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// The minimised total cost for this full-demand fit.
    pub metric: f64,
}

/// Add one non-negative activity variable per time slice, with its cost as the objective
/// coefficient.
fn add_activity_vars(
    problem: &mut Problem,
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
    market_prices: &PriceMap,
    year: u32,
) -> IndexMap<TimeSliceID, Variable> {
    assert_eq!(problem.num_cols(), 0); // must come first
    time_slice_info
        .iter_ids()
        .map(|time_slice| {
            let cost = asset.get_operating_cost(year, time_slice)
                + asset.get_input_cost_from_prices(market_prices, time_slice);
            (time_slice.clone(), problem.add_column(cost.value(), 0.0..))
        })
        .collect()
}

/// Fit one asset's activity profile exactly to the full demand profile.
///
/// Capacity is left unbounded here so that profile feasibility and cost can be appraised separately
/// from selection limits. Those limits are applied after options have been merit-ordered.
///
/// Returns `Ok(None)` when the constraints are infeasible for this asset. Other solver and
/// configuration errors are returned to the caller.
pub fn perform_optimisation(
    model: &Model,
    asset: &AssetRef,
    commodity: &Commodity,
    market_prices: &PriceMap,
    year: u32,
    demand: &DemandMap,
) -> Result<Option<ResultsMap>> {
    let mut problem = Problem::default();
    let activity_vars = add_activity_vars(
        &mut problem,
        asset,
        &model.time_slice_info,
        market_prices,
        year,
    );
    // Capacity is free during appraisal; candidate, committed and retention limits are selection
    // constraints, not profile-feasibility constraints.
    let capacity_var = problem.add_column(annual_fixed_cost(asset).value(), 0.0..);

    add_activity_constraints(
        &mut problem,
        asset,
        &activity_vars,
        capacity_var,
        &model.time_slice_info,
    );
    add_demand_constraints(
        &mut problem,
        asset,
        commodity,
        &model.time_slice_info,
        demand,
        &activity_vars,
    );
    add_utilisation_peak_constraints(&mut problem, model, asset, &activity_vars);

    let mut highs_model = problem.optimise(Sense::Minimise);
    apply_highs_options_from_toml(&mut highs_model, &model.parameters.highs.appraisal_options)
        .context("Failed to apply custom HiGHS options to population appraisal")?;
    // Appraisals may run in parallel via Rayon, so each HiGHS solve must remain single-threaded to
    // avoid over-subscription.
    highs_model.set_option("parallel", "off");

    // An infeasible profile is an expected non-option. Preserve all other errors so configuration
    // failures and unexpected solver states remain visible.
    let solved = match solve_optimal(highs_model) {
        Ok(solved) => solved,
        Err(ModelError::NonOptimal(HighsModelStatus::Infeasible)) => return Ok(None),
        Err(error) => return Err(error.into_anyhow()),
    };
    let metric = solved.objective_value();
    let solution = solved.get_solution();
    let values = solution.columns();
    // Activity variables are created first and capacity immediately after them; peak variables,
    // when present, follow capacity.
    let activities = activity_vars
        .keys()
        .zip(values.iter())
        .map(|(time_slice, &value)| (time_slice.clone(), Activity::new(value)))
        .collect();
    let capacity = Capacity::new(values[activity_vars.len()]);

    Ok(Some(ResultsMap {
        capacity,
        activity: activities,
        metric,
    }))
}

/// Convert a full-demand activity profile into activity per unit of fitted capacity.
///
/// The fitted capacity is positive whenever demand is positive; zero-capacity results are removed
/// by the appraisal caller before this conversion.
pub fn activity_per_capacity(
    activity: &IndexMap<TimeSliceID, Activity>,
    capacity: Capacity,
) -> IndexMap<TimeSliceID, ActivityPerCapacity> {
    activity
        .iter()
        .map(|(time_slice, activity)| (time_slice.clone(), *activity / capacity))
        .collect()
}
