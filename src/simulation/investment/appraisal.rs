//! Calculation for investment tools such as Levelised Cost of X (LCOX) and Net Present Value (NPV).
use super::DemandMap;
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::finance::{lcox, profitability_index};
use crate::model::Model;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity};
use anyhow::Result;
use highs::RowProblem as Problem;
use indexmap::IndexMap;
use std::cmp::Ordering;

pub mod coefficients;
mod constraints;
mod costs;
mod optimisation;
use coefficients::ObjectiveCoefficients;
use float_cmp::approx_eq;
use optimisation::{VariableMap, perform_optimisation};

/// The output of investment appraisal required to compare potential investment decisions
pub struct AppraisalOutput {
    /// The asset being appraised
    pub asset: AssetRef,
    /// The hypothetical capacity to install
    pub capacity: Capacity,
    /// Time slice level activity of the asset
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// The hypothetical unmet demand following investment in this asset
    pub unmet_demand: DemandMap,
    /// The comparison metric to compare investment decisions (lower is better)
    pub metric: f64,
    /// Capacity and activity coefficients used in the appraisal
    pub coefficients: ObjectiveCoefficients,
    /// Demand profile used in the appraisal
    pub demand: DemandMap,
}

impl AppraisalOutput {
    /// Compare this appraisal to another on the basis of the comparison metric.
    ///
    /// Note that if the metrics are approximately equal (as determined by the [`approx_eq!`] macro)
    /// then [`Ordering::Equal`] is returned. The reason for this is because different CPU
    /// architectures may lead to subtly different values for the comparison metrics and if the
    /// value is very similar to another, then it can lead to different decisions being made,
    /// depending on the user's platform (e.g. macOS ARM vs. Windows). We want to avoid this, if
    /// possible, which is why we use a more approximate comparison.
    pub fn compare_metric(&self, other: &Self) -> Ordering {
        assert!(
            !(self.metric.is_nan() || other.metric.is_nan()),
            "Appraisal metric cannot be NaN"
        );

        if approx_eq!(f64, self.metric, other.metric) {
            Ordering::Equal
        } else {
            self.metric.partial_cmp(&other.metric).unwrap()
        }
    }
}

/// Add variables to the problem for LCOX appraisal.
fn add_variables_to_lcox_problem(
    problem: &mut Problem,
    cost_coefficients: &ObjectiveCoefficients,
) -> VariableMap {
    // Create capacity variable
    let capacity_var = problem.add_column(cost_coefficients.capacity_coefficient.value(), 0.0..);

    // Create activity variables
    let mut activity_vars = IndexMap::new();
    for (time_slice, cost) in &cost_coefficients.activity_coefficients {
        let var = problem.add_column(cost.value(), 0.0..);
        activity_vars.insert(time_slice.clone(), var);
    }

    // Create unmet demand variables
    // One per time slice, all of which use the same coefficient
    let mut unmet_demand_vars = IndexMap::new();
    for time_slice in cost_coefficients.activity_coefficients.keys() {
        let var = problem.add_column(cost_coefficients.unmet_demand_coefficient.value(), 0.0..);
        unmet_demand_vars.insert(time_slice.clone(), var);
    }

    VariableMap {
        capacity_var,
        activity_vars,
        unmet_demand_vars,
    }
}

/// Add variables to the problem for npv appraisal.
fn add_variables_to_npv_problem(
    problem: &mut Problem,
    cost_coefficients: &ObjectiveCoefficients,
) -> VariableMap {
    // Create capacity variable (zeroed coefficient - doesn't affect optimisation)
    let capacity_var = problem.add_column(0.0, 0.0..);

    // Create activity variables
    let mut activity_vars = IndexMap::new();
    for (time_slice, cost) in &cost_coefficients.activity_coefficients {
        let var = problem.add_column(cost.value(), 0.0..);
        activity_vars.insert(time_slice.clone(), var);
    }

    // Create unmet demand variables
    // One per time slice, all of which use the same coefficient
    let mut unmet_demand_vars = IndexMap::new();
    for time_slice in cost_coefficients.activity_coefficients.keys() {
        let var = problem.add_column(cost_coefficients.unmet_demand_coefficient.value(), 0.0..);
        unmet_demand_vars.insert(time_slice.clone(), var);
    }

    VariableMap {
        capacity_var,
        activity_vars,
        unmet_demand_vars,
    }
}

/// Calculate LCOX for a hypothetical investment in the given asset.
///
/// This is more commonly referred to as Levelised Cost of *Electricity*, but as the model can
/// include other flows, we use the term LCOX.
fn calculate_lcox(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let mut problem = Problem::default();
    let variables = add_variables_to_lcox_problem(&mut problem, coefficients);

    // Perform optimisation to calculate capacity, activity and unmet demand
    let results = perform_optimisation(
        asset,
        max_capacity,
        commodity,
        demand,
        &model.time_slice_info,
        highs::Sense::Minimise,
        problem,
        &variables,
    )?;

    // Calculate LCOX for the hypothetical investment
    let annual_fixed_cost = coefficients.capacity_coefficient;
    let activity_costs = &coefficients.activity_coefficients;
    let cost_index = lcox(
        results.capacity,
        annual_fixed_cost,
        &results.activity,
        activity_costs,
    );

    // Return appraisal output
    Ok(AppraisalOutput {
        asset: asset.clone(),
        capacity: results.capacity,
        activity: results.activity,
        unmet_demand: results.unmet_demand,
        metric: cost_index.value(),
        coefficients: coefficients.clone(),
        demand: demand.clone(),
    })
}

/// Calculate NPV for a hypothetical investment in the given asset.
fn calculate_npv(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let mut problem = Problem::default();
    let variables = add_variables_to_npv_problem(&mut problem, coefficients);

    // Perform optimisation to calculate capacity, activity and unmet demand
    let results = perform_optimisation(
        asset,
        max_capacity,
        commodity,
        demand,
        &model.time_slice_info,
        highs::Sense::Maximise,
        problem,
        &variables,
    )?;

    // Calculate profitability index for the hypothetical investment
    let annual_fixed_cost = -coefficients.capacity_coefficient;
    let activity_surpluses = &coefficients.activity_coefficients;
    let profitability_index = profitability_index(
        results.capacity,
        annual_fixed_cost,
        &results.activity,
        activity_surpluses,
    );

    // Return appraisal output
    // Higher profitability index is better, so we make it negative for comparison
    Ok(AppraisalOutput {
        asset: asset.clone(),
        capacity: results.capacity,
        activity: results.activity,
        unmet_demand: results.unmet_demand,
        metric: -profitability_index.value(),
        coefficients: coefficients.clone(),
        demand: demand.clone(),
    })
}

/// Appraise the given investment with the specified objective type
pub fn appraise_investment(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    objective_type: &ObjectiveType,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let appraisal_method = match objective_type {
        ObjectiveType::LevelisedCostOfX => calculate_lcox,
        ObjectiveType::NetPresentValue => calculate_npv,
    };
    appraisal_method(model, asset, max_capacity, commodity, coefficients, demand)
}
