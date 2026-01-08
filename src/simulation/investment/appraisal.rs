//! Calculation for investment tools such as Levelised Cost of X (LCOX) and Net Present Value (NPV).
use super::DemandMap;
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::finance::{ProfitabilityIndex, lcox, profitability_index};
use crate::model::Model;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity, Dimensionless, Money, MoneyPerActivity, MoneyPerCapacity};
use anyhow::Result;
use costs::annual_fixed_cost;
use indexmap::IndexMap;
use std::any::Any;
use std::cmp::Ordering;

pub mod coefficients;
mod constraints;
mod costs;
mod optimisation;
use coefficients::ObjectiveCoefficients;
use float_cmp::approx_eq;
use optimisation::perform_optimisation;

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
    /// The comparison metric to compare investment decisions
    pub metric: Box<dyn MetricTrait>,
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
            !(self.metric.value().is_nan() || other.metric.value().is_nan()),
            "Appraisal metric cannot be NaN"
        );
        self.metric.compare(other.metric.as_ref())
    }
}

/// Trait for appraisal metrics that can be compared.
///
/// Implementers define how their values should be compared to determine
/// which investment option is preferable through the `compare` method.
pub trait MetricTrait: Any + Send + Sync {
    /// Returns the numeric value of this metric.
    fn value(&self) -> f64;

    /// Compares this metric with another of the same type.
    ///
    /// Returns `Ordering::Less` if `self` is better than `other`,
    /// `Ordering::Greater` if `other` is better, or `Ordering::Equal`
    /// if they are approximately equal.
    ///
    /// # Panics
    ///
    /// Panics if `other` is not the same concrete type as `self`.
    fn compare(&self, other: &dyn MetricTrait) -> Ordering;

    /// Helper for downcasting to enable type-safe comparison.
    fn as_any(&self) -> &dyn Any;
}

/// Levelised Cost of X (LCOX) metric.
///
/// Represents the average cost per unit of output. Lower values indicate
/// more cost-effective investments.
#[derive(Debug, Clone)]
pub struct LCOXMetric {
    /// The calculated cost value for this LCOX metric
    pub cost: MoneyPerActivity,
}

impl LCOXMetric {
    /// Creates a new `LCOXMetric` with the given cost.
    pub fn new(cost: MoneyPerActivity) -> Self {
        Self { cost }
    }
}

impl MetricTrait for LCOXMetric {
    fn value(&self) -> f64 {
        self.cost.value()
    }

    fn compare(&self, other: &dyn MetricTrait) -> Ordering {
        let other = other
            .as_any()
            .downcast_ref::<Self>()
            .expect("Cannot compare metrics of different types");

        if approx_eq!(MoneyPerActivity, self.cost, other.cost) {
            Ordering::Equal
        } else {
            // Lower cost is better
            self.cost.partial_cmp(&other.cost).unwrap()
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Net Present Value (NPV) metric
#[derive(Debug, Clone)]
pub struct NPVMetric {
    /// Profitability index data for this NPV metric
    pub profitability_index: ProfitabilityIndex,
}

impl NPVMetric {
    /// Creates a new `NPVMetric` with the given profitability index.
    pub fn new(profitability_index: ProfitabilityIndex) -> Self {
        Self {
            profitability_index,
        }
    }

    /// Returns true if this metric represents a zero fixed cost case.
    fn is_zero_fixed_cost(&self) -> bool {
        self.profitability_index.annualised_fixed_cost == Money(0.0)
    }
}

impl MetricTrait for NPVMetric {
    fn value(&self) -> f64 {
        if self.is_zero_fixed_cost() {
            self.profitability_index.total_annualised_surplus.value()
        } else {
            self.profitability_index.value().value()
        }
    }

    /// Higher profitability index values indicate more profitable investments.
    /// When annual fixed cost is zero, the profitability index is infinite and
    /// total surplus is used for comparison instead.
    fn compare(&self, other: &dyn MetricTrait) -> Ordering {
        let other = other
            .as_any()
            .downcast_ref::<Self>()
            .expect("Cannot compare metrics of different types");

        // Handle comparison based on fixed cost status
        match (self.is_zero_fixed_cost(), other.is_zero_fixed_cost()) {
            // Both have zero fixed cost: compare total surplus (higher is better)
            (true, true) => {
                let self_surplus = self.profitability_index.total_annualised_surplus;
                let other_surplus = other.profitability_index.total_annualised_surplus;

                if approx_eq!(Money, self_surplus, other_surplus) {
                    Ordering::Equal
                } else {
                    other_surplus.partial_cmp(&self_surplus).unwrap()
                }
            }
            // Both have non-zero fixed cost: compare profitability index (higher is better)
            (false, false) => {
                let self_pi = self.profitability_index.value();
                let other_pi = other.profitability_index.value();

                if approx_eq!(Dimensionless, self_pi, other_pi) {
                    Ordering::Equal
                } else {
                    other_pi.partial_cmp(&self_pi).unwrap()
                }
            }
            // Zero fixed cost is always better than non-zero fixed cost
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Calculate LCOX for a hypothetical investment in the given asset.
///
/// This is more commonly referred to as Levelised Cost of *Electricity*, but as the model can
/// include other flows, we use the term LCOX.
///
/// # Returns
///
/// An `AppraisalOutput` containing the hypothetical capacity, activity profile and unmet demand.
/// The returned `metric` is the LCOX value (lower is better).
fn calculate_lcox(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let results = perform_optimisation(
        asset,
        max_capacity,
        commodity,
        coefficients,
        demand,
        &model.time_slice_info,
        highs::Sense::Minimise,
    )?;

    let cost_index = lcox(
        results.capacity,
        coefficients.capacity_coefficient,
        &results.activity,
        &coefficients.activity_coefficients,
    );

    Ok(AppraisalOutput {
        asset: asset.clone(),
        capacity: results.capacity,
        activity: results.activity,
        unmet_demand: results.unmet_demand,
        metric: Box::new(LCOXMetric::new(cost_index)),
        coefficients: coefficients.clone(),
        demand: demand.clone(),
    })
}

/// Calculate NPV for a hypothetical investment in the given asset.
///
/// # Returns
///
/// An `AppraisalOutput` containing the hypothetical capacity, activity profile and unmet demand.
/// The returned `metric` is the negative of the profitability index so that, like LCOX,
/// higher metric values indicate a more desirable investment (i.e. higher NPV).
fn calculate_npv(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<Capacity>,
    commodity: &Commodity,
    coefficients: &ObjectiveCoefficients,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let results = perform_optimisation(
        asset,
        max_capacity,
        commodity,
        coefficients,
        demand,
        &model.time_slice_info,
        highs::Sense::Maximise,
    )?;

    let annual_fixed_cost = annual_fixed_cost(asset);
    assert!(
        annual_fixed_cost >= MoneyPerCapacity(0.0),
        "The current NPV calculation does not support negative annual fixed costs"
    );

    let profitability_index = profitability_index(
        results.capacity,
        annual_fixed_cost,
        &results.activity,
        &coefficients.activity_coefficients,
    );

    Ok(AppraisalOutput {
        asset: asset.clone(),
        capacity: results.capacity,
        activity: results.activity,
        unmet_demand: results.unmet_demand,
        metric: Box::new(NPVMetric::new(profitability_index)),
        coefficients: coefficients.clone(),
        demand: demand.clone(),
    })
}

/// Appraise the given investment with the specified objective type
///
/// # Returns
///
/// The `AppraisalOutput` produced by the selected appraisal method. The `metric` field is
/// comparable across methods.
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
