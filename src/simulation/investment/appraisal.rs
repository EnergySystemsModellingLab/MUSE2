//! Calculation for investment tools such as Levelised Cost of X (LCOX) and Net Present Value (NPV).
use super::DemandMap;
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::finance::{ProfitabilityIndex, lcox, profitability_index};
use crate::model::Model;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity, Money, MoneyPerActivity, MoneyPerCapacity};
use anyhow::Result;
use costs::annual_fixed_cost;
use erased_serde::Serialize as ErasedSerialize;
use indexmap::IndexMap;
use serde::Serialize;
use std::any::Any;
use std::cmp::Ordering;

pub mod coefficients;
mod constraints;
mod costs;
mod optimisation;
use coefficients::ObjectiveCoefficients;
use float_cmp::approx_eq;
use float_cmp::{ApproxEq, F64Margin};
use optimisation::perform_optimisation;

/// Compares two values with approximate equality checking.
///
/// Returns `Ordering::Equal` if the values are approximately equal
/// according to the default floating-point margin, otherwise returns
/// their relative ordering based on `a.partial_cmp(&b)`.
///
/// This is useful when comparing floating-point-based types where exact
/// equality may not be appropriate due to numerical precision limitations.
///
/// # Panics
///
/// Panics if `partial_cmp` returns `None` (i.e., if either value is NaN).
fn compare_approx<T>(a: T, b: T) -> Ordering
where
    T: Copy + PartialOrd + ApproxEq<Margin = F64Margin>,
{
    if a.approx_eq(b, F64Margin::default()) {
        Ordering::Equal
    } else {
        a.partial_cmp(&b).expect("Cannot compare NaN values")
    }
}

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

/// Supertrait for appraisal metrics that can be serialised and compared.
pub trait MetricTrait: ComparableMetric + ErasedSerialize {}
erased_serde::serialize_trait_object!(MetricTrait);

/// Trait for appraisal metrics that can be compared.
///
/// Implementers define how their values should be compared to determine
/// which investment option is preferable through the `compare` method.
pub trait ComparableMetric: Any + Send + Sync {
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
    fn compare(&self, other: &dyn ComparableMetric) -> Ordering;

    /// Helper for downcasting to enable type-safe comparison.
    fn as_any(&self) -> &dyn Any;
}

/// Levelised Cost of X (LCOX) metric.
///
/// Represents the average cost per unit of output. Lower values indicate
/// more cost-effective investments.
#[derive(Debug, Clone, Serialize)]
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

impl ComparableMetric for LCOXMetric {
    fn value(&self) -> f64 {
        self.cost.value()
    }

    fn compare(&self, other: &dyn ComparableMetric) -> Ordering {
        let other = other
            .as_any()
            .downcast_ref::<Self>()
            .expect("Cannot compare metrics of different types");

        compare_approx(self.cost, other.cost)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// `LCOXMetric` implements the `MetricTrait` supertrait.
impl MetricTrait for LCOXMetric {}

/// Net Present Value (NPV) metric
#[derive(Debug, Clone, Serialize)]
pub struct NPVMetric(ProfitabilityIndex);

impl NPVMetric {
    /// Creates a new `NPVMetric` with the given profitability index.
    pub fn new(profitability_index: ProfitabilityIndex) -> Self {
        Self(profitability_index)
    }

    /// Returns true if this metric represents a zero fixed cost case.
    fn is_zero_fixed_cost(&self) -> bool {
        approx_eq!(Money, self.0.annualised_fixed_cost, Money(0.0))
    }
}

impl ComparableMetric for NPVMetric {
    fn value(&self) -> f64 {
        if self.is_zero_fixed_cost() {
            self.0.total_annualised_surplus.value()
        } else {
            self.0.value().value()
        }
    }

    /// Higher profitability index values indicate more profitable investments.
    /// When annual fixed cost is zero, the profitability index is infinite and
    /// total surplus is used for comparison instead.
    fn compare(&self, other: &dyn ComparableMetric) -> Ordering {
        let other = other
            .as_any()
            .downcast_ref::<Self>()
            .expect("Cannot compare metrics of different types");

        // Handle comparison based on fixed cost status
        match (self.is_zero_fixed_cost(), other.is_zero_fixed_cost()) {
            // Both have zero fixed cost: compare total surplus (higher is better)
            (true, true) => {
                let self_surplus = self.0.total_annualised_surplus;
                let other_surplus = other.0.total_annualised_surplus;
                compare_approx(other_surplus, self_surplus)
            }
            // Both have non-zero fixed cost: compare profitability index (higher is better)
            (false, false) => {
                let self_pi = self.0.value();
                let other_pi = other.0.value();
                compare_approx(other_pi, self_pi)
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

/// `NPVMetric` implements the `MetricTrait` supertrait.
impl MetricTrait for NPVMetric {}

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
/// comparable with other appraisals of the same type (npv/lcox).
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::finance::ProfitabilityIndex;
    use crate::units::{Money, MoneyPerActivity};
    use rstest::rstest;

    /// Parametrised tests for LCOX metric comparison.
    #[rstest]
    #[case(10.0, 10.0, Ordering::Equal, "equal_costs")]
    #[case(5.0, 10.0, Ordering::Less, "first_lower_cost_is_better")]
    #[case(10.0, 5.0, Ordering::Greater, "second_lower_cost_is_better")]
    fn lcox_metric_comparison(
        #[case] cost1: f64,
        #[case] cost2: f64,
        #[case] expected: Ordering,
        #[case] description: &str,
    ) {
        let metric1 = LCOXMetric::new(MoneyPerActivity(cost1));
        let metric2 = LCOXMetric::new(MoneyPerActivity(cost2));

        assert_eq!(
            metric1.compare(&metric2),
            expected,
            "Failed comparison for case: {description}"
        );
    }

    /// Parametrised tests for NPV metric comparison.
    #[rstest]
    // Both zero AFC: compare by total surplus (higher is better)
    #[case(100.0, 0.0, 50.0, 0.0, Ordering::Less, "both_zero_afc_first_better")]
    #[case(
        50.0,
        0.0,
        100.0,
        0.0,
        Ordering::Greater,
        "both_zero_afc_second_better"
    )]
    #[case(100.0, 0.0, 100.0, 0.0, Ordering::Equal, "both_zero_afc_equal")]
    // Both approximately zero AFC (same as both zero): compare by total surplus (higher is better)
    #[case(
        100.0,
        1e-10,
        50.0,
        1e-10,
        Ordering::Less,
        "both_approx_zero_afc_first_better"
    )]
    #[case(
        100.0,
        1e-10,
        200.0,
        50.0,
        Ordering::Less,
        "approx_zero_afc_beats_nonzero"
    )]
    #[case(
        200.0,
        50.0,
        100.0,
        1e-10,
        Ordering::Greater,
        "nonzero_afc_loses_to_approx_zero"
    )]
    // Both non-zero AFC: compare by profitability index (higher is better)
    #[case(
        200.0,
        100.0,
        150.0,
        100.0,
        Ordering::Less,
        "both_nonzero_afc_first_better"
    )]
    #[case(
        150.0,
        100.0,
        200.0,
        100.0,
        Ordering::Greater,
        "both_nonzero_afc_second_better"
    )]
    #[case(200.0, 100.0, 200.0, 100.0, Ordering::Equal, "both_nonzero_afc_equal")]
    // Zero vs non-zero AFC: zero or approximately zero is always better
    #[case(
        10.0,
        0.0,
        1000.0,
        100.0,
        Ordering::Less,
        "first_zero_afc_beats_second_nonzero_afc"
    )]
    #[case(
        10.0,
        1e-10,
        1000.0,
        100.0,
        Ordering::Less,
        "first_approx_zero_afc_beats_second_nonzero_afc"
    )]
    #[case(
        1000.0,
        100.0,
        10.0,
        0.0,
        Ordering::Greater,
        "second_zero_afc_beats_first_nonzero_afc"
    )]
    #[case(
        1000.0,
        100.0,
        10.0,
        1e-10,
        Ordering::Greater,
        "second_nonzero_afc_beats_first_approx_zero_afc"
    )]
    fn npv_metric_comparison(
        #[case] surplus1: f64,
        #[case] fixed_cost1: f64,
        #[case] surplus2: f64,
        #[case] fixed_cost2: f64,
        #[case] expected: Ordering,
        #[case] description: &str,
    ) {
        let metric1 = NPVMetric::new(ProfitabilityIndex {
            total_annualised_surplus: Money(surplus1),
            annualised_fixed_cost: Money(fixed_cost1),
        });
        let metric2 = NPVMetric::new(ProfitabilityIndex {
            total_annualised_surplus: Money(surplus2),
            annualised_fixed_cost: Money(fixed_cost2),
        });

        assert_eq!(
            metric1.compare(&metric2),
            expected,
            "Failed comparison for case: {description}"
        );
    }
}
