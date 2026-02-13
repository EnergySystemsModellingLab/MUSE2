//! Calculation for investment tools such as Levelised Cost of X (LCOX) and Net Present Value (NPV).
use super::DemandMap;
use crate::agent::ObjectiveType;
use crate::asset::{Asset, AssetCapacity, AssetRef};
use crate::commodity::Commodity;
use crate::finance::{ProfitabilityIndex, lcox, profitability_index};
use crate::model::Model;
use crate::simulation::investment::appraisal::optimisation::ResultsMap;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity, Money, MoneyPerActivity, MoneyPerCapacity};
use anyhow::Result;
use costs::annual_fixed_cost;
use erased_serde::Serialize as ErasedSerialize;
use indexmap::IndexMap;
use log::debug;
use serde::Serialize;
use std::any::Any;
use std::cmp::Ordering;
use std::rc::Rc;

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

/// The output of investment appraisal required to compare potential investment decisions.
///
/// Note that this struct should be created with the [`AppraisalOutput::new`] constructor to check
/// the parameters.
pub struct AppraisalOutput {
    /// The asset being appraised
    pub asset: AssetRef,
    /// The hypothetical capacity to install
    pub capacity: AssetCapacity,
    /// Time slice level activity of the asset
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// The hypothetical unmet demand following investment in this asset
    pub unmet_demand: DemandMap,
    /// The comparison metric to compare investment decisions
    pub metric: Box<dyn MetricTrait>,
    /// Capacity and activity coefficients used in the appraisal
    pub coefficients: Rc<ObjectiveCoefficients>,
}

impl AppraisalOutput {
    /// Create a new `AppraisalOutput`.
    ///
    /// Returns `None` if the capacity is zero, otherwise `Some(AppraisalOutput)` with the specified
    /// parameters.
    pub fn new<T: MetricTrait>(
        asset: AssetRef,
        results: ResultsMap,
        metric: T,
        coefficients: Rc<ObjectiveCoefficients>,
    ) -> Option<Self> {
        if results.capacity.total_capacity() == Capacity(0.0) {
            debug!("Skipping investment option with zero capacity");
            return None;
        }

        Some(Self {
            asset,
            capacity: results.capacity,
            activity: results.activity,
            unmet_demand: results.unmet_demand,
            metric: Box::new(metric),
            coefficients,
        })
    }

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
    max_capacity: Option<AssetCapacity>,
    commodity: &Commodity,
    coefficients: &Rc<ObjectiveCoefficients>,
    demand: &DemandMap,
) -> Result<Option<AppraisalOutput>> {
    let results = perform_optimisation(
        asset,
        max_capacity,
        commodity,
        coefficients,
        demand,
        &model.time_slice_info,
        highs::Sense::Minimise,
    )?;

    let Some(cost_index) = lcox(
        results.capacity.total_capacity(),
        coefficients.capacity_coefficient,
        &results.activity,
        &coefficients.activity_coefficients,
    ) else {
        debug!("Skipping investment option with zero activity");
        return Ok(None);
    };

    Ok(AppraisalOutput::new(
        asset.clone(),
        results,
        LCOXMetric::new(cost_index),
        coefficients.clone(),
    ))
}

/// Calculate NPV for a hypothetical investment in the given asset.
///
/// # Returns
///
/// An `AppraisalOutput` containing the hypothetical capacity, activity profile and unmet demand.
fn calculate_npv(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<AssetCapacity>,
    commodity: &Commodity,
    coefficients: &Rc<ObjectiveCoefficients>,
    demand: &DemandMap,
) -> Result<Option<AppraisalOutput>> {
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
        results.capacity.total_capacity(),
        annual_fixed_cost,
        &results.activity,
        &coefficients.activity_coefficients,
    );

    Ok(AppraisalOutput::new(
        asset.clone(),
        results,
        NPVMetric::new(profitability_index),
        coefficients.clone(),
    ))
}

/// Appraise the given investment with the specified parameters.
///
/// # Returns
///
/// - An error, if something fatal has occurred (i.e. the optimisation failed)
/// - `None` if this is not a viable option (e.g. because the returned capacity would be zero)
/// - `Some(AppraisalOutput)` with the appraisal result if it is a viable option
pub fn appraise_investment(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<AssetCapacity>,
    commodity: &Commodity,
    objective_type: &ObjectiveType,
    coefficients: &Rc<ObjectiveCoefficients>,
    demand: &DemandMap,
) -> Result<Option<AppraisalOutput>> {
    let appraisal_method = match objective_type {
        ObjectiveType::LevelisedCostOfX => calculate_lcox,
        ObjectiveType::NetPresentValue => calculate_npv,
    };
    appraisal_method(model, asset, max_capacity, commodity, coefficients, demand)
}

/// Compare assets as a fallback if metrics are equal.
///
/// Commissioned assets are ordered before uncommissioned and newer before older.
///
/// Used as a fallback to sort assets when they have equal appraisal tool outputs.
fn compare_asset_fallback(asset1: &Asset, asset2: &Asset) -> Ordering {
    (asset2.is_commissioned(), asset2.commission_year())
        .cmp(&(asset1.is_commissioned(), asset1.commission_year()))
}

/// Sort appraisal outputs by their investment priority.
///
/// Primarily this is decided by their appraisal metric. When appraisal metrics are equal, a
/// tie-breaker fallback is used. Commissioned assets are preferred over uncommissioned assets, and
/// newer assets are preferred over older ones. The function does not guarantee that all ties will
/// be resolved.
pub fn sort_appraisal_outputs_by_investment_priority(outputs_for_opts: &mut [AppraisalOutput]) {
    outputs_for_opts.sort_by(|output1, output2| match output1.compare_metric(output2) {
        // If equal, we fall back on comparing asset properties
        Ordering::Equal => compare_asset_fallback(&output1.asset, &output2.asset),
        cmp => cmp,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentID;
    use crate::finance::ProfitabilityIndex;
    use crate::fixture::{agent_id, asset, process, region_id};
    use crate::process::Process;
    use crate::region::RegionID;
    use crate::units::{Money, MoneyPerActivity};
    use float_cmp::assert_approx_eq;
    use rstest::rstest;
    use std::rc::Rc;

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

    #[rstest]
    fn compare_assets_fallback(process: Process, region_id: RegionID, agent_id: AgentID) {
        let process = Rc::new(process);
        let capacity = Capacity(2.0);
        let asset1 = Asset::new_commissioned(
            agent_id.clone(),
            process.clone(),
            region_id.clone(),
            capacity,
            2015,
        )
        .unwrap();
        let asset2 =
            Asset::new_candidate(process.clone(), region_id.clone(), capacity, 2015).unwrap();
        let asset3 =
            Asset::new_commissioned(agent_id, process, region_id.clone(), capacity, 2010).unwrap();

        assert!(compare_asset_fallback(&asset1, &asset1).is_eq());
        assert!(compare_asset_fallback(&asset2, &asset2).is_eq());
        assert!(compare_asset_fallback(&asset3, &asset3).is_eq());
        assert!(compare_asset_fallback(&asset1, &asset2).is_lt());
        assert!(compare_asset_fallback(&asset2, &asset1).is_gt());
        assert!(compare_asset_fallback(&asset1, &asset3).is_lt());
        assert!(compare_asset_fallback(&asset3, &asset1).is_gt());
        assert!(compare_asset_fallback(&asset3, &asset2).is_lt());
        assert!(compare_asset_fallback(&asset2, &asset3).is_gt());
    }

    /// Creates appraisal from corresponding assets and metrics
    ///
    /// # Panics
    ///
    /// Panics if `assets` and `metrics` have different lengths
    fn appraisal_outputs(
        assets: Vec<Asset>,
        metrics: Vec<Box<dyn MetricTrait>>,
    ) -> Vec<AppraisalOutput> {
        assert_eq!(
            assets.len(),
            metrics.len(),
            "assets and metrics must have the same length"
        );

        assets
            .into_iter()
            .zip(metrics)
            .map(|(asset, metric)| AppraisalOutput {
                asset: AssetRef::from(asset),
                capacity: AssetCapacity::Continuous(Capacity(10.0)),
                coefficients: Rc::default(),
                activity: IndexMap::new(),
                unmet_demand: IndexMap::new(),
                metric,
            })
            .collect()
    }

    /// Creates appraisal outputs with given metrics.
    /// Copies the provided default asset for each metric.
    fn appraisal_outputs_with_investment_priority_invariant_to_assets(
        metrics: Vec<Box<dyn MetricTrait>>,
        asset: &Asset,
    ) -> Vec<AppraisalOutput> {
        let assets = vec![asset.clone(); metrics.len()];
        appraisal_outputs(assets, metrics)
    }

    /// Test sorting by LCOX metric when invariant to asset properties
    #[rstest]
    fn appraisal_sort_by_lcox_metric(asset: Asset) {
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(3.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(7.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        assert_approx_eq!(f64, outputs[0].metric.value(), 3.0); // Best (lowest)
        assert_approx_eq!(f64, outputs[1].metric.value(), 5.0);
        assert_approx_eq!(f64, outputs[2].metric.value(), 7.0); // Worst (highest)
    }

    /// Test sorting by NPV profitability index when invariant to asset properties
    #[rstest]
    fn appraisal_sort_by_npv_metric(asset: Asset) {
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(200.0),
                annualised_fixed_cost: Money(100.0),
            })),
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(300.0),
                annualised_fixed_cost: Money(100.0),
            })),
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(150.0),
                annualised_fixed_cost: Money(100.0),
            })),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        // Higher profitability index is better, so should be sorted: 3.0, 2.0, 1.5
        assert_approx_eq!(f64, outputs[0].metric.value(), 3.0); // Best (highest PI)
        assert_approx_eq!(f64, outputs[1].metric.value(), 2.0);
        assert_approx_eq!(f64, outputs[2].metric.value(), 1.5); // Worst (lowest PI)
    }

    /// Test that NPV metrics with zero annual fixed cost are prioritised above all others
    /// when invariant to asset properties
    #[rstest]
    fn appraisal_sort_by_npv_metric_zero_afc_prioritised(asset: Asset) {
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            // Very high profitability index but non-zero AFC
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(1000.0),
                annualised_fixed_cost: Money(100.0),
            })),
            // Zero AFC with modest surplus - should be prioritised first
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(50.0),
                annualised_fixed_cost: Money(0.0),
            })),
            // Another high profitability index but non-zero AFC
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(500.0),
                annualised_fixed_cost: Money(50.0),
            })),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        // Zero AFC should be first despite lower absolute surplus value
        assert_approx_eq!(f64, outputs[0].metric.value(), 50.0); // Zero AFC (uses surplus)
        assert_approx_eq!(f64, outputs[1].metric.value(), 10.0); // PI = 1000/100
        assert_approx_eq!(f64, outputs[2].metric.value(), 10.0); // PI = 500/50
    }

    /// Test that mixing LCOX and NPV metrics causes a runtime panic during comparison
    #[rstest]
    #[should_panic(expected = "Cannot compare metrics of different types")]
    fn appraisal_sort_by_mixed_metrics_panics(asset: Asset) {
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(NPVMetric::new(ProfitabilityIndex {
                total_annualised_surplus: Money(200.0),
                annualised_fixed_cost: Money(100.0),
            })),
            Box::new(LCOXMetric::new(MoneyPerActivity(3.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        // This should panic when trying to compare different metric types
        sort_appraisal_outputs_by_investment_priority(&mut outputs);
    }

    /// Test that when metrics are equal, commissioned assets are sorted by commission year (newer first)
    #[rstest]
    fn appraisal_sort_by_commission_year_when_metrics_equal(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);
        let commission_years = [2015, 2020, 2010];

        let assets: Vec<_> = commission_years
            .iter()
            .map(|&year| {
                Asset::new_commissioned(
                    agent_id.clone(),
                    process_rc.clone(),
                    region_id.clone(),
                    capacity,
                    year,
                )
                .unwrap()
            })
            .collect();

        // All metrics have the same value
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
        ];

        let mut outputs = appraisal_outputs(assets, metrics);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        // Should be sorted by commission year, newest first: 2020, 2015, 2010
        assert_eq!(outputs[0].asset.commission_year(), 2020);
        assert_eq!(outputs[1].asset.commission_year(), 2015);
        assert_eq!(outputs[2].asset.commission_year(), 2010);
    }

    /// Test that when metrics and commission years are equal, the original order is preserved
    #[rstest]
    fn appraisal_sort_maintains_order_when_all_equal(process: Process, region_id: RegionID) {
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);
        let commission_year = 2015;
        let agent_ids = ["agent1", "agent2", "agent3"];

        let assets: Vec<_> = agent_ids
            .iter()
            .map(|&id| {
                Asset::new_commissioned(
                    AgentID(id.into()),
                    process_rc.clone(),
                    region_id.clone(),
                    capacity,
                    commission_year,
                )
                .unwrap()
            })
            .collect();

        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
        ];

        let mut outputs = appraisal_outputs(assets.clone(), metrics);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        // Verify order is preserved - should match the original agent_ids array
        for (&expected_id, output) in agent_ids.iter().zip(outputs) {
            assert_eq!(output.asset.agent_id(), Some(&AgentID(expected_id.into())));
        }
    }

    /// Test that commissioned assets are prioritised over non-commissioned assets when metrics are equal
    #[rstest]
    fn appraisal_sort_commissioned_before_uncommissioned_when_metrics_equal(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);

        // Create a mix of commissioned and candidate (non-commissioned) assets
        let commissioned_asset_newer = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            2020,
        )
        .unwrap();

        let commissioned_asset_older = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            2015,
        )
        .unwrap();

        let candidate_asset =
            Asset::new_candidate(process_rc.clone(), region_id.clone(), capacity, 2020).unwrap();

        let assets = vec![
            candidate_asset.clone(),
            commissioned_asset_older.clone(),
            candidate_asset.clone(),
            commissioned_asset_newer.clone(),
        ];

        // All metrics have identical values to test fallback ordering
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
        ];

        let mut outputs = appraisal_outputs(assets, metrics);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        // Commissioned assets should be prioritised first
        assert!(outputs[0].asset.is_commissioned());
        assert!(outputs[0].asset.commission_year() == 2020);
        assert!(outputs[1].asset.is_commissioned());
        assert!(outputs[1].asset.commission_year() == 2015);

        // Non-commissioned assets should come after
        assert!(!outputs[2].asset.is_commissioned());
        assert!(!outputs[3].asset.is_commissioned());
    }

    /// Test that appraisal metric is prioritised over asset properties when sorting
    #[rstest]
    fn appraisal_metric_is_prioritised_over_asset_properties(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);

        // Create a mix of commissioned and candidate (non-commissioned) assets
        let commissioned_asset_newer = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            2020,
        )
        .unwrap();

        let commissioned_asset_older = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            2015,
        )
        .unwrap();

        let candidate_asset =
            Asset::new_candidate(process_rc.clone(), region_id.clone(), capacity, 2020).unwrap();

        let assets = vec![
            candidate_asset.clone(),
            commissioned_asset_older.clone(),
            candidate_asset.clone(),
            commissioned_asset_newer.clone(),
        ];

        // Make one metric slightly better than all others
        let baseline_metric_value = 5.0;
        let best_metric_value = baseline_metric_value - 1e-5;
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(best_metric_value))),
            Box::new(LCOXMetric::new(MoneyPerActivity(baseline_metric_value))),
            Box::new(LCOXMetric::new(MoneyPerActivity(baseline_metric_value))),
            Box::new(LCOXMetric::new(MoneyPerActivity(baseline_metric_value))),
        ];

        let mut outputs = appraisal_outputs(assets, metrics);
        sort_appraisal_outputs_by_investment_priority(&mut outputs);

        // non-commissioned asset prioritised because it has a slightly better metric
        assert_approx_eq!(f64, outputs[0].metric.value(), best_metric_value);
    }
}
