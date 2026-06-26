//! Calculation for investment tools such as Levelised Cost of X (LCOX) and Net Present Value (NPV).
use super::DemandMap;
use crate::agent::ObjectiveType;
use crate::asset::{Asset, AssetCapacity, AssetRef};
use crate::commodity::Commodity;
use crate::finance::{lcox, snas};
use crate::model::Model;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity, MoneyPerActivity, MoneyPerCapacity};
use anyhow::Result;
use costs::annual_fixed_cost;
use erased_serde::Serialize as ErasedSerialize;
use indexmap::IndexMap;
use optimisation::ResultsMap;
use serde::Serialize;
use std::any::Any;
use std::cmp::Ordering;
use std::rc::Rc;

pub mod coefficients;
mod constraints;
mod costs;
mod optimisation;
use coefficients::ObjectiveCoefficients;
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
    pub capacity: AssetCapacity,
    /// Time slice level activity of the asset
    pub activity: IndexMap<TimeSliceID, Activity>,
    /// The hypothetical unmet demand following investment in this asset
    pub unmet_demand: DemandMap,
    /// The comparison metric to compare investment decisions
    pub metric: Option<Box<dyn MetricTrait>>,
    /// Activity coefficients and market costs used in the appraisal
    pub coefficients: Rc<ObjectiveCoefficients>,
}

impl AppraisalOutput {
    /// Create a new `AppraisalOutput`
    fn new<T: MetricTrait>(
        asset: AssetRef,
        capacity: AssetCapacity,
        results: ResultsMap,
        metric: Option<T>,
        coefficients: Rc<ObjectiveCoefficients>,
    ) -> Self {
        Self {
            asset,
            capacity,
            activity: results.activity,
            unmet_demand: results.unmet_demand,
            metric: metric.map(|m| Box::new(m) as Box<dyn MetricTrait>),
            coefficients,
        }
    }
    /// Compare this appraisal to another on the basis of the comparison metric.
    ///
    /// Note that if the metrics are approximately equal (as determined by
    /// [`float_cmp::eq::ApproxEq`]), then [`Ordering::Equal`] is returned. The reason for this is
    /// because different CPU architectures may lead to subtly different values for the comparison
    /// metrics and if the value is very similar to another, then it can lead to different decisions
    /// being made, depending on the user's platform (e.g. macOS ARM vs. Windows). We want to avoid
    /// this, if possible, which is why we use a more approximate comparison.
    pub fn compare_metric(&self, other: &Self) -> Ordering {
        assert!(
            self.is_valid() && other.is_valid(),
            "Cannot compare non-valid outputs"
        );

        // We've already checked the metrics aren't `None` in `is_valid`
        self.metric
            .as_ref()
            .unwrap()
            .compare(other.metric.as_ref().unwrap().as_ref())
    }

    /// Whether this [`AppraisalOutput`] is a valid output.
    ///
    /// Specifically, it checks whether the metric is a valid value (not `None`) and that the
    /// calculated capacity is greater than zero.
    pub fn is_valid(&self) -> bool {
        self.metric.is_some() && self.capacity.total_capacity() > Capacity(0.0)
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

/// Net Present Value (NPV) tool metric.
///
/// In the NPV appraisal tool we compare options using the Specific Net Annualised Surplus (SNAS)
/// expressed per unit activity. Higher values indicate more profitable investments.
#[derive(Debug, Clone, Serialize)]
pub struct NPVMetric {
    /// The calculated SNAS value for this metric
    pub snas: MoneyPerActivity,
}

impl NPVMetric {
    /// Creates a new `NPVMetric` with the given SNAS value.
    pub fn new(snas: MoneyPerActivity) -> Self {
        Self { snas }
    }
}

impl ComparableMetric for NPVMetric {
    fn value(&self) -> f64 {
        self.snas.value()
    }

    fn compare(&self, other: &dyn ComparableMetric) -> Ordering {
        let other = other
            .as_any()
            .downcast_ref::<Self>()
            .expect("Cannot compare metrics of different types");

        compare_approx(other.snas, self.snas)
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
    max_capacity: AssetCapacity,
    commodity: &Commodity,
    coefficients: &Rc<ObjectiveCoefficients>,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let results =
        perform_optimisation(model, asset, max_capacity, commodity, coefficients, demand)?;

    let cost_index = lcox(
        max_capacity.total_capacity(),
        annual_fixed_cost(asset),
        &results.activity,
        &coefficients.market_costs,
    );

    Ok(AppraisalOutput::new(
        asset.clone(),
        max_capacity,
        results,
        cost_index.map(LCOXMetric::new),
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
    max_capacity: AssetCapacity,
    commodity: &Commodity,
    coefficients: &Rc<ObjectiveCoefficients>,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let results =
        perform_optimisation(model, asset, max_capacity, commodity, coefficients, demand)?;

    let annual_fixed_cost = annual_fixed_cost(asset);
    assert!(
        annual_fixed_cost >= MoneyPerCapacity(0.0),
        "The current NPV calculation does not support negative annual fixed costs"
    );

    let snas = snas(
        max_capacity.total_capacity(),
        annual_fixed_cost,
        &results.activity,
        &coefficients.market_costs,
    );

    Ok(AppraisalOutput::new(
        asset.clone(),
        max_capacity,
        results,
        snas.map(NPVMetric::new),
        coefficients.clone(),
    ))
}

/// Appraise the given investment with the specified objective type.
///
/// # Returns
///
/// The `AppraisalOutput` produced by the selected appraisal method. The `metric` field is
/// comparable with other appraisals of the same type (npv/lcox).
pub fn appraise_investment(
    model: &Model,
    asset: &AssetRef,
    max_capacity: Option<AssetCapacity>,
    commodity: &Commodity,
    objective_type: &ObjectiveType,
    coefficients: &Rc<ObjectiveCoefficients>,
    demand: &DemandMap,
) -> Result<AppraisalOutput> {
    let max_capacity = max_capacity.unwrap_or(asset.capacity());
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

/// Sort appraisal outputs by their investment priority and exclude non-feasible options.
///
/// Investment priority is primarily decided by appraisal metric. When appraisal metrics are equal,
/// a tie-breaker fallback is used. Commissioned assets are preferred over uncommissioned assets,
/// and newer assets are preferred over older ones. The function does not guarantee that all ties
/// will be resolved.
///
/// Before sorting, outputs are filtered using [`AppraisalOutput::is_valid`], which excludes entries
/// with invalid metrics (e.g. `None`) as well as zero capacity. This avoids meaningless or `NaN`
/// appraisal metrics that could cause the program to panic, so the length of the returned vector
/// may be less than the input.
///
/// # Returns
///
/// Returns the number of non-feasible assets which were removed.
pub fn sort_and_filter_appraisal_outputs(outputs: &mut Vec<AppraisalOutput>) -> usize {
    let old_len = outputs.len();
    outputs.retain(AppraisalOutput::is_valid);
    let num_nonfeasible = old_len - outputs.len();

    outputs.sort_by(|output1, output2| match output1.compare_metric(output2) {
        // If equal, we fall back on comparing asset properties
        Ordering::Equal => compare_asset_fallback(&output1.asset, &output2.asset),
        cmp => cmp,
    });

    num_nonfeasible
}

/// Counts the number of top appraisal outputs in a sorted slice that are indistinguishable
/// by both metric and fallback ordering. Excludes the first element from the count.
pub fn count_equal_and_best_appraisal_outputs(outputs: &[AppraisalOutput]) -> usize {
    if outputs.is_empty() {
        return 0;
    }
    outputs[1..]
        .iter()
        .take_while(|output| {
            output.compare_metric(&outputs[0]).is_eq()
                && compare_asset_fallback(&output.asset, &outputs[0].asset).is_eq()
        })
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentID;
    use crate::fixture::{agent_id, asset, process, region_id};
    use crate::process::Process;
    use crate::region::RegionID;
    use crate::units::MoneyPerActivity;
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
    #[case(10.0, 10.0, Ordering::Equal, "equal_costs")]
    #[case(5.0, 10.0, Ordering::Greater, "second_higher_metric_is_better")]
    #[case(10.0, 5.0, Ordering::Less, "first_higher_metric_is_better")]
    fn npv_metric_comparison(
        #[case] cost1: f64,
        #[case] cost2: f64,
        #[case] expected: Ordering,
        #[case] description: &str,
    ) {
        let metric1 = NPVMetric::new(MoneyPerActivity(cost1));
        let metric2 = NPVMetric::new(MoneyPerActivity(cost2));

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

    fn objective_coeffs() -> Rc<ObjectiveCoefficients> {
        Rc::new(ObjectiveCoefficients {
            activity_coefficients: IndexMap::new(),
            market_costs: IndexMap::new(),
        })
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
                coefficients: objective_coeffs(),
                activity: IndexMap::new(),
                unmet_demand: IndexMap::new(),
                metric: Some(metric),
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
        sort_and_filter_appraisal_outputs(&mut outputs);

        assert_approx_eq!(f64, outputs[0].metric.as_ref().unwrap().value(), 3.0); // Best (lowest)
        assert_approx_eq!(f64, outputs[1].metric.as_ref().unwrap().value(), 5.0);
        assert_approx_eq!(f64, outputs[2].metric.as_ref().unwrap().value(), 7.0); // Worst (highest)
    }

    /// Test sorting by NPV metric when invariant to asset properties
    #[rstest]
    fn appraisal_sort_by_npv_metric(asset: Asset) {
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(NPVMetric::new(MoneyPerActivity(5.0))),
            Box::new(NPVMetric::new(MoneyPerActivity(3.0))),
            Box::new(NPVMetric::new(MoneyPerActivity(7.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        sort_and_filter_appraisal_outputs(&mut outputs);

        assert_approx_eq!(f64, outputs[0].metric.as_ref().unwrap().value(), 7.0); // Best (highest)
        assert_approx_eq!(f64, outputs[1].metric.as_ref().unwrap().value(), 5.0);
        assert_approx_eq!(f64, outputs[2].metric.as_ref().unwrap().value(), 3.0); // Worst (lowest)
    }

    /// Test that mixing LCOX and NPV metrics causes a runtime panic during comparison
    #[rstest]
    #[should_panic(expected = "Cannot compare metrics of different types")]
    fn appraisal_sort_by_mixed_metrics_panics(asset: Asset) {
        let metrics: Vec<Box<dyn MetricTrait>> = vec![
            Box::new(LCOXMetric::new(MoneyPerActivity(5.0))),
            Box::new(NPVMetric::new(MoneyPerActivity(3.0))),
            Box::new(LCOXMetric::new(MoneyPerActivity(3.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        // This should panic when trying to compare different metric types
        sort_and_filter_appraisal_outputs(&mut outputs);
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
        sort_and_filter_appraisal_outputs(&mut outputs);

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
        sort_and_filter_appraisal_outputs(&mut outputs);

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
        sort_and_filter_appraisal_outputs(&mut outputs);

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
        sort_and_filter_appraisal_outputs(&mut outputs);

        // non-commissioned asset prioritised because it has a slightly better metric
        assert_approx_eq!(
            f64,
            outputs[0].metric.as_ref().unwrap().value(),
            best_metric_value
        );
    }

    /// Test that appraisal outputs with zero capacity are filtered out during sorting.
    #[rstest]
    fn appraisal_sort_filters_zero_capacity_outputs(asset: Asset) {
        let metric = LCOXMetric::new(MoneyPerActivity(1.0));
        let metrics = [
            Box::new(metric.clone()),
            Box::new(metric.clone()),
            Box::new(metric),
        ];

        // Create outputs with zero capacity
        let mut outputs: Vec<AppraisalOutput> = metrics
            .into_iter()
            .map(|metric| AppraisalOutput {
                asset: AssetRef::from(asset.clone()),
                capacity: AssetCapacity::Continuous(Capacity(0.0)),
                coefficients: objective_coeffs(),
                activity: IndexMap::new(),
                unmet_demand: IndexMap::new(),
                metric: Some(metric),
            })
            .collect();

        sort_and_filter_appraisal_outputs(&mut outputs);

        // All zero capacity outputs should be filtered out
        assert_eq!(outputs.len(), 0);
    }

    /// Test that appraisal outputs with an invalid metric are filtered out
    #[rstest]
    fn appraisal_sort_filters_invalid_metric(asset: Asset) {
        let output = AppraisalOutput {
            asset: AssetRef::from(asset),
            capacity: AssetCapacity::Continuous(Capacity(1.0)), // non-zero capacity
            coefficients: objective_coeffs(),
            activity: IndexMap::new(),
            unmet_demand: IndexMap::new(),
            metric: None,
        };
        let mut outputs = vec![output];

        sort_and_filter_appraisal_outputs(&mut outputs);

        // The invalid output should have been filtered out
        assert_eq!(outputs.len(), 0);
    }

    /// Tests for counting number of equal metrics using identical assets so only metric values
    /// affect the count.
    #[rstest]
    #[case(vec![5.0], 0, "single_element")]
    #[case(vec![5.0, 5.0, 5.0], 2, "all_equal_returns_len_minus_one")]
    #[case(vec![1.0, 2.0, 3.0], 0, "none_equal_to_best")]
    #[case(vec![5.0, 5.0, 9.0], 1, "partial_equality_stops_at_first_difference")]
    #[case(vec![5.0, 5.0, 9.0, 5.0], 1, "equality_does_not_resume_after_gap")]
    fn count_equal_best_lcox_metric(
        asset: Asset,
        #[case] metric_values: Vec<f64>,
        #[case] expected_count: usize,
        #[case] description: &str,
    ) {
        let metrics: Vec<Box<dyn MetricTrait>> = metric_values
            .into_iter()
            .map(|v| Box::new(LCOXMetric::new(MoneyPerActivity(v))) as Box<dyn MetricTrait>)
            .collect();

        let outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);

        assert_eq!(
            count_equal_and_best_appraisal_outputs(&outputs),
            expected_count,
            "Failed for case: {description}"
        );
    }

    /// Empty slice count should return 0.
    #[test]
    fn count_equal_best_empty_slice_returns_zero() {
        let outputs: Vec<AppraisalOutput> = vec![];
        assert_eq!(count_equal_and_best_appraisal_outputs(&outputs), 0);
    }

    /// Equal metrics but differing asset fallback (commissioned vs. candidate) →
    /// outputs are distinguishable, so count should be 0.
    #[rstest]
    fn count_equal_best_equal_metric_different_fallback_returns_zero(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);

        let commissioned = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            2020,
        )
        .unwrap();
        let candidate =
            Asset::new_candidate(process_rc.clone(), region_id.clone(), capacity, 2020).unwrap();

        let metric_value = MoneyPerActivity(5.0);
        let outputs = appraisal_outputs(
            vec![commissioned, candidate],
            vec![
                Box::new(LCOXMetric::new(metric_value)),
                Box::new(LCOXMetric::new(metric_value)),
            ],
        );

        assert_eq!(count_equal_and_best_appraisal_outputs(&outputs), 0);
    }

    /// Equal metrics and equal asset fallback (same commissioned status and commission year) →
    /// the second element is indistinguishable, so count should be 1.
    #[rstest]
    fn count_equal_best_equal_metric_and_equal_fallback_returns_one(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Rc::new(process);
        let capacity = Capacity(10.0);
        let year = 2020;

        let asset1 = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            year,
        )
        .unwrap();
        let asset2 = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            capacity,
            year,
        )
        .unwrap();

        let metric_value = MoneyPerActivity(5.0);
        let outputs = appraisal_outputs(
            vec![asset1, asset2],
            vec![
                Box::new(LCOXMetric::new(metric_value)),
                Box::new(LCOXMetric::new(metric_value)),
            ],
        );

        assert_eq!(count_equal_and_best_appraisal_outputs(&outputs), 1);
    }
}
