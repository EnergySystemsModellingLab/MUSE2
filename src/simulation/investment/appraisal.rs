//! Calculation for investment tools such as Levelised Cost of X (LCOX) and Net Present Value (NPV).
use super::DemandMap;
use crate::agent::{DecisionRule, ObjectiveType};
use crate::asset::{Asset, AssetRef};
use crate::finance::{lcox, snas};
use crate::units::{MoneyPerActivity, MoneyPerCapacity};
use anyhow::{Result, bail};
use costs::annual_fixed_cost;
use erased_serde::Serialize as ErasedSerialize;
use indexmap::IndexMap;
use serde::Serialize;
use std::any::Any;
use std::cmp::Ordering;
use std::sync::Arc;

pub mod coefficients;
mod constraints;
mod costs;
mod optimisation;
use coefficients::MarketCosts;
use float_cmp::{ApproxEq, F64Margin};
pub use optimisation::AppraisalOptimisation;
pub use optimisation::perform_optimisation;

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

impl MetricTrait for NPVMetric {}

/// Metric results keyed by candidate asset.
pub type AppraisalMetrics = IndexMap<AssetRef, Vec<Box<dyn MetricTrait>>>;

#[cfg(test)]
#[derive(Clone, Copy)]
enum AppraisalMetric {
    Lcox(Option<MoneyPerActivity>),
    Npv(Option<MoneyPerActivity>),
}

#[cfg(test)]
impl AppraisalMetric {
    fn boxed(self) -> Option<Box<dyn MetricTrait>> {
        match self {
            Self::Lcox(value) => value.map(|value| Box::new(LCOXMetric::new(value)) as _),
            Self::Npv(value) => value.map(|value| Box::new(NPVMetric::new(value)) as _),
        }
    }
}

fn compare_asset_metrics(
    (asset1, metrics1): (&AssetRef, &Vec<Box<dyn MetricTrait>>),
    (asset2, metrics2): (&AssetRef, &Vec<Box<dyn MetricTrait>>),
) -> Ordering {
    match metrics1
        .first()
        .zip(metrics2.first())
        .map_or(Ordering::Greater, |(metric1, metric2)| {
            metric1.compare(metric2.as_ref())
        }) {
        Ordering::Equal => compare_asset_fallback(&**asset1, &**asset2),
        ordering => ordering,
    }
}

/// Calculate LCOX from a completed appraisal optimisation.
///
/// This is more commonly referred to as Levelised Cost of *Electricity*, but as the model can
/// include other flows, we use the term LCOX.
///
/// # Returns
///
/// Returns the calculated LCOX metric (lower values are better).
fn calculate_lcox(
    optimisation: &AppraisalOptimisation,
    asset: &AssetRef,
    market_costs: &MarketCosts,
) -> Option<Box<dyn MetricTrait>> {
    let cost_index = lcox(
        asset.total_capacity(),
        annual_fixed_cost(asset),
        &optimisation.activity,
        market_costs,
    );
    cost_index.map(|cost| Box::new(LCOXMetric::new(cost)) as Box<dyn MetricTrait>)
}

/// Calculate NPV from a completed appraisal optimisation.
///
/// # Returns
///
/// Returns the calculated NPV metric.
fn calculate_npv(
    optimisation: &AppraisalOptimisation,
    asset: &AssetRef,
    market_costs: &MarketCosts,
) -> Option<Box<dyn MetricTrait>> {
    let annual_fixed_cost = annual_fixed_cost(asset);
    assert!(
        annual_fixed_cost >= MoneyPerCapacity(0.0),
        "The current NPV calculation does not support negative annual fixed costs"
    );

    let snas = snas(
        asset.total_capacity(),
        annual_fixed_cost,
        &optimisation.activity,
        market_costs,
    );
    snas.map(|value| Box::new(NPVMetric::new(value)) as Box<dyn MetricTrait>)
}

/// Calculate the metric for a completed appraisal optimisation.
///
/// # Returns
///
/// Returns the optimisation result and its calculated metric.
pub fn calculate_metric(
    asset: &AssetRef,
    objective_type: &ObjectiveType,
    market_costs: &Arc<MarketCosts>,
    optimisation: &AppraisalOptimisation,
) -> Box<dyn MetricTrait> {
    match objective_type {
        ObjectiveType::LevelisedCostOfX => calculate_lcox(optimisation, asset, market_costs)
            .expect("LCOX metric must be valid for an optimisation with activity"),
        ObjectiveType::NetPresentValue => calculate_npv(optimisation, asset, market_costs)
            .expect("NPV metric must be valid for an optimisation with activity"),
    }
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

/// Remove appraisal outputs with invalid metrics and return the number removed.
///
/// An output with no metric is considered non-feasible. Options skipped before appraisal, such as
/// assets with zero capacity, are not included in this count.
pub fn remove_nonfeasible_appraisal_outputs(outputs: &mut AppraisalMetrics) -> usize {
    let old_len = outputs.len();
    outputs.retain(|_, metrics| metrics.first().is_some());
    old_len - outputs.len()
}

/// Sort appraisal outputs by their investment priority.
///
/// Investment priority is primarily decided by appraisal metric. When appraisal metrics are equal,
/// a tie-breaker fallback is used. Commissioned assets are preferred over uncommissioned assets,
/// and newer assets are preferred over older ones. The function does not guarantee that all ties
/// will be resolved.
///
fn sort_appraisal_outputs(outputs: &mut AppraisalMetrics) {
    let mut sorted: Vec<_> = outputs.drain(..).collect();
    sorted.sort_by(|(asset1, metrics1), (asset2, metrics2)| {
        compare_asset_metrics((asset1, metrics1), (asset2, metrics2))
    });
    outputs.extend(sorted);
}

/// Make an investment decision according to the configured decision rule.
///
/// Returns all options which are equally good according to the decision rule. The options must
/// already have non-feasible outputs removed.
pub fn make_investment_decision(
    mut outputs: AppraisalMetrics,
    decision_rule: &DecisionRule,
) -> Result<Vec<AssetRef>> {
    match decision_rule {
        DecisionRule::Single => {
            sort_appraisal_outputs(&mut outputs);
            if outputs.is_empty() {
                return Ok(Vec::new());
            }

            let num_best_outputs = count_equal_and_best_appraisal_outputs(&outputs) + 1;
            let best_outputs = outputs
                .into_iter()
                .take(num_best_outputs)
                .map(|(asset, _)| asset)
                .collect();

            Ok(best_outputs)
        }
        DecisionRule::Weighted => bail!("The weighted decision rule is not yet supported"),
        DecisionRule::Lexicographical { .. } => {
            bail!("The lexicographical decision rule is not yet supported")
        }
    }
}

/// Sort appraisal outputs by their investment priority and exclude non-feasible options.
///
/// This low-level helper is retained for callers which need the complete sorted list. New
/// decision-making code should use [`remove_nonfeasible_appraisal_outputs`] followed by
/// [`make_investment_decision`].
pub fn sort_and_filter_appraisal_outputs(outputs: &mut AppraisalMetrics) -> usize {
    let num_nonfeasible = remove_nonfeasible_appraisal_outputs(outputs);
    sort_appraisal_outputs(outputs);
    num_nonfeasible
}

/// Counts the number of top appraisal outputs in a sorted slice that are indistinguishable
/// by both metric and fallback ordering. Excludes the first element from the count.
pub fn count_equal_and_best_appraisal_outputs(outputs: &AppraisalMetrics) -> usize {
    if outputs.is_empty() {
        return 0;
    }
    let mut outputs = outputs.iter();
    let (best_asset, best_metrics) = outputs.next().unwrap();
    outputs
        .take_while(|output| {
            compare_asset_metrics((output.0, output.1), (best_asset, best_metrics)).is_eq()
        })
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentID;
    use crate::asset::AssetCapacity;
    use crate::fixture::{agent_id, asset, process, region_id};
    use crate::process::Process;
    use crate::region::RegionID;
    use crate::units::{Capacity, MoneyPerActivity};
    use float_cmp::assert_approx_eq;
    use indexmap::indexmap;
    use rstest::rstest;
    use std::sync::Arc;

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
        let capacity = Capacity(2.0);
        let process = Arc::new(process);
        let asset1 = Asset::new_commissioned(
            agent_id.clone(),
            process.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2015,
        )
        .unwrap();
        let asset2 =
            Asset::new_candidate(process.clone(), region_id.clone(), capacity, 2015).unwrap();
        let asset3 = Asset::new_commissioned(
            agent_id,
            process,
            region_id.clone(),
            AssetCapacity::single(capacity),
            2010,
        )
        .unwrap();

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
    fn appraisal_outputs(assets: Vec<Asset>, metrics: Vec<AppraisalMetric>) -> AppraisalMetrics {
        assert_eq!(
            assets.len(),
            metrics.len(),
            "assets and metrics must have the same length"
        );

        assets
            .into_iter()
            .zip(metrics)
            .map(|(asset, metric)| (AssetRef::from(asset), metric.boxed().into_iter().collect()))
            .collect()
    }

    /// Creates appraisal outputs with given metrics.
    /// Copies the provided default asset for each metric.
    fn appraisal_outputs_with_investment_priority_invariant_to_assets(
        metrics: Vec<AppraisalMetric>,
        asset: &Asset,
    ) -> AppraisalMetrics {
        let assets = (0..metrics.len())
            .map(|index| {
                Asset::new_ready(
                    AgentID(format!("agent{index}").into()),
                    Arc::new(asset.process().clone()),
                    asset.region_id().clone(),
                    AssetCapacity::single(asset.total_capacity()),
                    asset.commission_year(),
                )
                .unwrap()
            })
            .collect();
        appraisal_outputs(assets, metrics)
    }

    /// Test sorting by LCOX metric when invariant to asset properties
    #[rstest]
    fn appraisal_sort_by_lcox_metric(asset: Asset) {
        let metrics = vec![
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(3.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(7.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        sort_and_filter_appraisal_outputs(&mut outputs);

        assert_approx_eq!(f64, outputs.get_index(0).unwrap().1[0].value(), 3.0); // Best (lowest)
        assert_approx_eq!(f64, outputs.get_index(1).unwrap().1[0].value(), 5.0);
        assert_approx_eq!(f64, outputs.get_index(2).unwrap().1[0].value(), 7.0); // Worst (highest)
    }

    /// Test sorting by NPV metric when invariant to asset properties
    #[rstest]
    fn appraisal_sort_by_npv_metric(asset: Asset) {
        let metrics = vec![
            AppraisalMetric::Npv(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Npv(Some(MoneyPerActivity(3.0))),
            AppraisalMetric::Npv(Some(MoneyPerActivity(7.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        sort_and_filter_appraisal_outputs(&mut outputs);

        assert_approx_eq!(f64, outputs.get_index(0).unwrap().1[0].value(), 7.0); // Best (highest)
        assert_approx_eq!(f64, outputs.get_index(1).unwrap().1[0].value(), 5.0);
        assert_approx_eq!(f64, outputs.get_index(2).unwrap().1[0].value(), 3.0); // Worst (lowest)
    }

    /// Test that mixing LCOX and NPV metrics causes a runtime panic during comparison
    #[rstest]
    #[should_panic(expected = "Cannot compare metrics of different types")]
    fn appraisal_sort_by_mixed_metrics_panics(asset: Asset) {
        let metrics = vec![
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Npv(Some(MoneyPerActivity(3.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(3.0))),
        ];

        let mut outputs =
            appraisal_outputs_with_investment_priority_invariant_to_assets(metrics, &asset);
        // This should panic when trying to compare different metric types
        sort_and_filter_appraisal_outputs(&mut outputs);
    }

    /// Test that when metrics are equal, assets are sorted by commission year (newer first)
    #[rstest]
    fn appraisal_sort_by_commission_year_when_metrics_equal(process: Process, region_id: RegionID) {
        let process_rc = Arc::new(process);
        let capacity = Capacity(10.0);
        let commission_years = [2015, 2020, 2010];

        let assets: Vec<_> = commission_years
            .iter()
            .map(|&year| {
                Asset::new_ready(
                    AgentID(format!("agent{year}").into()),
                    process_rc.clone(),
                    region_id.clone(),
                    AssetCapacity::single(capacity),
                    year,
                )
                .unwrap()
            })
            .collect();

        // All metrics have the same value
        let metrics = vec![
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
        ];

        let mut outputs = appraisal_outputs(assets, metrics);
        sort_and_filter_appraisal_outputs(&mut outputs);

        // Should be sorted by commission year, newest first: 2020, 2015, 2010
        assert_eq!(outputs.get_index(0).unwrap().0.commission_year(), 2020);
        assert_eq!(outputs.get_index(1).unwrap().0.commission_year(), 2015);
        assert_eq!(outputs.get_index(2).unwrap().0.commission_year(), 2010);
    }

    /// Test that when metrics and commission years are equal, the original order is preserved
    #[rstest]
    fn appraisal_sort_maintains_order_when_all_equal(process: Process, region_id: RegionID) {
        let process_rc = Arc::new(process);
        let capacity = AssetCapacity::single(Capacity(10.0));
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

        let metrics = vec![
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
        ];

        let mut outputs = appraisal_outputs(assets.clone(), metrics);
        sort_and_filter_appraisal_outputs(&mut outputs);

        // Verify order is preserved - should match the original agent_ids array
        for (&expected_id, output) in agent_ids.iter().zip(outputs) {
            assert_eq!(output.0.agent_id(), Some(&AgentID(expected_id.into())));
        }
    }

    /// Test that commissioned assets are prioritised over non-commissioned assets when metrics are equal
    #[rstest]
    fn appraisal_sort_commissioned_before_uncommissioned_when_metrics_equal(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Arc::new(process);
        let capacity = Capacity(10.0);

        // Create a mix of commissioned and ready (non-commissioned) assets
        let commissioned_asset = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2015,
        )
        .unwrap();

        let ready_asset1 = Asset::new_ready(
            AgentID("agent2".into()),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();
        let ready_asset2 = Asset::new_ready(
            AgentID("agent3".into()),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();
        let ready_asset3 = Asset::new_ready(
            AgentID("agent4".into()),
            process_rc,
            region_id,
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();

        let assets = vec![ready_asset1, commissioned_asset, ready_asset2, ready_asset3];

        // All metrics have identical values to test fallback ordering
        let metrics = vec![
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(5.0))),
        ];

        let mut outputs = appraisal_outputs(assets, metrics);
        sort_and_filter_appraisal_outputs(&mut outputs);

        // Commissioned assets should be prioritised first
        assert!(outputs.get_index(0).unwrap().0.is_commissioned());
        assert_eq!(outputs.get_index(0).unwrap().0.commission_year(), 2015);

        // Non-commissioned assets should come after
        assert!(!outputs.get_index(2).unwrap().0.is_commissioned());
        assert!(!outputs.get_index(3).unwrap().0.is_commissioned());
    }

    /// Test that appraisal metric is prioritised over asset properties when sorting
    #[rstest]
    fn appraisal_metric_is_prioritised_over_asset_properties(
        process: Process,
        region_id: RegionID,
        agent_id: AgentID,
    ) {
        let process_rc = Arc::new(process);
        let capacity = Capacity(10.0);

        // Create a mix of commissioned and candidate (non-commissioned) assets
        let commissioned_asset = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2015,
        )
        .unwrap();

        let candidate_asset1 = Asset::new_ready(
            AgentID("agent2".into()),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();
        let candidate_asset2 = Asset::new_ready(
            AgentID("agent3".into()),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();
        let candidate_asset3 = Asset::new_ready(
            AgentID("agent4".into()),
            process_rc,
            region_id,
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();

        let assets = vec![
            candidate_asset1,
            commissioned_asset,
            candidate_asset2,
            candidate_asset3,
        ];

        // Make one metric slightly better than all others
        let baseline_metric_value = 5.0;
        let best_metric_value = baseline_metric_value - 0.1;
        let metrics = vec![
            AppraisalMetric::Lcox(Some(MoneyPerActivity(best_metric_value))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(baseline_metric_value))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(baseline_metric_value))),
            AppraisalMetric::Lcox(Some(MoneyPerActivity(baseline_metric_value))),
        ];

        let mut outputs = appraisal_outputs(assets, metrics);
        sort_and_filter_appraisal_outputs(&mut outputs);

        // non-commissioned asset prioritised because it has a slightly better metric
        assert_approx_eq!(
            f64,
            outputs.get_index(0).unwrap().1[0].value(),
            best_metric_value
        );
    }

    /// Test that appraisal outputs with an invalid metric are filtered out
    #[rstest]
    fn appraisal_sort_filters_invalid_metric(asset: Asset) {
        let mut outputs = indexmap! { AssetRef::from(asset) => Vec::new() };

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
        let metrics: Vec<AppraisalMetric> = metric_values
            .into_iter()
            .map(|v| AppraisalMetric::Lcox(Some(MoneyPerActivity(v))))
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
        let outputs = AppraisalMetrics::new();
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
        let process_rc = Arc::new(process);
        let capacity = Capacity(10.0);

        let commissioned = Asset::new_commissioned(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            2020,
        )
        .unwrap();
        let candidate =
            Asset::new_candidate(process_rc.clone(), region_id.clone(), capacity, 2020).unwrap();

        let metric_value = MoneyPerActivity(5.0);
        let outputs = appraisal_outputs(
            vec![commissioned, candidate],
            vec![
                AppraisalMetric::Lcox(Some(metric_value)),
                AppraisalMetric::Lcox(Some(metric_value)),
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
        let process_rc = Arc::new(process);
        let capacity = Capacity(10.0);
        let year = 2020;

        let asset1 = Asset::new_ready(
            agent_id.clone(),
            process_rc.clone(),
            region_id.clone(),
            AssetCapacity::single(capacity),
            year,
        )
        .unwrap();
        let asset2 = Asset::new_ready(
            AgentID("agent2".into()),
            process_rc,
            region_id.clone(),
            AssetCapacity::single(capacity),
            year,
        )
        .unwrap();

        let metric_value = MoneyPerActivity(5.0);
        let outputs = appraisal_outputs(
            vec![asset1, asset2],
            vec![
                AppraisalMetric::Lcox(Some(metric_value)),
                AppraisalMetric::Lcox(Some(metric_value)),
            ],
        );

        assert_eq!(count_equal_and_best_appraisal_outputs(&outputs), 1);
    }
}
