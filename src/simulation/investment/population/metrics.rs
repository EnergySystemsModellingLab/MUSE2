//! Appraisal results and merit-order sorting for the population agent.
use super::optimisation::activity_per_capacity;
use crate::asset::AssetRef;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, ActivityPerCapacity, Capacity};
use float_cmp::{ApproxEq, F64Margin};
use indexmap::IndexMap;
use std::cmp::Ordering;

/// The result of appraising one option against the complete demand profile.
pub struct AppraisalOutput {
    /// The asset being appraised.
    pub asset: AssetRef,
    /// Capacity required to supply the full demand profile.
    pub capacity: Capacity,
    /// Activity per unit capacity for the fitted profile.
    pub activity_per_capacity: IndexMap<TimeSliceID, ActivityPerCapacity>,
    /// Total objective value for supplying the full demand profile.
    pub metric: f64,
}

impl AppraisalOutput {
    /// Build an appraisal result and convert its activity profile to per-capacity values.
    pub fn new(
        asset: AssetRef,
        capacity: Capacity,
        activity: &IndexMap<TimeSliceID, Activity>,
        metric: f64,
    ) -> Self {
        Self {
            asset,
            capacity,
            activity_per_capacity: activity_per_capacity(activity, capacity),
            metric,
        }
    }
}

/// Compare metrics approximately to avoid platform-specific choices caused by tiny solver
/// differences (for example, between different CPU architectures).
///
/// # Panics
///
/// Panics if either metric is NaN.
fn compare_metric(left: f64, right: f64) -> Ordering {
    if left.approx_eq(right, F64Margin::default()) {
        Ordering::Equal
    } else {
        left.partial_cmp(&right)
            .expect("Cannot compare NaN metrics")
    }
}

/// Compare assets as a fallback when their metrics are equal.
///
/// Commissioned assets are preferred over candidates, and newer assets are preferred over older
/// commissioned assets. This preserves the population agent's existing equal-metric tie-break.
fn compare_asset_fallback(asset1: &AssetRef, asset2: &AssetRef) -> Ordering {
    (asset2.is_commissioned(), asset2.commission_year())
        .cmp(&(asset1.is_commissioned(), asset1.commission_year()))
}

/// Sort by investment priority, preserving the existing commissioned/newer tie-break rules.
///
/// Lower optimisation cost is better. If metrics are approximately equal, the asset fallback is
/// used; remaining ties retain the input order.
pub fn sort_appraisal_outputs(outputs: &mut [AppraisalOutput]) {
    outputs.sort_by(
        |left, right| match compare_metric(left.metric, right.metric) {
            Ordering::Equal => compare_asset_fallback(&left.asset, &right.asset),
            ordering => ordering,
        },
    );
}

/// Count the best options indistinguishable from the first under metric and fallback ordering.
///
/// The first option is excluded from the count. The caller uses this to log when the selected
/// option is not uniquely best.
pub fn count_equal_and_best_appraisal_outputs(outputs: &[AppraisalOutput]) -> usize {
    let Some(best) = outputs.first() else {
        return 0;
    };
    outputs[1..]
        .iter()
        .take_while(|output| {
            compare_metric(output.metric, best.metric).is_eq()
                && compare_asset_fallback(&output.asset, &best.asset).is_eq()
        })
        .count()
}
