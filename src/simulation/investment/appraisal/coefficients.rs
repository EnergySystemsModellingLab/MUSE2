//! Calculation of cost coefficients for investment tools.
use crate::asset::AssetRef;
use crate::model::Model;
use crate::simulation::investment::appraisal::costs::annual_fixed_cost;
use crate::simulation::prices::Prices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{MoneyPerActivity, MoneyPerCapacity};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;

/// Calculates cost coefficients for a set of assets for a given objective type.
///
/// Returns a map from each asset to its [`ObjectiveCoefficients`], which holds a per-time-slice
/// activity coefficient and market cost.
///
/// Activity coefficients are revenue from flows (including the primary output) minus operating
/// cost, calculated using shadow prices. A small positive epsilon is added to each activity
/// coefficient so that assets with near-zero net value still appear in dispatch.
///
/// Market costs are calculated using market prices rather than shadow prices. For NPV they use the
/// same revenue-minus-operating-cost calculation as the activity coefficients. For LCOX the sign is
/// inverted (as the value represents a cost) and the primary output (commodity of interest) is
/// excluded.
pub fn calculate_activity_coefficients_for_assets(
    model: &Model,
    assets: &[AssetRef],
    prices: &Prices,
    year: u32,
) -> HashMap<AssetRef, Arc<IndexMap<TimeSliceID, MoneyPerActivity>>> {
    assets
        .iter()
        .map(|asset| {
            let coefficient = calculate_activity_coefficients_for_asset(
                asset,
                &model.time_slice_info,
                prices,
                year,
            );
            (asset.clone(), Arc::new(coefficient))
        })
        .collect()
}

/// Calculates activity coefficients for a single asset
pub fn calculate_activity_coefficients_for_asset(
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
    prices: &Prices,
    year: u32,
) -> IndexMap<TimeSliceID, MoneyPerActivity> {
    let mut activity_coefficients = IndexMap::new();
    for time_slice in time_slice_info.iter_ids() {
        // Get the operating cost of the asset. This includes the variable operating cost, levies and
        // flow costs, but excludes costs/revenues from commodity consumption/production.
        let operating_cost = asset.get_operating_cost(year, time_slice);
        let input_cost = asset.get_input_cost_from_prices(&prices.shadow, time_slice);

        activity_coefficients.insert(time_slice.clone(), operating_cost + input_cost);
    }

    activity_coefficients
}

/// Calculate capacity coefficients for all assets
pub fn calculate_capacity_coefficients_for_assets(
    assets: &[AssetRef],
) -> HashMap<AssetRef, MoneyPerCapacity> {
    assets
        .iter()
        .map(|asset| (asset.clone(), annual_fixed_cost(asset)))
        .collect()
}
