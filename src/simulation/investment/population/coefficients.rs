//! Calculation of cost coefficients for investment tools.
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::model::Model;
use crate::simulation::PriceMap;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{MoneyPerActivity, MoneyPerFlow};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;

/// Per-time-slice cost coefficients for an asset.
///
/// These coefficients are calculated according to the agent's `ObjectiveType` and are used by the
/// investment appraisal routines. Both the activity coefficients used in the appraisal optimisation
/// and the costs used to calculate the appraisal metric are derived from market prices.
#[derive(Clone)]
pub struct ObjectiveCoefficients {
    /// Net revenue per unit of activity, including fallback revenue and epsilon, in each time slice
    pub activity_coefficients: IndexMap<TimeSliceID, MoneyPerActivity>,
    /// Costs per unit of activity for the appraisal metric, excluding fallback revenue and epsilon
    pub metric_costs: IndexMap<TimeSliceID, MoneyPerActivity>,
}

/// Calculates cost coefficients for a set of assets for a given objective type.
///
/// Returns a map from each asset to its [`ObjectiveCoefficients`], which holds a per-time-slice
/// activity coefficient and metric cost.
///
/// Activity coefficients are revenue from flows (including the primary output) minus operating
/// cost, calculated using market prices, plus the primary output's fallback revenue. A small positive
/// epsilon is added to each activity coefficient so that assets with near-zero net value still appear
/// in dispatch.
///
/// Metric costs are operating cost minus flow revenue at market prices. NPV includes all flows,
/// whereas LCOX excludes revenue from the primary output (commodity of interest). Neither includes
/// fallback revenue or epsilon.
pub fn calculate_coefficients_for_assets(
    model: &Model,
    objective_type: ObjectiveType,
    assets: &[AssetRef],
    market_prices: &PriceMap,
    fallback_prices: &PriceMap,
    year: u32,
) -> HashMap<AssetRef, Arc<ObjectiveCoefficients>> {
    assets
        .iter()
        .map(|asset| {
            let coefficient = calculate_coefficients_for_asset(
                asset,
                objective_type,
                &model.time_slice_info,
                market_prices,
                fallback_prices,
                year,
            );
            (asset.clone(), Arc::new(coefficient))
        })
        .collect()
}

/// Calculates cost coefficients for a single asset
fn calculate_coefficients_for_asset(
    asset: &AssetRef,
    objective_type: ObjectiveType,
    time_slice_info: &TimeSliceInfo,
    market_prices: &PriceMap,
    fallback_prices: &PriceMap,
    year: u32,
) -> ObjectiveCoefficients {
    // Small constant added to each activity coefficient to ensure break-even/slightly negative
    // assets are still dispatched
    const EPSILON_ACTIVITY_COEFFICIENT: MoneyPerActivity = MoneyPerActivity(f64::EPSILON * 100.0);

    let mut activity_coefficients = IndexMap::new();
    let mut metric_costs = IndexMap::new();
    let primary_output_flow = asset.primary_output().unwrap();
    let asset_region = asset.region_id();
    for time_slice in time_slice_info.iter_ids() {
        // Get the operating cost of the asset. This includes the variable operating cost, levies and
        // flow costs, but excludes costs/revenues from commodity consumption/production.
        let operating_cost = asset.get_operating_cost(year, time_slice);
        let net_revenue = asset.get_revenue_from_flows(market_prices, time_slice) - operating_cost;

        let fallback_revenue = fallback_prices
            .get(&primary_output_flow.commodity.id, asset_region, time_slice)
            .unwrap_or(MoneyPerFlow(0.0))
            * primary_output_flow.coeff;

        activity_coefficients.insert(
            time_slice.clone(),
            fallback_revenue + net_revenue + EPSILON_ACTIVITY_COEFFICIENT,
        );

        let metric_cost = match objective_type {
            ObjectiveType::LevelisedCostOfX => {
                operating_cost
                    - asset.get_revenue_from_flows_excluding_primary(market_prices, time_slice)
            }
            ObjectiveType::NetPresentValue => -net_revenue,
        };
        metric_costs.insert(time_slice.clone(), metric_cost);
    }

    ObjectiveCoefficients {
        activity_coefficients,
        metric_costs,
    }
}
