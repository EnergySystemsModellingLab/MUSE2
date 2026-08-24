//! Calculation of cost coefficients for investment tools.
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::model::Model;
use crate::simulation::PriceMap;
use crate::simulation::prices::Prices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{MoneyPerActivity, MoneyPerFlow};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::sync::Arc;

/// Cost per unit of activity in each time slice.
pub type ActivityCoefficients = IndexMap<TimeSliceID, MoneyPerActivity>;

/// Market costs associated with an asset for each time slice.
pub type MarketCosts = IndexMap<TimeSliceID, MoneyPerActivity>;

/// Calculates activity coefficients for a set of assets.
pub fn calculate_activity_coefficients_for_assets(
    model: &Model,
    assets: &[AssetRef],
    prices: &Prices,
    year: u32,
) -> HashMap<AssetRef, Arc<ActivityCoefficients>> {
    assets
        .iter()
        .map(|asset| {
            let coefficients = calculate_activity_coefficients_for_asset(
                asset,
                &model.time_slice_info,
                prices,
                year,
            );
            (asset.clone(), Arc::new(coefficients))
        })
        .collect()
}

/// Calculates objective-specific market costs for a set of assets.
pub fn calculate_market_costs_for_assets(
    model: &Model,
    objective_type: &ObjectiveType,
    assets: &[AssetRef],
    prices: &Prices,
    year: u32,
) -> HashMap<AssetRef, Arc<MarketCosts>> {
    assets
        .iter()
        .map(|asset| {
            let costs = calculate_market_costs_for_asset(
                asset,
                objective_type,
                &model.time_slice_info,
                prices,
                year,
            );
            (asset.clone(), Arc::new(costs))
        })
        .collect()
}

/// Calculates activity coefficients for a single asset.
pub fn calculate_activity_coefficients_for_asset(
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
    prices: &Prices,
    year: u32,
) -> ActivityCoefficients {
    // Small constant added to each activity coefficient to ensure break-even/slightly negative
    // assets are still dispatched
    const EPSILON_ACTIVITY_COEFFICIENT: MoneyPerActivity = MoneyPerActivity(f64::EPSILON * 100.0);

    // Activity coefficients
    let mut activity_coefficients = IndexMap::new();
    let primary_output_flow = asset.primary_output().unwrap();
    let asset_region = asset.region_id();
    for time_slice in time_slice_info.iter_ids() {
        // Get the operating cost of the asset. This includes the variable operating cost, levies and
        // flow costs, but excludes costs/revenues from commodity consumption/production.
        let operating_cost = asset.get_operating_cost(year, time_slice);
        let net_operating_cost =
            -calculate_asset_revenues(asset, operating_cost, time_slice, &prices.shadow);

        let fallback_cost = prices
            .fallback
            .get(&primary_output_flow.commodity.id, asset_region, time_slice)
            .unwrap_or(MoneyPerFlow(0.0))
            * primary_output_flow.coeff;

        activity_coefficients.insert(
            time_slice.clone(),
            fallback_cost - net_operating_cost + EPSILON_ACTIVITY_COEFFICIENT,
        );
    }

    activity_coefficients
}

/// Calculates objective-specific market costs for a single asset.
pub fn calculate_market_costs_for_asset(
    asset: &AssetRef,
    objective_type: &ObjectiveType,
    time_slice_info: &TimeSliceInfo,
    prices: &Prices,
    year: u32,
) -> MarketCosts {
    let mut market_costs = IndexMap::new();
    for time_slice in time_slice_info.iter_ids() {
        let operating_cost = asset.get_operating_cost(year, time_slice);
        let market_cost = match objective_type {
            ObjectiveType::LevelisedCostOfX => {
                calculate_asset_costs_for_lcox(asset, operating_cost, time_slice, &prices.market)
            }
            ObjectiveType::NetPresentValue => {
                -calculate_asset_revenues(asset, operating_cost, time_slice, &prices.market)
            }
        };
        market_costs.insert(time_slice.clone(), market_cost);
    }

    market_costs
}

/// Calculate the revenue from all flows minus operating cost
fn calculate_asset_revenues(
    asset: &AssetRef,
    operating_cost: MoneyPerActivity,
    time_slice: &TimeSliceID,
    prices: &PriceMap,
) -> MoneyPerActivity {
    // Revenue from flows including the primary output
    let revenue_from_flows = asset.get_revenue_from_flows(prices, time_slice);

    // The activity coefficient is the revenue from flows minus the operating cost (net revenue)
    revenue_from_flows - operating_cost
}

/// Calculate asset costs for LCOX objective.
///
/// Excludes revenues from the primary output (commodity of interest).
fn calculate_asset_costs_for_lcox(
    asset: &AssetRef,
    operating_cost: MoneyPerActivity,
    time_slice: &TimeSliceID,
    prices: &PriceMap,
) -> MoneyPerActivity {
    // Revenue from flows excluding the primary output
    let revenue_from_flows = asset.get_revenue_from_flows_excluding_primary(prices, time_slice);

    // The activity coefficient is the operating cost minus the revenue from non-primary flows
    operating_cost - revenue_from_flows
}
