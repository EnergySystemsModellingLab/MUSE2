//! Calculation of cost coefficients for investment tools.
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::model::Model;
use crate::simulation::PriceMap;
use crate::simulation::prices::Prices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::MoneyPerActivity;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::rc::Rc;

/// Per-time-slice cost coefficients for an asset.
///
/// These coefficients are calculated according to the agent's `ObjectiveType` and are used by the
/// investment appraisal routines. They comprise the activity coefficients (revenue minus operating
/// cost, derived from shadow prices) used in the appraisal optimisation, together with the market
/// costs (derived from market prices).
#[derive(Clone)]
pub struct ObjectiveCoefficients {
    /// Cost per unit of activity in each time slice
    pub activity_coefficients: IndexMap<TimeSliceID, MoneyPerActivity>,
    /// Market costs associated with asset for each time slice
    pub market_costs: IndexMap<TimeSliceID, MoneyPerActivity>,
}

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
pub fn calculate_coefficients_for_assets(
    model: &Model,
    objective_type: &ObjectiveType,
    assets: &[AssetRef],
    prices: &Prices,
    year: u32,
) -> HashMap<AssetRef, Rc<ObjectiveCoefficients>> {
    assets
        .iter()
        .map(|asset| {
            let coefficient = calculate_coefficients_for_asset(
                asset,
                objective_type,
                &model.time_slice_info,
                prices,
                year,
            );
            (asset.clone(), Rc::new(coefficient))
        })
        .collect()
}

/// Calculates cost coefficients for a single asset
pub fn calculate_coefficients_for_asset(
    asset: &AssetRef,
    objective_type: &ObjectiveType,
    time_slice_info: &TimeSliceInfo,
    prices: &Prices,
    year: u32,
) -> ObjectiveCoefficients {
    // Small constant added to each activity coefficient to ensure break-even/slightly negative
    // assets are still dispatched
    const EPSILON_ACTIVITY_COEFFICIENT: MoneyPerActivity = MoneyPerActivity(f64::EPSILON * 100.0);

    // Activity coefficients
    let mut activity_coefficients = IndexMap::new();
    let mut market_costs = IndexMap::new();
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
            .unwrap()
            * primary_output_flow.coeff;

        activity_coefficients.insert(
            time_slice.clone(),
            fallback_cost - net_operating_cost + EPSILON_ACTIVITY_COEFFICIENT,
        );

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

    ObjectiveCoefficients {
        activity_coefficients,
        market_costs,
    }
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
