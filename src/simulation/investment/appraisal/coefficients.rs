//! Calculation of cost coefficients for investment tools.
use super::costs::annual_fixed_cost;
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::model::Model;
use crate::simulation::CommodityPrices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{MoneyPerActivity, MoneyPerCapacity, MoneyPerFlow};
use indexmap::IndexMap;
use std::collections::HashMap;

/// Map storing cost coefficients for an asset.
///
/// These are calculated according to the objective type of the agent owning the asset.
/// Map storing coefficients for each variable
pub struct ObjectiveCoefficients {
    /// Cost per unit of capacity
    pub capacity_coefficient: MoneyPerCapacity,
    /// Cost per unit of activity in each time slice
    pub activity_coefficients: IndexMap<TimeSliceID, MoneyPerActivity>,
    /// Unmet demand coefficient
    pub unmet_demand_coefficient: MoneyPerFlow,
}

/// Calculates cost coefficients for a set of assets for a given objective type.
pub fn calculate_coefficients_for_assets(
    model: &Model,
    objective_type: &ObjectiveType,
    assets: &[AssetRef],
    prices: &CommodityPrices,
    year: u32,
) -> HashMap<AssetRef, ObjectiveCoefficients> {
    assets
        .iter()
        .map(|asset| {
            let coefficient = match objective_type {
                ObjectiveType::LevelisedCostOfX => calculate_coefficients_for_lcox(
                    asset,
                    &model.time_slice_info,
                    prices,
                    model.parameters.value_of_lost_load,
                    year,
                ),
                ObjectiveType::NetPresentValue => {
                    calculate_coefficients_for_npv(asset, &model.time_slice_info, prices, year)
                }
            };
            (asset.clone(), coefficient)
        })
        .collect()
}

/// Calculates the cost coefficients for LCOX.
pub fn calculate_coefficients_for_lcox(
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
    prices: &CommodityPrices,
    value_of_lost_load: MoneyPerFlow,
    year: u32,
) -> ObjectiveCoefficients {
    // Capacity coefficient
    let capacity_coefficient = annual_fixed_cost(asset);

    // Activity coefficients
    let mut activity_coefficients = IndexMap::new();
    for time_slice in time_slice_info.iter_ids() {
        let coefficient = calculate_activity_coefficient_for_lcox(asset, time_slice, prices, year);
        activity_coefficients.insert(time_slice.clone(), coefficient);
    }

    // Unmet demand coefficient
    let unmet_demand_coefficient = value_of_lost_load;

    ObjectiveCoefficients {
        capacity_coefficient,
        activity_coefficients,
        unmet_demand_coefficient,
    }
}

/// Calculates the cost coefficients for NPV.
pub fn calculate_coefficients_for_npv(
    asset: &AssetRef,
    time_slice_info: &TimeSliceInfo,
    prices: &CommodityPrices,
    year: u32,
) -> ObjectiveCoefficients {
    // Capacity coefficient
    let capacity_coefficient = -annual_fixed_cost(asset);

    // Activity coefficients
    let mut activity_coefficients = IndexMap::new();
    for time_slice in time_slice_info.iter_ids() {
        let coefficient = calculate_activity_coefficient_for_npv(asset, time_slice, prices, year);
        activity_coefficients.insert(time_slice.clone(), coefficient);
    }

    // Unmet demand coefficient (we don't apply a cost to unmet demand, so we set this to zero)
    let unmet_demand_coefficient = MoneyPerFlow(0.0);

    ObjectiveCoefficients {
        capacity_coefficient,
        activity_coefficients,
        unmet_demand_coefficient,
    }
}

/// Calculate a single activity coefficient for the LCOX objective for a given time slice.
fn calculate_activity_coefficient_for_lcox(
    asset: &AssetRef,
    time_slice: &TimeSliceID,
    prices: &CommodityPrices,
    year: u32,
) -> MoneyPerActivity {
    let operating_cost = asset.get_operating_cost(year, time_slice);

    // Revenue from flows excluding the primary output
    let revenue_from_flows = asset.get_revenue_from_flows(prices, time_slice, true);

    // The activity coefficient is the operating cost minus the revenue from flows
    operating_cost - revenue_from_flows
}

/// Calculate a single activity coefficient for the NPV objective for a given time slice.
fn calculate_activity_coefficient_for_npv(
    asset: &AssetRef,
    time_slice: &TimeSliceID,
    prices: &CommodityPrices,
    year: u32,
) -> MoneyPerActivity {
    let operating_cost = asset.get_operating_cost(year, time_slice);

    // Revenue from flows
    let revenue_from_flows = asset.get_revenue_from_flows(prices, time_slice, false);

    // The activity coefficient is the revenue from flows minus the operating cost
    revenue_from_flows - operating_cost
}
