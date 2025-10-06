//! Calculation of cost coefficients for investment tools.
use super::costs::{activity_cost, activity_surplus, annual_fixed_cost};
use crate::agent::ObjectiveType;
use crate::asset::AssetRef;
use crate::model::Model;
use crate::simulation::prices::ReducedCosts;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{MoneyPerActivity, MoneyPerCapacity, MoneyPerFlow};
use indexmap::IndexMap;
use std::collections::HashMap;

/// Map storing cost coefficients for an asset.
///
/// These are calculated according to the objective type of the agent owning the asset.
/// Map storing coefficients for each variable
#[derive(Clone)]
pub struct ObjectiveCoefficients {
    /// Cost per unit of capacity
    pub capacity_coefficient: MoneyPerCapacity,
    /// Cost per unit of activity in each time slice
    pub activity_coefficients: IndexMap<TimeSliceID, MoneyPerActivity>,
    /// Unmet demand coefficient
    pub unmet_demand_coefficient: MoneyPerFlow,
}

/// Calculates cost coefficients for all assets for a given objective type.
pub fn calculate_coefficients_for_assets(
    model: &Model,
    objective_type: &ObjectiveType,
    assets: &[AssetRef],
    reduced_costs: &ReducedCosts,
) -> HashMap<AssetRef, ObjectiveCoefficients> {
    assets
        .iter()
        .map(|asset| {
            let coefficient = match objective_type {
                ObjectiveType::LevelisedCostOfX => calculate_coefficients_for_lcox(
                    asset,
                    &model.time_slice_info,
                    reduced_costs,
                    model.parameters.value_of_lost_load,
                ),
                ObjectiveType::NetPresentValue => {
                    calculate_coefficients_for_npv(asset, &model.time_slice_info, reduced_costs)
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
    reduced_costs: &ReducedCosts,
    value_of_lost_load: MoneyPerFlow,
) -> ObjectiveCoefficients {
    // Capacity coefficient
    let capacity_coefficient = annual_fixed_cost(asset);

    // Activity coefficients
    let mut activity_coefficients = IndexMap::new();
    for time_slice in time_slice_info.iter_ids() {
        let coefficient = activity_cost(asset, reduced_costs, time_slice);
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
    reduced_costs: &ReducedCosts,
) -> ObjectiveCoefficients {
    // Capacity coefficient
    let capacity_coefficient = -annual_fixed_cost(asset);

    // Activity coefficients
    let mut activity_coefficients = IndexMap::new();
    for time_slice in time_slice_info.iter_ids() {
        let coefficient = activity_surplus(asset, reduced_costs, time_slice);
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
