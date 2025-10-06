//! Costs for the optimisation problem.
use crate::asset::{AssetRef, AssetState};
use crate::finance::annual_capital_cost;
use crate::simulation::prices::ReducedCosts;
use crate::time_slice::TimeSliceID;
use crate::units::{MoneyPerActivity, MoneyPerCapacity, Year};

/// Calculates the cost per unit of activity for an asset.
pub fn activity_cost(
    asset: &AssetRef,
    reduced_costs: &ReducedCosts,
    time_slice: &TimeSliceID,
) -> MoneyPerActivity {
    reduced_costs.get(asset, time_slice)
}

/// Calculates the surplus per unit of activity for an asset.
pub fn activity_surplus(
    asset: &AssetRef,
    reduced_costs: &ReducedCosts,
    time_slice: &TimeSliceID,
) -> MoneyPerActivity {
    -reduced_costs.get(asset, time_slice)
}

/// Calculates the annual fixed costs per unit of capacity for an asset.
///
/// The behaviour depends on whether the asset is commissioned or a candidate:
/// - For a commissioned asset, this only includes operating costs.
/// - For a candidate asset, this includes both operating and capital costs.
pub fn annual_fixed_cost(asset: &AssetRef) -> MoneyPerCapacity {
    match asset.state() {
        AssetState::Commissioned { .. } => annual_fixed_cost_for_existing(asset),
        AssetState::Candidate => annual_fixed_cost_for_candidate(asset),
        _ => {
            panic!("annual_fixed_cost should only be called with Commissioned or Candidate assets")
        }
    }
}

fn annual_fixed_cost_for_existing(asset: &AssetRef) -> MoneyPerCapacity {
    let fixed_operating_cost = asset.process_parameter().fixed_operating_cost;
    fixed_operating_cost * Year(1.0)
}

fn annual_capital_cost_for_candidate(asset: &AssetRef) -> MoneyPerCapacity {
    let capital_cost = asset.process_parameter().capital_cost;
    let lifetime = asset.process_parameter().lifetime;
    let discount_rate = asset.process_parameter().discount_rate;
    annual_capital_cost(capital_cost, lifetime, discount_rate)
}

fn annual_fixed_cost_for_candidate(asset: &AssetRef) -> MoneyPerCapacity {
    let fixed_operating_cost = asset.process_parameter().fixed_operating_cost;
    let annual_fixed_operating_cost = fixed_operating_cost * Year(1.0);
    let capital_costs = annual_capital_cost_for_candidate(asset);
    annual_fixed_operating_cost + capital_costs
}
