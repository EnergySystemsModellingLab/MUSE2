//! Costs for the optimisation problem.
use crate::asset::{AssetRef, AssetState};
use crate::units::{MoneyPerCapacity, Year};

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

fn annual_fixed_cost_for_candidate(asset: &AssetRef) -> MoneyPerCapacity {
    let fixed_operating_cost = asset.process_parameter().fixed_operating_cost;
    let annual_fixed_operating_cost = fixed_operating_cost * Year(1.0);
    let capital_costs = asset.get_annual_capital_cost_per_capacity();
    annual_fixed_operating_cost + capital_costs
}
