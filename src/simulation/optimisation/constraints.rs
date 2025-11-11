//! Code for adding constraints to the dispatch optimisation problem.
use super::VariableMap;
use crate::asset::{AssetIterator, AssetRef};
use crate::commodity::{CommodityID, CommodityType};
use crate::model::Model;
use crate::region::RegionID;
use crate::time_slice::{TimeSliceID, TimeSliceSelection};
use crate::units::UnitType;
use highs::RowProblem as Problem;
use indexmap::IndexMap;

/// Corresponding variables for a constraint along with the row offset in the solution
pub struct KeysWithOffset<T> {
    offset: usize,
    keys: Vec<T>,
}

impl<T> KeysWithOffset<T> {
    /// Zip the keys with the corresponding dual values in the solution, accounting for the offset
    pub fn zip_duals<'a, U>(&'a self, duals: &'a [f64]) -> impl Iterator<Item = (&'a T, U)>
    where
        U: UnitType,
    {
        assert!(
            self.offset + self.keys.len() <= duals.len(),
            "Bad constraint keys: dual rows out of range"
        );

        self.keys
            .iter()
            .zip(duals[self.offset..].iter().copied().map(U::new))
    }
}

/// Indicates the commodity ID and time slice selection covered by each commodity balance constraint
pub type CommodityBalanceKeys = KeysWithOffset<(CommodityID, RegionID, TimeSliceSelection)>;

/// Indicates the asset ID and time slice covered by each activity constraint
pub type ActivityKeys = KeysWithOffset<(AssetRef, TimeSliceID)>;

/// The keys for different constraints
pub struct ConstraintKeys {
    /// Keys for commodity balance constraints
    pub commodity_balance_keys: CommodityBalanceKeys,
    /// Keys for activity constraints
    pub activity_keys: ActivityKeys,
}

/// Add constraints for the dispatch model.
///
/// Note: the ordering of constraints is important, as the dual values of the constraints must later
/// be retrieved to calculate commodity prices.
///
/// # Arguments
///
/// * `problem` - The optimisation problem
/// * `variables` - The variables in the problem
/// * `model` - The model
/// * `assets` - The asset pool
/// * `markets_to_balance` - The subset of markets to apply balance constraints to
/// * `markets_to_allow_unmet_demand` - The subset of markets to assign unmet demand variables to
/// * `year` - Current milestone year
///
/// # Returns
///
/// Keys for the different constraints.
pub fn add_model_constraints<'a, I>(
    problem: &mut Problem,
    variables: &VariableMap,
    model: &'a Model,
    assets: &I,
    markets_to_balance: &'a [(CommodityID, RegionID)],
    markets_to_allow_unmet_demand: &'a [(CommodityID, RegionID)],
    year: u32,
) -> ConstraintKeys
where
    I: Iterator<Item = &'a AssetRef> + Clone + 'a,
{
    let commodity_balance_keys = add_commodity_balance_constraints(
        problem,
        variables,
        model,
        assets,
        markets_to_balance,
        markets_to_allow_unmet_demand,
        year,
    );

    let activity_keys = add_activity_constraints(problem, variables);

    // Return constraint keys
    ConstraintKeys {
        commodity_balance_keys,
        activity_keys,
    }
}

/// Add asset-level input-output commodity balances.
///
/// These constraints fix the supply-demand balance for the whole system.
///
/// See description in [the dispatch optimisation documentation][1].
///
/// [1]: https://energysystemsmodellinglab.github.io/MUSE2/model/dispatch_optimisation.html#commodity-balance-for--cin-mathbfcmathrmsed-
fn add_commodity_balance_constraints<'a, I>(
    problem: &mut Problem,
    variables: &VariableMap,
    model: &'a Model,
    assets: &I,
    markets_to_balance: &'a [(CommodityID, RegionID)],
    markets_to_allow_unmet_demand: &'a [(CommodityID, RegionID)],
    year: u32,
) -> CommodityBalanceKeys
where
    I: Iterator<Item = &'a AssetRef> + Clone + 'a,
{
    // Row offset in problem. This line **must** come before we add more constraints.
    let offset = problem.num_rows();

    let mut keys = Vec::new();
    let mut terms = Vec::new();
    for (commodity_id, region_id) in markets_to_balance {
        let commodity = &model.commodities[commodity_id];
        if !matches!(
            commodity.kind,
            CommodityType::SupplyEqualsDemand | CommodityType::ServiceDemand
        ) {
            continue;
        }

        for ts_selection in model
            .time_slice_info
            .iter_selections_at_level(commodity.time_slice_level)
        {
            for (asset, flow) in assets
                .clone()
                .filter_region(region_id)
                .flows_for_commodity(commodity_id)
            {
                // If the commodity has a time slice level of season/annual, the constraint will
                // cover multiple time slices
                for (time_slice, _) in ts_selection.iter(&model.time_slice_info) {
                    let var = variables.get_activity_var(asset, time_slice);
                    terms.push((var, flow.coeff.value()));
                }
            }

            // It is possible that a commodity may not be produced or consumed by anything in a
            // given milestone year, in which case it doesn't make sense to add a commodity
            // balance constraint
            if terms.is_empty() {
                continue;
            }

            // Also include unmet demand variables if required
            if markets_to_allow_unmet_demand.contains(&(commodity_id.clone(), region_id.clone())) {
                for (time_slice, _) in ts_selection.iter(&model.time_slice_info) {
                    let var = variables.get_unmet_demand_var(commodity_id, region_id, time_slice);
                    terms.push((var, 1.0));
                }
            }

            // For SED commodities, the LHS must be >=0 and for SVD commodities, it must be >=
            // the exogenous demand supplied by the user
            let min = if commodity.kind == CommodityType::ServiceDemand {
                commodity
                    .demand
                    .get(&(region_id.clone(), year, ts_selection.clone()))
                    .unwrap()
                    .value()
            } else {
                0.0
            };
            problem.add_row(min.., terms.drain(..));
            keys.push((
                commodity_id.clone(),
                region_id.clone(),
                ts_selection.clone(),
            ));
        }
    }

    CommodityBalanceKeys { offset, keys }
}

/// Add constraints on the activity of different assets.
///
/// This ensures that assets do not exceed their specified capacity and availability for each time
/// slice.
///
/// See description in [the dispatch optimisation documentation][1].
///
/// [1]: https://energysystemsmodellinglab.github.io/MUSE2/model/dispatch_optimisation.html#a4-constraints-capacity--availability-for-standard-assets--a-in-mathbfastd-
fn add_activity_constraints(problem: &mut Problem, variables: &VariableMap) -> ActivityKeys {
    // Row offset in problem. This line **must** come before we add more constraints.
    let offset = problem.num_rows();

    let mut keys = Vec::new();
    let capacity_vars: IndexMap<&AssetRef, highs::Col> = variables.iter_capacity_vars().collect();
    for (asset, time_slice, activity_var) in variables.iter_activity_vars() {
        if let Some(&capacity_var) = capacity_vars.get(asset) {
            // Asset has flexible capacity.
            let per_capacity_limits = asset.get_activity_per_capacity_limits(time_slice);
            let lower_limit = per_capacity_limits.start().value();
            let upper_limit = per_capacity_limits.end().value();

            // Upper bound: activity ≤ capacity * upper_limit
            problem.add_row(..=0.0, [(activity_var, 1.0), (capacity_var, -upper_limit)]);

            // Lower bound: activity ≥ capacity * lower_limit
            problem.add_row(..=0.0, [(activity_var, -1.0), (capacity_var, lower_limit)]);

            // Store keys for retrieving duals later.
            // TODO: bad idea to push identical keys twice. Safe for now so long as we don't use the
            // activity duals for anything important when using flexible capacity assets.
            keys.push((asset.clone(), time_slice.clone()));
            keys.push((asset.clone(), time_slice.clone()));
        } else {
            // Fixed-capacity asset: simple absolute activity limits.
            let activity_limits = asset.get_activity_limits(time_slice);
            let range = activity_limits.start().value()..=activity_limits.end().value();
            problem.add_row(range, [(activity_var, 1.0)]);

            // Store keys for retrieving duals later.
            keys.push((asset.clone(), time_slice.clone()));
        }
    }

    ActivityKeys { offset, keys }
}
