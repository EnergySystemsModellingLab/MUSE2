//! Code for adding constraints to the dispatch optimisation problem.
use super::VariableMap;
use crate::asset::{AssetIterator, AssetRef};
use crate::commodity::{CommodityID, CommodityType};
use crate::model::Model;
use crate::region::RegionID;
use crate::time_slice::{TimeSliceInfo, TimeSliceSelection};
use crate::units::UnitType;
use highs::RowProblem as Problem;
use indexmap::IndexMap;

/// Corresponding variables for a constraint along with the row offset in the solution
pub struct KeysWithOffset<T> {
    /// Row offset in the solver's row ordering corresponding to the first key in `keys`.
    ///
    /// This offset is used to index into the solver duals vector when mapping dual
    /// values back to the stored `keys`.
    offset: usize,
    /// Keys for each constraint row. The number of keys equals the number of rows
    /// covered starting at `offset`.
    keys: Vec<T>,
}

impl<T> KeysWithOffset<T> {
    /// Zip the keys with the corresponding dual values in the solution, accounting for the offset.
    ///
    /// The returned iterator yields pairs of `(key, dual)` where `dual` is wrapped in the
    /// unit type `U: UnitType`. The method asserts that the provided `duals` slice contains
    /// at least `offset + keys.len()` elements.
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
pub type ActivityKeys = KeysWithOffset<(AssetRef, TimeSliceSelection)>;

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
        year,
    );

    let activity_keys =
        add_activity_constraints(problem, variables, &model.time_slice_info, assets.clone());

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
/// Returns a `CommodityBalanceKeys` where `offset` is the row index of the first
/// commodity-balance constraint added to `problem` and `keys` lists the
/// `(commodity, region, time_selection)` entries in the same order as the rows.
///
/// [1]: https://energysystemsmodellinglab.github.io/MUSE2/model/dispatch_optimisation.html#commodity-balance-for--cin-mathbfcmathrmsed-
fn add_commodity_balance_constraints<'a, I>(
    problem: &mut Problem,
    variables: &VariableMap,
    model: &'a Model,
    assets: &I,
    markets_to_balance: &'a [(CommodityID, RegionID)],
    year: u32,
) -> CommodityBalanceKeys
where
    I: Iterator<Item = &'a AssetRef> + Clone + 'a,
{
    // Row offset in problem. This line **must** come before we add more constraints.
    // It denotes the index in the solver's row ordering that corresponds to the first
    // commodity-balance row added below and is used later to slice the duals array.
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
            if !variables.unmet_demand_var_idx.is_empty() {
                for (time_slice, _) in ts_selection.iter(&model.time_slice_info) {
                    let var = variables.get_unmet_demand_var(commodity_id, region_id, time_slice);
                    terms.push((var, 1.0));
                }
            }

            // For SED commodities, the LHS must be >=0 and for SVD commodities, it must be >=
            // the exogenous demand supplied by the user
            let min = if commodity.kind == CommodityType::ServiceDemand {
                commodity.demand[&(region_id.clone(), year, ts_selection.clone())].value()
            } else {
                0.0
            };
            // Consume collected terms into a row. `terms.drain(..)` ensures the vector is
            // emptied for the next selection.
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
/// Returns an `ActivityKeys` where `offset` is the row index of the first
/// activity constraint added and `keys` enumerates the `(asset, time_selection)`
/// entries in the same row order. Note that for flexible-capacity assets two rows
/// (upper and lower bounds) are added per selection; in that case the same key is
/// stored twice to match the solver ordering.
///
/// [1]: https://energysystemsmodellinglab.github.io/MUSE2/model/dispatch_optimisation.html#a4-constraints-capacity--availability-for-standard-assets--a-in-mathbfastd-
fn add_activity_constraints<'a, I>(
    problem: &mut Problem,
    variables: &VariableMap,
    time_slice_info: &TimeSliceInfo,
    assets: I,
) -> ActivityKeys
where
    I: Iterator<Item = &'a AssetRef> + 'a,
{
    // Row offset in problem. This line **must** come before we add more constraints.
    // It denotes the index into the solver's row ordering for the first activity constraint
    // added below and is used when mapping duals back to assets/time selections.
    let offset = problem.num_rows();

    let mut keys = Vec::new();
    let capacity_vars: IndexMap<&AssetRef, highs::Col> = variables.iter_capacity_vars().collect();

    // Create constraints for each asset
    for asset in assets {
        if let Some(&capacity_var) = capacity_vars.get(asset) {
            // Asset with flexible capacity
            for (ts_selection, limits) in asset.iter_activity_per_capacity_limits() {
                let mut upper_limit = limits.end().value();
                let mut lower_limit = limits.start().value();

                // If the asset is divisible, the capacity variable represents number of units,
                // so we need to multiply the per-capacity limits by the unit size.
                if let Some(unit_size) = asset.unit_size() {
                    upper_limit *= unit_size.value();
                    lower_limit *= unit_size.value();
                }

                // Collect capacity and activity terms
                // We have a single capacity term, and activity terms for all time slices in the selection
                let mut terms_upper = vec![(capacity_var, -upper_limit)];
                let mut terms_lower = vec![(capacity_var, -lower_limit)];
                for (time_slice, _) in ts_selection.iter(time_slice_info) {
                    let var = variables.get_activity_var(asset, time_slice);
                    terms_upper.push((var, 1.0));
                    terms_lower.push((var, 1.0));
                }

                // Upper bound: sum(activity) - (capacity * upper_limit_per_capacity) ≤ 0
                problem.add_row(..=0.0, &terms_upper);

                // Lower bound: sum(activity) - (capacity * lower_limit_per_capacity) ≥ 0
                problem.add_row(0.0.., &terms_lower);

                // Store keys for retrieving duals later.
                // TODO: a bit of a hack pushing identical keys twice. Safe for now so long as we don't
                // use the activity duals for anything important when using flexible capacity assets.
                keys.push((asset.clone(), ts_selection.clone()));
                keys.push((asset.clone(), ts_selection.clone()));
            }
        } else {
            // Fixed-capacity asset: simple absolute activity limits.
            for (ts_selection, limits) in asset.iter_activity_limits() {
                let limits = limits.start().value()..=limits.end().value();

                // Collect activity terms for the time slices in this selection
                let terms = ts_selection
                    .iter(time_slice_info)
                    .map(|(time_slice, _)| (variables.get_activity_var(asset, time_slice), 1.0))
                    .collect::<Vec<_>>();

                // Constraint: sum of activities in selection within limits
                problem.add_row(limits, &terms);

                // Store keys for retrieving duals later.
                keys.push((asset.clone(), ts_selection.clone()));
            }
        }
    }

    ActivityKeys { offset, keys }
}
