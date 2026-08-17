//! Code for adding constraints to the dispatch optimisation problem.
use super::VariableMap;
use crate::asset::{AssetCapacity, AssetIterator, AssetRef};
use crate::commodity::{CommodityID, CommodityType};
use crate::model::Model;
use crate::region::RegionID;
use crate::time_slice::{Season, TimeSliceInfo, TimeSliceSelection};
use crate::units::{Flow, MoneyPerCapacityPerYear, UnitType, Year};
use highs::RowProblem as Problem;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

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

/// Map containing the seasonal peak variables for each (asset, season) pair
type SeasonalPeakVariableMap = IndexMap<(AssetRef, Season), highs::Col>;

/// Map containing the annual peak variables for each asset
type AnnualPeakVariableMap = IndexMap<AssetRef, highs::Col>;

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
    candidate_assets: &'a [AssetRef],
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
        candidate_assets,
    );

    let activity_keys =
        add_activity_constraints(problem, variables, &model.time_slice_info, assets.clone());

    add_utilisation_peak_constraints(problem, model, assets.clone(), variables);

    add_equal_utilisation_constraints(problem, variables, &model.time_slice_info, assets.clone());

    // Return constraint keys
    ConstraintKeys {
        commodity_balance_keys,
        activity_keys,
    }
}

/// Add seasonal and annual utilisation peak constraints to the problem.
fn add_utilisation_peak_constraints<'a, I>(
    problem: &mut Problem,
    model: &Model,
    assets: I,
    variables: &VariableMap,
) where
    I: Iterator<Item = &'a AssetRef> + Clone,
{
    let has_seasonal_penalty =
        model.parameters.seasonal_utilisation_penalty > MoneyPerCapacityPerYear(0.0);
    let has_annual_penalty =
        model.parameters.annual_utilisation_penalty > MoneyPerCapacityPerYear(0.0);

    // If neither penalties are applied, we don't need to add any variables and constraints
    if !has_seasonal_penalty && !has_annual_penalty {
        return;
    }

    // So long as either penalty is applied, we need to add seasonal peak variables and constraints
    let seasonal_peak_vars = add_seasonal_peak_variables(problem, model, assets.clone());
    add_seasonal_peak_constraints(
        problem,
        variables,
        &model.time_slice_info,
        &seasonal_peak_vars,
    );

    // If the annual penalty is applied, we also need to add annual peak variables and constraints
    if has_annual_penalty {
        let annual_peak_vars = add_annual_peak_variables(problem, model, assets);
        add_annual_peak_constraints(
            problem,
            &model.time_slice_info,
            &annual_peak_vars,
            &seasonal_peak_vars,
        );
    }
}

/// Add seasonal peak variables to the problem for each (asset, season) pair.
fn add_seasonal_peak_variables<'a, I>(
    problem: &mut Problem,
    model: &Model,
    assets: I,
) -> SeasonalPeakVariableMap
where
    I: Iterator<Item = &'a AssetRef>,
{
    let mut seasonal_peak_vars = SeasonalPeakVariableMap::new();
    for asset in assets {
        for (season, duration) in &model.time_slice_info.seasons {
            // Scale penalty by season duration
            let col_factor = model.parameters.seasonal_utilisation_penalty * *duration;
            let variable = problem.add_column(col_factor.value(), 0.0..);
            seasonal_peak_vars.insert((asset.clone(), season.clone()), variable);
        }
    }
    seasonal_peak_vars
}

/// Add annual peak variables to the problem for each asset.
fn add_annual_peak_variables<'a, I>(
    problem: &mut Problem,
    model: &Model,
    assets: I,
) -> AnnualPeakVariableMap
where
    I: Iterator<Item = &'a AssetRef>,
{
    // Penalty is applied over the whole year, so scale by 1 year
    let col_factor = model.parameters.annual_utilisation_penalty * Year(1.0);
    assets
        .map(|asset| {
            let variable = problem.add_column(col_factor.value(), 0.0..);
            (asset.clone(), variable)
        })
        .collect()
}

/// Add constraints linking seasonal peak variables to activity variables for each (asset, season) pair.
fn add_seasonal_peak_constraints(
    problem: &mut Problem,
    variables: &VariableMap,
    time_slice_info: &TimeSliceInfo,
    seasonal_peak_vars: &SeasonalPeakVariableMap,
) {
    for ((asset, season), &peak_variable) in seasonal_peak_vars {
        let activity_per_capacity = asset.process().capacity_to_activity;
        let season_selection = TimeSliceSelection::Season(season.clone());
        for (time_slice, ts_length) in season_selection.iter(time_slice_info) {
            let time_slice_fraction = ts_length / Year(1.0);
            let activity_per_capacity_in_time_slice = activity_per_capacity * time_slice_fraction;
            let capacity_required_per_activity = 1.0 / activity_per_capacity_in_time_slice.value();

            // One unit of capacity supports `activity_per_capacity_in_time_slice` activity in
            // this time slice. The peak variable therefore measures the capacity required by
            // the activity in the time slice.
            problem.add_row(
                0.0..,
                [
                    (peak_variable, 1.0),
                    (
                        variables.get_activity_var(asset, time_slice),
                        -capacity_required_per_activity,
                    ),
                ],
            );
        }
    }
}

/// Add constraints linking seasonal peak variables to annual peak variables for each asset.
fn add_annual_peak_constraints(
    problem: &mut Problem,
    time_slice_info: &TimeSliceInfo,
    annual_peak_vars: &AnnualPeakVariableMap,
    seasonal_peak_vars: &SeasonalPeakVariableMap,
) {
    for (asset, &annual_peak_variable) in annual_peak_vars {
        for season in time_slice_info.seasons.keys() {
            let seasonal_peak_variable = seasonal_peak_vars
                .get(&(asset.clone(), season.clone()))
                .expect("Missing seasonal peak variable for annual peak constraint");
            problem.add_row(
                0.0..,
                [(annual_peak_variable, 1.0), (*seasonal_peak_variable, -1.0)],
            );
        }
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
#[doc = concat!("[1]: ", crate::docs_url!("model/dispatch_optimisation.html#commodity-balance-constraints"))]
fn add_commodity_balance_constraints<'a, I>(
    problem: &mut Problem,
    variables: &VariableMap,
    model: &'a Model,
    assets: &I,
    markets_to_balance: &'a [(CommodityID, RegionID)],
    year: u32,
    candidate_assets: &'a [AssetRef],
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

            // Add a small epsilon to the lower bound to force some dispatch by candidate assets,
            // ensuring they receive a nonzero shadow price.
            let epsilon = candidate_balance_epsilon(
                candidate_assets,
                region_id,
                commodity_id,
                &ts_selection,
                model.parameters.commodity_balance_epsilon,
            );

            // For SVD commodities, the lower bound is the exogenous demand (or epsilon if larger).
            // For SED commodities, the lower bound is just epsilon.
            let min = match commodity.kind {
                CommodityType::ServiceDemand => {
                    commodity.demand[&(region_id.clone(), year, ts_selection.clone())].max(epsilon)
                }
                _ => epsilon,
            };

            // Consume collected terms into a row. `terms.drain(..)` ensures the vector is
            // emptied for the next selection.
            problem.add_row(min.value().., terms.drain(..));
            keys.push((
                commodity_id.clone(),
                region_id.clone(),
                ts_selection.clone(),
            ));
        }
    }

    CommodityBalanceKeys { offset, keys }
}

/// Calculate the epsilon to add to the lower bound of a commodity balance constraint, to force
/// some dispatch by candidate assets so they receive a nonzero shadow price.
///
/// Returns `epsilon` if the total maximum output from candidate assets in `region_id` for
/// `commodity_id` in `ts_selection` exceeds `epsilon`, otherwise returns zero (to avoid making
/// the balance constraint infeasible).
fn candidate_balance_epsilon(
    candidate_assets: &[AssetRef],
    region_id: &RegionID,
    commodity_id: &CommodityID,
    ts_selection: &TimeSliceSelection,
    epsilon: Flow,
) -> Flow {
    let max_candidate_output: Flow = candidate_assets
        .iter()
        .filter_region(region_id)
        .flat_map(|a| {
            let max_activity = *a.get_activity_limits_for_selection(ts_selection).end();
            a.iter_output_flows()
                .filter(|flow| &flow.commodity.id == commodity_id)
                .map(move |flow| flow.coeff * max_activity)
        })
        .sum();
    if max_candidate_output > epsilon {
        epsilon
    } else {
        Flow(0.0)
    }
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
#[doc = concat!("[1]: ", crate::docs_url!("model/dispatch_optimisation.html#asset-activity-limits"))]
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

                // If the asset capacity is discrete, the capacity variable represents number of
                // units, so we need to multiply the per-capacity limits by the unit size.
                if let AssetCapacity::Discrete(_, unit_size) = asset.capacity() {
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

/// Groups assets that have equivalent dispatch properties.
///
/// Assets are first bucketed by `dispatch_equivalence_hash()` to avoid unnecessary pairwise
/// comparisons. Within each hash bucket, assets are compared using `is_dispatch_equivalent()`,
/// which is the authoritative check for equivalence. This also handles hash collisions correctly.
///
/// The caller must ensure that `assets` contains only assets eligible for equal-utilisation
/// constraints (i.e. flexible-capacity assets have already been filtered out).
fn group_dispatch_equivalent_assets<'a, I>(assets: I) -> Vec<Vec<&'a AssetRef>>
where
    I: Iterator<Item = &'a AssetRef>,
{
    // Group assets by comparing each one with the first asset in each group. The hash index
    // avoids comparing an asset with groups that cannot contain an equivalent asset, while the
    // exact comparison preserves correctness in the event of hash collisions.
    let mut asset_groups: Vec<Vec<&AssetRef>> = Vec::new();
    let mut group_indices: HashMap<u64, Vec<usize>> = HashMap::new();
    for asset in assets {
        let hash = asset.dispatch_equivalence_hash();

        // Only groups with the same hash can possibly match.
        let candidate_groups = group_indices.get(&hash);

        // Find a group whose representative is actually equivalent.
        let matching_group = candidate_groups
            .into_iter()
            .flatten()
            .find(|&&group_index| asset_groups[group_index][0].is_dispatch_equivalent(asset));

        if let Some(group_index) = matching_group {
            asset_groups[*group_index].push(asset);
        } else {
            let group_index = asset_groups.len();
            asset_groups.push(vec![asset]);
            group_indices.entry(hash).or_default().push(group_index);
        }
    }

    asset_groups
}

/// Add constraints requiring dispatch-equivalent assets to have equal utilisation in each time
/// slice.
///
/// Flexible-capacity assets are excluded because their maximum activity depends on a decision
/// variable. The constraints added here are not included in [`ConstraintKeys`], as their duals
/// are not currently used.
fn add_equal_utilisation_constraints<'a, I>(
    problem: &mut Problem,
    variables: &VariableMap,
    time_slice_info: &TimeSliceInfo,
    assets: I,
) where
    I: Iterator<Item = &'a AssetRef> + 'a,
{
    // Identify flexible-capacity assets so we can exclude them from the constraints
    let flexible_assets: HashSet<_> = variables
        .iter_capacity_vars()
        .map(|(asset, _)| asset)
        .collect();

    let asset_groups =
        group_dispatch_equivalent_assets(assets.filter(|asset| !flexible_assets.contains(asset)));

    // For each group of assets, add constraints to force equal utilisation in each time slice
    // This is done by anchoring each asset to the first asset in the group (-> (n-1) constraints
    // for a group of n assets)
    for assets in asset_groups {
        let Some((reference_asset, others)) = assets.split_first() else {
            continue;
        };

        let reference_max = reference_asset.max_activity().value();

        for asset in others {
            let asset_max = asset.max_activity().value();

            for time_slice in time_slice_info.iter_ids() {
                // Constraint: (act_a * max_b) - (act_b * max_a) = 0
                problem.add_row(
                    0.0..=0.0,
                    [
                        (variables.get_activity_var(asset, time_slice), reference_max),
                        (
                            variables.get_activity_var(reference_asset, time_slice),
                            -asset_max,
                        ),
                    ],
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::Asset;
    use crate::commodity::Commodity;
    use crate::fixture::{asset, process, process_flows_map, svd_commodity};
    use crate::process::Process;
    use crate::process::{FlowType, ProcessFlow};
    use crate::time_slice::TimeSliceSelection;
    use crate::units::{Capacity, FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use rstest::rstest;
    use std::sync::Arc;

    #[rstest]
    // Max candidate output (2.0) < epsilon (10.0) → zero (guard prevents infeasibility)
    #[case(10.0, 0.0)]
    // Max candidate output (2.0) > epsilon (1.0) → epsilon returned
    #[case(1.0, 1.0)]
    fn candidate_balance_epsilon_works(
        #[case] epsilon: f64,
        #[case] expected: f64,
        svd_commodity: Commodity,
        mut process: Process,
    ) {
        let commodity_rc = Arc::new(svd_commodity);

        // Add an output flow for the commodity to the process. With capacity 2.0, cap2act 1.0,
        // and full availability over a single annual time slice, max_candidate_output = 2.0.
        let flow = ProcessFlow {
            commodity: Arc::clone(&commodity_rc),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        process.flows = process_flows_map(
            process.regions.clone(),
            Arc::new(indexmap! { commodity_rc.id.clone() => flow }),
        );

        let result = candidate_balance_epsilon(
            &[AssetRef::from(asset(process))],
            &"GBR".into(),
            &commodity_rc.id,
            &TimeSliceSelection::Annual,
            Flow(epsilon),
        );
        assert_eq!(result, Flow(expected));
    }

    #[test]
    fn groups_no_assets() {
        assert!(group_dispatch_equivalent_assets(std::iter::empty()).is_empty());
    }

    #[rstest]
    fn groups_equivalent_assets(asset: Asset) {
        let mut equivalent = asset.clone();
        equivalent.set_capacity(AssetCapacity::Continuous(Capacity(3.0)));
        let assets = [AssetRef::from(asset), AssetRef::from(equivalent)];

        let groups = group_dispatch_equivalent_assets(assets.iter());

        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].len(), 2);
    }

    #[rstest]
    fn groups_equivalent_assets_separately_from_non_equivalent_assets(
        asset: Asset,
        mut process: Process,
    ) {
        let mut equivalent = asset.clone();
        equivalent.set_capacity(AssetCapacity::Continuous(Capacity(3.0)));
        Arc::make_mut(process.parameters.get_mut(&("GBR".into(), 2015)).unwrap())
            .variable_operating_cost = crate::units::MoneyPerActivity(1.0);
        let different = Asset::new_ready(
            "agent1".into(),
            Arc::new(process),
            "GBR".into(),
            Capacity(2.0),
            2015,
        )
        .unwrap();
        let assets = [
            AssetRef::from(asset),
            AssetRef::from(equivalent),
            AssetRef::from(different),
        ];

        let groups = group_dispatch_equivalent_assets(assets.iter());

        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].len(), 2);
        assert_eq!(groups[1].len(), 1);
    }
}
