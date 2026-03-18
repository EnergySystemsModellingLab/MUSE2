//! Code for calculating commodity prices used by the simulation.
use crate::asset::AssetRef;
use crate::commodity::{CommodityID, CommodityMap, PricingStrategy};
use crate::input::try_insert;
use crate::model::Model;
use crate::region::RegionID;
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceSelection};
use crate::units::{Activity, Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
use anyhow::Result;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

/// Weighted average accumulator for `MoneyPerFlow` prices.
#[derive(Clone, Copy, Debug)]
struct WeightedAverageAccumulator {
    /// The numerator of the weighted average, i.e. the sum of value * weight across all entries.
    numerator: MoneyPerFlow,
    /// The denominator of the weighted average, i.e. the sum of weights across all entries.
    denominator: Dimensionless,
}

impl Default for WeightedAverageAccumulator {
    fn default() -> Self {
        Self {
            numerator: MoneyPerFlow(0.0),
            denominator: Dimensionless(0.0),
        }
    }
}

impl WeightedAverageAccumulator {
    /// Add a weighted value to the accumulator.
    fn add(&mut self, value: MoneyPerFlow, weight: Dimensionless) {
        self.numerator += value * weight;
        self.denominator += weight;
    }

    /// Solve the weighted average.
    ///
    /// Returns `None` if the denominator is zero (or close to zero)
    fn finalise(self) -> Option<MoneyPerFlow> {
        (self.denominator > Dimensionless::EPSILON).then(|| self.numerator / self.denominator)
    }
}

/// Weighted average accumulator with a backup weighting path for `MoneyPerFlow` prices.
#[derive(Clone, Copy, Debug, Default)]
struct WeightedAverageBackupAccumulator {
    /// Primary weighted average path.
    primary: WeightedAverageAccumulator,
    /// Backup weighted average path.
    backup: WeightedAverageAccumulator,
}

impl WeightedAverageBackupAccumulator {
    /// Add a weighted value to the accumulator with a backup weight.
    fn add(&mut self, value: MoneyPerFlow, weight: Dimensionless, backup_weight: Dimensionless) {
        self.primary.add(value, weight);
        self.backup.add(value, backup_weight);
    }

    /// Solve the weighted average, falling back to backup weights if needed.
    ///
    /// Returns `None` if both denominators are zero (or close to zero).
    fn finalise(self) -> Option<MoneyPerFlow> {
        self.primary.finalise().or_else(|| self.backup.finalise())
    }
}

/// Calculate commodity prices.
///
/// Calculate prices for each commodity/region/time-slice according to the commodity's configured
/// `PricingStrategy`.
///
/// # Arguments
///
/// * `model` - The model
/// * `solution` - Solution to dispatch optimisation
/// * `year` - The year for which prices are being calculated
///
/// # Returns
///
/// A `CommodityPrices` mapping `(commodity, region, time_slice)` to `MoneyPerFlow` representing
/// endogenous prices computed from the optimisation solution.
pub fn calculate_prices(model: &Model, solution: &Solution, year: u32) -> Result<CommodityPrices> {
    // Collect shadow prices for all SED/SVD commodities
    let shadow_prices = CommodityPrices::from_iter(solution.iter_commodity_balance_duals());

    // Set up empty prices map
    let mut result = CommodityPrices::default();

    // Lazily computed only if at least one FullCost market is encountered.
    let mut annual_activities: Option<HashMap<AssetRef, Activity>> = None;

    // Get investment order for the year - prices will be calculated in the reverse of this order
    let investment_order = &model.investment_order[&year];

    // Iterate over investment sets in reverse order. Markets within the same set can be priced
    // simultaneously, since they are independent (apart from Cycle sets when using cost-based
    // pricing strategies, which get flagged at the validation stage).
    for investment_set in investment_order.iter().rev() {
        // Partition markets by pricing strategy into a map keyed by `PricingStrategy`.
        // For now, commodities use a single strategy for all regions, but this may change in the future.
        let mut pricing_sets = HashMap::new();
        for (commodity_id, region_id) in investment_set.iter_markets() {
            let commodity = &model.commodities[commodity_id];
            if commodity.pricing_strategy == PricingStrategy::Unpriced {
                continue;
            }
            pricing_sets
                .entry(&commodity.pricing_strategy)
                .or_insert_with(HashSet::new)
                .insert((commodity_id.clone(), region_id.clone()));
        }

        // Add prices for shadow-priced commodities
        if let Some(shadow_set) = pricing_sets.get(&PricingStrategy::Shadow) {
            for (commodity_id, region_id, time_slice) in shadow_prices.keys() {
                if shadow_set.contains(&(commodity_id.clone(), region_id.clone())) {
                    let price = shadow_prices
                        .get(commodity_id, region_id, time_slice)
                        .unwrap();
                    result.insert(commodity_id, region_id, time_slice, price);
                }
            }
        }

        // Add prices for scarcity-adjusted commodities
        if let Some(scarcity_set) = pricing_sets.get(&PricingStrategy::ScarcityAdjusted) {
            add_scarcity_adjusted_prices(
                solution.iter_activity_duals(),
                &shadow_prices,
                &mut result,
                scarcity_set,
            );
        }

        // Add prices for marginal cost commodities
        if let Some(marginal_set) = pricing_sets.get(&PricingStrategy::MarginalCost) {
            add_marginal_cost_prices(
                solution.iter_activity_for_existing(),
                solution.iter_activity_keys_for_candidates(),
                &mut result,
                year,
                marginal_set,
                &model.commodities,
                &model.time_slice_info,
            );
        }

        // Add prices for marginal average commodities
        if let Some(marginal_avg_set) = pricing_sets.get(&PricingStrategy::MarginalCostAverage) {
            add_marginal_cost_average_prices(
                solution.iter_activity_for_existing(),
                solution.iter_activity_keys_for_candidates(),
                &mut result,
                year,
                marginal_avg_set,
                &model.commodities,
                &model.time_slice_info,
            );
        }

        // Add prices for full cost commodities
        if let Some(fullcost_set) = pricing_sets.get(&PricingStrategy::FullCost) {
            let annual_activities = annual_activities.get_or_insert_with(|| {
                calculate_annual_activities(solution.iter_activity_for_existing())
            });
            add_full_cost_prices(
                solution.iter_activity_for_existing(),
                solution.iter_activity_keys_for_candidates(),
                annual_activities,
                &mut result,
                year,
                fullcost_set,
                &model.commodities,
                &model.time_slice_info,
            );
        }

        // Add prices for full average commodities
        if let Some(full_avg_set) = pricing_sets.get(&PricingStrategy::FullCostAverage) {
            let annual_activities = annual_activities.get_or_insert_with(|| {
                calculate_annual_activities(solution.iter_activity_for_existing())
            });
            add_full_cost_average_prices(
                solution.iter_activity_for_existing(),
                solution.iter_activity_keys_for_candidates(),
                annual_activities,
                &mut result,
                year,
                full_avg_set,
                &model.commodities,
                &model.time_slice_info,
            );
        }
    }

    // Return the completed prices map
    Ok(result)
}

/// A map relating commodity ID + region + time slice to current price (endogenous)
#[derive(Default, Clone)]
pub struct CommodityPrices(IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>);

impl CommodityPrices {
    /// Insert a price for the given commodity, region and time slice.
    ///
    /// Panics if a price for the given key already exists.
    pub fn insert(
        &mut self,
        commodity_id: &CommodityID,
        region_id: &RegionID,
        time_slice: &TimeSliceID,
        price: MoneyPerFlow,
    ) {
        let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
        try_insert(&mut self.0, &key, price).unwrap();
    }

    /// Extend the prices map, panic if any key already exists
    pub fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = ((CommodityID, RegionID, TimeSliceID), MoneyPerFlow)>,
    {
        for (key, price) in iter {
            try_insert(&mut self.0, &key, price).unwrap();
        }
    }

    /// Extend this map by applying each selection-level price to all time slices
    /// contained in that selection.
    ///
    /// Panics if any individual commodity/region/time slice key already exists in the map.
    fn extend_selection_prices(
        &mut self,
        group_prices: &IndexMap<(CommodityID, RegionID, TimeSliceSelection), MoneyPerFlow>,
        time_slice_info: &TimeSliceInfo,
    ) {
        for ((commodity_id, region_id, selection), &selection_price) in group_prices {
            for (time_slice_id, _) in selection.iter(time_slice_info) {
                self.insert(commodity_id, region_id, time_slice_id, selection_price);
            }
        }
    }

    /// Iterate over the map.
    ///
    /// # Returns
    ///
    /// An iterator of tuples containing commodity ID, region ID, time slice and price.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&CommodityID, &RegionID, &TimeSliceID, MoneyPerFlow)> {
        self.0
            .iter()
            .map(|((commodity_id, region_id, ts), price)| (commodity_id, region_id, ts, *price))
    }

    /// Get the price for the specified commodity for a given region and time slice
    pub fn get(
        &self,
        commodity_id: &CommodityID,
        region_id: &RegionID,
        time_slice: &TimeSliceID,
    ) -> Option<MoneyPerFlow> {
        self.0
            .get(&(commodity_id.clone(), region_id.clone(), time_slice.clone()))
            .copied()
    }

    /// Iterate over the price map's keys
    pub fn keys(
        &self,
    ) -> indexmap::map::Keys<'_, (CommodityID, RegionID, TimeSliceID), MoneyPerFlow> {
        self.0.keys()
    }

    /// Calculate time slice-weighted average prices for each commodity-region pair
    ///
    /// This method aggregates prices across time slices by weighting each price
    /// by the duration of its time slice, providing a more representative annual average.
    ///
    /// Note: this assumes that all time slices are present for each commodity-region pair, and
    /// that all time slice lengths sum to 1. This is not checked by this method.
    fn time_slice_weighted_averages(
        &self,
        time_slice_info: &TimeSliceInfo,
    ) -> HashMap<(CommodityID, RegionID), MoneyPerFlow> {
        let mut weighted_prices = HashMap::new();

        for ((commodity_id, region_id, time_slice_id), price) in &self.0 {
            // NB: Time slice fractions will sum to one
            let weight = time_slice_info.time_slices[time_slice_id] / Year(1.0);
            let key = (commodity_id.clone(), region_id.clone());
            weighted_prices
                .entry(key)
                .and_modify(|v| *v += *price * weight)
                .or_insert_with(|| *price * weight);
        }

        weighted_prices
    }

    /// Check if time slice-weighted average prices are within relative tolerance of another price
    /// set.
    ///
    /// This method calculates time slice-weighted average prices for each commodity-region pair
    /// and compares them. Both objects must have exactly the same set of commodity-region pairs,
    /// otherwise it will panic.
    ///
    /// Additionally, this method assumes that all time slices are present for each commodity-region
    /// pair, and that all time slice lengths sum to 1. This is not checked by this method.
    pub fn within_tolerance_weighted(
        &self,
        other: &Self,
        tolerance: Dimensionless,
        time_slice_info: &TimeSliceInfo,
    ) -> bool {
        let self_averages = self.time_slice_weighted_averages(time_slice_info);
        let other_averages = other.time_slice_weighted_averages(time_slice_info);

        for (key, &price) in &self_averages {
            let other_price = other_averages[key];
            let abs_diff = (price - other_price).abs();

            // Special case: last price was zero
            if price == MoneyPerFlow(0.0) {
                // Current price is zero but other price is nonzero
                if other_price != MoneyPerFlow(0.0) {
                    return false;
                }
            // Check if price is within tolerance
            } else if abs_diff / price.abs() > tolerance {
                return false;
            }
        }
        true
    }
}

impl<'a> FromIterator<(&'a CommodityID, &'a RegionID, &'a TimeSliceID, MoneyPerFlow)>
    for CommodityPrices
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (&'a CommodityID, &'a RegionID, &'a TimeSliceID, MoneyPerFlow)>,
    {
        let map = iter
            .into_iter()
            .map(|(commodity_id, region_id, time_slice, price)| {
                (
                    (commodity_id.clone(), region_id.clone(), time_slice.clone()),
                    price,
                )
            })
            .collect();
        CommodityPrices(map)
    }
}

impl IntoIterator for CommodityPrices {
    type Item = ((CommodityID, RegionID, TimeSliceID), MoneyPerFlow);
    type IntoIter = indexmap::map::IntoIter<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Calculate scarcity-adjusted prices for a set of commodities and add to an existing prices map.
///
/// # Arguments
///
/// * `activity_duals` - Iterator over activity duals from optimisation solution
/// * `shadow_prices` - Shadow prices for all commodities
/// * `existing_prices` - Existing prices map to extend with scarcity-adjusted prices
/// * `markets_to_price` - Set of markets to calculate scarcity-adjusted prices for
fn add_scarcity_adjusted_prices<'a, I>(
    activity_duals: I,
    shadow_prices: &CommodityPrices,
    existing_prices: &mut CommodityPrices,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
) where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
{
    // Calculate highest activity dual for each commodity/region/time slice
    let mut highest_duals = IndexMap::new();
    for (asset, time_slice, dual) in activity_duals {
        let region_id = asset.region_id();

        // Iterate over the output flows of this asset
        // Only consider flows for commodities we are pricing
        for flow in asset.iter_output_flows().filter(|flow| {
            markets_to_price.contains(&(flow.commodity.id.clone(), region_id.clone()))
        }) {
            // Update the highest dual for this commodity/time slice
            highest_duals
                .entry((
                    flow.commodity.id.clone(),
                    region_id.clone(),
                    time_slice.clone(),
                ))
                .and_modify(|current_dual| {
                    if dual > *current_dual {
                        *current_dual = dual;
                    }
                })
                .or_insert(dual);
        }
    }

    // Add this to the shadow price for each commodity/region/time slice and insert into the map
    for ((commodity, region, time_slice), highest_dual) in &highest_duals {
        // There should always be a shadow price for commodities we are considering here, so it
        // should be safe to unwrap
        let shadow_price = shadow_prices.get(commodity, region, time_slice).unwrap();
        // highest_dual is in units of MoneyPerActivity, and shadow_price is in MoneyPerFlow, but
        // this is correct according to Adam
        let scarcity_price = shadow_price + MoneyPerFlow(highest_dual.value());
        existing_prices.insert(commodity, region, time_slice, scarcity_price);
    }
}

/// Calculate marginal cost prices for a set of commodities and add to an existing prices map.
///
/// This pricing strategy aims to incorporate the marginal cost of commodity production into the price.
///
/// For a given asset, a marginal cost can be calculated for each SED/SVD output, which is the sum of:
/// - Generic activity costs: Activity-related costs not tied to a specific SED/SVD output
///   (variable operating costs, cost of purchasing inputs, plus all levies and flow costs not
///   associated with specific SED/SVD outputs). These are shared over all SED/SVD
///   outputs according to their flow coefficients.
/// - Commodity-specific activity costs: flow costs/levies for the specific SED/SVD output.
///
/// ---
///
/// For example, consider an asset A(SED) -> B(SED) + 2C(SED) + D(OTH), with the following costs:
/// - Variable operating cost: 5 per unit activity
/// - Production levy on C: 3 per unit flow
/// - Production levy on D: 4 per unit flow
/// - Shadow price of A: 1 per unit flow
///
/// Then:
/// - Generic activity cost per activity = (1 + 5 + 4) = 10
/// - Generic activity cost per SED/SVD output = 10 / (1 + 2) = 3.333
/// - Marginal cost of B = 3.333
/// - Marginal cost of C = 3.333 + 3 = 6.333
///
/// ---
///
/// For each region, the price in each time slice is taken from the installed asset with the highest
/// marginal cost. If there are no producers of the commodity in that region (in particular, this
/// may occur when there's no demand for the commodity), then candidate assets are considered: we
/// take the price from the candidate asset with the _lowest_ marginal cost, assuming full
/// utilisation (i.e. the single candidate asset that would be most competitive if a small amount of
/// demand was added).
///
/// For commodities with seasonal/annual time slice levels, marginal costs are weighted by
/// activity (or the maximum potential activity for candidates) to get a time slice-weighted average
/// marginal cost for each asset, before taking the max across assets. Consequently, the price of
/// these commodities is flat within each season/year.
///
/// # Arguments
///
/// * `activity_for_existing` - Iterator over `(asset, time_slice, activity)` from optimisation
///   solution for existing assets
/// * `activity_keys_for_candidates` - Iterator over `(asset, time_slice)` for candidate assets
/// * `existing_prices` - Existing prices to use as inputs and extend. This is expected to include
///   prices from all markets upstream of the markets we are calculating for.
/// * `year` - The year for which prices are being calculated
/// * `markets_to_price` - Set of markets to calculate marginal prices for
/// * `commodities` - Map of all commodities (used to look up each commodity's `time_slice_level`)
/// * `time_slice_info` - Time slice information (used to expand groups to individual time slices)
fn add_marginal_cost_prices<'a, I, J>(
    activity_for_existing: I,
    activity_keys_for_candidates: J,
    existing_prices: &mut CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID)>,
{
    // Accumulator map to collect marginal costs from existing assets. For each (commodity, region,
    // ts selection), this maps each asset to a weighted average of the marginal costs for that
    // commodity across all time slices in the selection, weighted by activity (using activity
    // limits as a backup weight if there is zero activity across the selection). The granularity of
    // the selection depends on the time slice level of the commodity (i.e. individual, season, year).
    let mut existing_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        IndexMap<AssetRef, WeightedAverageBackupAccumulator>,
    > = IndexMap::new();

    // Iterate over existing assets and their activities
    for (asset, time_slice, activity) in activity_for_existing {
        let region_id = asset.region_id();

        // Get activity limits: used as a backup weight if no activity across the group
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let ts_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Accumulate marginal cost for this asset, weighted by activity (using the activity
            // limit as a backup weight)
            existing_accum
                .entry((commodity_id.clone(), region_id.clone(), ts_selection))
                .or_default()
                .entry(asset.clone())
                .or_default()
                .add(
                    marginal_cost,
                    Dimensionless(activity.value()),
                    Dimensionless(activity_limit.value()),
                );
        }
    }

    // For each group, finalise per-asset weighted averages then take the max across assets
    let group_prices: IndexMap<_, MoneyPerFlow> = existing_accum
        .into_iter()
        .filter_map(|(key, per_asset)| {
            per_asset
                .into_values()
                .filter_map(WeightedAverageBackupAccumulator::finalise)
                .reduce(|current, value| current.max(value))
                .map(|v| (key, v))
        })
        .collect();

    // Accumulator map to collect marginal costs from candidate assets. Similar to existing_accum,
    // but costs are weighted according to activity limits (i.e. assuming full utilisation).
    let mut cand_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        IndexMap<AssetRef, WeightedAverageAccumulator>,
    > = IndexMap::new();

    // Iterate over candidate assets (assuming full utilization)
    for (asset, time_slice) in activity_keys_for_candidates {
        let region_id = asset.region_id();

        // Get activity limits: used to weight marginal costs for seasonal/annual commodities
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let ts_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Skip groups already covered by existing assets
            if group_prices.contains_key(&(
                commodity_id.clone(),
                region_id.clone(),
                ts_selection.clone(),
            )) {
                continue;
            }

            // Accumulate marginal cost for this candidate asset, weighted by the activity limit
            cand_accum
                .entry((commodity_id.clone(), region_id.clone(), ts_selection))
                .or_default()
                .entry(asset.clone())
                .or_default()
                .add(marginal_cost, Dimensionless(activity_limit.value()));
        }
    }

    // For each group, finalise per-candidate weighted averages then take the min across candidates
    let cand_group_prices = cand_accum.into_iter().filter_map(|(key, per_candidate)| {
        per_candidate
            .into_values()
            .filter_map(WeightedAverageAccumulator::finalise)
            .reduce(|current, value| current.min(value))
            .map(|v| (key, v))
    });

    // Merge existing and candidate group prices
    let mut all_group_prices = group_prices;
    all_group_prices.extend(cand_group_prices);

    // Expand selection-level prices to individual time slices and add to the main prices map
    existing_prices.extend_selection_prices(&all_group_prices, time_slice_info);
}

/// Calculate marginal cost prices for a set of commodities using a load-weighted average across
/// assets and add to an existing prices map.
///
/// Similar to `calculate_marginal_cost_prices`, but takes a weighted average across assets
/// according to output rather than taking the max.
///
/// Candidate assets are treated the same way as in `calculate_marginal_cost_prices` (i.e. take the
/// min across candidate assets).
fn add_marginal_cost_average_prices<'a, I, J>(
    activity_for_existing: I,
    activity_keys_for_candidates: J,
    existing_prices: &mut CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID)>,
{
    // Accumulator map to collect marginal costs from existing assets. Collects a weighted average
    // for each (commodity, region, ts selection), across all contributing assets, weighted
    // according to output (with a backup weight based on potential output if there is zero
    // activity across the selection). The granularity of the selection depends on the time slice
    // level of the commodity (i.e. individual, season, year).
    let mut existing_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        WeightedAverageBackupAccumulator,
    > = IndexMap::new();

    // Iterate over existing assets and their activities
    for (asset, time_slice, activity) in activity_for_existing {
        let region_id = asset.region_id();

        // Get activity limits: used to calculate backup potential-output weights.
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let time_slice_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Marginal costs will be weighted by output (activity * coefficient)
            let output_coeff = asset
                .get_flow(&commodity_id)
                .expect("Commodity should be an output flow for this asset")
                .coeff;
            let output_weight = Dimensionless((activity * output_coeff).value());
            let backup_output_weight = Dimensionless((activity_limit * output_coeff).value());

            // Accumulate marginal cost for this group, weighted by output with a backup
            // potential-output weight.
            existing_accum
                .entry((
                    commodity_id.clone(),
                    region_id.clone(),
                    time_slice_selection,
                ))
                .or_default()
                .add(marginal_cost, output_weight, backup_output_weight);
        }
    }

    // For each group, finalise weighted averages
    let group_prices: IndexMap<_, MoneyPerFlow> = existing_accum
        .into_iter()
        .filter_map(|(key, accum)| accum.finalise().map(|v| (key, v)))
        .collect();

    // Accumulator map to collect marginal costs from candidate assets. For each (commodity, region,
    // ts selection), this maps each candidate to a weighted average of the marginal costs for that
    // commodity across all time slices in the selection, weighted by activity limits.
    let mut cand_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        IndexMap<AssetRef, WeightedAverageAccumulator>,
    > = IndexMap::new();

    // Iterate over candidate assets (assuming full utilisation)
    for (asset, time_slice) in activity_keys_for_candidates {
        let region_id = asset.region_id();

        // Get activity limits: used to weight marginal costs for seasonal/annual commodities
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let time_slice_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Skip groups already covered by existing assets
            if group_prices.contains_key(&(
                commodity_id.clone(),
                region_id.clone(),
                time_slice_selection.clone(),
            )) {
                continue;
            }

            // Accumulate marginal cost for this candidate, weighted by the activity limit
            cand_accum
                .entry((
                    commodity_id.clone(),
                    region_id.clone(),
                    time_slice_selection,
                ))
                .or_default()
                .entry(asset.clone())
                .or_default()
                .add(marginal_cost, Dimensionless(activity_limit.value()));
        }
    }

    // For each group, finalise per-candidate weighted averages then reduce to the min across candidates
    let cand_group_prices = cand_accum.into_iter().filter_map(|(key, per_candidate)| {
        per_candidate
            .into_values()
            .filter_map(WeightedAverageAccumulator::finalise)
            .reduce(|current, value| current.min(value))
            .map(|v| (key, v))
    });

    // Merge existing and candidate group prices
    let mut all_group_prices = group_prices;
    all_group_prices.extend(cand_group_prices);

    // Expand selection-level prices to individual time slices and add to the main prices map
    existing_prices.extend_selection_prices(&all_group_prices, time_slice_info);
}

/// Calculate annual activities for each asset by summing across all time slices
fn calculate_annual_activities<'a, I>(activities: I) -> HashMap<AssetRef, Activity>
where
    I: IntoIterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
{
    activities
        .into_iter()
        .map(|(asset, _ts, activity)| (asset.clone(), activity))
        .fold(HashMap::new(), |mut acc, (asset, activity)| {
            acc.entry(asset)
                .and_modify(|e| *e += activity)
                .or_insert(activity);
            acc
        })
}

/// Calculate full cost prices for a set of commodities and add to an existing prices map.
///
/// This pricing strategy aims to incorporate the full cost of commodity production into the price.
///
/// For a given asset, a full cost can be calculated for each SED/SVD output, which is the sum of:
/// - Annual capital costs/fixed operating costs: Calculated based on the capacity of the asset
///   and the total annual output. If an asset has multiple SED/SVD outputs, then these costs are
///   shared equally over all outputs according to their flow coefficients.
/// - Generic activity costs: Activity-related costs not tied to a specific SED/SVD output
///   (variable operating costs, cost of purchasing inputs, plus all levies and flow costs not
///   associated with specific SED/SVD outputs). As above, these are shared over all SED/SVD
///   outputs according to their flow coefficients.
/// - Commodity-specific activity costs: flow costs/levies for the specific SED/SVD output.
///
/// ---
///
/// For example, consider an asset A(SED) -> B(SED) + 2C(SED) + D(OTH), with the following costs:
/// - Annual capital cost + fixed operating cost: 2.5 per unit capacity
/// - Variable operating cost: 5 per unit activity
/// - Production levy on C: 3 per unit flow
/// - Production levy on D: 4 per unit flow
/// - Shadow price of A: 1 per unit flow
///
/// If capacity is 4 and annual activity is 2:
/// - Annual capital + fixed operating cost per activity = (2.5 * 4) / 2 = 5
/// - Annual capital + fixed operating cost per SED/SVD output = 5 / (1 + 2) = 1.666
/// - Generic activity cost per activity = (1 + 5 + 4) = 10
/// - Generic activity cost per SED/SVD output = 10 / (1 + 2) = 3.333
/// - Full cost of B = 1.666 + 3.333 = 5.0
/// - Full cost of C = 1.666 + 3.333 + 3 = 8.0
///
/// ---
///
/// For each region, the price in each time slice is taken from the installed asset with the highest
/// full cost (excluding assets with zero annual activity, as the full cost of these as calculated
/// above would be infinite). If there are no producers of the commodity in that region (in
/// particular, this may occur when there's no demand for the commodity), then candidate assets are
/// considered: we take the price from the candidate asset with the _lowest_ full cost, assuming
/// maximum possible dispatch (i.e. the single candidate asset that would be most competitive if a
/// small amount of demand was added).
///
/// For commodities with seasonal/annual time slice levels, costs are weighted by activity (or the
/// maximum potential activity for candidates) to get a time slice-weighted average cost for each
/// asset, before taking the max across assets. Consequently, the price of these commodities is
/// flat within each season/year.
///
/// # Arguments
///
/// * `activity_for_existing` - Iterator over `(asset, time_slice, activity)` from optimisation
///   solution for existing assets
/// * `activity_keys_for_candidates` - Iterator over `(asset, time_slice)` for candidate assets
/// * `existing_prices` - Existing prices to use as inputs and extend. This is expected to include
///   prices from all markets upstream of the markets we are calculating for.
/// * `year` - The year for which prices are being calculated
/// * `markets_to_price` - Set of markets to calculate full cost prices for
/// * `commodities` - Map of all commodities (used to look up each commodity's `time_slice_level`)
/// * `time_slice_info` - Time slice information (used to expand groups to individual time slices)
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn add_full_cost_prices<'a, I, J>(
    activity_for_existing: I,
    activity_keys_for_candidates: J,
    annual_activities: &HashMap<AssetRef, Activity>,
    existing_prices: &mut CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID)>,
{
    // Accumulator map to collect full costs from existing assets. For each (commodity, region,
    // ts selection), this maps each asset to a weighted average of the full costs for that
    // commodity across all time slices in the selection, weighted by activity (using activity
    // limits as a backup weight if there is zero activity across the selection). The granularity of
    // the selection depends on the time slice level of the commodity (i.e. individual, season, year).
    let mut existing_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        IndexMap<AssetRef, WeightedAverageBackupAccumulator>,
    > = IndexMap::new();

    // Cache of annual fixed costs per flow for each asset, to avoid recalculating
    let mut annual_fixed_costs: HashMap<_, _> = HashMap::new();

    // Iterate over existing assets and their activities
    for (asset, time_slice, activity) in activity_for_existing {
        let annual_activity = annual_activities[asset];
        let region_id = asset.region_id();

        // If annual activity is zero, we can't calculate a capital cost per flow, so skip this
        // asset.
        if annual_activity < Activity::EPSILON {
            continue;
        }

        // Get activity limits: used as a backup weight if no activity across the group
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let ts_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Get/calculate fixed costs per flow for this asset
            let annual_fixed_costs_per_flow = annual_fixed_costs
                .entry(asset.clone())
                .or_insert_with(|| asset.get_annual_fixed_costs_per_flow(annual_activity));

            // Accumulate full cost for this asset, weighted by activity (using the activity limit
            // as a backup weight)
            existing_accum
                .entry((commodity_id.clone(), region_id.clone(), ts_selection))
                .or_default()
                .entry(asset.clone())
                .or_default()
                .add(
                    marginal_cost + *annual_fixed_costs_per_flow,
                    Dimensionless(activity.value()),
                    Dimensionless(activity_limit.value()),
                );
        }
    }

    // For each group, finalise per-asset weighted averages then reduce to the max across assets
    let group_prices: IndexMap<_, MoneyPerFlow> = existing_accum
        .into_iter()
        .filter_map(|(key, per_asset)| {
            per_asset
                .into_values()
                .filter_map(WeightedAverageBackupAccumulator::finalise)
                .reduce(|current, value| current.max(value))
                .map(|v| (key, v))
        })
        .collect();

    // Accumulator map to collect full costs from candidate assets. Similar to existing_accum, but
    // costs are weighted according to activity limits (i.e. assuming full utilisation).
    let mut cand_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        IndexMap<AssetRef, WeightedAverageAccumulator>,
    > = IndexMap::new();

    // Iterate over candidate assets (assuming full utilization)
    for (asset, time_slice) in activity_keys_for_candidates {
        let region_id = asset.region_id();

        // Get activity limits: used to weight marginal costs for seasonal/annual commodities
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let ts_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Skip groups already covered by existing assets
            if group_prices.contains_key(&(
                commodity_id.clone(),
                region_id.clone(),
                ts_selection.clone(),
            )) {
                continue;
            }

            // Get/calculate fixed costs per flow for this asset (assume full utilisation)
            let annual_fixed_costs_per_flow =
                annual_fixed_costs.entry(asset.clone()).or_insert_with(|| {
                    asset.get_annual_fixed_costs_per_flow(
                        *asset
                            .get_activity_limits_for_selection(&TimeSliceSelection::Annual)
                            .end(),
                    )
                });

            // Accumulate full cost for this candidate asset, weighted by the activity limit
            cand_accum
                .entry((commodity_id.clone(), region_id.clone(), ts_selection))
                .or_default()
                .entry(asset.clone())
                .or_default()
                .add(
                    marginal_cost + *annual_fixed_costs_per_flow,
                    Dimensionless(activity_limit.value()),
                );
        }
    }

    // For each group, finalise per-candidate weighted averages then reduce to the min across candidates
    let cand_group_prices = cand_accum.into_iter().filter_map(|(key, per_candidate)| {
        per_candidate
            .into_values()
            .filter_map(WeightedAverageAccumulator::finalise)
            .reduce(|current, value| current.min(value))
            .map(|v| (key, v))
    });

    // Merge existing and candidate group prices
    let mut all_group_prices = group_prices;
    all_group_prices.extend(cand_group_prices);

    // Expand selection-level prices to individual time slices and add to the main prices map
    existing_prices.extend_selection_prices(&all_group_prices, time_slice_info);
}

/// Calculate full cost prices for a set of commodities using a load-weighted average across
/// assets and add to an existing prices map.
///
/// Similar to `calculate_full_cost_prices`, but takes a weighted average across assets
/// according to output rather than taking the max.
///
/// Candidate assets are treated the same way as in `calculate_full_cost_prices` (i.e. take the min
/// across candidate assets).
#[allow(clippy::too_many_arguments, clippy::too_many_lines)]
fn add_full_cost_average_prices<'a, I, J>(
    activity_for_existing: I,
    activity_keys_for_candidates: J,
    annual_activities: &HashMap<AssetRef, Activity>,
    existing_prices: &mut CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID)>,
{
    // Accumulator map to collect full costs from existing assets. Collects a weighted average
    // for each (commodity, region, ts selection), across all contributing assets, weighted
    // according to output (with a backup weight based on potential output if there is zero
    // activity across the selection). The granularity of the selection depends on the time slice
    // level of the commodity (i.e. individual, season, year).
    let mut existing_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        WeightedAverageBackupAccumulator,
    > = IndexMap::new();

    // Cache of annual fixed costs per flow for each asset, to avoid recalculating
    let mut annual_fixed_costs: HashMap<_, _> = HashMap::new();

    // Iterate over existing assets and their activities
    for (asset, time_slice, activity) in activity_for_existing {
        let annual_activity = annual_activities[asset];
        let region_id = asset.region_id();

        // If annual activity is zero, we can't calculate a capital cost per flow, so skip this
        // asset.
        if annual_activity < Activity::EPSILON {
            continue;
        }

        // Get activity limits: used to calculate backup potential-output weights.
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let ts_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Full costs will be weighted by output (activity * coefficient)
            let output_coeff = asset
                .get_flow(&commodity_id)
                .expect("Commodity should be an output flow for this asset")
                .coeff;
            let output_weight = Dimensionless((activity * output_coeff).value());
            let backup_output_weight = Dimensionless((activity_limit * output_coeff).value());

            // Get/calculate fixed costs per flow for this asset
            let annual_fixed_costs_per_flow = annual_fixed_costs
                .entry(asset.clone())
                .or_insert_with(|| asset.get_annual_fixed_costs_per_flow(annual_activity));

            // Accumulate full costs (marginal cost + fixed cost per flow), weighted by output
            // with a backup potential-output weight.
            existing_accum
                .entry((commodity_id.clone(), region_id.clone(), ts_selection))
                .or_default()
                .add(
                    marginal_cost + *annual_fixed_costs_per_flow,
                    output_weight,
                    backup_output_weight,
                );
        }
    }

    // For each group, finalise weighted averages
    let group_prices: IndexMap<_, MoneyPerFlow> = existing_accum
        .into_iter()
        .filter_map(|(key, accum)| accum.finalise().map(|v| (key, v)))
        .collect();

    // Accumulator map to collect marginal costs from candidate assets. For each (commodity, region,
    // ts selection), this maps each candidate to a weighted average of the full costs for that
    // commodity across all time slices in the selection, weighted by activity limits.
    let mut cand_accum: IndexMap<
        (CommodityID, RegionID, TimeSliceSelection),
        IndexMap<AssetRef, WeightedAverageAccumulator>,
    > = IndexMap::new();

    // Iterate over candidate assets (assuming full utilization)
    for (asset, time_slice) in activity_keys_for_candidates {
        let region_id = asset.region_id();

        // Get activity limits: used to weight marginal costs for seasonal/annual commodities
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            existing_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Get the time slice selection according to the commodity's time slice level
            let ts_selection = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Skip groups already covered by existing assets
            if group_prices.contains_key(&(
                commodity_id.clone(),
                region_id.clone(),
                ts_selection.clone(),
            )) {
                continue;
            }

            // Get/calculate fixed costs per flow for this asset (assume full utilisation)
            let annual_fixed_costs_per_flow =
                annual_fixed_costs.entry(asset.clone()).or_insert_with(|| {
                    asset.get_annual_fixed_costs_per_flow(
                        *asset
                            .get_activity_limits_for_selection(&TimeSliceSelection::Annual)
                            .end(),
                    )
                });

            // Accumulate full costs for this group, weighted by the activity limit
            cand_accum
                .entry((commodity_id.clone(), region_id.clone(), ts_selection))
                .or_default()
                .entry(asset.clone())
                .or_default()
                .add(
                    marginal_cost + *annual_fixed_costs_per_flow,
                    Dimensionless(activity_limit.value()),
                );
        }
    }

    // For each group, finalise per-candidate weighted averages then reduce to the min across candidates
    let cand_group_prices = cand_accum.into_iter().filter_map(|(key, per_candidate)| {
        per_candidate
            .into_values()
            .filter_map(WeightedAverageAccumulator::finalise)
            .reduce(|current, value| current.min(value))
            .map(|v| (key, v))
    });

    // Merge existing and candidate group prices
    let mut all_group_prices = group_prices;
    all_group_prices.extend(cand_group_prices);

    // Expand selection-level prices to individual time slices and add to the main prices map
    existing_prices.extend_selection_prices(&all_group_prices, time_slice_info);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::Asset;
    use crate::asset::AssetRef;
    use crate::commodity::{Commodity, CommodityID, CommodityMap};
    use crate::fixture::{
        commodity_id, other_commodity, region_id, sed_commodity, time_slice, time_slice_info,
    };
    use crate::process::{ActivityLimits, FlowType, Process, ProcessFlow, ProcessParameter};
    use crate::region::RegionID;
    use crate::time_slice::TimeSliceID;
    use crate::units::ActivityPerCapacity;
    use crate::units::{
        Activity, Capacity, Dimensionless, FlowPerActivity, MoneyPerActivity, MoneyPerCapacity,
        MoneyPerCapacityPerYear, MoneyPerFlow,
    };
    use float_cmp::assert_approx_eq;
    use indexmap::{IndexMap, IndexSet};
    use rstest::rstest;
    use std::collections::{HashMap, HashSet};
    use std::rc::Rc;

    fn build_process_flow(commodity: &Commodity, coeff: f64, cost: MoneyPerFlow) -> ProcessFlow {
        ProcessFlow {
            commodity: Rc::new(commodity.clone()),
            coeff: FlowPerActivity(coeff),
            kind: FlowType::Fixed,
            cost,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn build_process(
        flows: IndexMap<CommodityID, ProcessFlow>,
        region_id: &RegionID,
        year: u32,
        time_slice_info: &TimeSliceInfo,
        variable_operating_cost: MoneyPerActivity,
        fixed_operating_cost: MoneyPerCapacityPerYear,
        capital_cost: MoneyPerCapacity,
        lifetime: u32,
        discount_rate: Dimensionless,
    ) -> Process {
        let mut process_flows_map = HashMap::new();
        process_flows_map.insert((region_id.clone(), year), Rc::new(flows));

        let mut process_parameter_map = HashMap::new();
        let proc_param = ProcessParameter {
            capital_cost,
            fixed_operating_cost,
            variable_operating_cost,
            lifetime,
            discount_rate,
        };
        process_parameter_map.insert((region_id.clone(), year), Rc::new(proc_param));

        let mut activity_limits_map = HashMap::new();
        activity_limits_map.insert(
            (region_id.clone(), year),
            Rc::new(ActivityLimits::new_with_full_availability(time_slice_info)),
        );

        let regions: IndexSet<RegionID> = IndexSet::from([region_id.clone()]);

        Process {
            id: "p1".into(),
            description: "test process".into(),
            years: 2010..=2020,
            activity_limits: activity_limits_map,
            flows: process_flows_map,
            parameters: process_parameter_map,
            regions,
            primary_output: None,
            capacity_to_activity: ActivityPerCapacity(1.0),
            investment_constraints: HashMap::new(),
            unit_size: None,
        }
    }

    fn assert_price_approx(
        prices: &CommodityPrices,
        commodity: &CommodityID,
        region: &RegionID,
        time_slice: &TimeSliceID,
        expected: MoneyPerFlow,
    ) {
        let p = prices.get(commodity, region, time_slice).unwrap();
        assert_approx_eq!(MoneyPerFlow, p, expected);
    }

    #[rstest]
    #[case(MoneyPerFlow(100.0), MoneyPerFlow(100.0), Dimensionless(0.0), true)] // exactly equal
    #[case(MoneyPerFlow(100.0), MoneyPerFlow(105.0), Dimensionless(0.1), true)] // within tolerance
    #[case(MoneyPerFlow(-100.0), MoneyPerFlow(-105.0), Dimensionless(0.1), true)] // within tolerance, both negative
    #[case(MoneyPerFlow(0.0), MoneyPerFlow(0.0), Dimensionless(0.1), true)] // both zero
    #[case(MoneyPerFlow(100.0), MoneyPerFlow(105.0), Dimensionless(0.01), false)] // difference bigger than tolerance
    #[case(MoneyPerFlow(100.0), MoneyPerFlow(-105.0), Dimensionless(0.1), false)] // comparing positive and negative prices
    #[case(MoneyPerFlow(0.0), MoneyPerFlow(10.0), Dimensionless(0.1), false)] // comparing zero and positive
    #[case(MoneyPerFlow(0.0), MoneyPerFlow(-10.0), Dimensionless(0.1), false)] // comparing zero and negative
    #[case(MoneyPerFlow(10.0), MoneyPerFlow(0.0), Dimensionless(0.1), false)] // comparing positive and zero
    #[case(MoneyPerFlow(-10.0), MoneyPerFlow(0.0), Dimensionless(0.1), false)] // comparing negative and zero
    fn within_tolerance_scenarios(
        #[case] price1: MoneyPerFlow,
        #[case] price2: MoneyPerFlow,
        #[case] tolerance: Dimensionless,
        #[case] expected: bool,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) {
        let mut prices1 = CommodityPrices::default();
        let mut prices2 = CommodityPrices::default();

        // Set up two price sets for a single commodity/region/time slice
        let commodity = CommodityID::new("test_commodity");
        let region = RegionID::new("test_region");
        prices1.insert(&commodity, &region, &time_slice, price1);
        prices2.insert(&commodity, &region, &time_slice, price2);

        assert_eq!(
            prices1.within_tolerance_weighted(&prices2, tolerance, &time_slice_info),
            expected
        );
    }

    #[rstest]
    fn time_slice_weighted_averages(
        commodity_id: CommodityID,
        region_id: RegionID,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) {
        let mut prices = CommodityPrices::default();

        // Insert a price
        prices.insert(&commodity_id, &region_id, &time_slice, MoneyPerFlow(100.0));

        let averages = prices.time_slice_weighted_averages(&time_slice_info);

        // With single time slice (duration=1.0), average should equal the price
        assert_eq!(averages[&(commodity_id, region_id)], MoneyPerFlow(100.0));
    }

    #[rstest]
    fn marginal_cost_example(
        sed_commodity: Commodity,
        other_commodity: Commodity,
        region_id: RegionID,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) {
        // Use the same setup as in the docstring example for marginal cost pricing
        let mut a = sed_commodity.clone();
        a.id = "A".into();
        let mut b = sed_commodity.clone();
        b.id = "B".into();
        let mut c = sed_commodity.clone();
        c.id = "C".into();
        let mut d = other_commodity.clone();
        d.id = "D".into();

        let mut flows = IndexMap::new();
        flows.insert(
            a.id.clone(),
            build_process_flow(&a, -1.0, MoneyPerFlow(0.0)),
        );
        flows.insert(b.id.clone(), build_process_flow(&b, 1.0, MoneyPerFlow(0.0)));
        flows.insert(c.id.clone(), build_process_flow(&c, 2.0, MoneyPerFlow(3.0)));
        flows.insert(d.id.clone(), build_process_flow(&d, 1.0, MoneyPerFlow(4.0)));

        let process = build_process(
            flows,
            &region_id,
            2015u32,
            &time_slice_info,
            MoneyPerActivity(5.0),        // variable operating cost
            MoneyPerCapacityPerYear(0.0), // fixed operating cost
            MoneyPerCapacity(0.0),        // capital cost
            5,                            // lifetime
            Dimensionless(1.0),           // discount rate
        );

        let asset =
            Asset::new_candidate(Rc::new(process), region_id.clone(), Capacity(1.0), 2015u32)
                .unwrap();
        let asset_ref = AssetRef::from(asset);
        let shadow_prices =
            CommodityPrices::from_iter(vec![(&a.id, &region_id, &time_slice, MoneyPerFlow(1.0))]);
        let mut markets = HashSet::new();
        markets.insert((b.id.clone(), region_id.clone()));
        markets.insert((c.id.clone(), region_id.clone()));

        let mut commodities = CommodityMap::new();
        commodities.insert(b.id.clone(), Rc::new(b.clone()));
        commodities.insert(c.id.clone(), Rc::new(c.clone()));

        let existing = vec![(&asset_ref, &time_slice, Activity(1.0))];
        let candidates = Vec::new();

        let mut prices = shadow_prices.clone();
        add_marginal_cost_prices(
            existing.into_iter(),
            candidates.into_iter(),
            &mut prices,
            2015u32,
            &markets,
            &commodities,
            &time_slice_info,
        );

        assert_price_approx(
            &prices,
            &b.id,
            &region_id,
            &time_slice,
            MoneyPerFlow(10.0 / 3.0),
        );
        assert_price_approx(
            &prices,
            &c.id,
            &region_id,
            &time_slice,
            MoneyPerFlow(10.0 / 3.0 + 3.0),
        );
    }

    #[rstest]
    fn full_cost_example(
        sed_commodity: Commodity,
        other_commodity: Commodity,
        region_id: RegionID,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) {
        // Use the same setup as in the docstring example for full cost pricing
        let mut a = sed_commodity.clone();
        a.id = "A".into();
        let mut b = sed_commodity.clone();
        b.id = "B".into();
        let mut c = sed_commodity.clone();
        c.id = "C".into();
        let mut d = other_commodity.clone();
        d.id = "D".into();

        let mut flows = IndexMap::new();
        flows.insert(
            a.id.clone(),
            build_process_flow(&a, -1.0, MoneyPerFlow(0.0)),
        );
        flows.insert(b.id.clone(), build_process_flow(&b, 1.0, MoneyPerFlow(0.0)));
        flows.insert(c.id.clone(), build_process_flow(&c, 2.0, MoneyPerFlow(3.0)));
        flows.insert(d.id.clone(), build_process_flow(&d, 1.0, MoneyPerFlow(4.0)));

        let process = build_process(
            flows,
            &region_id,
            2015u32,
            &time_slice_info,
            MoneyPerActivity(5.0),        // variable operating cost
            MoneyPerCapacityPerYear(1.0), //  fixed operating cost
            MoneyPerCapacity(1.5),        // capital cost per capacity so annualised=1.5
            1,                            // lifetime so annualised = capital_cost
            Dimensionless(0.0),           // discount rate
        );

        let asset =
            Asset::new_candidate(Rc::new(process), region_id.clone(), Capacity(4.0), 2015u32)
                .unwrap();
        let asset_ref = AssetRef::from(asset);
        let shadow_prices =
            CommodityPrices::from_iter(vec![(&a.id, &region_id, &time_slice, MoneyPerFlow(1.0))]);
        let mut markets = HashSet::new();
        markets.insert((b.id.clone(), region_id.clone()));
        markets.insert((c.id.clone(), region_id.clone()));

        let mut commodities = CommodityMap::new();
        commodities.insert(b.id.clone(), Rc::new(b.clone()));
        commodities.insert(c.id.clone(), Rc::new(c.clone()));

        let existing = vec![(&asset_ref, &time_slice, Activity(2.0))];
        let candidates = Vec::new();

        let mut annual_activities = HashMap::new();
        annual_activities.insert(asset_ref.clone(), Activity(2.0));

        let mut prices = shadow_prices.clone();
        add_full_cost_prices(
            existing.into_iter(),
            candidates.into_iter(),
            &annual_activities,
            &mut prices,
            2015u32,
            &markets,
            &commodities,
            &time_slice_info,
        );

        assert_price_approx(&prices, &b.id, &region_id, &time_slice, MoneyPerFlow(5.0));
        assert_price_approx(&prices, &c.id, &region_id, &time_slice, MoneyPerFlow(8.0));
    }

    #[test]
    fn weighted_average_accumulator_single_value() {
        let mut accum = WeightedAverageAccumulator::default();
        accum.add(MoneyPerFlow(100.0), Dimensionless(1.0));
        assert_eq!(accum.finalise(), Some(MoneyPerFlow(100.0)));
    }

    #[test]
    fn weighted_average_accumulator_different_weights() {
        let mut accum = WeightedAverageAccumulator::default();
        accum.add(MoneyPerFlow(100.0), Dimensionless(1.0));
        accum.add(MoneyPerFlow(200.0), Dimensionless(2.0));
        // (100*1 + 200*2) / (1+2) = 500/3 ≈ 166.667
        let result = accum.finalise().unwrap();
        assert_approx_eq!(MoneyPerFlow, result, MoneyPerFlow(500.0 / 3.0));
    }

    #[test]
    fn weighted_average_accumulator_zero_weight() {
        let accum = WeightedAverageAccumulator::default();
        assert_eq!(accum.finalise(), None);
    }

    #[test]
    fn weighted_average_backup_accumulator_primary_preferred() {
        let mut accum = WeightedAverageBackupAccumulator::default();
        accum.add(MoneyPerFlow(100.0), Dimensionless(3.0), Dimensionless(1.0));
        accum.add(MoneyPerFlow(200.0), Dimensionless(1.0), Dimensionless(1.0));
        // Primary is non-zero, use it: (100*3 + 200*1) / (3+1) = 125
        // (backup would be (100*1 + 200*1) / (1+1) = 150, but we don't use it)
        assert_eq!(accum.finalise(), Some(MoneyPerFlow(125.0)));
    }

    #[test]
    fn weighted_average_backup_accumulator_fallback() {
        let mut accum = WeightedAverageBackupAccumulator::default();
        accum.add(MoneyPerFlow(100.0), Dimensionless(0.0), Dimensionless(2.0));
        accum.add(MoneyPerFlow(200.0), Dimensionless(0.0), Dimensionless(2.0));
        // Primary is zero, fallback to backup: (100*2 + 200*2) / (2+2) = 150
        assert_eq!(accum.finalise(), Some(MoneyPerFlow(150.0)));
    }

    #[test]
    fn weighted_average_backup_accumulator_both_zero() {
        let accum = WeightedAverageBackupAccumulator::default();
        assert_eq!(accum.finalise(), None);
    }
}
