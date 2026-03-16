//! Code for calculating commodity prices used by the simulation.
use crate::asset::AssetRef;
use crate::commodity::{CommodityID, CommodityMap, PricingStrategy};
use crate::model::Model;
use crate::region::RegionID;
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceSelection};
use crate::units::{Activity, Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
use anyhow::Result;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

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
    // simultaneously, since they are independent (apart from Cycle sets when using the "marginal"
    // and "full" strategies, which get flagged at the validation stage).
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
            let scarcity_prices = calculate_scarcity_adjusted_prices(
                solution.iter_activity_duals(),
                &shadow_prices,
                scarcity_set,
            );
            result.extend(scarcity_prices);
        }

        // Add prices for marginal cost commodities
        if let Some(marginal_set) = pricing_sets.get(&PricingStrategy::MarginalCost) {
            let marginal_cost_prices = calculate_marginal_cost_prices(
                solution.iter_activity_for_existing(),
                solution.iter_activity_keys_for_candidates(),
                &result,
                year,
                marginal_set,
                &model.commodities,
                &model.time_slice_info,
            );
            result.extend(marginal_cost_prices);
        }

        // Add prices for full cost commodities
        if let Some(fullcost_set) = pricing_sets.get(&PricingStrategy::FullCost) {
            let annual_activities = annual_activities.get_or_insert_with(|| {
                calculate_annual_activities(solution.iter_activity_for_existing())
            });
            let full_cost_prices = calculate_full_cost_prices(
                solution.iter_activity_for_existing(),
                solution.iter_activity_keys_for_candidates(),
                annual_activities,
                &result,
                year,
                fullcost_set,
                &model.commodities,
                &model.time_slice_info,
            );
            result.extend(full_cost_prices);
        }
    }

    // Return the completed prices map
    Ok(result)
}

/// A map relating commodity ID + region + time slice to current price (endogenous)
#[derive(Default, Clone)]
pub struct CommodityPrices(IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>);

impl CommodityPrices {
    /// Insert a price for the given commodity, region and time slice
    pub fn insert(
        &mut self,
        commodity_id: &CommodityID,
        region_id: &RegionID,
        time_slice: &TimeSliceID,
        price: MoneyPerFlow,
    ) {
        let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
        self.0.insert(key, price);
    }

    /// Extend the prices map, panic if any key already exists
    pub fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = ((CommodityID, RegionID, TimeSliceID), MoneyPerFlow)>,
    {
        for (key, price) in iter {
            let existing = self.0.insert(key.clone(), price).is_some();
            assert!(!existing, "Key {key:?} already exists in the map");
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

/// Calculate scarcity-adjusted prices for a set of commodities.
///
/// # Arguments
///
/// * `activity_duals` - Iterator over activity duals from optimisation solution
/// * `shadow_prices` - Shadow prices for all commodities
/// * `markets_to_price` - Set of markets to calculate scarcity-adjusted prices for
///
/// # Returns
///
/// A map of scarcity-adjusted prices for the specified markets in all time slices
fn calculate_scarcity_adjusted_prices<'a, I>(
    activity_duals: I,
    shadow_prices: &CommodityPrices,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
) -> IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
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

    // Add this to the shadow price for each commodity/region/time slice
    let mut scarcity_prices = IndexMap::new();
    for ((commodity, region, time_slice), highest_dual) in &highest_duals {
        // There should always be a shadow price for commodities we are considering here, so it
        // should be safe to unwrap
        let shadow_price = shadow_prices.get(commodity, region, time_slice).unwrap();
        // highest_dual is in units of MoneyPerActivity, and shadow_price is in MoneyPerFlow, but
        // this is correct according to Adam
        let scarcity_price = shadow_price + MoneyPerFlow(highest_dual.value());
        scarcity_prices.insert(
            (commodity.clone(), region.clone(), time_slice.clone()),
            scarcity_price,
        );
    }

    scarcity_prices
}

/// Helper struct for accumulating weighted marginal costs for a group of (asset, commodity, region,
/// time slice selection) tuples when calculating marginal cost or full cost prices.
///
/// For seasonal/annual commodities, marginal costs are weighted by activity (or the activity limit
/// for assets with no activity) to get a time slice-weighted average marginal cost for the group.
struct GroupAccum {
    // Sum of (marginal cost * activity) across all tuples in the group
    weighted_cost_numerator: MoneyPerFlow,
    // Sum of activity across all tuples in the group
    weighted_cost_denominator: Dimensionless,
    // Backup numerator and denominator for the case where there's no activity across the group
    backup_numerator: MoneyPerFlow,
    backup_denominator: Dimensionless,
}

impl Default for GroupAccum {
    fn default() -> Self {
        Self {
            weighted_cost_numerator: MoneyPerFlow(0.0),
            weighted_cost_denominator: Dimensionless(0.0),
            backup_numerator: MoneyPerFlow(0.0),
            backup_denominator: Dimensionless(0.0),
        }
    }
}

impl GroupAccum {
    /// Add a marginal cost to the accumulator
    fn add(&mut self, marginal_cost: MoneyPerFlow, activity: Activity, activity_limit: Activity) {
        self.weighted_cost_numerator += marginal_cost * Dimensionless(activity.value());
        self.weighted_cost_denominator += Dimensionless(activity.value());
        self.backup_numerator += marginal_cost * Dimensionless(activity_limit.value());
        self.backup_denominator += Dimensionless(activity_limit.value());
    }

    /// Solve the weighted average marginal cost for the group, using the backup weights (activity
    /// limits) if there's no activity across the group. If both denominators are zero, return None.
    fn solve(&self) -> Option<MoneyPerFlow> {
        if self.weighted_cost_denominator > Dimensionless::EPSILON {
            Some(self.weighted_cost_numerator / self.weighted_cost_denominator)
        } else if self.backup_denominator > Dimensionless::EPSILON {
            Some(self.backup_numerator / self.backup_denominator)
        } else {
            None
        }
    }
}

/// Expand a map of per-group prices to individual time slices.
fn expand_groups_to_prices(
    group_prices: &IndexMap<(CommodityID, RegionID, TimeSliceSelection), MoneyPerFlow>,
    time_slice_info: &TimeSliceInfo,
    out: &mut IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>,
) {
    for ((commodity_id, region_id, group), &group_price) in group_prices {
        for (ts, _) in group.iter(time_slice_info) {
            out.insert(
                (commodity_id.clone(), region_id.clone(), ts.clone()),
                group_price,
            );
        }
    }
}

/// Calculate marginal cost prices for a set of commodities.
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
/// * `upstream_prices` - Prices for commodities upstream of the ones we are calculating prices for
/// * `year` - The year for which prices are being calculated
/// * `markets_to_price` - Set of markets to calculate marginal prices for
/// * `commodities` - Map of all commodities (used to look up each commodity's `time_slice_level`)
/// * `time_slice_info` - Time slice information (used to expand groups to individual time slices)
///
/// # Returns
///
/// A map of marginal cost prices for the specified markets in all time slices
fn calculate_marginal_cost_prices<'a, I, J>(
    activity_for_existing: I,
    activity_keys_for_candidates: J,
    upstream_prices: &CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) -> IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID)>,
{
    // For each (asset, commodity, region, group), accumulate marginal costs. For seasonal/annual
    // commodities, marginal costs are weighted by activity (or the activity limit for assets with
    // no activity)
    let mut existing_accum: IndexMap<_, GroupAccum> = IndexMap::new();
    for (asset, time_slice, activity) in activity_for_existing {
        let region_id = asset.region_id();

        // Get activity limits: used as a backup weight if no activity across the group
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            upstream_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            let group = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);
            let key = (
                asset.clone(),
                commodity_id.clone(),
                region_id.clone(),
                group,
            );
            let accum = existing_accum.entry(key).or_default();
            accum.add(marginal_cost, activity, activity_limit);
        }
    }

    // Compute per-group weighted-average marginal cost per asset, then take the max across assets
    let mut group_prices: IndexMap<_, MoneyPerFlow> = IndexMap::new();
    let mut priced_groups: HashSet<_> = HashSet::new();
    for ((_, commodity_id, region_id, group), accum) in &existing_accum {
        // Solve the weighted average marginal cost for this group
        let Some(avg_cost) = accum.solve() else {
            continue;
        };

        // Take the max across assets for each group
        group_prices
            .entry((commodity_id.clone(), region_id.clone(), group.clone()))
            .and_modify(|c| *c = c.max(avg_cost))
            .or_insert(avg_cost);
        priced_groups.insert((commodity_id.clone(), region_id.clone(), group.clone()));
    }

    // Expand each group to individual time slices
    let mut prices: IndexMap<_, MoneyPerFlow> = IndexMap::new();
    expand_groups_to_prices(&group_prices, time_slice_info, &mut prices);

    // Candidate assets (assume full utilisation)
    let mut cand_accum: IndexMap<_, GroupAccum> = IndexMap::new();
    for (asset, time_slice) in activity_keys_for_candidates {
        let region_id = asset.region_id();

        // Get activity limits: used to weight marginal costs for seasonal/annual commodities
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            upstream_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            let group = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Skip groups already covered by existing assets
            if priced_groups.contains(&(commodity_id.clone(), region_id.clone(), group.clone())) {
                continue;
            }

            let key = (
                asset.clone(),
                commodity_id.clone(),
                region_id.clone(),
                group,
            );
            let accum = cand_accum.entry(key).or_default();
            accum.add(marginal_cost, Activity(0.0), activity_limit);
        }
    }

    // Compute per-group weighted average per candidate, then take the min across candidates
    // (i.e. the single most competitive candidate if a small amount of demand was added)
    let mut cand_group_prices: IndexMap<_, MoneyPerFlow> = IndexMap::new();
    for ((_, commodity_id, region_id, group), accum) in &cand_accum {
        let Some(avg_cost) = accum.solve() else {
            continue;
        };
        cand_group_prices
            .entry((commodity_id.clone(), region_id.clone(), group.clone()))
            .and_modify(|c| *c = c.min(avg_cost))
            .or_insert(avg_cost);
    }

    // Expand candidate groups to individual time slices
    expand_groups_to_prices(&cand_group_prices, time_slice_info, &mut prices);

    prices
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

/// Calculate full cost prices for a set of commodities.
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
/// * `upstream_prices` - Prices for commodities upstream of the ones we are calculating prices for
/// * `year` - The year for which prices are being calculated
/// * `markets_to_price` - Set of markets to calculate full cost prices for
/// * `commodities` - Map of all commodities (used to look up each commodity's `time_slice_level`)
/// * `time_slice_info` - Time slice information (used to expand groups to individual time slices)
///
/// # Returns
///
/// A map of full cost prices for the specified markets in all time slices
#[allow(clippy::too_many_arguments)]
fn calculate_full_cost_prices<'a, I, J>(
    activity_for_existing: I,
    activity_keys_for_candidates: J,
    annual_activities: &HashMap<AssetRef, Activity>,
    upstream_prices: &CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) -> IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID)>,
{
    // For each (asset, commodity, region, group), accumulate marginal costs. For seasonal/annual
    // commodities, marginal costs are weighted by activity (or the activity limit for assets with
    // no activity)
    let mut existing_accum: IndexMap<_, GroupAccum> = IndexMap::new();
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
            upstream_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            let group = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);
            let key = (
                asset.clone(),
                commodity_id.clone(),
                region_id.clone(),
                group,
            );
            let accum = existing_accum.entry(key).or_default();
            accum.add(marginal_cost, activity, activity_limit);
        }
    }

    // Compute per-group weighted-average marginal cost per asset, add fixed costs, then take max
    let mut group_prices: IndexMap<_, MoneyPerFlow> = IndexMap::new();
    let mut priced_groups: HashSet<_> = HashSet::new();
    let mut existing_fixed_costs_cache: HashMap<_, MoneyPerFlow> = HashMap::new();
    for ((asset, commodity_id, region_id, group), accum) in &existing_accum {
        // Solve the weighted average marginal cost for this group
        let Some(avg_mc) = accum.solve() else {
            continue;
        };

        // Add fixed costs to get the full cost.
        let annual_fixed_costs_per_flow = *existing_fixed_costs_cache
            .entry(asset.clone())
            .or_insert_with(|| asset.get_annual_fixed_costs_per_flow(annual_activities[asset]));
        let full_cost = avg_mc + annual_fixed_costs_per_flow;

        // Take the max across assets for each group
        group_prices
            .entry((commodity_id.clone(), region_id.clone(), group.clone()))
            .and_modify(|c| *c = c.max(full_cost))
            .or_insert(full_cost);
        priced_groups.insert((commodity_id.clone(), region_id.clone(), group.clone()));
    }

    // Expand each group to individual time slices
    let mut prices: IndexMap<_, MoneyPerFlow> = IndexMap::new();
    expand_groups_to_prices(&group_prices, time_slice_info, &mut prices);

    // Candidate assets (assume full utilisation)
    let mut cand_accum: IndexMap<_, GroupAccum> = IndexMap::new();
    for (asset, time_slice) in activity_keys_for_candidates {
        let region_id = asset.region_id();

        // Get activity limits: used to weight marginal costs for seasonal/annual commodities
        let activity_limit = *asset
            .get_activity_limits_for_selection(&TimeSliceSelection::Single(time_slice.clone()))
            .end();

        // Iterate over the marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            upstream_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            let group = commodities[&commodity_id]
                .time_slice_level
                .containing_selection(time_slice);

            // Skip groups already covered by existing assets
            if priced_groups.contains(&(commodity_id.clone(), region_id.clone(), group.clone())) {
                continue;
            }

            let key = (
                asset.clone(),
                commodity_id.clone(),
                region_id.clone(),
                group,
            );
            let accum = cand_accum.entry(key).or_default();
            accum.add(marginal_cost, Activity(0.0), activity_limit);
        }
    }

    // Compute per-group weighted average, add fixed costs, take the min across candidates
    let mut cand_fixed_costs_cache: HashMap<_, MoneyPerFlow> = HashMap::new();
    let mut cand_group_prices: IndexMap<_, MoneyPerFlow> = IndexMap::new();
    for ((asset, commodity_id, region_id, group), accum) in &cand_accum {
        // Solve the weighted average marginal cost for this group
        let Some(avg_mc) = accum.solve() else {
            continue;
        };

        // Add fixed costs to get the full cost. For candidates we assume full utilisation
        let annual_fixed_costs_per_flow = *cand_fixed_costs_cache
            .entry(asset.clone())
            .or_insert_with(|| {
                asset.get_annual_fixed_costs_per_flow(
                    *asset
                        .get_activity_limits_for_selection(&TimeSliceSelection::Annual)
                        .end(),
                )
            });
        let full_cost = avg_mc + annual_fixed_costs_per_flow;

        // Take the min across candidates for each group (i.e. the single most competitive candidate
        // if a small amount of demand was added)
        cand_group_prices
            .entry((commodity_id.clone(), region_id.clone(), group.clone()))
            .and_modify(|c| *c = c.min(full_cost))
            .or_insert(full_cost);
    }

    // Expand candidate groups to individual time slices
    expand_groups_to_prices(&cand_group_prices, time_slice_info, &mut prices);

    prices
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
        prices: &IndexMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>,
        commodity: &CommodityID,
        region: &RegionID,
        time_slice: &TimeSliceID,
        expected: MoneyPerFlow,
    ) {
        let p = prices[&(commodity.clone(), region.clone(), time_slice.clone())];
        assert!((p - expected).abs() < MoneyPerFlow::EPSILON);
    }

    #[test]
    fn group_accum_empty_returns_none() {
        assert!(GroupAccum::default().solve().is_none());
    }

    #[test]
    fn group_accum_returns_none_when_both_denominators_zero() {
        let mut accum = GroupAccum::default();
        accum.add(MoneyPerFlow(10.0), Activity(0.0), Activity(0.0));
        assert!(accum.solve().is_none());
    }

    #[test]
    fn group_accum_uses_activity_weight() {
        let mut accum = GroupAccum::default();
        // Two entries: cost 10 with activity 1, cost 20 with activity 3 → weighted avg = 17.5
        accum.add(MoneyPerFlow(10.0), Activity(1.0), Activity(1.0));
        accum.add(MoneyPerFlow(20.0), Activity(3.0), Activity(3.0));
        let result = accum.solve().unwrap();
        assert!((result - MoneyPerFlow(17.5)).abs() < MoneyPerFlow::EPSILON);
    }

    #[test]
    fn group_accum_single_entry_returns_same_cost() {
        let mut accum = GroupAccum::default();
        accum.add(MoneyPerFlow(42.0), Activity(5.0), Activity(5.0));
        assert_eq!(accum.solve().unwrap(), MoneyPerFlow(42.0));
    }

    #[test]
    fn group_accum_falls_back_to_activity_limit_when_no_activity() {
        let mut accum = GroupAccum::default();
        // Zero activity, but non-zero activity limits → should use backup weights
        accum.add(MoneyPerFlow(10.0), Activity(0.0), Activity(1.0));
        accum.add(MoneyPerFlow(20.0), Activity(0.0), Activity(3.0));
        let result = accum.solve().unwrap();
        assert!((result - MoneyPerFlow(17.5)).abs() < MoneyPerFlow::EPSILON);
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

        let prices = calculate_marginal_cost_prices(
            existing.into_iter(),
            candidates.into_iter(),
            &shadow_prices,
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

        let prices = calculate_full_cost_prices(
            existing.into_iter(),
            candidates.into_iter(),
            &annual_activities,
            &shadow_prices,
            2015u32,
            &markets,
            &commodities,
            &time_slice_info,
        );

        assert_price_approx(&prices, &b.id, &region_id, &time_slice, MoneyPerFlow(5.0));
        assert_price_approx(&prices, &c.id, &region_id, &time_slice, MoneyPerFlow(8.0));
    }
}
