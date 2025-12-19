//! Code for updating the simulation state.
use crate::asset::AssetRef;
use crate::commodity::{CommodityID, PricingStrategy};
use crate::model::Model;
use crate::region::RegionID;
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceSelection};
use crate::units::{Activity, Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
use anyhow::Result;
use itertools::iproduct;
use std::collections::{BTreeMap, HashMap, HashSet, btree_map};

/// Iterator item type for asset activity iterators
type Item<'a> = (&'a AssetRef, &'a TimeSliceID, Activity);

/// Calculate commodity prices.
///
/// Prices for each commodity are calculated based on their respective pricing strategies.
///
/// # Arguments
///
/// * `model` - The model
/// * `solution` - Solution to dispatch optimisation
/// * `year` - The year for which prices are being calculated
pub fn calculate_prices(model: &Model, solution: &Solution, year: u32) -> Result<CommodityPrices> {
    // Compute shadow prices for all SED/SVD commodities (needed by all strategies)
    let shadow_prices = CommodityPrices::from_iter(solution.iter_commodity_balance_duals());

    // Partition markets by pricing strategy into a map keyed by `PricingStrategy`.
    // For now, commodities use a single strategy for all regions, but this may change in the future.
    let mut pricing_sets = HashMap::new();
    for ((commodity_id, commodity), region_id) in
        iproduct!(&model.commodities, model.iter_regions())
    {
        if commodity.pricing_strategy == PricingStrategy::Unpriced {
            continue;
        }
        pricing_sets
            .entry(&commodity.pricing_strategy)
            .or_insert_with(HashSet::new)
            .insert((commodity_id.clone(), region_id.clone()));
    }

    // Set up empty prices map
    let mut result = CommodityPrices::default();

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
            solution.iter_activity_for_candidates(),
            &shadow_prices,
            year,
            marginal_set,
        );
        result.extend(marginal_cost_prices);
    }

    // Add prices for full cost commodities
    if let Some(fullcost_set) = pricing_sets.get(&PricingStrategy::FullCost) {
        let annual_activities = calculate_annual_activities(solution.iter_activity_for_existing());
        let full_cost_prices = calculate_full_cost_prices(
            solution.iter_activity_for_existing(),
            solution.iter_activity_for_candidates(),
            &annual_activities,
            &shadow_prices,
            year,
            fullcost_set,
        );
        result.extend(full_cost_prices);
    }

    // Return the completed prices map
    Ok(result)
}

/// A map relating commodity ID + region + time slice to current price (endogenous)
#[derive(Default, Clone)]
pub struct CommodityPrices(BTreeMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>);

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
    pub fn keys(&self) -> btree_map::Keys<'_, (CommodityID, RegionID, TimeSliceID), MoneyPerFlow> {
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
            *weighted_prices.entry(key).or_default() += *price * weight;
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
    type IntoIter =
        std::collections::btree_map::IntoIter<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>;

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
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
{
    // Calculate highest activity dual for each commodity/region/time slice
    let mut highest_duals = HashMap::new();
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
    let mut scarcity_prices = HashMap::new();
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
/// If any existing assets produce a given commodity in a particular region and time slice, the
/// price is taken from the asset with the highest marginal cost among those existing assets. If _no_
/// existing assets produce the commodity in that region and time slice (in particular, this will
/// occur when there's no demand for the commodity), then candidate assets are considered: we
/// take the price from the candidate asset with the _lowest_ marginal cost, assuming full utilisation
/// (i.e. the single candidate asset that would be most competitive if a small amount of demand was
/// added).
///
/// Note: this should be similar to the "shadow price" strategy, which is also based on marginal
/// costs of the most expensive producer, but may be more successful in cases where there are
//  multiple SED/SVD outputs per asset.
///
/// # Arguments
///
/// * `activity_for_existing` - Iterator over activity from optimisation solution for existing
///   assets
/// * `activity_for_candidates` - Iterator over activity from optimisation solution for candidate
///   assets. Note: we only need the keys, since we assume full utilisation for candidates.
/// * `annual_activities` - Map of annual activities for each asset computed by
///   `calculate_annual_activities`. This only needs to include existing assets.
/// * `shadow_prices` - Shadow prices for all commodities
/// * `year` - The year for which prices are being calculated
/// * `markets_to_price` - Set of markets to calculate full cost prices for
///
/// # Returns
///
/// A map of marginal cost prices for the specified markets in all time slices
fn calculate_marginal_cost_prices<'a, I, J>(
    activity_for_existing: I,
    activity_for_candidates: J,
    shadow_prices: &CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = Item<'a>>,
    J: Iterator<Item = Item<'a>>,
{
    let mut prices: HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow> = HashMap::new();

    // Start by looking at existing assets
    // Calculate highest marginal cost for each commodity/region/time slice
    // Keep track of keys with prices - missing keys will be handled by candidates later
    let mut priced_by_existing = HashSet::new();
    for (asset, time_slice, activity) in activity_for_existing {
        let region_id = asset.region_id();

        // Only proceed if the asset has non-zero activity in this time slice
        if activity < Activity::EPSILON {
            continue;
        }

        // Iterate over all the SED/SVD marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            shadow_prices,
            year,
            time_slice,
            |commodity_id: &CommodityID| {
                markets_to_price.contains(&(commodity_id.clone(), region_id.clone()))
            },
        ) {
            // Update the highest cost for this commodity/region/time slice
            let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
            prices
                .entry(key.clone())
                .and_modify(|c| *c = c.max(marginal_cost))
                .or_insert(marginal_cost);
            priced_by_existing.insert(key);
        }
    }

    // Next, look at candidate assets for any markets not covered by existing assets
    // For these, we take the _lowest_ marginal cost
    for (asset, time_slice, _activity) in activity_for_candidates {
        let region_id = asset.region_id();

        // Only consider markets not already priced by existing assets
        let should_process = |cid: &CommodityID| {
            markets_to_price.contains(&(cid.clone(), region_id.clone()))
                && !priced_by_existing.contains(&(
                    cid.clone(),
                    region_id.clone(),
                    time_slice.clone(),
                ))
        };

        // Iterate over all the SED/SVD marginal costs for markets we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            shadow_prices,
            year,
            time_slice,
            |cid: &CommodityID| should_process(cid),
        ) {
            // Update the _lowest_ cost for this commodity/region/time slice
            let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
            prices
                .entry(key.clone())
                .and_modify(|c| *c = c.min(marginal_cost))
                .or_insert(marginal_cost);
        }
    }

    // Return the calculated marginal prices
    prices
}

/// Calculated annual activities for each asset by summing across all time slices
fn calculate_annual_activities<'a, I>(activities: I) -> HashMap<AssetRef, Activity>
where
    I: IntoIterator<Item = Item<'a>>,
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
/// If any existing assets produce a given commodity in a particular region and time slice, the
/// price is taken from the asset with the highest full cost among those existing assets. If _no_
/// existing assets produce the commodity in that region and time slice (in particular, this will
/// occur when there's no demand for the commodity), then candidate assets are considered: we
/// take the price from the candidate asset with the _lowest_ full cost, assuming maximum
/// possible dispatch (i.e. the single candidate asset that would be most competitive if a small
/// amount of demand was added).
///
/// # Arguments
///
/// * `activity_for_existing` - Iterator over activity from optimisation solution for existing
///   assets
/// * `activity_for_candidates` - Iterator over activity from optimisation solution for candidate
///   assets. Note: we only need the keys, since we assume full dispatch for candidates.
/// * `annual_activities` - Map of annual activities for each asset computed by
///   `calculate_annual_activities`. This only needs to include existing assets.
/// * `shadow_prices` - Shadow prices for all commodities
/// * `year` - The year for which prices are being calculated
/// * `markets_to_price` - Set of markets to calculate full cost prices for
///
/// # Returns
///
/// A map of full cost prices for the specified markets in all time slices
fn calculate_full_cost_prices<'a, I, J>(
    activity_for_existing: I,
    activity_for_candidates: J,
    annual_activities: &HashMap<AssetRef, Activity>,
    shadow_prices: &CommodityPrices,
    year: u32,
    markets_to_price: &HashSet<(CommodityID, RegionID)>,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = Item<'a>>,
    J: Iterator<Item = Item<'a>>,
{
    let mut prices: HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow> = HashMap::new();

    // Start by looking at existing assets
    // Calculate highest full cost for each commodity/region/time slice
    // Keep track of keys with prices - missing keys will be handled by candidates later
    let mut annual_capital_costs_cache = HashMap::new();
    let mut priced_by_existing = HashSet::new();
    for (asset, time_slice, activity) in activity_for_existing {
        let annual_activity = annual_activities[asset];
        let region_id = asset.region_id();

        // Only proceed if the asset has non-zero activity in this time slice
        if activity < Activity::EPSILON {
            continue;
        }

        // Only proceed if the asset produces at least one commodity we need prices for
        if !asset
            .iter_output_flows()
            .any(|flow| markets_to_price.contains(&(flow.commodity.id.clone(), region_id.clone())))
        {
            continue;
        }

        // Calculate/cache annual capital cost for this asset
        let annual_capital_cost_per_flow = *annual_capital_costs_cache
            .entry(asset.clone())
            .or_insert_with(|| asset.get_annual_capital_cost_per_flow(annual_activity));

        // Iterate over all the SED/SVD marginal costs for commodities we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            shadow_prices,
            year,
            time_slice,
            |cid: &CommodityID| markets_to_price.contains(&(cid.clone(), region_id.clone())),
        ) {
            // Add capital cost per flow to marginal cost to get full cost
            let marginal_cost = marginal_cost + annual_capital_cost_per_flow;

            // Update the highest cost for this commodity/region/time slice
            let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
            prices
                .entry(key.clone())
                .and_modify(|c| *c = c.max(marginal_cost))
                .or_insert(marginal_cost);
            priced_by_existing.insert(key);
        }
    }

    // Next, look at candidate assets for any markets not covered by existing assets
    // For these we assume full utilisation, and take the _lowest_ full cost
    for (asset, time_slice, _activity) in activity_for_candidates {
        let region_id = asset.region_id();

        // Only consider markets not already priced by existing assets
        let should_process = |cid: &CommodityID| {
            markets_to_price.contains(&(cid.clone(), region_id.clone()))
                && !priced_by_existing.contains(&(
                    cid.clone(),
                    region_id.clone(),
                    time_slice.clone(),
                ))
        };

        // Only proceed if the asset produces at least one commodity we need prices for
        if !asset
            .iter_output_flows()
            .any(|flow| should_process(&flow.commodity.id))
        {
            continue;
        }

        // Calculate/cache annual capital cost per flow for this asset assuming full dispatch
        // (bound by the activity limits of the asset)
        let annual_capital_cost_per_flow = *annual_capital_costs_cache
            .entry(asset.clone())
            .or_insert_with(|| {
                asset.get_annual_capital_cost_per_flow(
                    *asset
                        .get_activity_limits_for_selection(&TimeSliceSelection::Annual)
                        .end(),
                )
            });

        // Iterate over all the SED/SVD marginal costs for markets we need prices for
        for (commodity_id, marginal_cost) in asset.iter_marginal_costs_with_filter(
            shadow_prices,
            year,
            time_slice,
            |cid: &CommodityID| should_process(cid),
        ) {
            // Add capital cost per flow to marginal cost to get full cost
            let full_cost = marginal_cost + annual_capital_cost_per_flow;

            // Update the _lowest_ cost for this commodity/region/time slice
            let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
            prices
                .entry(key)
                .and_modify(|c| *c = c.min(full_cost))
                .or_insert(full_cost);
        }
    }

    // Return the calculated full cost prices
    prices
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::Asset;
    use crate::asset::AssetRef;
    use crate::commodity::{Commodity, CommodityID};
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

    fn build_process(
        flows: IndexMap<CommodityID, ProcessFlow>,
        region_id: &RegionID,
        year: u32,
        time_slice_info: &TimeSliceInfo,
        variable_operating_cost: MoneyPerActivity,
        capital_cost: MoneyPerCapacity,
        lifetime: u32,
        discount_rate: Dimensionless,
    ) -> Process {
        let mut process_flows_map = HashMap::new();
        process_flows_map.insert((region_id.clone(), year), Rc::new(flows));

        let mut process_parameter_map = HashMap::new();
        let proc_param = ProcessParameter {
            capital_cost,
            fixed_operating_cost: MoneyPerCapacityPerYear(0.0),
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
        prices: &HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>,
        commodity: &CommodityID,
        region: &RegionID,
        time_slice: &TimeSliceID,
        expected: MoneyPerFlow,
    ) {
        let p = prices[&(commodity.clone(), region.clone(), time_slice.clone())];
        assert!((p - expected).abs() < MoneyPerFlow::EPSILON);
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
    fn test_within_tolerance_scenarios(
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
    fn test_time_slice_weighted_averages(
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
    fn test_marginal_cost_example(
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
            MoneyPerActivity(5.0), // variable operating cost
            MoneyPerCapacity(0.0), // capital cost
            5,                     // lifetime
            Dimensionless(1.0),    // discount rate
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

        let existing = vec![(&asset_ref, &time_slice, Activity(1.0))];
        let candidates = Vec::new();

        let prices = calculate_marginal_cost_prices(
            existing.into_iter(),
            candidates.into_iter(),
            &shadow_prices,
            2015u32,
            &markets,
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
    fn test_full_cost_example(
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
            MoneyPerActivity(5.0), // variable operating cost
            MoneyPerCapacity(2.5), // capital cost per capacity so annualised=2.5
            1,                     // lifetime so annualised = capital_cost
            Dimensionless(0.0),    // discount rate
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
        );

        assert_price_approx(&prices, &b.id, &region_id, &time_slice, MoneyPerFlow(5.0));
        assert_price_approx(&prices, &c.id, &region_id, &time_slice, MoneyPerFlow(8.0));
    }
}
