//! Code for updating the simulation state.
use crate::ISSUES_URL;
use crate::asset::{AssetRef, AssetState};
use crate::commodity::{CommodityID, PricingStrategy};
use crate::model::{ALLOW_BROKEN_OPTION_NAME, Model};
use crate::process::FlowDirection;
use crate::region::RegionID;
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
use anyhow::{Result, ensure};
use std::collections::{BTreeMap, HashMap, HashSet, btree_map};

/// Calculate commodity prices.
///
/// Note that the behaviour will be different depending on the [`PricingStrategy`] for each
/// commodity.
///
/// # Arguments
///
/// * `model` - The model
/// * `solution` - Solution to dispatch optimisation
/// * `year` - The year for which prices are being calculated
pub fn calculate_prices(model: &Model, solution: &Solution, year: u32) -> Result<CommodityPrices> {
    // Compute shadow prices for all SED/SVD commodities (needed by all strategies)
    let shadow_prices = CommodityPrices::from_iter(solution.iter_commodity_balance_duals());

    // Partition commodities by pricing strategy
    let mut scarcity_set = HashSet::new();
    let mut marginal_set = HashSet::new();
    let mut fullcost_set = HashSet::new();
    let mut unpriced_set = HashSet::new();
    for (commodity_id, commodity) in &model.commodities {
        match commodity.pricing_strategy {
            PricingStrategy::ScarcityAdjusted => {
                scarcity_set.insert(commodity_id.clone());
            }
            PricingStrategy::MarginalCost => {
                marginal_set.insert(commodity_id.clone());
            }
            PricingStrategy::FullCost => {
                fullcost_set.insert(commodity_id.clone());
            }
            PricingStrategy::Unpriced => {
                unpriced_set.insert(commodity_id.clone());
            }
            PricingStrategy::Shadow => { /* nothing else required */ }
            PricingStrategy::Default => {
                unreachable!(
                    "Default pricing strategy should have been resolved during data loading"
                )
            }
        }
    }

    // Set up prices map
    // We will start with a clone of the shadow prices map and replace/remove values as needed
    let mut result = shadow_prices.clone();

    // Remove prices for unpriced commodities
    // For now, there should be no shadow prices for unpriced commodities, so let's panic if there
    // are any, just as a sanity check. In the future we may end up with shadow prices for unpriced
    // commodities, so this will need to be revisitied.
    for (commodity_id, region_id, time_slice) in shadow_prices.keys() {
        if unpriced_set.contains(commodity_id) {
            result.remove(commodity_id, region_id, time_slice);
            panic!("Shadow price found for unpriced commodity");
        }
    }

    // Update prices for scarcity-adjusted commodities
    // Any markets for which scarcity pricing cannot be calculated will retain their shadow prices
    if !scarcity_set.is_empty() {
        ensure!(
            model.parameters.allow_broken_options,
            "The `scarcity` pricing strategy is known to be broken. \
            Commodity prices may be incorrect if assets have more than one output commodity. \
            See: {ISSUES_URL}/677. \
            To run anyway, set the {ALLOW_BROKEN_OPTION_NAME} option to true."
        );
        let scarcity_prices = calculate_scarcity_adjusted_prices(
            solution.iter_activity_duals(),
            &shadow_prices,
            &scarcity_set,
        );
        result.update(scarcity_prices);
    }

    // Update prices for marginal cost commodities
    // Any markets for which marginal cost pricing cannot be calculated will retain their shadow prices
    if !marginal_set.is_empty() {
        let marginal_cost_prices = calculate_marginal_cost_prices(
            solution.iter_activity(),
            &shadow_prices,
            year,
            &marginal_set,
        );
        result.update(marginal_cost_prices);
    }

    // Update prices for full cost commodities
    // Any markets for which full cost pricing cannot be calculated will retain their shadow prices
    if !fullcost_set.is_empty() {
        let annual_activities = calculate_annual_activities(solution.iter_activity());
        let full_cost_prices = calculate_full_cost_prices(
            solution.iter_activity(),
            &annual_activities,
            &shadow_prices,
            year,
            &fullcost_set,
        );
        result.update(full_cost_prices);
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

    /// Update the prices map with the given iterator of entries
    ///
    /// If a price doesn't already exist for a given commodity/region/time slice, it will panic.
    pub fn update<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = ((CommodityID, RegionID, TimeSliceID), MoneyPerFlow)>,
    {
        for (key, price) in iter {
            let previous = self.0.insert(key, price);
            assert!(
                previous.is_some(),
                "Attempted to update price for non-existent commodity/region/time slice"
            );
        }
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

    /// Remove the specified entry from the map
    pub fn remove(
        &mut self,
        commodity_id: &CommodityID,
        region_id: &RegionID,
        time_slice: &TimeSliceID,
    ) -> Option<MoneyPerFlow> {
        self.0
            .remove(&(commodity_id.clone(), region_id.clone(), time_slice.clone()))
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
/// * `activity_duals` - Iterator over activity duals from optimisation solution
/// * `shadow_prices` - Shadow prices for all commodities
/// * `commodities_to_price` - Set of commodity IDs to calculate scarcity-adjusted prices for
fn calculate_scarcity_adjusted_prices<'a, I>(
    activity_duals: I,
    shadow_prices: &CommodityPrices,
    commodities_to_price: &HashSet<CommodityID>,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
{
    // Calculate highest activity dual for each commodity/region/time slice
    let mut highest_duals = HashMap::new();
    for (asset, time_slice, dual) in activity_duals {
        // Iterate over the output flows of this asset
        // Only consider flows for commodities we are pricing
        for flow in asset.iter_flows().filter(|flow| {
            flow.direction() == FlowDirection::Output
                && commodities_to_price.contains(&flow.commodity.id)
        }) {
            // Update the highest dual for this commodity/time slice
            highest_duals
                .entry((
                    flow.commodity.id.clone(),
                    asset.region_id().clone(),
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
/// # Arguments
/// * `activity_keys` - Iterator over activity keys from optimisation solution
/// * `shadow_prices` - Shadow prices for all commodities
/// * `year` - The year for which prices are being calculated
fn calculate_marginal_cost_prices<'a, I>(
    activity: I,
    shadow_prices: &CommodityPrices,
    year: u32,
    commodities_to_price: &HashSet<CommodityID>,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: IntoIterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
{
    // Calculate highest marginal cost for each commodity/region/time slice
    let mut highest_costs = HashMap::new();
    for (asset, time_slice, activity) in activity {
        // Skip candidate assets
        if asset.state() == &AssetState::Candidate {
            continue;
        }

        // Skip if activity is zero/very small
        if activity < Activity(f64::EPSILON) {
            continue;
        }

        // Get the marginal cost per unit of activity
        let marginal_cost_per_activity =
            asset.get_marginal_cost_per_activity(shadow_prices, year, time_slice);

        // Iterate over the output flows of this asset
        // Only consider flows for commodities we are pricing
        for flow in asset.iter_flows().filter(|flow| {
            flow.direction() == FlowDirection::Output
                && commodities_to_price.contains(&flow.commodity.id)
        }) {
            // Get the marginal cost per unit of flow for this output
            // FlowDirection::Output excludes zero-coeff outputs so no risk of division by zero
            let marginal_cost = marginal_cost_per_activity / flow.coeff;

            // Update the highest marginal cost for this commodity/time slice
            highest_costs
                .entry((
                    flow.commodity.id.clone(),
                    asset.region_id().clone(),
                    time_slice.clone(),
                ))
                .and_modify(|current_cost| {
                    if marginal_cost > *current_cost {
                        *current_cost = marginal_cost;
                    }
                })
                .or_insert(marginal_cost);
        }
    }

    highest_costs
}

/// Calculated annual activities for each asset by summing across all time slices
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
/// # Arguments
/// * `activity_keys` - Iterator over activity keys from optimisation solution
/// * `annual_activities` - Map of annual activities for each asset computed by `calculate_annual_activities`
/// * `shadow_prices` - Shadow prices for all commodities
/// * `year` - The year for which prices are being calculated
/// * `commodities_to_price` - Set of commodity IDs to calculate full cost prices for
fn calculate_full_cost_prices<'a, I>(
    activity: I,
    annual_activities: &HashMap<AssetRef, Activity>,
    shadow_prices: &CommodityPrices,
    year: u32,
    commodities_to_price: &HashSet<CommodityID>,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: IntoIterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
{
    // Calculate highest full cost for each commodity/region/time slice
    let mut highest_costs = HashMap::new();
    for (asset, time_slice, activity) in activity {
        // Skip candidate assets
        if asset.state() == &AssetState::Candidate {
            continue;
        }

        // Skip if activity is zero/very small
        if activity < Activity(f64::EPSILON) {
            continue;
        }

        // Get the full cost per unit of activity
        let full_cost_per_activity = asset.get_full_cost_per_activity(
            shadow_prices,
            year,
            time_slice,
            annual_activities[asset],
        );

        // Iterate over the output flows of this asset
        // Only consider flows for commodities we are pricing
        for flow in asset.iter_flows().filter(|flow| {
            flow.direction() == FlowDirection::Output
                && commodities_to_price.contains(&flow.commodity.id)
        }) {
            // Get the full cost per unit of flow for this output
            // FlowDirection::Output excludes zero-coeff outputs so no risk of division by zero
            let full_cost = full_cost_per_activity / flow.coeff;

            // Update the highest full cost for this commodity/time slice
            highest_costs
                .entry((
                    flow.commodity.id.clone(),
                    asset.region_id().clone(),
                    time_slice.clone(),
                ))
                .and_modify(|current_cost| {
                    if full_cost > *current_cost {
                        *current_cost = full_cost;
                    }
                })
                .or_insert(full_cost);
        }
    }

    highest_costs
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::CommodityID;
    use crate::fixture::{commodity_id, region_id, time_slice, time_slice_info};
    use crate::region::RegionID;
    use crate::time_slice::TimeSliceID;
    use rstest::rstest;

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
}
