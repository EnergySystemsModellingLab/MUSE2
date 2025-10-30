//! Code for updating the simulation state.
use crate::asset::AssetRef;
use crate::commodity::MarketID;
use crate::model::{Model, PricingStrategy};
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
use std::collections::{BTreeMap, HashMap, btree_map};

/// Calculate commodity prices.
///
/// Note that the behaviour will be different depending on the [`PricingStrategy`] the user has
/// selected.
///
/// # Arguments
///
/// * `model` - The model
/// * `solution` - Solution to dispatch optimisation
pub fn calculate_prices(model: &Model, solution: &Solution) -> CommodityPrices {
    let shadow_prices = CommodityPrices::from_iter(solution.iter_commodity_balance_duals());
    match model.parameters.pricing_strategy {
        // Use raw shadow prices
        PricingStrategy::ShadowPrices => shadow_prices,
        // Adjust prices for scarcity
        PricingStrategy::ScarcityAdjusted => shadow_prices
            .clone()
            .with_scarcity_adjustment(solution.iter_activity_duals()),
    }
}

/// A map relating commodity ID + region + time slice to current price (endogenous)
#[derive(Default, Clone)]
pub struct CommodityPrices(BTreeMap<(MarketID, TimeSliceID), MoneyPerFlow>);

impl CommodityPrices {
    /// Remove the impact of scarcity on prices.
    ///
    /// # Arguments
    ///
    /// * `activity_duals` - Value of activity duals from solution
    fn with_scarcity_adjustment<'a, I>(mut self, activity_duals: I) -> Self
    where
        I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
    {
        let highest_duals = get_highest_activity_duals(activity_duals);

        // Add the highest activity dual for each commodity/region/time slice to each commodity
        // balance dual
        for (key, highest) in &highest_duals {
            if let Some(price) = self.0.get_mut(key) {
                // highest is in units of MoneyPerActivity, but this is correct according to Adam
                *price += MoneyPerFlow(highest.value());
            }
        }

        self
    }

    /// Extend the prices map, possibly overwriting values
    pub fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = ((MarketID, TimeSliceID), MoneyPerFlow)>,
    {
        self.0.extend(iter);
    }

    /// Insert a price for the given commodity, region and time slice
    pub fn insert(&mut self, market: &MarketID, time_slice: &TimeSliceID, price: MoneyPerFlow) {
        let key = (market.clone(), time_slice.clone());
        self.0.insert(key, price);
    }

    /// Iterate over the map.
    ///
    /// # Returns
    ///
    /// An iterator of tuples containing commodity ID, region ID, time slice and price.
    pub fn iter(&self) -> impl Iterator<Item = (&MarketID, &TimeSliceID, MoneyPerFlow)> {
        self.0
            .iter()
            .map(|((market, ts), price)| (market, ts, *price))
    }

    /// Get the price for the specified commodity for a given region and time slice
    pub fn get(&self, market: &MarketID, time_slice: &TimeSliceID) -> Option<MoneyPerFlow> {
        self.0.get(&(market.clone(), time_slice.clone())).copied()
    }

    /// Iterate over the price map's keys
    pub fn keys(&self) -> btree_map::Keys<'_, (MarketID, TimeSliceID), MoneyPerFlow> {
        self.0.keys()
    }

    /// Remove the specified entry from the map
    pub fn remove(&mut self, market: &MarketID, time_slice: &TimeSliceID) -> Option<MoneyPerFlow> {
        self.0.remove(&(market.clone(), time_slice.clone()))
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
    ) -> HashMap<MarketID, MoneyPerFlow> {
        let mut weighted_prices = HashMap::new();

        for ((market, time_slice_id), price) in &self.0 {
            // NB: Time slice fractions will sum to one
            let weight = time_slice_info.time_slices[time_slice_id] / Year(1.0);
            *weighted_prices.entry(market.clone()).or_default() += *price * weight;
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

impl<'a> FromIterator<(&'a MarketID, &'a TimeSliceID, MoneyPerFlow)> for CommodityPrices {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (&'a MarketID, &'a TimeSliceID, MoneyPerFlow)>,
    {
        let map = iter
            .into_iter()
            .map(|(market, time_slice, price)| ((market.clone(), time_slice.clone()), price))
            .collect();
        CommodityPrices(map)
    }
}

impl IntoIterator for CommodityPrices {
    type Item = ((MarketID, TimeSliceID), MoneyPerFlow);
    type IntoIter = std::collections::btree_map::IntoIter<(MarketID, TimeSliceID), MoneyPerFlow>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn get_highest_activity_duals<'a, I>(
    activity_duals: I,
) -> HashMap<(MarketID, TimeSliceID), MoneyPerActivity>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
{
    // Calculate highest activity dual for each commodity/region/time slice
    let mut highest_duals = HashMap::new();
    for (asset, time_slice, dual) in activity_duals {
        // Iterate over all output flows
        for flow in asset.iter_flows().filter(|flow| flow.is_output()) {
            // Update the highest dual for this commodity/time slice
            highest_duals
                .entry((
                    MarketID::from((&flow.commodity.id, asset.region_id())),
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

    highest_duals
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{market_id, time_slice, time_slice_info};
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
        market_id: MarketID,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) {
        let mut prices1 = CommodityPrices::default();
        let mut prices2 = CommodityPrices::default();

        // Set up two price sets for a single market/timeslice
        prices1.insert(&market_id, &time_slice, price1);
        prices2.insert(&market_id, &time_slice, price2);

        assert_eq!(
            prices1.within_tolerance_weighted(&prices2, tolerance, &time_slice_info),
            expected
        );
    }

    #[rstest]
    fn test_time_slice_weighted_averages(
        market_id: MarketID,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) {
        let mut prices = CommodityPrices::default();

        // Insert a price
        prices.insert(&market_id, &time_slice, MoneyPerFlow(100.0));

        let averages = prices.time_slice_weighted_averages(&time_slice_info);

        // With single time slice (duration=1.0), average should equal the price
        assert_eq!(averages[&market_id], MoneyPerFlow(100.0));
    }
}
