//! Code for updating the simulation state.
use crate::asset::AssetRef;
use crate::commodity::{CommodityID, PricingStrategy};
use crate::model::Model;
use crate::process::FlowDirection;
use crate::region::RegionID;
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
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
pub fn calculate_prices(model: &Model, solution: &Solution, year: u32) -> CommodityPrices {
    // Get raw data needed to calculate prices
    let shadow_prices = CommodityPrices::from_iter(solution.iter_commodity_balance_duals());
    let highest_activity_duals = get_highest_activity_duals(solution.iter_activity_duals());
    let highest_marginal_costs =
        get_highest_marginal_costs(solution.iter_activity_keys(), &shadow_prices, year);
    let annual_activities = get_annual_activities(solution.iter_activity());
    let highest_full_costs = get_highest_full_costs(
        solution.iter_activity(),
        &annual_activities,
        &shadow_prices,
        year,
    );

    // Set up empty commodity prices map
    let mut prices = CommodityPrices::default();

    // Calculate prices for all markets/time slices for which we have a shadow price
    for (commodity_id, region_id, time_slice, shadow_price) in shadow_prices.iter() {
        let commodity = &model.commodities[commodity_id];
        let price = match commodity.pricing_strategy {
            PricingStrategy::ShadowPrices => shadow_price,
            PricingStrategy::ScarcityAdjusted => {
                // Get highest activity dual for this commodity/region/time slice
                let highest_dual = highest_activity_duals
                    [&(commodity_id.clone(), region_id.clone(), time_slice.clone())];

                // Add highest activity dual to shadow price
                // highest_dual is in units of MoneyPerActivity, but this is correct according to Adam
                shadow_price + MoneyPerFlow(highest_dual.value())
            }
            PricingStrategy::MarginalCost => {
                highest_marginal_costs
                    [&(commodity_id.clone(), region_id.clone(), time_slice.clone())]
            }
            PricingStrategy::FullCost => {
                highest_full_costs[&(commodity_id.clone(), region_id.clone(), time_slice.clone())]
            }
        };
        prices.insert(commodity_id, region_id, time_slice, price);
    }

    prices
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

fn get_highest_activity_duals<'a, I>(
    activity_duals: I,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerActivity>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
{
    // Calculate highest activity dual for each commodity/region/time slice
    let mut highest_duals = HashMap::new();
    for (asset, time_slice, dual) in activity_duals {
        // Iterate over all output flows
        for flow in asset
            .iter_flows()
            .filter(|flow| flow.direction() == FlowDirection::Output)
        {
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

    highest_duals
}

fn get_highest_marginal_costs<'a, I>(
    activity_keys: I,
    shadow_prices: &CommodityPrices,
    year: u32,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = &'a (AssetRef, TimeSliceID)>,
{
    // Calculate highest marginal cost for each commodity/region/time slice
    let mut highest_costs = HashMap::new();
    for (asset, time_slice) in activity_keys {
        // Skip assets that have no primary output
        let primary_output_id = match asset.primary_output() {
            Some(output) => &output.commodity.id,
            None => continue,
        };
        let marginal_cost =
            asset.get_marginal_cost_of_primary_output(shadow_prices, year, time_slice);

        // Update the highest marginal cost for this commodity/time slice
        highest_costs
            .entry((
                primary_output_id.clone(),
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

    highest_costs
}

fn get_annual_activities<'a, I>(activity: I) -> HashMap<AssetRef, Activity>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
{
    // Calculate annual activity for each asset
    let mut annual_activities = HashMap::new();
    for (asset, _time_slice, activity) in activity {
        annual_activities
            .entry(asset.clone())
            .and_modify(|existing_activity| {
                *existing_activity += activity;
            })
            .or_insert(activity);
    }

    annual_activities
}

fn get_highest_full_costs<'a, I>(
    activity: I,
    annual_activities: &HashMap<AssetRef, Activity>,
    shadow_prices: &CommodityPrices,
    year: u32,
) -> HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
{
    // Calculate highest full cost for each commodity/region/time slice
    let mut highest_costs = HashMap::new();
    for (asset, time_slice, _) in activity {
        // Skip assets that have no primary output
        let primary_output_id = match asset.primary_output() {
            Some(output) => &output.commodity.id,
            None => continue,
        };
        let full_cost = asset.get_full_cost_of_primary_output(
            shadow_prices,
            year,
            time_slice,
            annual_activities[asset],
        );

        // Update the highest full cost for this commodity/time slice
        highest_costs
            .entry((
                primary_output_id.clone(),
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
