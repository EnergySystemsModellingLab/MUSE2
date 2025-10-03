//! Code for updating the simulation state.
use crate::asset::{Asset, AssetRef};
use crate::commodity::CommodityID;
use crate::model::{Model, PricingStrategy};
use crate::process::ProcessFlow;
use crate::region::RegionID;
use crate::simulation::optimisation::Solution;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Dimensionless, MoneyPerActivity, MoneyPerFlow, Year};
use indexmap::IndexMap;
use itertools::iproduct;
use std::collections::{BTreeMap, HashMap};

/// A map of reduced costs for different assets in different time slices
///
/// This is the system cost associated with one unit of activity (`MoneyPerActivity`) for each asset
/// in each time slice.
///
/// For candidate assets this is calculated directly from the activity variable duals.
///
/// For existing assets this is calculated from the operating cost and the revenue from flows.
///
/// These may be used in the investment algorithm, depending on the appraisal method, to compare the
/// cost effectiveness of different potential investment decisions.
#[derive(Default, Clone)]
pub struct ReducedCosts(IndexMap<(AssetRef, TimeSliceID), MoneyPerActivity>);

impl ReducedCosts {
    /// Get the reduced cost for the specified asset and time slice
    ///
    /// If no reduced cost is found for the asset, the reduced cost is returned for the relevant
    /// candidate asset. This can occur the first year an asset is commissioned, or if an asset
    /// was not selected in an earlier iteration of the ironing out loop.
    pub fn get(&self, asset: &AssetRef, time_slice: &TimeSliceID) -> MoneyPerActivity {
        *self
            .0
            .get(&(asset.clone(), time_slice.clone()))
            .unwrap_or_else(|| {
                &self.0[&(
                    Asset::new_candidate_from_commissioned(asset).into(),
                    time_slice.clone(),
                )]
            })
    }

    /// Extend the reduced costs map
    pub fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = ((AssetRef, TimeSliceID), MoneyPerActivity)>,
    {
        self.0.extend(iter);
    }

    /// Iterate over the map
    pub fn iter(&self) -> impl Iterator<Item = (&(AssetRef, TimeSliceID), &MoneyPerActivity)> {
        self.0.iter()
    }

    /// Iterate mutably over the map
    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (&(AssetRef, TimeSliceID), &mut MoneyPerActivity)> {
        self.0.iter_mut()
    }
}

impl FromIterator<((AssetRef, TimeSliceID), MoneyPerActivity)> for ReducedCosts {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = ((AssetRef, TimeSliceID), MoneyPerActivity)>,
    {
        ReducedCosts(iter.into_iter().collect())
    }
}

impl IntoIterator for ReducedCosts {
    type Item = ((AssetRef, TimeSliceID), MoneyPerActivity);
    type IntoIter = indexmap::map::IntoIter<(AssetRef, TimeSliceID), MoneyPerActivity>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<IndexMap<(AssetRef, TimeSliceID), MoneyPerActivity>> for ReducedCosts {
    fn from(map: IndexMap<(AssetRef, TimeSliceID), MoneyPerActivity>) -> Self {
        ReducedCosts(map)
    }
}

/// Calculate commodity prices and reduced costs for assets.
///
/// Note that the behaviour will be different depending on the [`PricingStrategy`] the user has
/// selected.
///
/// # Arguments
///
/// * `model` - The model
/// * `solution` - Solution to dispatch optimisation
/// * `existing_assets` - Existing assets
/// * `year` - Current milestone year
pub fn calculate_prices_and_reduced_costs(
    model: &Model,
    solution: &Solution,
    existing_assets: &[AssetRef],
    year: u32,
) -> (CommodityPrices, ReducedCosts) {
    let mut prices = CommodityPrices::default();
    let mut reduced_costs = ReducedCosts::default();

    let shadow_prices = CommodityPrices::from_iter(solution.iter_commodity_balance_duals());
    let reduced_costs_for_candidates: ReducedCosts = solution
        .iter_reduced_costs_for_candidates()
        .map(|(asset, time_slice, cost)| ((asset.clone(), time_slice.clone()), cost))
        .collect();

    let (new_prices, reduced_costs_for_candidates) = match model.parameters.pricing_strategy {
        // Use raw shadow prices and reduced costs
        PricingStrategy::ShadowPrices => (
            shadow_prices.with_levies(model, year),
            reduced_costs_for_candidates,
        ),
        // Adjust prices for scarcity and then remove this adjustment from reduced costs
        PricingStrategy::ScarcityAdjusted => {
            let adjusted_prices = shadow_prices
                .clone()
                .with_scarcity_adjustment(solution.iter_activity_duals())
                .with_levies(model, year);
            let unadjusted_prices = shadow_prices.with_levies(model, year);
            let mut reduced_costs_for_candidates = reduced_costs_for_candidates;

            // Remove adjustment
            remove_scarcity_influence_from_candidate_reduced_costs(
                &mut reduced_costs_for_candidates,
                &adjusted_prices,
                &unadjusted_prices,
            );

            (adjusted_prices, reduced_costs_for_candidates)
        }
    };

    // Use old prices for any commodities for which price is missing
    prices.extend(new_prices);

    // Add new reduced costs, using old values if not provided
    reduced_costs.extend(reduced_costs_for_candidates);
    reduced_costs.extend(reduced_costs_for_existing(
        &model.time_slice_info,
        existing_assets,
        &prices,
        year,
    ));

    (prices, reduced_costs)
}

/// A map relating commodity ID + region + time slice to current price (endogenous)
#[derive(Default, Clone)]
pub struct CommodityPrices(BTreeMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>);

impl CommodityPrices {
    /// Add prices based on levies/incentives.
    ///
    /// If a commodity already has a price based on the previous dual-based calculation, we choose
    /// the higher of the two.
    ///
    /// # Arguments
    ///
    /// * `model` - The model
    /// * `year` - The milestone year of interest
    fn with_levies(mut self, model: &Model, year: u32) -> Self {
        for (region_id, time_slice) in
            iproduct!(model.iter_regions(), model.time_slice_info.iter_ids())
        {
            let levy_key = (region_id.clone(), year, time_slice.clone());
            for commodity in model.commodities.values() {
                if let Some(levy) = commodity.levies.get(&levy_key) {
                    let key = (commodity.id.clone(), region_id.clone(), time_slice.clone());
                    self.0
                        .entry(key)
                        .and_modify(|price| *price = price.max(levy.value))
                        .or_insert(levy.value);
                }
            }
        }

        self
    }

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
        T: IntoIterator<Item = ((CommodityID, RegionID, TimeSliceID), MoneyPerFlow)>,
    {
        self.0.extend(iter);
    }

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
                .and_modify(|sum| *sum += *price * weight)
                .or_insert(*price * weight);
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
        for flow in asset.iter_flows().filter(|flow| flow.is_output()) {
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

/// Remove the effect of scarcity on candidate assets' reduced costs
fn remove_scarcity_influence_from_candidate_reduced_costs(
    reduced_costs: &mut ReducedCosts,
    adjusted_prices: &CommodityPrices,
    unadjusted_prices: &CommodityPrices,
) {
    for ((asset, time_slice), cost) in reduced_costs.iter_mut() {
        *cost += asset
            .iter_flows()
            .map(|flow| {
                get_scarcity_adjustment(
                    flow,
                    asset.region_id(),
                    time_slice,
                    adjusted_prices,
                    unadjusted_prices,
                )
            })
            .sum();
    }
}

/// Get the scarcity adjustment for the given flow/region/time slice combination.
///
/// The return value may be negative.
fn get_scarcity_adjustment(
    flow: &ProcessFlow,
    region_id: &RegionID,
    time_slice: &TimeSliceID,
    adjusted_prices: &CommodityPrices,
    unadjusted_prices: &CommodityPrices,
) -> MoneyPerActivity {
    let adjusted = adjusted_prices
        .get(&flow.commodity.id, region_id, time_slice)
        .expect("No adjusted price found");
    let unadjusted = unadjusted_prices
        .get(&flow.commodity.id, region_id, time_slice)
        .expect("No unadjusted price found");
    flow.coeff * (unadjusted - adjusted)
}

/// Calculate reduced costs for existing assets
fn reduced_costs_for_existing<'a>(
    time_slice_info: &'a TimeSliceInfo,
    assets: &'a [AssetRef],
    prices: &'a CommodityPrices,
    year: u32,
) -> impl Iterator<Item = ((AssetRef, TimeSliceID), MoneyPerActivity)> + 'a {
    iproduct!(assets, time_slice_info.iter_ids()).map(move |(asset, time_slice)| {
        let operating_cost = asset.get_operating_cost(year, time_slice);
        let revenue_from_flows = asset
            .iter_flows()
            .map(|flow| {
                flow.coeff
                    * prices
                        .get(&flow.commodity.id, asset.region_id(), time_slice)
                        .unwrap()
            })
            .sum();
        let reduced_cost = operating_cost - revenue_from_flows;

        ((asset.clone(), time_slice.clone()), reduced_cost)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::CommodityID;
    use crate::fixture::{
        asset, assets, commodity_id, process, region_id, time_slice, time_slice_info,
    };
    use crate::process::Process;
    use crate::region::RegionID;
    use crate::time_slice::TimeSliceID;
    use indexmap::indexmap;
    use rstest::rstest;

    #[rstest]
    fn test_get_reduced_cost(process: Process, time_slice: TimeSliceID) {
        let asset_pool = assets(asset(process));
        let asset = asset_pool.as_slice().first().unwrap();

        // Create reduced costs with only the candidate version
        let candidate = Asset::new_candidate_from_commissioned(asset);
        let mut reduced_costs = ReducedCosts::from(indexmap! {
            (candidate.into(), time_slice.clone()) => MoneyPerActivity(42.0)
        });

        // Should fallback to candidate when asset not found
        let result = reduced_costs.get(asset, &time_slice);
        assert_eq!(result, MoneyPerActivity(42.0));

        // Add a reduced cost for the asset
        reduced_costs.extend(indexmap! {
            (asset.clone(), time_slice.clone()) => MoneyPerActivity(100.0)
        });

        // Now should return the asset's reduced cost
        let result = reduced_costs.get(asset, &time_slice);
        assert_eq!(result, MoneyPerActivity(100.0));
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
}
