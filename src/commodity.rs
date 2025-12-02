//! Commodities are substances or forms of energy that can be produced and consumed by processes.
use crate::id::{define_id_getter, define_id_type};
use crate::region::RegionID;
use crate::time_slice::{TimeSliceID, TimeSliceLevel, TimeSliceSelection};
use crate::units::{Flow, MoneyPerFlow};
use indexmap::IndexMap;
use serde::de::Error;
use serde::de::value::StrDeserializer;
use serde::{Deserialize, Deserializer};
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::HashMap;
use std::rc::Rc;

define_id_type! {CommodityID}

/// A map of [`Commodity`]s, keyed by commodity ID
pub type CommodityMap = IndexMap<CommodityID, Rc<Commodity>>;

/// A map of [`MoneyPerFlow`]s, keyed by region ID, year and time slice ID for a specific levy
pub type CommodityLevyMap = HashMap<(RegionID, u32, TimeSliceID), MoneyPerFlow>;

/// A map of demand values, keyed by region ID, year and time slice selection
pub type DemandMap = HashMap<(RegionID, u32, TimeSliceSelection), Flow>;

/// A commodity within the simulation.
///
/// Represents a substance (e.g. CO2) or form of energy (e.g. electricity) that can be produced or
/// consumed by processes.
#[derive(PartialEq, Debug, Deserialize, Clone)]
pub struct Commodity {
    /// Unique identifier for the commodity (e.g. "ELC")
    pub id: CommodityID,
    /// Text description of commodity (e.g. "electricity")
    pub description: String,
    /// Commodity balance type
    #[serde(rename = "type")] // NB: we can't name a field type as it's a reserved keyword
    pub kind: CommodityType,
    /// The time slice level for commodity balance
    pub time_slice_level: TimeSliceLevel,
    /// Defines the strategy used for calculating commodity prices
    #[serde(deserialize_with = "deserialize_pricing_strategy")]
    pub pricing_strategy: PricingStrategy,
    /// Production levies for this commodity for different combinations of region, year and time slice.
    ///
    /// May be empty if there are no production levies for this commodity, otherwise there must be
    /// entries for every combination of parameters. Note that these values can be negative,
    /// indicating an incentive.
    #[serde(skip)]
    pub levies_prod: CommodityLevyMap,
    /// Consumption levies for this commodity for different combinations of region, year and time slice.
    ///
    /// May be empty if there are no consumption levies for this commodity, otherwise there must be
    /// entries for every combination of parameters. Note that these values can be negative,
    /// indicating an incentive.
    #[serde(skip)]
    pub levies_cons: CommodityLevyMap,
    /// Demand as defined in input files. Will be empty for non-service-demand commodities.
    ///
    /// The [`TimeSliceSelection`] part of the key is always at the same [`TimeSliceLevel`] as the
    /// `time_slice_level` field. E.g. if the `time_slice_level` is seasonal, then there will be
    /// keys representing each season (and not e.g. individual time slices).
    #[serde(skip)]
    pub demand: DemandMap,
}
define_id_getter! {Commodity, CommodityID}

/// Type of balance for application of cost
#[derive(Eq, PartialEq, Clone, Debug, DeserializeLabeledStringEnum, Hash)]
pub enum BalanceType {
    /// Applies to production, with an equal and opposite levy/incentive on consumption
    #[string = "net"]
    Net,
    /// Applies to consumption only
    #[string = "cons"]
    Consumption,
    /// Applies to production only
    #[string = "prod"]
    Production,
}

/// Commodity balance type
#[derive(PartialEq, Debug, DeserializeLabeledStringEnum, Clone)]
pub enum CommodityType {
    /// Supply and demand of this commodity must be balanced
    #[string = "sed"]
    SupplyEqualsDemand,
    /// Specifies a demand (specified in input files) which must be met by the simulation
    #[string = "svd"]
    ServiceDemand,
    /// Either an input or an output to the simulation.
    ///
    /// This represents a commodity which can either be produced or consumed, but not both.
    #[string = "oth"]
    Other,
}

/// The strategy used for calculating commodity prices
#[derive(Debug, PartialEq, Clone, DeserializeLabeledStringEnum)]
pub enum PricingStrategy {
    /// Take commodity prices directly from the shadow prices
    #[string = "shadow_prices"]
    ShadowPrices,
    /// Adjust shadow prices for scarcity
    #[string = "scarcity_adjusted"]
    ScarcityAdjusted,
}

fn deserialize_pricing_strategy<'de, D>(deserializer: D) -> Result<PricingStrategy, D::Error>
where
    D: Deserializer<'de>,
{
    let s: Option<String> = Option::deserialize(deserializer)?;
    match s.as_deref() {
        None | Some("") => Ok(PricingStrategy::ShadowPrices),
        Some(other) => PricingStrategy::deserialize(StrDeserializer::<D::Error>::new(other)).map_err(|_| {
            D::Error::custom(format!(
                "Invalid pricing_strategy '{other}'; Expected 'shadow_prices' or 'scarcity_adjusted'"
            ))
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::time_slice::TimeSliceSelection;

    #[test]
    fn test_demand_map() {
        let ts_selection = TimeSliceSelection::Single(TimeSliceID {
            season: "all-year".into(),
            time_of_day: "all-day".into(),
        });
        let value = Flow(0.25);
        let mut map = DemandMap::new();
        map.insert(("North".into(), 2020, ts_selection.clone()), value);

        assert_eq!(map[&("North".into(), 2020, ts_selection)], value)
    }

    #[test]
    fn test_commodity_levy_map() {
        let ts = TimeSliceID {
            season: "winter".into(),
            time_of_day: "day".into(),
        };
        let value = MoneyPerFlow(0.5);
        let mut map = CommodityLevyMap::new();
        assert!(
            map.insert(("GBR".into(), 2010, ts.clone()), value.clone())
                .is_none()
        );
        assert_eq!(map[&("GBR".into(), 2010, ts)], value);
    }
}
