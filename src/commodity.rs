//! Commodities are substances or forms of energy that can be produced and consumed by processes.
use crate::id::{define_id_getter, define_id_type};
use crate::region::RegionID;
use crate::time_slice::{TimeSliceID, TimeSliceLevel, TimeSliceSelection};
use crate::units::{Flow, MoneyPerFlow};
use indexmap::IndexMap;
use serde::Deserialize;
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::HashMap;
use std::rc::Rc;

define_id_type! {CommodityID}

/// A map of [`Commodity`]s, keyed by commodity ID
pub type CommodityMap = IndexMap<CommodityID, Rc<Commodity>>;

/// A map of [`CommodityLevy`]s, keyed by region ID, year and time slice ID
pub type CommodityLevyMap = HashMap<(RegionID, u32, TimeSliceID), CommodityLevy>;

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
    /// Levies for this commodity for different combinations of region, year and time slice.
    ///
    /// May be empty if there are no levies for this commodity, otherwise there must be entries for
    /// every combination of parameters. Note that these values can be negative, indicating an
    /// incentive.
    #[serde(skip)]
    pub levies: CommodityLevyMap,
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
#[derive(PartialEq, Clone, Debug, DeserializeLabeledStringEnum)]
pub enum BalanceType {
    /// Applies to both consumption and production
    #[string = "net"]
    Net,
    /// Applies to consumption only
    #[string = "cons"]
    Consumption,
    /// Applies to production only
    #[string = "prod"]
    Production,
}

/// Represents a tax or other external cost on a commodity, as specified in input data.
///
/// For example, a CO2 price could be specified in input data to be applied to net CO2. Note that
/// the value can also be negative, indicating an incentive.
#[derive(PartialEq, Clone, Debug)]
pub struct CommodityLevy {
    /// Type of balance for application of cost
    pub balance_type: BalanceType,
    /// Cost per unit commodity
    pub value: MoneyPerFlow,
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
        let value = CommodityLevy {
            balance_type: BalanceType::Consumption,
            value: MoneyPerFlow(0.5),
        };
        let mut map = CommodityLevyMap::new();
        assert!(
            map.insert(("GBR".into(), 2010, ts.clone()), value.clone())
                .is_none()
        );
        assert_eq!(map[&("GBR".into(), 2010, ts)], value);
    }
}
