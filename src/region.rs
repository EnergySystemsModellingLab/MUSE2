//! Regions represent different geographical areas in which agents, processes, etc. are active.
use crate::id::{IDCollection, define_id_getter, define_id_type};
use anyhow::{Result, ensure};
use indexmap::{IndexMap, IndexSet};
use serde::Deserialize;

define_id_type! {RegionID}

/// A map of [`Region`]s, keyed by region ID
pub type RegionMap = IndexMap<RegionID, Region>;

/// Represents a region with an ID and a longer description.
#[derive(Debug, Deserialize, PartialEq)]
pub struct Region {
    /// A unique identifier for a region (e.g. "GBR").
    pub id: RegionID,
    /// A text description of the region (e.g. "United Kingdom").
    pub description: String,
}
define_id_getter! {Region, RegionID}

/// Parse a string of regions separated by semicolons into a vector of `RegionID`.
///
/// The string can be either "all" (case-insensitive), a single region, or a semicolon-separated
/// list of regions (e.g. "GBR;FRA;USA" or "GBR; FRA; USA")
pub fn parse_region_str(s: &str, region_ids: &IndexSet<RegionID>) -> Result<IndexSet<RegionID>> {
    let s = s.trim();
    ensure!(!s.is_empty(), "No regions provided");

    if s.eq_ignore_ascii_case("all") {
        return Ok(region_ids.clone());
    }

    s.split(';')
        .map(|y| Ok(region_ids.get_id(y.trim())?.clone()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_region_str_works() {
        let region_ids: IndexSet<RegionID> = ["GBR".into(), "USA".into()].into_iter().collect();

        // List of regions
        let parsed = parse_region_str("GBR;USA", &region_ids).unwrap();
        assert_eq!(parsed.len(), 2);
        assert!(parsed.contains(&RegionID::from("GBR")));
        assert!(parsed.contains(&RegionID::from("USA")));

        // All regions
        let parsed = parse_region_str("all", &region_ids).unwrap();
        assert_eq!(parsed.len(), 2);
        assert!(parsed.contains(&RegionID::from("GBR")));
        assert!(parsed.contains(&RegionID::from("USA")));

        // Single region
        let parsed = parse_region_str("GBR", &region_ids).unwrap();
        assert_eq!(parsed.len(), 1);
        assert!(parsed.contains(&RegionID::from("GBR")));

        // Empty string
        let result = parse_region_str("", &region_ids);
        assert!(result.is_err());

        // Invalid region
        let result = parse_region_str("GBR;INVALID", &region_ids);
        assert!(result.is_err());
    }
}
