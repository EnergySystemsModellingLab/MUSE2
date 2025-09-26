//! Code for reading in the commodity levies CSV file.
use super::super::{input_err_msg, read_csv, try_insert};
use crate::commodity::{BalanceType, CommodityID, CommodityLevy, CommodityLevyMap};
use crate::id::IDCollection;
use crate::region::{RegionID, parse_region_str};
use crate::time_slice::TimeSliceInfo;
use crate::units::MoneyPerFlow;
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use indexmap::IndexSet;
use log::warn;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

const COMMODITY_LEVIES_FILE_NAME: &str = "commodity_levies.csv";

/// Cost parameters for each commodity
#[derive(PartialEq, Debug, Deserialize, Clone)]
struct CommodityLevyRaw {
    /// Unique identifier for the commodity (e.g. "ELC")
    commodity_id: String,
    /// The region(s) to which the levy applies.
    regions: String,
    /// Type of balance for application of cost.
    balance_type: BalanceType,
    /// The year(s) to which the cost applies.
    years: String,
    /// The time slice to which the cost applies.
    time_slice: String,
    /// Cost per unit commodity
    value: MoneyPerFlow,
}

/// Read costs associated with each commodity from levies CSV file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `commodity_ids` - All possible commodity IDs
/// * `region_ids` - All possible region IDs
/// * `time_slice_info` - Information about time slices
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// A map containing levies, grouped by commodity ID.
pub fn read_commodity_levies(
    model_dir: &Path,
    commodity_ids: &IndexSet<CommodityID>,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<HashMap<CommodityID, CommodityLevyMap>> {
    let file_path = model_dir.join(COMMODITY_LEVIES_FILE_NAME);
    let commodity_levies_csv = read_csv::<CommodityLevyRaw>(&file_path)?;
    read_commodity_levies_iter(
        commodity_levies_csv,
        commodity_ids,
        region_ids,
        time_slice_info,
        milestone_years,
    )
    .with_context(|| input_err_msg(&file_path))
}

/// Read costs associated with each commodity from an iterator over raw cost entries.
///
/// # Arguments
///
/// * `iter` - An iterator over raw commodity levy entries
/// * `commodity_ids` - All possible commodity IDs
/// * `region_ids` - All possible region IDs
/// * `time_slice_info` - Information about time slices
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// A map containing levies, grouped by commodity ID.
fn read_commodity_levies_iter<I>(
    iter: I,
    commodity_ids: &IndexSet<CommodityID>,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<HashMap<CommodityID, CommodityLevyMap>>
where
    I: Iterator<Item = CommodityLevyRaw>,
{
    let mut map = HashMap::new();

    // Keep track of commodity/region combinations specified. We will check that all years and
    // time slices are covered for each commodity/region combination.
    let mut commodity_regions: HashMap<CommodityID, IndexSet<RegionID>> = HashMap::new();

    for cost in iter {
        let commodity_id = commodity_ids.get_id(&cost.commodity_id)?;
        let regions = parse_region_str(&cost.regions, region_ids)?;
        let years = parse_year_str(&cost.years, milestone_years.iter().copied())?;
        let ts_selection = time_slice_info.get_selection(&cost.time_slice)?;

        // Get or create CommodityLevyMap for this commodity
        let map = map
            .entry(commodity_id.clone())
            .or_insert_with(CommodityLevyMap::new);

        // Create CommodityLevy
        let cost = CommodityLevy {
            balance_type: cost.balance_type,
            value: cost.value,
        };

        // Insert cost into map for each region/year/time slice
        for region in &regions {
            commodity_regions
                .entry(commodity_id.clone())
                .or_default()
                .insert(region.clone());
            for year in &years {
                for (time_slice, _) in ts_selection.iter(time_slice_info) {
                    try_insert(
                        map,
                        &(region.clone(), *year, time_slice.clone()),
                        cost.clone(),
                    )?;
                }
            }
        }
    }

    // Validate map and complete with missing regions/years/time slices
    for (commodity_id, regions) in &commodity_regions {
        let map = map.get_mut(commodity_id).unwrap();
        validate_commodity_levy_map(map, regions, milestone_years, time_slice_info)
            .with_context(|| format!("Missing costs for commodity {commodity_id}"))?;

        for region_id in region_ids.difference(regions) {
            add_missing_region_to_commodity_levy_map(
                map,
                region_id,
                milestone_years,
                time_slice_info,
            );
            warn!(
                "No levy specified for commodity {commodity_id} in region {region_id}. Assuming zero levy."
            );
        }
    }

    Ok(map)
}

/// Add missing region to commodity levy map with zero cost for all years and time slices.
///
/// # Arguments
///
/// * `map` - The commodity levy map to update
/// * `region_id` - The region ID to add
/// * `milestone_years` - All milestone years
/// * `time_slice_info` - Information about time slices
fn add_missing_region_to_commodity_levy_map(
    map: &mut CommodityLevyMap,
    region_id: &RegionID,
    milestone_years: &[u32],
    time_slice_info: &TimeSliceInfo,
) {
    for year in milestone_years {
        for time_slice in time_slice_info.iter_ids() {
            map.insert(
                (region_id.clone(), *year, time_slice.clone()),
                CommodityLevy {
                    balance_type: BalanceType::Net,
                    value: MoneyPerFlow(0.0),
                },
            );
        }
    }
}

/// Validate that the commodity levy map contains entries for all regions, years and time slices.
///
/// # Arguments
///
/// * `map` - The commodity levy map to validate
/// * `regions` - The set of regions that should be covered
/// * `milestone_years` - All milestone years
/// * `time_slice_info` - Information about time slices
///
/// # Returns
///
/// Nothing if the map is valid. An error if the map is missing any entries.
fn validate_commodity_levy_map(
    map: &CommodityLevyMap,
    regions: &IndexSet<RegionID>,
    milestone_years: &[u32],
    time_slice_info: &TimeSliceInfo,
) -> Result<()> {
    // Check that all regions, years and time slices are covered
    for region_id in regions {
        for year in milestone_years {
            for time_slice in time_slice_info.iter_ids() {
                ensure!(
                    map.contains_key(&(region_id.clone(), *year, time_slice.clone())),
                    "Missing cost for region {region_id}, year {year}, time slice {time_slice}"
                );
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{assert_error, region_id, time_slice, time_slice_info};
    use crate::time_slice::TimeSliceID;
    use crate::units::Year;
    use rstest::{fixture, rstest};

    #[fixture]
    fn region_ids(region_id: RegionID) -> IndexSet<RegionID> {
        IndexSet::from([region_id])
    }

    #[fixture]
    fn cost_map(time_slice: TimeSliceID) -> CommodityLevyMap {
        let cost = CommodityLevy {
            balance_type: BalanceType::Net,
            value: MoneyPerFlow(1.0),
        };

        let mut map = CommodityLevyMap::new();
        map.insert(("GBR".into(), 2020, time_slice.clone()), cost.clone());
        map
    }

    #[rstest]
    fn test_validate_commodity_levies_map_valid(
        cost_map: CommodityLevyMap,
        time_slice_info: TimeSliceInfo,
        region_ids: IndexSet<RegionID>,
    ) {
        // Valid map
        assert!(
            validate_commodity_levy_map(&cost_map, &region_ids, &[2020], &time_slice_info).is_ok()
        );
    }

    #[rstest]
    fn test_validate_commodity_levies_map_invalid_missing_region(
        cost_map: CommodityLevyMap,
        time_slice_info: TimeSliceInfo,
    ) {
        // Missing region
        let region_ids = IndexSet::from(["GBR".into(), "FRA".into()]);
        assert_error!(
            validate_commodity_levy_map(&cost_map, &region_ids, &[2020], &time_slice_info),
            "Missing cost for region FRA, year 2020, time slice winter.day"
        );
    }

    #[rstest]
    fn test_validate_commodity_levies_map_invalid_missing_year(
        cost_map: CommodityLevyMap,
        time_slice_info: TimeSliceInfo,
        region_ids: IndexSet<RegionID>,
    ) {
        // Missing year
        assert_error!(
            validate_commodity_levy_map(&cost_map, &region_ids, &[2020, 2030], &time_slice_info),
            "Missing cost for region GBR, year 2030, time slice winter.day"
        );
    }

    #[rstest]
    fn test_validate_commodity_levies_map_invalid(
        cost_map: CommodityLevyMap,
        region_ids: IndexSet<RegionID>,
    ) {
        // Missing time slice
        let time_slice = TimeSliceID {
            season: "winter".into(),
            time_of_day: "night".into(),
        };
        let time_slice_info = TimeSliceInfo {
            seasons: [("winter".into(), Year(1.0))].into(),
            times_of_day: ["day".into(), "night".into()].into(),
            time_slices: [
                (time_slice.clone(), Year(0.5)),
                (time_slice.clone(), Year(0.5)),
            ]
            .into(),
        };
        assert_error!(
            validate_commodity_levy_map(&cost_map, &region_ids, &[2020], &time_slice_info),
            "Missing cost for region GBR, year 2020, time slice winter.night"
        );
    }

    #[rstest]
    fn test_add_missing_region_to_commodity_levy_map(
        cost_map: CommodityLevyMap,
        time_slice_info: TimeSliceInfo,
        region_id: RegionID,
    ) {
        let mut cost_map = cost_map;
        add_missing_region_to_commodity_levy_map(
            &mut cost_map,
            &region_id,
            &[2020],
            &time_slice_info,
        );

        // Check that costs have been added for the new region
        for time_slice in time_slice_info.iter_ids() {
            assert_eq!(
                cost_map.get(&(region_id.clone(), 2020, time_slice.clone())),
                Some(&CommodityLevy {
                    balance_type: BalanceType::Net,
                    value: MoneyPerFlow(0.0)
                })
            );
        }
    }
}
