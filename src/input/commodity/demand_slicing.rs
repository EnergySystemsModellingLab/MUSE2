//! Demand slicing determines how annual demand is distributed across the year.
use super::super::{check_values_sum_to_one_approx, input_err_msg, read_csv};
use crate::commodity::CommodityID;
use crate::id::IDCollection;
use crate::input::commodity::demand::BorrowedCommodityMap;
use crate::region::RegionID;
use crate::time_slice::{TimeSliceInfo, TimeSliceSelection};
use crate::units::Dimensionless;
use anyhow::{Context, Result, ensure};
use indexmap::IndexSet;
use itertools::{Itertools, iproduct};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

const DEMAND_SLICING_FILE_NAME: &str = "demand_slicing.csv";

#[derive(Clone, Deserialize)]
struct DemandSlice {
    commodity_id: String,
    region_id: String,
    time_slice: String,
    fraction: Dimensionless,
}

/// A map relating commodity, region and time slice selection to the fraction of annual demand
pub type DemandSliceMap = HashMap<(CommodityID, RegionID, TimeSliceSelection), Dimensionless>;

/// Read demand slices from specified model directory.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `svd_commodities` - Map of service demand commodities
/// * `region_ids` - All possible IDs for regions
/// * `commodity_regions` - Pairs of commodities + regions listed in demand CSV file
/// * `time_slice_info` - Information about seasons and times of day
pub fn read_demand_slices(
    model_dir: &Path,
    svd_commodities: &BorrowedCommodityMap,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
) -> Result<DemandSliceMap> {
    let file_path = model_dir.join(DEMAND_SLICING_FILE_NAME);
    let demand_slices_csv = read_csv(&file_path)?;
    read_demand_slices_from_iter(
        demand_slices_csv,
        svd_commodities,
        region_ids,
        time_slice_info,
    )
    .with_context(|| input_err_msg(file_path))
}

/// Read demand slices from an iterator
fn read_demand_slices_from_iter<I>(
    iter: I,
    svd_commodities: &BorrowedCommodityMap,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
) -> Result<DemandSliceMap>
where
    I: Iterator<Item = DemandSlice>,
{
    let mut demand_slices = DemandSliceMap::new();

    for slice in iter {
        let commodity = svd_commodities
            .get(slice.commodity_id.as_str())
            .with_context(|| {
                format!(
                    "Can only provide demand slice data for SVD commodities. Found entry for '{}'",
                    slice.commodity_id
                )
            })?;
        let region_id = region_ids.get_id(&slice.region_id)?;

        // We need to know how many time slices are covered by the current demand slice entry and
        // how long they are relative to one another so that we can divide up the demand for this
        // entry appropriately
        let ts_selection = time_slice_info.get_selection(&slice.time_slice)?;

        // Share demand between the time slice selections in proportion to duration
        let iter = time_slice_info
            .calculate_share(&ts_selection, commodity.time_slice_level, slice.fraction)
            .with_context(|| {
                format!(
                    "Cannot provide demand at {:?} level when commodity time slice level is {:?}",
                    ts_selection.level(),
                    commodity.time_slice_level
                )
            })?;
        for (ts_selection, demand_fraction) in iter {
            let existing = demand_slices
                .insert(
                    (
                        commodity.id.clone(),
                        region_id.clone(),
                        ts_selection.clone(),
                    ),
                    demand_fraction,
                )
                .is_some();
            ensure!(
                !existing,
                "Duplicate demand slicing entry (or same time slice covered by more than one entry) \
                (commodity: {}, region: {}, time slice(s): {})",
                commodity.id,
                region_id,
                ts_selection
            );
        }
    }

    validate_demand_slices(svd_commodities, region_ids, &demand_slices, time_slice_info)?;

    Ok(demand_slices)
}

/// Check that the [`DemandSliceMap`] is well formed.
///
/// Specifically, check:
///
/// * It is non-empty
/// * For every commodity + region pair, there must be entries covering every time slice
/// * The demand fractions for all entries related to a commodity + region pair sum to one
fn validate_demand_slices(
    svd_commodities: &BorrowedCommodityMap,
    region_ids: &IndexSet<RegionID>,
    demand_slices: &DemandSliceMap,
    time_slice_info: &TimeSliceInfo,
) -> Result<()> {
    for (commodity, region_id) in iproduct!(svd_commodities.values(), region_ids) {
        time_slice_info
            .iter_selections_at_level(commodity.time_slice_level)
            .map(|ts_selection| {
                demand_slices
                    .get(&(
                        commodity.id.clone(),
                        region_id.clone(),
                        ts_selection.clone(),
                    ))
                    .with_context(|| {
                        format!(
                            "Demand slice missing for time slice(s) '{}' (commodity: {}, region {})",
                            ts_selection, commodity.id, region_id
                        )
                    })
            })
            .process_results(|iter| {
                check_values_sum_to_one_approx(iter.copied()).context("Invalid demand fractions")
            })??;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{assert_error, get_svd_map, svd_commodity, time_slice_info};
    use crate::time_slice::TimeSliceID;
    use crate::units::Year;
    use rstest::{fixture, rstest};
    use std::iter;

    #[fixture]
    pub fn region_ids() -> IndexSet<RegionID> {
        IndexSet::from(["GBR".into()])
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_valid(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Valid
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "winter".into(),
            fraction: Dimensionless(1.0),
        };
        let time_slice = time_slice_info
            .get_time_slice_id_from_str("winter.day")
            .unwrap();
        let key = ("commodity1".into(), "GBR".into(), time_slice.into());
        let expected = DemandSliceMap::from_iter(iter::once((key, Dimensionless(1.0))));
        assert_eq!(
            read_demand_slices_from_iter(
                iter::once(demand_slice.clone()),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            )
            .unwrap(),
            expected
        );
    }

    fn demand_slice_entry(
        season: &str,
        time_of_day: &str,
        fraction: Dimensionless,
    ) -> ((CommodityID, RegionID, TimeSliceSelection), Dimensionless) {
        (
            (
                "commodity1".into(),
                "GBR".into(),
                TimeSliceID {
                    season: season.into(),
                    time_of_day: time_of_day.into(),
                }
                .into(),
            ),
            fraction,
        )
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_valid_multiple_time_slices(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Valid, multiple time slices
        let svd_commodities = get_svd_map(&svd_commodity);
        let time_slice_info = TimeSliceInfo {
            seasons: [("winter".into(), Year(0.5)), ("summer".into(), Year(0.5))]
                .into_iter()
                .collect(),
            times_of_day: ["day".into(), "night".into()].into_iter().collect(),
            time_slices: [
                (
                    TimeSliceID {
                        season: "summer".into(),
                        time_of_day: "day".into(),
                    },
                    Year(3.0 / 16.0),
                ),
                (
                    TimeSliceID {
                        season: "summer".into(),
                        time_of_day: "night".into(),
                    },
                    Year(5.0 / 16.0),
                ),
                (
                    TimeSliceID {
                        season: "winter".into(),
                        time_of_day: "day".into(),
                    },
                    Year(3.0 / 16.0),
                ),
                (
                    TimeSliceID {
                        season: "winter".into(),
                        time_of_day: "night".into(),
                    },
                    Year(5.0 / 16.0),
                ),
            ]
            .into_iter()
            .collect(),
        };
        let demand_slices = [
            DemandSlice {
                commodity_id: "commodity1".into(),
                region_id: "GBR".into(),
                time_slice: "winter".into(),
                fraction: Dimensionless(0.5),
            },
            DemandSlice {
                commodity_id: "commodity1".into(),
                region_id: "GBR".into(),
                time_slice: "summer".into(),
                fraction: Dimensionless(0.5),
            },
        ];

        let expected = DemandSliceMap::from_iter([
            demand_slice_entry("summer", "day", Dimensionless(3.0 / 16.0)),
            demand_slice_entry("summer", "night", Dimensionless(5.0 / 16.0)),
            demand_slice_entry("winter", "day", Dimensionless(3.0 / 16.0)),
            demand_slice_entry("winter", "night", Dimensionless(5.0 / 16.0)),
        ]);

        assert_eq!(
            read_demand_slices_from_iter(
                demand_slices.into_iter(),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            )
            .unwrap(),
            expected
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_empty_file(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Empty CSV file
        let svd_commodities = get_svd_map(&svd_commodity);
        assert_error!(
            read_demand_slices_from_iter(
                iter::empty(),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Demand slice missing for time slice(s) 'winter.day' (commodity: commodity1, region GBR)"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_bad_commodity(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Bad commodity
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity2".into(),
            region_id: "GBR".into(),
            time_slice: "winter.day".into(),
            fraction: Dimensionless(1.0),
        };
        assert_error!(
            read_demand_slices_from_iter(
                iter::once(demand_slice.clone()),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Can only provide demand slice data for SVD commodities. Found entry for 'commodity2'"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_bad_region(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Bad region
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "FRA".into(),
            time_slice: "winter.day".into(),
            fraction: Dimensionless(1.0),
        };
        assert_error!(
            read_demand_slices_from_iter(
                iter::once(demand_slice.clone()),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Unknown ID FRA found"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_bad_time_slice(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Bad time slice selection
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "summer".into(),
            fraction: Dimensionless(1.0),
        };
        assert_error!(
            read_demand_slices_from_iter(
                iter::once(demand_slice.clone()),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "'summer' is not a valid season"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_missing_time_slices(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Some time slices uncovered
        let svd_commodities = get_svd_map(&svd_commodity);
        let time_slice_info = TimeSliceInfo {
            seasons: [("winter".into(), Year(0.5)), ("summer".into(), Year(0.5))]
                .into_iter()
                .collect(),
            times_of_day: iter::once("day".into()).collect(),
            time_slices: [
                (
                    TimeSliceID {
                        season: "winter".into(),
                        time_of_day: "day".into(),
                    },
                    Year(0.5),
                ),
                (
                    TimeSliceID {
                        season: "summer".into(),
                        time_of_day: "day".into(),
                    },
                    Year(0.5),
                ),
            ]
            .into_iter()
            .collect(),
        };
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "winter".into(),
            fraction: Dimensionless(1.0),
        };
        assert_error!(
            read_demand_slices_from_iter(
                iter::once(demand_slice.clone()),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Demand slice missing for time slice(s) 'summer.day' (commodity: commodity1, region GBR)"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_duplicate_time_slice(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Same time slice twice
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "winter.day".into(),
            fraction: Dimensionless(0.5),
        };
        assert_error!(
            read_demand_slices_from_iter(
                iter::repeat_n(demand_slice.clone(), 2),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Duplicate demand slicing entry (or same time slice covered by more than one entry) \
                (commodity: commodity1, region: GBR, time slice(s): winter.day)"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_season_time_slice_conflict(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Whole season and single time slice conflicting
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "winter.day".into(),
            fraction: Dimensionless(0.5),
        };
        let demand_slice_season = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "winter".into(),
            fraction: Dimensionless(0.5),
        };
        assert_error!(
            read_demand_slices_from_iter(
                [demand_slice, demand_slice_season].into_iter(),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Duplicate demand slicing entry (or same time slice covered by more than one entry) \
                (commodity: commodity1, region: GBR, time slice(s): winter.day)"
        );
    }

    #[rstest]
    fn test_read_demand_slices_from_iter_invalid_bad_fractions(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        time_slice_info: TimeSliceInfo,
    ) {
        // Fractions don't sum to one
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand_slice = DemandSlice {
            commodity_id: "commodity1".into(),
            region_id: "GBR".into(),
            time_slice: "winter".into(),
            fraction: Dimensionless(0.5),
        };
        assert_error!(
            read_demand_slices_from_iter(
                iter::once(demand_slice),
                &svd_commodities,
                &region_ids,
                &time_slice_info,
            ),
            "Invalid demand fractions"
        );
    }
}
