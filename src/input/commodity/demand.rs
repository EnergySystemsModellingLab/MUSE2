//! Code for working with demand for a given commodity. Demand can vary by region, year and time
//! slice.
use super::super::{format_items_with_cap, input_err_msg, read_csv};
use super::demand_slicing::{DemandSliceMap, read_demand_slices};
use crate::commodity::{Commodity, CommodityID, CommodityType, DemandMap};
use crate::id::IDCollection;
use crate::region::RegionID;
use crate::time_slice::{TimeSliceInfo, TimeSliceLevel};
use crate::units::Flow;
use anyhow::{Context, Result, ensure};
use indexmap::{IndexMap, IndexSet};
use itertools::iproduct;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

const DEMAND_FILE_NAME: &str = "demand.csv";

/// Represents a single demand entry in the dataset.
#[allow(clippy::struct_field_names)]
#[derive(Debug, Clone, Deserialize, PartialEq)]
struct Demand {
    /// The commodity this demand entry refers to
    commodity_id: String,
    /// The region of the demand entry
    region_id: String,
    /// The year of the demand entry
    year: u32,
    /// Annual demand quantity
    demand: Flow,
}

/// A map relating commodity, region and year to annual demand
pub type AnnualDemandMap = HashMap<(CommodityID, RegionID, u32), (TimeSliceLevel, Flow)>;

/// A map containing a references to commodities
pub type BorrowedCommodityMap<'a> = HashMap<CommodityID, &'a Commodity>;

/// Reads demand data from CSV files.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `commodity_ids` - All possible IDs of commodities
/// * `region_ids` - All possible IDs for regions
/// * `time_slice_info` - Information about seasons and times of day
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// This function returns [`DemandMap`]s grouped by commodity ID.
pub fn read_demand(
    model_dir: &Path,
    commodities: &IndexMap<CommodityID, Commodity>,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<HashMap<CommodityID, DemandMap>> {
    // Demand only applies to SVD commodities
    let svd_commodities = commodities
        .iter()
        .filter(|(_, commodity)| commodity.kind == CommodityType::ServiceDemand)
        .map(|(id, commodity)| (id.clone(), commodity))
        .collect();

    let demand = read_demand_file(model_dir, &svd_commodities, region_ids, milestone_years)?;
    let slices = read_demand_slices(model_dir, &svd_commodities, region_ids, time_slice_info)?;

    Ok(compute_demand_maps(time_slice_info, &demand, &slices))
}

/// Read the demand.csv file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `svd_commodities` - Map of service demand commodities
/// * `region_ids` - All possible IDs for regions
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// Annual demand data, grouped by commodity, region and milestone year.
fn read_demand_file(
    model_dir: &Path,
    svd_commodities: &BorrowedCommodityMap,
    region_ids: &IndexSet<RegionID>,
    milestone_years: &[u32],
) -> Result<AnnualDemandMap> {
    let file_path = model_dir.join(DEMAND_FILE_NAME);
    let iter = read_csv(&file_path)?;
    read_demand_from_iter(iter, svd_commodities, region_ids, milestone_years)
        .with_context(|| input_err_msg(file_path))
}

/// Read the demand data from an iterator.
///
/// # Arguments
///
/// * `iter` - An iterator of [`Demand`]s
/// * `svd_commodities` - Map of service demand commodities
/// * `region_ids` - All possible IDs for regions
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// A map of demand and time slice level for every combination of commodity, region and milestone
/// year.
fn read_demand_from_iter<I>(
    iter: I,
    svd_commodities: &BorrowedCommodityMap,
    region_ids: &IndexSet<RegionID>,
    milestone_years: &[u32],
) -> Result<AnnualDemandMap>
where
    I: Iterator<Item = Demand>,
{
    let mut map = AnnualDemandMap::new();
    for demand in iter {
        let commodity = svd_commodities
            .get(demand.commodity_id.as_str())
            .with_context(|| {
                format!(
                    "Can only provide demand data for SVD commodities. Found entry for '{}'",
                    demand.commodity_id
                )
            })?;
        let region_id = region_ids.get_id(&demand.region_id)?;

        ensure!(
            milestone_years.binary_search(&demand.year).is_ok(),
            "Year {} is not a milestone year. \
            Input of non-milestone years is currently not supported.",
            demand.year
        );

        ensure!(
            demand.demand.is_finite() && demand.demand >= Flow(0.0),
            "Demand must be a valid number greater than or equal to zero"
        );

        ensure!(
            map.insert(
                (commodity.id.clone(), region_id.clone(), demand.year),
                (commodity.time_slice_level, demand.demand)
            )
            .is_none(),
            "Duplicate demand entries (commodity: {}, region: {}, year: {})",
            commodity.id,
            region_id,
            demand.year
        );
    }

    // Check that demand data is specified for all combinations of commodity, region and year
    for commodity_id in svd_commodities.keys() {
        let mut missing_keys = Vec::new();
        for (region_id, year) in iproduct!(region_ids, milestone_years) {
            if !map.contains_key(&(commodity_id.clone(), region_id.clone(), *year)) {
                missing_keys.push((region_id.clone(), *year));
            }
        }
        ensure!(
            missing_keys.is_empty(),
            "Commodity {commodity_id} is missing demand data for {}",
            format_items_with_cap(&missing_keys)
        );
    }

    Ok(map)
}

/// Calculate the demand for each combination of commodity, region, year and time slice.
///
/// # Arguments
///
/// * `time_slice_info` - Information about time slices
/// * `demand` - Total annual demand for combinations of commodity, region and year
/// * `slices` - How annual demand is shared between time slices
///
/// # Returns
///
/// [`DemandMap`]s for combinations of region, year and time slice, grouped by the commodity to
/// which the demand applies.
fn compute_demand_maps(
    time_slice_info: &TimeSliceInfo,
    demand: &AnnualDemandMap,
    slices: &DemandSliceMap,
) -> HashMap<CommodityID, DemandMap> {
    let mut map = HashMap::new();
    for ((commodity_id, region_id, year), (level, annual_demand)) in demand {
        for ts_selection in time_slice_info.iter_selections_at_level(*level) {
            let slice_key = (
                commodity_id.clone(),
                region_id.clone(),
                ts_selection.clone(),
            );

            // NB: This has already been checked, so shouldn't fail
            let demand_fraction = slices[&slice_key];

            // Get or create entry
            let map = map
                .entry(commodity_id.clone())
                .or_insert_with(DemandMap::new);

            // Add a new demand entry
            map.insert(
                (region_id.clone(), *year, ts_selection.clone()),
                *annual_demand * demand_fraction,
            );
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{assert_error, get_svd_map, region_ids, svd_commodity};
    use rstest::rstest;
    use std::fs::File;
    use std::io::Write;
    use std::path::Path;
    use tempfile::tempdir;

    #[rstest]
    fn test_read_demand_from_iter(svd_commodity: Commodity, region_ids: IndexSet<RegionID>) {
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = [
            Demand {
                year: 2020,
                region_id: "GBR".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(10.0),
            },
            Demand {
                year: 2020,
                region_id: "USA".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(11.0),
            },
        ];

        // Valid
        assert!(
            read_demand_from_iter(demand.into_iter(), &svd_commodities, &region_ids, &[2020])
                .is_ok()
        );
    }

    #[rstest]
    fn test_read_demand_from_iter_bad_commodity_id(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Bad commodity ID
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = [
            Demand {
                year: 2020,
                region_id: "GBR".to_string(),
                commodity_id: "commodity2".to_string(),
                demand: Flow(10.0),
            },
            Demand {
                year: 2020,
                region_id: "USA".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(11.0),
            },
            Demand {
                year: 2020,
                region_id: "Spain".to_string(),
                commodity_id: "commodity3".to_string(),
                demand: Flow(0.0),
            },
        ];
        assert_error!(
            read_demand_from_iter(demand.into_iter(), &svd_commodities, &region_ids, &[2020]),
            "Can only provide demand data for SVD commodities. Found entry for 'commodity2'"
        );
    }

    #[rstest]
    fn test_read_demand_from_iter_bad_region_id(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Bad region ID
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = [
            Demand {
                year: 2020,
                region_id: "FRA".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(10.0),
            },
            Demand {
                year: 2020,
                region_id: "USA".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(11.0),
            },
        ];
        assert_error!(
            read_demand_from_iter(demand.into_iter(), &svd_commodities, &region_ids, &[2020]),
            "Unknown ID FRA found"
        );
    }

    #[rstest]
    fn test_read_demand_from_iter_bad_year(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Bad year
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = [
            Demand {
                year: 2010,
                region_id: "GBR".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(10.0),
            },
            Demand {
                year: 2020,
                region_id: "USA".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(11.0),
            },
        ];
        assert_error!(
            read_demand_from_iter(demand.into_iter(), &svd_commodities, &region_ids, &[2020]),
            "Year 2010 is not a milestone year. \
            Input of non-milestone years is currently not supported."
        );
    }

    #[rstest]
    #[case(-1.0)]
    #[case(f64::NAN)]
    #[case(f64::NEG_INFINITY)]
    #[case(f64::INFINITY)]
    fn test_read_demand_from_iter_bad_demand(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
        #[case] quantity: f64,
    ) {
        // Bad demand quantity
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = [Demand {
            year: 2020,
            region_id: "GBR".to_string(),
            commodity_id: "commodity1".to_string(),
            demand: Flow(quantity),
        }];
        assert_error!(
            read_demand_from_iter(demand.into_iter(), &svd_commodities, &region_ids, &[2020],),
            "Demand must be a valid number greater than or equal to zero"
        );
    }

    #[rstest]
    fn test_read_demand_from_iter_multiple_entries(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Multiple entries for same commodity and region
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = [
            Demand {
                year: 2020,
                region_id: "GBR".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(10.0),
            },
            Demand {
                year: 2020,
                region_id: "GBR".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(10.0),
            },
            Demand {
                year: 2020,
                region_id: "USA".to_string(),
                commodity_id: "commodity1".to_string(),
                demand: Flow(11.0),
            },
        ];
        assert_error!(
            read_demand_from_iter(demand.into_iter(), &svd_commodities, &region_ids, &[2020]),
            "Duplicate demand entries (commodity: commodity1, region: GBR, year: 2020)"
        );
    }

    #[rstest]
    fn test_read_demand_from_iter_missing_year(
        svd_commodity: Commodity,
        region_ids: IndexSet<RegionID>,
    ) {
        // Missing entry for a milestone year
        let svd_commodities = get_svd_map(&svd_commodity);
        let demand = Demand {
            year: 2020,
            region_id: "GBR".to_string(),
            commodity_id: "commodity1".to_string(),
            demand: Flow(10.0),
        };
        assert!(
            read_demand_from_iter(
                std::iter::once(demand),
                &svd_commodities,
                &region_ids,
                &[2020, 2030]
            )
            .is_err()
        );
    }

    /// Create an example demand file in dir_path
    fn create_demand_file(dir_path: &Path) {
        let file_path = dir_path.join(DEMAND_FILE_NAME);
        let mut file = File::create(file_path).unwrap();
        writeln!(
            file,
            "commodity_id,region_id,year,demand\n\
            commodity1,GBR,2020,10\n\
            commodity1,USA,2020,11\n"
        )
        .unwrap();
    }

    #[rstest]
    fn test_read_demand_file(svd_commodity: Commodity, region_ids: IndexSet<RegionID>) {
        let svd_commodities = get_svd_map(&svd_commodity);
        let dir = tempdir().unwrap();
        create_demand_file(dir.path());
        let milestone_years = [2020];
        let expected = AnnualDemandMap::from_iter([
            (
                ("commodity1".into(), "GBR".into(), 2020),
                (TimeSliceLevel::DayNight, Flow(10.0)),
            ),
            (
                ("commodity1".into(), "USA".into(), 2020),
                (TimeSliceLevel::DayNight, Flow(11.0)),
            ),
        ]);
        let demand =
            read_demand_file(dir.path(), &svd_commodities, &region_ids, &milestone_years).unwrap();
        assert_eq!(demand, expected);
    }
}
