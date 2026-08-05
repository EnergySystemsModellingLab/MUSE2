//! Code for reading commodity constraints from a CSV file.
use super::super::{input_err_msg, read_csv_optional};
use crate::commodity::{BalanceType, CommodityConstraint, CommodityConstraintsMap, CommodityID};
use crate::id::IDCollection;
use crate::input::{parse_range, parse_year_str, try_insert};
use crate::region::{RegionID, parse_region_str};
use crate::time_slice::TimeSliceInfo;
use anyhow::{Context, Result};
use indexmap::IndexSet;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

const COMMODITY_CONSTRAINTS_FILE_NAME: &str = "commodity_constraints.csv";

/// Constraints for each commodity
#[derive(PartialEq, Debug, Deserialize)]
struct CommodityConstraintRaw {
    /// The type of commodity constraint?
    commodity_constraint: String,
    /// Unique identifier for the commodity
    commodity_id: String,
    /// Region id
    region_id: String,
    /// Type of balance
    balance_type: BalanceType,
    /// The year to which the constraint applies
    year: String,
    /// The time slice to which the constraint applies
    time_slice: String,
    /// Limits on the value of the commodity
    limits: String,
}

impl CommodityConstraintRaw {
    fn validate(&self) -> Result<()> {
        Ok(())
    }
}

/// Read the commodity constraints CSV file.
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
/// A `HashMap<CommodityID, CommodityConstraintsMap>` mapping commodity IDs to their
/// commodity-constraints maps, or an error.
pub fn read_commodity_constraints(
    model_dir: &Path,
    commodity_ids: &IndexSet<CommodityID>,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<HashMap<CommodityID, CommodityConstraintsMap>> {
    let file_path = model_dir.join(COMMODITY_CONSTRAINTS_FILE_NAME);
    let commodity_constraints_csv = read_csv_optional(&file_path)?;
    read_commodity_constraints_from_iter(
        commodity_constraints_csv,
        commodity_ids,
        region_ids,
        time_slice_info,
        milestone_years,
    )
    .with_context(|| input_err_msg(&file_path))
}

/// Process raw commodity-constraint records into a constraints map.
///
/// # Arguments
///
/// * `iter` - Iterator over `CommodityConstraintRaw` records
/// * `commodity_ids` - All possible commodity IDs
/// * `region_ids` - All possible region IDs
/// * `time_slice_info` - Information about time slices
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// A `HashMap<CommodityID, CommodityConstraintsMap>` mapping commodity IDs to their
/// commodity-constraints maps, or an error.
fn read_commodity_constraints_from_iter<I>(
    iter: I,
    commodity_ids: &IndexSet<CommodityID>,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<HashMap<CommodityID, CommodityConstraintsMap>>
where
    I: Iterator<Item = CommodityConstraintRaw>,
{
    let mut map: HashMap<CommodityID, CommodityConstraintsMap> = HashMap::new();

    for record in iter {
        // Validate the raw record
        record.validate()?;

        // Parse fields in the record
        let commodity_constraint = &record.commodity_constraint;
        let commodity_id = commodity_ids.get_id(&record.commodity_id)?;
        let region_id = parse_region_str(&record.region_id, region_ids)?;
        let year = parse_year_str(&record.year, milestone_years)?;
        let ts_selection = time_slice_info.get_selection(&record.time_slice)?;
        let limits = parse_range(&record.limits, 0.0..=f64::INFINITY)
            .with_context(|| format!("Could not parse availabilities range: {}", record.limits))?;

        let constraint = Rc::new(CommodityConstraint {});
        let map = map.entry(commodity_id.clone()).or_default();
        try_insert(map, &commodity_id.clone(), constraint.clone());
    }
    Ok(map)
}
