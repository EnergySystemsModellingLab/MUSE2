//! Code for reading commodity constraints from a CSV file.
use super::super::{input_err_msg, read_csv_optional};
use crate::commodity::{BalanceType, CommodityConstraint, CommodityConstraintsMap, CommodityID};
use crate::id::IDCollection;
use crate::input::{parse_range, parse_year_str};
use crate::region::RegionID;
use crate::time_slice::TimeSliceInfo;
use crate::units::Flow;
use anyhow::{Context, Result, ensure};
use indexmap::IndexSet;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

const COMMODITY_CONSTRAINTS_FILE_NAME: &str = "commodity_constraints.csv";

/// Constraints for each commodity
#[derive(PartialEq, Debug, Deserialize)]
struct CommodityConstraintRaw {
    /// Unique identifier for the commodity
    commodity_id: String,
    /// Region id
    region_id: String,
    /// Type of balance
    balance_type: BalanceType,
    /// The year(s) to which the constraint applies
    years: String,
    /// The time slice to which the constraint applies
    time_slice: String,
    /// Limits on the value of the commodity
    limits: String,
}

impl CommodityConstraintRaw {
    fn validate(&self) -> Result<()> {
        // Net production already constrained by commodity balance constraints
        ensure!(
            self.balance_type != BalanceType::Net,
            "Balance type cannot be 'net' for commodity constraints"
        );

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
        record.validate()?;

        // Extract fields from record
        let commodity_id = commodity_ids.get_id(&record.commodity_id)?;
        let region_id = region_ids.get_id(&record.region_id)?;
        let years = parse_year_str(&record.years, milestone_years)?;
        let ts_selection = time_slice_info.get_selection(&record.time_slice)?;
        let limits = parse_range(&record.limits, Flow(0.0)..=Flow(f64::INFINITY))
            .with_context(|| format!("Could not parse constraint range: {}", record.limits))?;

        // For each record, store that constraint per year
        let commodity_map = map.entry(commodity_id.clone()).or_default();
        for year in &years {
            let constraint = CommodityConstraint {
                balance_type: record.balance_type.clone(),
                ts_selection: ts_selection.clone(),
                limits: limits.clone(),
            };
            commodity_map
                .entry((region_id.clone(), *year))
                .and_modify(|constraints| constraints.push(constraint.clone()))
                .or_insert(vec![constraint]);
        }
    }
    Ok(map)
}
