//! Code for reading commodity constraints from a CSV file.
//!
//! The `commodity_constraints.csv` file is optional. If it is provided, the
//! `please_give_me_broken_results` option in `model.toml` must be set to `true` because commodity
//! constraints are experimental.
use super::super::{input_err_msg, read_csv_optional};
use crate::commodity::{
    BalanceType, Commodity, CommodityConstraint, CommodityConstraintsMap, CommodityID,
    CommodityType,
};
use crate::id::{GetIDValue, IDCollection};
use crate::input::{parse_range, parse_year_str};
use crate::model::{ALLOW_DANGEROUS_OPTION_NAME, dangerous_model_options_enabled};
use crate::region::RegionID;
use crate::time_slice::TimeSliceInfo;
use crate::units::Flow;
use anyhow::{Context, Result, ensure};
use indexmap::{IndexMap, IndexSet};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

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
/// * `commodities` - The commodities in the model
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
    commodities: &IndexMap<CommodityID, Commodity>,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<HashMap<CommodityID, CommodityConstraintsMap>> {
    let file_path = model_dir.join(COMMODITY_CONSTRAINTS_FILE_NAME);
    let commodity_constraints_csv = read_csv_optional(&file_path)?;
    let commodity_constraints = read_commodity_constraints_from_iter(
        commodity_constraints_csv,
        commodities,
        region_ids,
        time_slice_info,
        milestone_years,
    )
    .with_context(|| input_err_msg(&file_path))?;

    ensure!(
        commodity_constraints.is_empty() || dangerous_model_options_enabled(),
        "Commodity constraints are currently experimental. To use them, set the \
        {ALLOW_DANGEROUS_OPTION_NAME} option to true."
    );

    Ok(commodity_constraints)
}

/// Process raw commodity-constraint records into a constraints map.
///
/// # Arguments
///
/// * `iter` - Iterator over `CommodityConstraintRaw` records
/// * `commodities` - The commodities in the model
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
    commodities: &IndexMap<CommodityID, Commodity>,
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
        let (commodity_id, commodity) = commodities.get_id_value(&record.commodity_id)?;
        ensure!(
            commodity.kind != CommodityType::ServiceDemand,
            "SVD commodities are not permitted to have commodity constraints"
        );
        let region_id = region_ids.get_id(&record.region_id)?;
        let years = parse_year_str(&record.years, milestone_years)?;
        let ts_selection = time_slice_info.get_selection(&record.time_slice)?;
        let limits = parse_range(&record.limits, Flow(0.0)..=Flow(f64::INFINITY))
            .with_context(|| format!("Could not parse constraint range: {}", record.limits))?;

        // For each record, store that constraint per year
        let commodity_map = map.entry(commodity_id.clone()).or_default();
        for year in &years {
            let constraint = CommodityConstraint {
                region_id: region_id.clone(),
                balance_type: record.balance_type.clone(),
                ts_selection: ts_selection.clone(),
                limits: limits.clone(),
            };
            commodity_map
                .entry(*year)
                .and_modify(|constraints| constraints.push(constraint.clone()))
                .or_insert(vec![constraint]);
        }
    }
    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use crate::input::commodity::{COMMODITY_FILE_NAME, read_commodities_file};
    use crate::time_slice::{TimeSliceID, TimeSliceSelection};
    use crate::units::Year;
    use float_cmp::assert_approx_eq;
    use std::fs;
    use tempfile::tempdir;

    fn validate_raw_constraint(balance_type: BalanceType) -> Result<()> {
        let constraint = CommodityConstraintRaw {
            commodity_id: "test_commodity".into(),
            region_id: "test_region".into(),
            balance_type,
            years: "2020".into(),
            time_slice: "annual".into(),
            limits: "1.2..2.3".into(),
        };
        constraint.validate()
    }

    #[test]
    fn validate_constraints_valid() {
        validate_raw_constraint(BalanceType::Production).unwrap();
    }

    #[test]
    fn validate_constraints_invalid() {
        // Invalid balance_type specified
        let invalid = validate_raw_constraint(BalanceType::Net);
        assert_error!(
            invalid,
            "Balance type cannot be 'net' for commodity constraints"
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn read_commodity_constraints_from_iter_success() {
        // Create a model dir and write a simple commodities CSV file
        let dir = tempdir().unwrap();
        let model_dir = dir.path();

        // Create simple commodities CSV to simplify creating `Commodity`s
        let commodities_csv = concat!(
            "id,description,type,time_slice_level,units\n",
            "ELCTRI,Electricity,sed,season,PJ\n",
            "CO2EMT,CO2 emitted,oth,season,ktCO2\n",
        );
        fs::write(model_dir.join(COMMODITY_FILE_NAME), commodities_csv).unwrap();

        // Create basic model inputs
        let commodities = read_commodities_file(model_dir).unwrap();

        let region_ids: IndexSet<RegionID> = ["GBR".into()].into_iter().collect();
        let time_slice_info = TimeSliceInfo {
            seasons: [("summer".into(), Year(0.5)), ("winter".into(), Year(0.5))].into(),
            times_of_day: ["day".into(), "night".into()].into(),
            time_slices: [
                (
                    TimeSliceID {
                        season: "summer".into(),
                        time_of_day: "day".into(),
                    },
                    Year(0.25),
                ),
                (
                    TimeSliceID {
                        season: "summer".into(),
                        time_of_day: "night".into(),
                    },
                    Year(0.25),
                ),
                (
                    TimeSliceID {
                        season: "winter".into(),
                        time_of_day: "day".into(),
                    },
                    Year(0.25),
                ),
                (
                    TimeSliceID {
                        season: "winter".into(),
                        time_of_day: "night".into(),
                    },
                    Year(0.25),
                ),
            ]
            .into(),
        };

        let milestone_years = vec![2030];

        let constraints = [
            CommodityConstraintRaw {
                commodity_id: "ELCTRI".into(),
                region_id: "GBR".into(),
                balance_type: BalanceType::Consumption,
                years: "2030".into(),
                time_slice: "summer".into(),
                limits: "12.34..56.78".into(),
            },
            CommodityConstraintRaw {
                commodity_id: "CO2EMT".into(),
                region_id: "GBR".into(),
                balance_type: BalanceType::Consumption,
                years: "2030".into(),
                time_slice: "winter".into(),
                limits: "..9.99".into(),
            },
            CommodityConstraintRaw {
                commodity_id: "CO2EMT".into(),
                region_id: "GBR".into(),
                balance_type: BalanceType::Production,
                years: "2030".into(),
                time_slice: "summer".into(),
                limits: "9.99..".into(),
            },
        ];

        // Create the constraints map
        let constraints_map = read_commodity_constraints_from_iter(
            constraints.into_iter(),
            &commodities,
            &region_ids,
            &time_slice_info,
            &milestone_years,
        )
        .unwrap();

        // ELCTRI constraint
        let elctri = &constraints_map[&CommodityID::from("ELCTRI")][&2030][0];
        assert_eq!(elctri.balance_type, BalanceType::Consumption);
        assert_eq!(
            elctri.ts_selection,
            TimeSliceSelection::Season("summer".into())
        );
        assert_approx_eq!(f64, elctri.limits.start().value(), 12.34);
        assert_approx_eq!(f64, elctri.limits.end().value(), 56.78);

        // CO2EMT constraints
        let co2emt = &constraints_map[&CommodityID::from("CO2EMT")][&2030];
        assert_eq!(co2emt[0].balance_type, BalanceType::Consumption);
        assert_eq!(
            co2emt[0].ts_selection,
            TimeSliceSelection::Season("winter".into())
        );
        assert_approx_eq!(f64, co2emt[0].limits.start().value(), 0.0);
        assert_approx_eq!(f64, co2emt[0].limits.end().value(), 9.99);

        assert_eq!(co2emt[1].balance_type, BalanceType::Production);
        assert_eq!(
            co2emt[1].ts_selection,
            TimeSliceSelection::Season("summer".into())
        );
        assert_approx_eq!(f64, co2emt[1].limits.start().value(), 9.99);
        assert_approx_eq!(f64, co2emt[1].limits.end().value(), f64::INFINITY);
    }

    #[test]
    fn read_commodity_constraints_fails_with_invalid_csv() {
        // Create a model dir and write invalid CSV content to force
        // read_commodity_constraints_from_iter failure
        let dir = tempdir().unwrap();
        let model_dir = dir.path();

        // Create invalid commodity constraints CSV content
        let file_path = model_dir.join(COMMODITY_CONSTRAINTS_FILE_NAME);
        fs::write(&file_path, "invalid,commodity,constraints\nbad,row\n").unwrap();

        // Create empty model inputs
        let commodities: IndexMap<CommodityID, Commodity> = IndexMap::new();
        let region_ids: IndexSet<RegionID> = IndexSet::new();
        let time_slice_info = TimeSliceInfo::default();
        let milestone_years: Vec<u32> = vec![2020, 2030];

        // Try to create the constraints map
        let result = read_commodity_constraints(
            model_dir,
            &commodities,
            &region_ids,
            &time_slice_info,
            &milestone_years,
        );

        // Check failure and file path present in error message
        assert!(result.is_err());
        let err_text = format!("{:#}", result.unwrap_err());
        assert!(
            err_text.contains(COMMODITY_CONSTRAINTS_FILE_NAME),
            "error message should include file name context, got: {err_text}"
        );
    }
}
