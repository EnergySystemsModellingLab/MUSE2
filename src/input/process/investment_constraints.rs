//! Code for reading process investment constraints CSV file
use super::super::input_err_msg;
use crate::input::read_csv_optional;
use crate::process::{
    ProcessID, ProcessInvestmentConstraint, ProcessInvestmentConstraintsMap, ProcessMap,
};
use crate::region::parse_region_str;
use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

const PROCESS_INVESTMENT_CONSTRAINTS_FILE_NAME: &str = "process_investment_constraints.csv";

/// Represents a row of the process investment constraints CSV file
#[derive(Deserialize)]
struct ProcessInvestmentConstraintRaw {
    constraint_name: String,
    process_id: String,
    regions: String,
    commission_years: u32,
    addition_limit: f64,
}

impl ProcessInvestmentConstraintRaw {
    /// Validate the constraint record for logical consistency and required fields
    fn validate(&self) -> Result<()> {
        // Validate that value is finite
        ensure!(
            self.addition_limit.is_finite(),
            "Constraint value must be finite for constraint '{}'",
            self.constraint_name
        );

        // For addition constraints, value must be non-negative
        ensure!(
            self.addition_limit >= 0.0,
            "Addition constraint value must be non-negative for constraint '{}'",
            self.constraint_name
        );

        Ok(())
    }
}

/// Read the process investment constraints CSV file.
///
/// This file contains information about investment constraints that limit how processes can be
/// deployed, either through growth rates, absolute additions, or capacity limits.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `processes` - Map of processes to validate against
/// * `milestone_years` - Milestone years of simulation to validate against
///
/// # Returns
///
/// A map keyed by process ID containing investment constraints maps, or an error.
pub fn read_process_investment_constraints(
    model_dir: &Path,
    processes: &ProcessMap,
    milestone_years: &[u32],
) -> Result<HashMap<ProcessID, ProcessInvestmentConstraintsMap>> {
    let file_path = model_dir.join(PROCESS_INVESTMENT_CONSTRAINTS_FILE_NAME);
    let constraints_csv = read_csv_optional(&file_path)?;
    read_process_investment_constraints_from_iter(constraints_csv, processes, milestone_years)
        .with_context(|| input_err_msg(&file_path))
}

/// Process raw process investment constraint input data into a constraints map.
///
/// # Arguments
///
/// * `iter` - Iterator of raw process investment constraint records
/// * `processes` - Map of processes to validate against
/// * `milestone_years` - Milestone years of simulation to validate against
///
/// # Returns
///
/// A map keyed by process ID containing investment constraints maps, or an error.
fn read_process_investment_constraints_from_iter<I>(
    iter: I,
    processes: &ProcessMap,
    milestone_years: &[u32],
) -> Result<HashMap<ProcessID, ProcessInvestmentConstraintsMap>>
where
    I: Iterator<Item = ProcessInvestmentConstraintRaw>,
{
    let mut map: HashMap<ProcessID, ProcessInvestmentConstraintsMap> = HashMap::new();

    for record in iter {
        // Validate the raw record
        record.validate()?;

        // Verify the process exists
        let (process_id, process) = processes
            .get_key_value(record.process_id.as_str())
            .with_context(|| format!("Process {} not found", record.process_id))?;

        // Parse and validate regions
        let process_regions = &process.regions;
        let record_regions =
            parse_region_str(&record.regions, process_regions).with_context(|| {
                format!(
                    "Invalid region for process {process_id}. Valid regions are {process_regions:?}"
                )
            })?;

        // Validate year is a milestone year
        ensure!(
            milestone_years.contains(&record.commission_years),
            "Year {} is not a milestone year for constraint '{}'. Valid milestone years are: {:?}",
            record.commission_years,
            record.constraint_name,
            milestone_years
        );

        // Validate year is within process operational years
        let process_years: Vec<u32> = process.years.clone().collect();
        ensure!(
            process_years.contains(&record.commission_years),
            "Year {} is not valid for process {}. Valid years are: {:?}",
            record.commission_years,
            process_id,
            process_years
        );

        for region in &record_regions {
            let constraint = ProcessInvestmentConstraint {
                addition_limit: Some(record.addition_limit),
            };

            let process_map = map.entry(process_id.clone()).or_default();
            process_map
                .entry((region.clone(), record.commission_years))
                .or_insert_with(|| Rc::new(constraint));
        }
    }

    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::processes;
    use crate::region::RegionID;
    use rstest::rstest;

    fn create_raw_constraint(addition_limit: f64) -> ProcessInvestmentConstraintRaw {
        ProcessInvestmentConstraintRaw {
            constraint_name: "test_constraint".into(),
            process_id: "test_process".into(),
            regions: "ALL".into(),
            commission_years: 2030,
            addition_limit: addition_limit,
        }
    }

    #[rstest]
    fn test_read_process_investment_constraints_from_iter(processes: ProcessMap) {
        // Create milestone years matching the process years
        let milestone_years: Vec<u32> = vec![2010, 2015, 2020];

        // Create constraint records for the test process
        let constraints = vec![
            ProcessInvestmentConstraintRaw {
                constraint_name: "gbr_2015_limit".into(),
                process_id: "process1".into(),
                regions: "GBR".into(),
                commission_years: 2015,
                addition_limit: 100.0,
            },
            ProcessInvestmentConstraintRaw {
                constraint_name: "usa_2015_limit".into(),
                process_id: "process1".into(),
                regions: "USA".into(),
                commission_years: 2015,
                addition_limit: 200.0,
            },
            ProcessInvestmentConstraintRaw {
                constraint_name: "all_regions_2020_limit".into(),
                process_id: "process1".into(),
                regions: "ALL".into(),
                commission_years: 2020,
                addition_limit: 50.0,
            },
        ];

        // Read constraints into the map
        let result = read_process_investment_constraints_from_iter(
            constraints.into_iter(),
            &processes,
            &milestone_years,
        )
        .unwrap();

        // Verify the constraints were correctly stored
        let process_id: ProcessID = "process1".into();
        let process_constraints = result
            .get(&process_id)
            .expect("Process constraints should exist");

        // Check GBR 2015 constraint
        let gbr_region: RegionID = "GBR".into();
        let gbr_constraint = process_constraints
            .get(&(gbr_region.clone(), 2015))
            .expect("GBR 2015 constraint should exist");
        assert_eq!(gbr_constraint.addition_limit, Some(100.0));

        // Check USA 2015 constraint
        let usa_region: RegionID = "USA".into();
        let usa_constraint = process_constraints
            .get(&(usa_region.clone(), 2015))
            .expect("USA 2015 constraint should exist");
        assert_eq!(usa_constraint.addition_limit, Some(200.0));

        // Check that ALL regions constraint created entries for both regions in 2020
        let gbr_2020_constraint = process_constraints
            .get(&(gbr_region, 2020))
            .expect("GBR 2020 constraint should exist");
        assert_eq!(gbr_2020_constraint.addition_limit, Some(50.0));

        let usa_2020_constraint = process_constraints
            .get(&(usa_region, 2020))
            .expect("USA 2020 constraint should exist");
        assert_eq!(usa_2020_constraint.addition_limit, Some(50.0));

        // Verify total number of constraints (2 for 2015 + 2 for 2020)
        assert_eq!(process_constraints.len(), 4);
    }

    #[rstest]
    fn test_read_constraints_invalid_year(processes: ProcessMap) {
        // Create constraint with year not in milestone years
        let milestone_years = vec![2015, 2020];

        let constraints = vec![ProcessInvestmentConstraintRaw {
            constraint_name: "invalid_year_constraint".into(),
            process_id: "process1".into(),
            regions: "GBR".into(),
            commission_years: 2025, // Not in milestone_years
            addition_limit: 100.0,
        }];

        // Should fail with milestone year validation error
        let result = read_process_investment_constraints_from_iter(
            constraints.into_iter(),
            &processes,
            &milestone_years,
        );

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("not a milestone year")
        );
    }

    #[rstest]
    fn test_read_constraints_year_outside_process_years(processes: ProcessMap) {
        // Create constraint with year outside process operational years
        // Process years are 2010..=2020 from the fixture
        let milestone_years = vec![2005, 2015, 2025];

        let constraints = vec![ProcessInvestmentConstraintRaw {
            constraint_name: "out_of_range_year".into(),
            process_id: "process1".into(),
            regions: "GBR".into(),
            commission_years: 2025, // Outside process years (2010-2020)
            addition_limit: 100.0,
        }];

        // Should fail with process year validation error
        let result = read_process_investment_constraints_from_iter(
            constraints.into_iter(),
            &processes,
            &milestone_years,
        );

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("not valid for process")
        );
    }

    #[test]
    fn test_validate_addition_with_finite_value() {
        // Valid: addition constraint with positive value
        let valid = create_raw_constraint(10.0);
        assert!(valid.validate().is_ok());

        // Valid: addition constraint with zero value
        let valid = create_raw_constraint(0.0);
        assert!(valid.validate().is_ok());

        // Not valid: addition constraint with negative value
        let valid = create_raw_constraint(-10.0);
        assert!(valid.validate().is_err());
    }

    #[test]
    fn test_validate_addition_rejects_infinite() {
        // Invalid: infinite value
        let invalid = create_raw_constraint(f64::INFINITY);
        assert!(invalid.validate().is_err());

        // Invalid: NaN value
        let invalid = create_raw_constraint(f64::NAN);
        assert!(invalid.validate().is_err());
    }
}
