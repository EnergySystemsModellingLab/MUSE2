//! Code for reading process investment constraints from a CSV file.
use super::super::input_err_msg;
use crate::input::{read_csv_optional, try_insert};
use crate::process::{
    ProcessID, ProcessInvestmentConstraint, ProcessInvestmentConstraintsMap, ProcessMap,
};
use crate::region::parse_region_str;
use crate::units::{CapacityPerYear, Year};
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use itertools::iproduct;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

const PROCESS_INVESTMENT_CONSTRAINTS_FILE_NAME: &str = "process_investment_constraints.csv";

/// Represents a row of the process investment constraints CSV file
#[derive(PartialEq, Debug, Deserialize)]
struct ProcessInvestmentConstraintRaw {
    process_id: String,
    regions: String,
    commission_years: String,
    addition_limit: CapacityPerYear,
}

impl ProcessInvestmentConstraintRaw {
    /// Validate the constraint record for logical consistency and required fields
    fn validate(&self) -> Result<()> {
        // Validate that value is finite
        ensure!(
            self.addition_limit.is_finite() && self.addition_limit >= CapacityPerYear(0.0),
            "Invalid value for addition constraint: '{}'; must be non-negative and finite.",
            self.addition_limit
        );

        Ok(())
    }
}

/// Read the process investment constraints CSV file.
///
/// This file contains information about investment constraints that limit how processes can be
/// deployed (growth rates, absolute additions, capacity limits).
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `processes` - Map of processes to validate against
/// * `milestone_years` - Milestone years of simulation to validate against
///
/// # Returns
///
/// A `HashMap<ProcessID, ProcessInvestmentConstraintsMap>` mapping process IDs to their
/// investment-constraints maps, or an error.
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

/// Process raw investment-constraint records into a constraints map.
///
/// # Arguments
///
/// * `iter` - Iterator over `ProcessInvestmentConstraintRaw` records
/// * `processes` - Map of known processes to validate against
/// * `milestone_years` - Milestone years used by the model
///
/// # Returns
///
/// A `HashMap<ProcessID, ProcessInvestmentConstraintsMap>` mapping process IDs to their
/// investment-constraints maps, or an error.
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

        // Parse associated commission years
        let milestone_years_in_process_range: Vec<u32> = milestone_years
            .iter()
            .copied()
            .filter(|year| process.years.contains(year))
            .collect();
        let constraint_years = parse_year_str(&record.commission_years, &milestone_years_in_process_range)
            .with_context(|| {
                format!(
                    "Invalid year for constraint on process {process_id}. Valid years are {milestone_years_in_process_range:?}",
                )
            })?;

        // Create constraints for each region and year combination
        // For a given milestone year, the addition limit should be multiplied
        // by the number of years since the previous milestone year. Any
        // addition limits specified for the first milestone year are ignored.
        let process_map = map.entry(process_id.clone()).or_default();
        for (region, &year) in iproduct!(&record_regions, &constraint_years) {
            // Calculate years since previous milestone year
            // We can ignore constraints in the first milestone year as no investments are performed then
            let idx = milestone_years.iter().position(|y| *y == year).expect(
                "Year should be in milestone_years since it was validated by parse_year_str",
            );
            if idx == 0 {
                continue;
            }
            let prev_year = milestone_years[idx - 1];
            let years_since_prev = year - prev_year;

            // Multiply the addition limit by the number of years since previous milestone.
            let scaled_limit = record.addition_limit * Year(years_since_prev as f64);

            let constraint = Rc::new(ProcessInvestmentConstraint {
                addition_limit: Some(scaled_limit),
            });

            try_insert(process_map, &(region.clone(), year), constraint.clone())?;
        }
    }
    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{assert_error, processes};
    use crate::region::RegionID;
    use crate::units::Capacity;
    use rstest::rstest;

    fn validate_raw_constraint(addition_limit: CapacityPerYear) -> Result<()> {
        let constraint = ProcessInvestmentConstraintRaw {
            process_id: "test_process".into(),
            regions: "ALL".into(),
            commission_years: "2030".into(),
            addition_limit,
        };
        constraint.validate()
    }

    #[rstest]
    fn read_constraints_only_uses_milestone_years_within_process_range(processes: ProcessMap) {
        // Process years are 2010..=2020 from the fixture (excludes 2008)
        let milestone_years = vec![2008, 2012, 2016];

        let constraints = vec![ProcessInvestmentConstraintRaw {
            process_id: "process1".into(),
            regions: "GBR".into(),
            commission_years: "ALL".into(), // Should apply to milestone years [2012, 2016]
            addition_limit: CapacityPerYear(100.0),
        }];

        let result = read_process_investment_constraints_from_iter(
            constraints.into_iter(),
            &processes,
            &milestone_years,
        )
        .unwrap();

        let process_id: ProcessID = "process1".into();
        let process_constraints = result
            .get(&process_id)
            .expect("Process constraints should exist");

        let gbr_region: RegionID = "GBR".into();

        // Should have constraints for milestone years within process year
        // range
        assert_eq!(process_constraints.len(), 2);
        assert!(process_constraints.contains_key(&(gbr_region.clone(), 2012)));
        assert!(process_constraints.contains_key(&(gbr_region.clone(), 2016)));

        // All other years should not have constraints
        let process = processes.get(&process_id).unwrap();
        for year in process.years.clone() {
            if ![2012, 2016].contains(&year) {
                assert!(
                    !process_constraints.contains_key(&(gbr_region.clone(), year)),
                    "Should not contain constraint for year {year}"
                );
            }
        }
    }

    #[rstest]
    fn read_process_investment_constraints_from_iter_works(processes: ProcessMap) {
        // Create milestone years matching the process years
        let milestone_years: Vec<u32> = vec![2010, 2015, 2020];

        // Create constraint records for the test process
        let constraints = vec![
            ProcessInvestmentConstraintRaw {
                process_id: "process1".into(),
                regions: "GBR".into(),
                commission_years: "2010".into(),
                addition_limit: CapacityPerYear(100.0),
            },
            ProcessInvestmentConstraintRaw {
                process_id: "process1".into(),
                regions: "ALL".into(),
                commission_years: "2015".into(),
                addition_limit: CapacityPerYear(200.0),
            },
            ProcessInvestmentConstraintRaw {
                process_id: "process1".into(),
                regions: "USA".into(),
                commission_years: "2020".into(),
                addition_limit: CapacityPerYear(50.0),
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

        let gbr_region: RegionID = "GBR".into();
        let usa_region: RegionID = "USA".into();

        // GBR 2010 constraint is for the first milestone year and should be ignored
        assert!(
            !process_constraints.contains_key(&(gbr_region.clone(), 2010)),
            "GBR 2010 constraint should not exist"
        );

        // Check GBR 2015 constraint (from ALL regions), scaled by years since previous milestone (5 years)
        let gbr_2015 = process_constraints
            .get(&(gbr_region, 2015))
            .expect("GBR 2015 constraint should exist");
        assert_eq!(gbr_2015.addition_limit, Some(Capacity(200.0 * 5.0)));

        // Check USA 2015 constraint (from ALL regions), scaled by 5 years
        let usa_2015 = process_constraints
            .get(&(usa_region.clone(), 2015))
            .expect("USA 2015 constraint should exist");
        assert_eq!(usa_2015.addition_limit, Some(Capacity(200.0 * 5.0)));

        // Check USA 2020 constraint, scaled by years since previous milestone (5 years)
        let usa_2020 = process_constraints
            .get(&(usa_region, 2020))
            .expect("USA 2020 constraint should exist");
        assert_eq!(usa_2020.addition_limit, Some(Capacity(50.0 * 5.0)));

        // Verify total number of constraints (GBR 2015, USA 2015, USA 2020 = 3)
        assert_eq!(process_constraints.len(), 3);
    }

    #[rstest]
    fn read_constraints_all_regions_all_years(processes: ProcessMap) {
        // Create milestone years matching the process years
        let milestone_years: Vec<u32> = vec![2010, 2015, 2020];

        // Create a constraint that applies to all regions and all years
        let constraints = vec![ProcessInvestmentConstraintRaw {
            process_id: "process1".into(),
            regions: "ALL".into(),
            commission_years: "ALL".into(),
            addition_limit: CapacityPerYear(75.0),
        }];

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

        let gbr_region: RegionID = "GBR".into();
        let usa_region: RegionID = "USA".into();

        // Verify constraint exists for all region-year combinations except the first milestone year
        for &year in &milestone_years[1..] {
            let gbr_constraint = process_constraints
                .get(&(gbr_region.clone(), year))
                .unwrap_or_else(|| panic!("GBR {year} constraint should exist"));
            // scaled by years since previous milestone (5 years)
            assert_eq!(gbr_constraint.addition_limit, Some(Capacity(75.0 * 5.0)));

            let usa_constraint = process_constraints
                .get(&(usa_region.clone(), year))
                .unwrap_or_else(|| panic!("USA {year} constraint should exist"));
            assert_eq!(usa_constraint.addition_limit, Some(Capacity(75.0 * 5.0)));
        }

        // Verify total number of constraints (2 regions × 2 years = 4)
        assert_eq!(process_constraints.len(), 4);
    }

    #[rstest]
    fn read_constraints_year_outside_milestone_years(processes: ProcessMap) {
        // Create constraint with year outside milestone years
        // Process years are 2010..=2020 from the fixture
        let milestone_years = vec![2010, 2015, 2020];

        let constraints = vec![ProcessInvestmentConstraintRaw {
            process_id: "process1".into(),
            regions: "GBR".into(),
            commission_years: "2025".into(), // Outside milestone years (2010-2020)
            addition_limit: CapacityPerYear(100.0),
        }];

        // Should fail with milestone year validation error
        let result = read_process_investment_constraints_from_iter(
            constraints.into_iter(),
            &processes,
            &milestone_years,
        );
        assert_error!(
            result,
            "Invalid year for constraint on process process1. Valid years are [2010, 2015, 2020]"
        );
    }

    #[test]
    fn validate_addition_with_finite_value() {
        // Valid: addition constraint with positive value
        let valid = validate_raw_constraint(CapacityPerYear(10.0));
        valid.unwrap();

        // Valid: addition constraint with zero value
        let valid = validate_raw_constraint(CapacityPerYear(0.0));
        valid.unwrap();

        // Not valid: addition constraint with negative value
        let invalid = validate_raw_constraint(CapacityPerYear(-10.0));
        assert_error!(
            invalid,
            "Invalid value for addition constraint: '-10'; must be non-negative and finite."
        );
    }

    #[test]
    fn validate_addition_rejects_infinite() {
        // Invalid: infinite value
        let invalid = validate_raw_constraint(CapacityPerYear(f64::INFINITY));
        assert_error!(
            invalid,
            "Invalid value for addition constraint: 'inf'; must be non-negative and finite."
        );

        // Invalid: NaN value
        let invalid = validate_raw_constraint(CapacityPerYear(f64::NAN));
        assert_error!(
            invalid,
            "Invalid value for addition constraint: 'NaN'; must be non-negative and finite."
        );
    }
}
