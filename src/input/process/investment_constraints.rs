//! Code for reading process investment constraints CSV file
use super::super::input_err_msg;
use super::availability::LimitType;
use crate::input::read_csv_optional;
use crate::process::{
    InvestmentConstraintValue, ProcessID, ProcessInvestmentConstraint,
    ProcessInvestmentConstraintsMap, ProcessMap,
};
use crate::region::parse_region_str;
use anyhow::{Context, Result, bail, ensure};
use serde::Deserialize;
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

const PROCESS_INVESTMENT_CONSTRAINTS_FILE_NAME: &str = "process_investment_constraints.csv";

/// Represents a row of the process investment constraints CSV file
#[derive(Deserialize)]
struct ProcessInvestmentConstraintRaw {
    constraint_name: String,
    process_id: String,
    #[serde(default = "default_region")]
    regions: String,
    commission_years: u32,
    limit_type: LimitType,
    constraint_type: ConstraintType,
    growth_constraint_seed: Option<f64>,
    value: f64,
}

/// Default value for region field when not specified
fn default_region() -> String {
    "ALL".to_string()
}

impl ProcessInvestmentConstraintRaw {
    /// Validate the constraint record for logical consistency and required fields
    fn validate(&self) -> Result<()> {
        // Validate that growth_constraint_seed is provided when constraint_type is growth
        if matches!(self.constraint_type, ConstraintType::Growth) {
            bail!("Growth constraints are not supported yet!")
        }
        if matches!(self.constraint_type, ConstraintType::Limit) {
            bail!("Limit constraints are not supported yet!")
        }

        // Validate that value is finite
        ensure!(
            self.value.is_finite(),
            "Constraint value must be finite for constraint '{}'",
            self.constraint_name
        );

        Ok(())
    }
}

/// The type of investment constraint being applied
#[derive(DeserializeLabeledStringEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstraintType {
    /// Growth rate constraint (relative change between periods)
    #[string = "growth"]
    Growth,
    /// Addition constraint (absolute change between periods)
    #[string = "addition"]
    Addition,
    /// Absolute limit constraint (total capacity limit)
    #[string = "limit"]
    Limit,
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

        // Create a processed constraint for each region
        for region in &record_regions {
            let constraint_value = match record.constraint_type {
                ConstraintType::Addition => InvestmentConstraintValue::Addition {
                    addition_limit: record.value,
                },
                ConstraintType::Growth => InvestmentConstraintValue::Growth {
                    growth_constraint_seed: record.growth_constraint_seed.unwrap(),
                },
                ConstraintType::Limit => InvestmentConstraintValue::Limit {},
            };

            let limit_type_str = match record.limit_type {
                LimitType::LowerBound => "LowerBound",
                LimitType::UpperBound => "UpperBound",
                LimitType::Equality => "Equality",
            };

            let constraint = ProcessInvestmentConstraint {
                constraint_name: record.constraint_name.clone(),
                constraint_value,
                limit_type: limit_type_str.to_string(),
            };

            let process_map = map.entry(process_id.clone()).or_default();
            let constraints_vec = process_map
                .entry((region.clone(), record.commission_years))
                .or_insert_with(|| Rc::new(Vec::new()));

            let constraints_mut = Rc::get_mut(constraints_vec)
                .expect("Should have exclusive access during construction");
            constraints_mut.push(constraint);
        }
    }

    validate_constraint_consistency(&map)?;

    Ok(map)
}

/// Validate that constraints are internally consistent
///
/// Checks for duplicate constraint names within each process/region/year combination.
fn validate_constraint_consistency(
    map: &HashMap<ProcessID, ProcessInvestmentConstraintsMap>,
) -> Result<()> {
    for (process_id, constraints_map) in map {
        for ((region_id, year), constraints) in constraints_map {
            // Check for duplicate constraint names
            let mut seen = HashSet::new();
            for constraint in constraints.iter() {
                if !seen.insert(&constraint.constraint_name) {
                    bail!(
                        "Duplicate constraint name '{}' for process '{}', region '{}', year {}",
                        constraint.constraint_name,
                        process_id,
                        region_id,
                        year
                    );
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_raw_constraint(
        constraint_type: ConstraintType,
        limit_type: LimitType,
        value: f64,
        growth_constraint_seed: Option<f64>,
    ) -> ProcessInvestmentConstraintRaw {
        ProcessInvestmentConstraintRaw {
            constraint_name: "test_constraint".into(),
            process_id: "test_process".into(),
            regions: "ALL".into(),
            commission_years: 2030,
            limit_type,
            constraint_type,
            growth_constraint_seed,
            value,
        }
    }

    #[test]
    fn test_validate_growth_not_supported() {
        // Growth constraints should fail with "not supported yet" message
        let constraint = create_raw_constraint(
            ConstraintType::Growth,
            LimitType::UpperBound,
            0.1,
            Some(100.0),
        );
        let result = constraint.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not supported"));
    }

    #[test]
    fn test_validate_limit_not_supported() {
        // Limit constraints should fail with "not supported yet" message
        let constraint =
            create_raw_constraint(ConstraintType::Limit, LimitType::UpperBound, 100.0, None);
        let result = constraint.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not supported"));
    }

    #[test]
    fn test_validate_addition_with_finite_value() {
        // Valid: addition constraint with positive value
        let valid =
            create_raw_constraint(ConstraintType::Addition, LimitType::UpperBound, 10.0, None);
        assert!(valid.validate().is_ok());

        // Valid: addition constraint with zero value
        let valid =
            create_raw_constraint(ConstraintType::Addition, LimitType::UpperBound, 0.0, None);
        assert!(valid.validate().is_ok());

        // Valid: addition constraint with negative value (constraint allows this)
        let valid =
            create_raw_constraint(ConstraintType::Addition, LimitType::LowerBound, -10.0, None);
        assert!(valid.validate().is_ok());
    }

    #[test]
    fn test_validate_addition_rejects_infinite() {
        // Invalid: infinite value
        let invalid = create_raw_constraint(
            ConstraintType::Addition,
            LimitType::UpperBound,
            f64::INFINITY,
            None,
        );
        assert!(invalid.validate().is_err());

        // Invalid: NaN value
        let invalid = create_raw_constraint(
            ConstraintType::Addition,
            LimitType::UpperBound,
            f64::NAN,
            None,
        );
        assert!(invalid.validate().is_err());
    }
}
