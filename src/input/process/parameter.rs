//! Code for reading process parameters CSV file
use super::super::{format_items_with_cap, input_err_msg, read_csv, try_insert};
use crate::process::{ProcessID, ProcessMap, ProcessParameter, ProcessParameterMap};
use crate::region::parse_region_str;
use crate::units::{Dimensionless, MoneyPerActivity, MoneyPerCapacity, MoneyPerCapacityPerYear};
use crate::year::parse_year_str;
use ::log::warn;
use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

const PROCESS_PARAMETERS_FILE_NAME: &str = "process_parameters.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct ProcessParameterRaw {
    process_id: String,
    regions: String,
    years: String,
    capital_cost: MoneyPerCapacity,
    fixed_operating_cost: MoneyPerCapacityPerYear,
    variable_operating_cost: MoneyPerActivity,
    lifetime: u32,
    discount_rate: Option<Dimensionless>,
}

impl ProcessParameterRaw {
    fn into_parameter(self) -> Result<ProcessParameter> {
        self.validate()?;

        Ok(ProcessParameter {
            capital_cost: self.capital_cost,
            fixed_operating_cost: self.fixed_operating_cost,
            variable_operating_cost: self.variable_operating_cost,
            lifetime: self.lifetime,
            discount_rate: self.discount_rate.unwrap_or(Dimensionless(0.0)),
        })
    }
}

impl ProcessParameterRaw {
    /// Validates the `ProcessParameterRaw` instance.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - `lifetime` is 0.
    /// - `discount_rate` is present and less than 0.0.
    ///
    /// # Warnings
    ///
    /// Logs a warning if:
    /// - `discount_rate` is present and greater than 1.0.
    ///
    /// # Returns
    ///
    /// Returns `Ok(())` if all validations pass.
    fn validate(&self) -> Result<()> {
        ensure!(
            self.lifetime > 0,
            "Error in parameter for process {}: Lifetime must be greater than 0",
            self.process_id
        );

        if let Some(dr) = self.discount_rate {
            ensure!(
                dr >= Dimensionless(0.0),
                "Error in parameter for process {}: Discount rate must be positive",
                self.process_id
            );

            if dr > Dimensionless(1.0) {
                warn!(
                    "Warning in parameter for process {}: Discount rate is greater than 1",
                    self.process_id
                );
            }
        }

        Ok(())
    }
}

/// Read process parameters from the specified model directory
pub fn read_process_parameters(
    model_dir: &Path,
    processes: &ProcessMap,
    base_year: u32,
) -> Result<HashMap<ProcessID, ProcessParameterMap>> {
    let file_path = model_dir.join(PROCESS_PARAMETERS_FILE_NAME);
    let iter = read_csv::<ProcessParameterRaw>(&file_path)?;
    read_process_parameters_from_iter(iter, processes, base_year)
        .with_context(|| input_err_msg(&file_path))
}

fn read_process_parameters_from_iter<I>(
    iter: I,
    processes: &ProcessMap,
    base_year: u32,
) -> Result<HashMap<ProcessID, ProcessParameterMap>>
where
    I: Iterator<Item = ProcessParameterRaw>,
{
    let mut map: HashMap<ProcessID, ProcessParameterMap> = HashMap::new();
    for param_raw in iter {
        // Get process
        let (id, process) = processes
            .get_key_value(param_raw.process_id.as_str())
            .with_context(|| format!("Process {} not found", param_raw.process_id))?;

        // Get years
        let process_years = &process.years;
        let parameter_years =
            parse_year_str(&param_raw.years, process_years).with_context(|| {
                format!("Invalid year for process {id}. Valid years are {process_years:?}")
            })?;

        // Get regions
        let process_regions = &process.regions;
        let parameter_regions = parse_region_str(&param_raw.regions, process_regions)
            .with_context(|| {
                format!("Invalid region for process {id}. Valid regions are {process_regions:?}")
            })?;

        // Insert parameter into the map
        let param = Rc::new(param_raw.into_parameter()?);
        let entry = map.entry(id.clone()).or_default();
        for year in parameter_years {
            for region in parameter_regions.clone() {
                try_insert(entry, &(region, year), param.clone())?;
            }
        }
    }

    check_process_parameters(processes, &map, base_year)?;

    Ok(map)
}

/// Check parameters cover all years and regions of the process
fn check_process_parameters(
    processes: &ProcessMap,
    map: &HashMap<ProcessID, ProcessParameterMap>,
    base_year: u32,
) -> Result<()> {
    for (process_id, process) in processes {
        let parameters = map
            .get(process_id)
            .with_context(|| format!("Missing parameters for process {process_id}"))?;

        let reference_years = &process.years;
        let reference_regions = &process.regions;

        // Only give an error for missing parameters >=base_year, so that users are not obliged to
        // supply them for every valid year before the time horizon
        let mut missing_keys = Vec::new();
        for year in reference_years.iter().filter(|year| **year >= base_year) {
            for region in reference_regions {
                let key = (region.clone(), *year);
                if !parameters.contains_key(&key) {
                    missing_keys.push(key);
                }
            }
        }
        ensure!(
            missing_keys.is_empty(),
            "Process {process_id} is missing parameters for the following regions and years: {}",
            format_items_with_cap(&missing_keys)
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{assert_error, process_parameter_map, processes, region_id};
    use crate::process::{ProcessID, ProcessMap, ProcessParameterMap};
    use crate::region::RegionID;
    use rstest::rstest;
    use std::collections::HashMap;

    fn create_param_raw(
        lifetime: u32,
        discount_rate: Option<Dimensionless>,
    ) -> ProcessParameterRaw {
        ProcessParameterRaw {
            process_id: "id".to_string(),
            capital_cost: MoneyPerCapacity(0.0),
            fixed_operating_cost: MoneyPerCapacityPerYear(0.0),
            variable_operating_cost: MoneyPerActivity(0.0),
            lifetime,
            discount_rate,
            years: "all".to_string(),
            regions: "all".to_string(),
        }
    }

    fn create_param(discount_rate: Dimensionless) -> ProcessParameter {
        ProcessParameter {
            capital_cost: MoneyPerCapacity(0.0),
            fixed_operating_cost: MoneyPerCapacityPerYear(0.0),
            variable_operating_cost: MoneyPerActivity(0.0),
            lifetime: 1,
            discount_rate,
        }
    }

    #[test]
    fn test_param_raw_into_param_ok() {
        // No missing values
        let raw = create_param_raw(1, Some(Dimensionless(1.0)));
        assert_eq!(
            raw.into_parameter().unwrap(),
            create_param(Dimensionless(1.0))
        );

        // Missing discount_rate
        let raw = create_param_raw(1, None);
        assert_eq!(
            raw.into_parameter().unwrap(),
            create_param(Dimensionless(0.0))
        );
    }

    #[rstest]
    fn check_process_parameters_ok(
        processes: ProcessMap,
        process_parameter_map: ProcessParameterMap,
    ) {
        let mut param_map: HashMap<ProcessID, ProcessParameterMap> = HashMap::new();
        let process_id = processes.keys().next().unwrap().clone();
        let base_year = 2010;

        param_map.insert(process_id, process_parameter_map.clone());
        let result = check_process_parameters(&processes, &param_map, base_year);
        assert!(result.is_ok());
    }

    #[rstest]
    fn check_process_parameters_ok_missing_before_base_year(
        processes: ProcessMap,
        mut process_parameter_map: ProcessParameterMap,
        region_id: RegionID,
    ) {
        let mut param_map: HashMap<ProcessID, ProcessParameterMap> = HashMap::new();
        let process_id = processes.keys().next().unwrap().clone();
        let base_year = 2015;

        // Remove one entry before base_year
        process_parameter_map.remove(&(region_id, 2012)).unwrap();
        param_map.insert(process_id, process_parameter_map);

        let result = check_process_parameters(&processes, &param_map, base_year);
        assert!(result.is_ok());
    }

    #[rstest]
    fn check_process_parameters_missing(
        processes: ProcessMap,
        mut process_parameter_map: ProcessParameterMap,
        region_id: RegionID,
    ) {
        let mut param_map: HashMap<ProcessID, ProcessParameterMap> = HashMap::new();
        let process_id = processes.keys().next().unwrap().clone();
        let base_year = 2010;

        // Remove one region-year key to simulate missing parameter
        process_parameter_map.remove(&(region_id, 2010)).unwrap();
        param_map.insert(process_id, process_parameter_map);

        let result = check_process_parameters(&processes, &param_map, base_year);
        assert_error!(
            result,
            "Process process1 is missing parameters for the following regions and years: \
            [(RegionID(\"GBR\"), 2010)]"
        );
    }

    #[test]
    fn test_param_raw_validate_bad_lifetime() {
        // lifetime = 0
        assert!(
            create_param_raw(0, Some(Dimensionless(1.0)))
                .validate()
                .is_err()
        );
    }

    #[test]
    fn test_param_raw_validate_bad_discount_rate() {
        // discount rate = -1
        assert!(
            create_param_raw(1, Some(Dimensionless(-1.0)))
                .validate()
                .is_err()
        );
    }
}
