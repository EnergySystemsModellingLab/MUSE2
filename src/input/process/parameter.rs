//! Code for reading process parameters from a CSV file
use super::super::{format_items_with_cap, input_err_msg, read_csv, try_insert};
use crate::id::GetIDValue;
use crate::process::{ProcessID, ProcessMap, ProcessParameter, ProcessParameterMap};
use crate::region::parse_region_str;
use crate::units::{Dimensionless, MoneyPerActivity, MoneyPerCapacity, MoneyPerCapacityPerYear};
use ::log::warn;
use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

const PROCESS_PARAMETERS_FILE_NAME: &str = "process_parameters.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct ProcessParameterRaw {
    process_id: String,
    regions: String,
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
) -> Result<HashMap<ProcessID, ProcessParameterMap>> {
    let file_path = model_dir.join(PROCESS_PARAMETERS_FILE_NAME);
    let iter = read_csv::<ProcessParameterRaw>(&file_path)?;
    read_process_parameters_from_iter(iter, processes).with_context(|| input_err_msg(&file_path))
}

fn read_process_parameters_from_iter<I>(
    iter: I,
    processes: &ProcessMap,
) -> Result<HashMap<ProcessID, ProcessParameterMap>>
where
    I: Iterator<Item = ProcessParameterRaw>,
{
    let mut map: HashMap<ProcessID, ProcessParameterMap> = HashMap::new();
    for param_raw in iter {
        // Get process
        let (id, process) = processes.get_id_value(&param_raw.process_id)?;

        // Get regions
        let process_regions = &process.regions;
        let parameter_regions = parse_region_str(&param_raw.regions, process_regions)
            .with_context(|| {
                format!("Invalid region for process {id}. Valid regions are {process_regions:?}")
            })?;

        // Insert parameter into the map
        let param = Arc::new(param_raw.into_parameter()?);
        let entry = map.entry(id.clone()).or_default();
        for region in parameter_regions {
            try_insert(entry, &region, param.clone())?;
        }
    }

    check_process_parameters(processes, &map)?;

    Ok(map)
}

/// Check parameters cover all regions of the process
fn check_process_parameters(
    processes: &ProcessMap,
    map: &HashMap<ProcessID, ProcessParameterMap>,
) -> Result<()> {
    for (process_id, process) in processes {
        let parameters = map
            .get(process_id)
            .with_context(|| format!("Missing parameters for process {process_id}"))?;

        let reference_regions = &process.regions;

        let mut missing_keys = Vec::new();
        for region in reference_regions {
            if !parameters.contains_key(region) {
                missing_keys.push(region.clone());
            }
        }
        ensure!(
            missing_keys.is_empty(),
            "Process {process_id} is missing parameters for the following regions: {}",
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
    fn param_raw_into_param_ok() {
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
        param_map.insert(process_id, process_parameter_map.clone());
        let result = check_process_parameters(&processes, &param_map);
        result.unwrap();
    }

    #[rstest]
    fn check_process_parameters_missing(
        processes: ProcessMap,
        mut process_parameter_map: ProcessParameterMap,
        region_id: RegionID,
    ) {
        let mut param_map: HashMap<ProcessID, ProcessParameterMap> = HashMap::new();
        let process_id = processes.keys().next().unwrap().clone();
        // Remove one region key to simulate missing parameter
        process_parameter_map.remove(&region_id).unwrap();
        param_map.insert(process_id, process_parameter_map);

        let result = check_process_parameters(&processes, &param_map);
        assert_error!(
            result,
            "Process process1 is missing parameters for the following regions: \
            [RegionID(\"GBR\")]"
        );
    }

    #[test]
    fn param_raw_validate_bad_lifetime() {
        // lifetime = 0
        assert!(
            create_param_raw(0, Some(Dimensionless(1.0)))
                .validate()
                .is_err()
        );
    }

    #[test]
    fn param_raw_validate_bad_discount_rate() {
        // discount rate = -1
        assert!(
            create_param_raw(1, Some(Dimensionless(-1.0)))
                .validate()
                .is_err()
        );
    }
}
