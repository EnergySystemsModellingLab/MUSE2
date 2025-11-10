//! Code for reading process availabilities CSV file
use super::super::{input_err_msg, read_csv, try_insert};
use crate::process::{ProcessActivityLimitsMap, ProcessID, ProcessMap};
use crate::region::parse_region_str;
use crate::time_slice::{TimeSliceInfo, TimeSliceSelection};
use crate::units::PerYear;
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use itertools::iproduct;
use serde::Deserialize;
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::path::Path;
use std::rc::Rc;

const PROCESS_AVAILABILITIES_FILE_NAME: &str = "process_availabilities.csv";

/// Represents a row of the process availabilities CSV file
#[derive(Deserialize)]
struct ProcessAvailabilityRaw {
    process_id: String,
    regions: String,
    commission_years: String,
    time_slice: String,
    limit_type: LimitType,
    value: PerYear,
}

impl ProcessAvailabilityRaw {
    fn validate(&self) -> Result<()> {
        // Check availability value
        ensure!(
            self.value >= PerYear(0.0) && self.value <= PerYear(1.0),
            "Value for availability must be between 0 and 1 inclusive"
        );

        Ok(())
    }

    /// Get this limit as a range
    fn to_range(&self) -> RangeInclusive<PerYear> {
        // We know ts_length also represents a fraction of a year, so this is ok.
        match self.limit_type {
            LimitType::LowerBound => self.value..=PerYear(1.0),
            LimitType::UpperBound => PerYear(0.0)..=self.value,
            LimitType::Equality => self.value..=self.value,
        }
    }
}

/// The type of limit given for availability
#[derive(DeserializeLabeledStringEnum)]
enum LimitType {
    #[string = "lo"]
    LowerBound,
    #[string = "up"]
    UpperBound,
    #[string = "fx"]
    Equality,
}

/// Read the process availabilities CSV file.
///
/// This file contains information about the availability of processes over the course of a year as
/// a proportion of their maximum capacity.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `processes` - Map of processes
/// * `time_slice_info` - Information about seasons and times of day
///
/// # Returns
///
/// A [`HashMap`] with process IDs as the keys and [`ProcessActivityLimitsMap`]s as the values or an
/// error.
pub fn read_process_availabilities(
    model_dir: &Path,
    processes: &ProcessMap,
    time_slice_info: &TimeSliceInfo,
) -> Result<HashMap<ProcessID, ProcessActivityLimitsMap>> {
    let file_path = model_dir.join(PROCESS_AVAILABILITIES_FILE_NAME);
    let process_availabilities_csv = read_csv(&file_path)?;
    read_process_availabilities_from_iter(process_availabilities_csv, processes, time_slice_info)
        .with_context(|| input_err_msg(&file_path))
}

/// Process raw process availabilities input data into [`ProcessActivityLimitsMap`]s.
///
/// # Arguments
///
/// * `iter` - Iterator of raw process availability records
/// * `processes` - Map of processes
/// * `time_slice_info` - Information about seasons and times of day
///
/// # Returns
///
/// A [`HashMap`] with process IDs as the keys and [`ProcessActivityLimitsMap`]s as the values or an
/// error.
fn read_process_availabilities_from_iter<I>(
    iter: I,
    processes: &ProcessMap,
    time_slice_info: &TimeSliceInfo,
) -> Result<HashMap<ProcessID, ProcessActivityLimitsMap>>
where
    I: Iterator<Item = ProcessAvailabilityRaw>,
{
    let mut map = HashMap::new();
    for record in iter {
        record.validate()?;

        // Get process
        let (id, process) = processes
            .get_key_value(record.process_id.as_str())
            .with_context(|| format!("Process {} not found", record.process_id))?;

        // Get regions
        let process_regions = &process.regions;
        let record_regions =
            parse_region_str(&record.regions, process_regions).with_context(|| {
                format!("Invalid region for process {id}. Valid regions are {process_regions:?}")
            })?;

        // Get years
        let process_years = &process.years;
        let record_years =
            parse_year_str(&record.commission_years, process_years).with_context(|| {
                format!("Invalid year for process {id}. Valid years are {process_years:?}")
            })?;

        // Get time slices
        let ts_selection = time_slice_info.get_selection(&record.time_slice)?;

        // Insert the activity limit into the map
        let limits_map: &mut ProcessActivityLimitsMap = map.entry(id.clone()).or_default();
        for (region_id, year) in iproduct!(&record_regions, &record_years) {
            let limits_map_inner = limits_map.entry((region_id.clone(), *year)).or_default();
            let limits_map_inner = Rc::get_mut(limits_map_inner).unwrap();
            try_insert(limits_map_inner, &ts_selection, record.to_range())?;
        }
    }

    add_fallback_limits(&mut map, processes);

    Ok(map)
}

/// Add limits of 0..=1 for whole year for every region and year, everywhere needed.
///
/// The reason this is needed is because users are not required to provide limits for every single
/// time slice (in fact, there may be none at all), and we need to add a constraint to the
/// optimisation so that the total activity for the year does not exceed the maximum for a given
/// asset.
fn add_fallback_limits(
    all_availabilities: &mut HashMap<ProcessID, ProcessActivityLimitsMap>,
    processes: &ProcessMap,
) {
    for (process_id, process) in processes {
        // A map of maps: the outer map is keyed by region and year; the inner one by time slice
        // selection
        let map_for_process = all_availabilities.entry(process_id.clone()).or_default();

        // Iterate over all regions and years for this process and insert a fallback availability
        // limit covering the whole year, in case it's needed
        for (region, &year) in iproduct!(&process.regions, &process.years) {
            // Should succeed because there should be only one reference
            let map_for_region_year =
                Rc::get_mut(map_for_process.entry((region.clone(), year)).or_default()).unwrap();
            map_for_region_year
                .entry(TimeSliceSelection::Annual)
                .or_insert(PerYear(0.0)..=PerYear(1.0));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_process_availability_raw(
        limit_type: LimitType,
        value: PerYear,
    ) -> ProcessAvailabilityRaw {
        ProcessAvailabilityRaw {
            process_id: "process".into(),
            regions: "region".into(),
            commission_years: "2010".into(),
            time_slice: "day".into(),
            limit_type,
            value,
        }
    }

    #[test]
    fn test_validate() {
        // Valid
        let valid = create_process_availability_raw(LimitType::LowerBound, PerYear(0.5));
        assert!(valid.validate().is_ok());
        let valid = create_process_availability_raw(LimitType::LowerBound, PerYear(0.0));
        assert!(valid.validate().is_ok());
        let valid = create_process_availability_raw(LimitType::LowerBound, PerYear(1.0));
        assert!(valid.validate().is_ok());

        // Invalid: negative value
        let invalid = create_process_availability_raw(LimitType::LowerBound, PerYear(-0.5));
        assert!(invalid.validate().is_err());

        // Invalid: value greater than 1
        let invalid = create_process_availability_raw(LimitType::LowerBound, PerYear(1.5));
        assert!(invalid.validate().is_err());

        // Invalid: infinity value
        let invalid =
            create_process_availability_raw(LimitType::LowerBound, PerYear(f64::INFINITY));
        assert!(invalid.validate().is_err());

        // Invalid: negative infinity value
        let invalid =
            create_process_availability_raw(LimitType::LowerBound, PerYear(f64::NEG_INFINITY));
        assert!(invalid.validate().is_err());

        // Invalid: NaN value
        let invalid = create_process_availability_raw(LimitType::LowerBound, PerYear(f64::NAN));
        assert!(invalid.validate().is_err());
    }

    #[test]
    fn test_to_bounds() {
        // Lower bound
        let raw = create_process_availability_raw(LimitType::LowerBound, PerYear(0.5));
        let bounds = raw.to_range();
        assert_eq!(bounds, PerYear(0.5)..=PerYear(1.0));

        // Upper bound
        let raw = create_process_availability_raw(LimitType::UpperBound, PerYear(0.5));
        let bounds = raw.to_range();
        assert_eq!(bounds, PerYear(0.0)..=PerYear(0.5));

        // Equality
        let raw = create_process_availability_raw(LimitType::Equality, PerYear(0.5));
        let bounds = raw.to_range();
        assert_eq!(bounds, PerYear(0.5)..=PerYear(0.5));
    }
}
