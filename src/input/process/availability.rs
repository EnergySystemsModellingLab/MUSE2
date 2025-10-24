//! Code for reading process availabilities CSV file
use super::super::{format_items_with_cap, input_err_msg, read_csv, try_insert};
use crate::process::{Process, ProcessActivityLimitsMap, ProcessID, ProcessMap};
use crate::region::parse_region_str;
use crate::time_slice::TimeSliceInfo;
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
/// * `base_year` - First milestone year of simulation
///
/// # Returns
///
/// A [`HashMap`] with process IDs as the keys and [`ProcessActivityLimitsMap`]s as the values or an
/// error.
pub fn read_process_availabilities(
    model_dir: &Path,
    processes: &ProcessMap,
    time_slice_info: &TimeSliceInfo,
    base_year: u32,
) -> Result<HashMap<ProcessID, ProcessActivityLimitsMap>> {
    let file_path = model_dir.join(PROCESS_AVAILABILITIES_FILE_NAME);
    let process_availabilities_csv = read_csv(&file_path)?;
    read_process_availabilities_from_iter(
        process_availabilities_csv,
        processes,
        time_slice_info,
        base_year,
    )
    .with_context(|| input_err_msg(&file_path))
}

/// Process raw process availabilities input data into [`ProcessActivityLimitsMap`]s.
///
/// # Arguments
///
/// * `iter` - Iterator of raw process availability records
/// * `processes` - Map of processes
/// * `time_slice_info` - Information about seasons and times of day
/// * `base_year` - First milestone year of simulation
///
/// # Returns
///
/// A [`HashMap`] with process IDs as the keys and [`ProcessActivityLimitsMap`]s as the values or an
/// error.
fn read_process_availabilities_from_iter<I>(
    iter: I,
    processes: &ProcessMap,
    time_slice_info: &TimeSliceInfo,
    base_year: u32,
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
        let limits_map = map
            .entry(id.clone())
            .or_insert_with(ProcessActivityLimitsMap::new);
        for (region_id, year) in iproduct!(&record_regions, &record_years) {
            let limits_map_inner = limits_map
                .entry((region_id.clone(), *year))
                .or_insert_with(|| Rc::new(HashMap::new()));
            let limits_map_inner = Rc::get_mut(limits_map_inner).unwrap();
            for (time_slice, _) in ts_selection.iter(time_slice_info) {
                try_insert(limits_map_inner, time_slice, record.to_range())?;
            }
        }
    }

    validate_activity_limits_maps(&map, processes, time_slice_info, base_year)?;

    Ok(map)
}

/// Check that the activity limits cover every time slice and all regions/years of the process
fn validate_activity_limits_maps(
    all_availabilities: &HashMap<ProcessID, ProcessActivityLimitsMap>,
    processes: &ProcessMap,
    time_slice_info: &TimeSliceInfo,
    base_year: u32,
) -> Result<()> {
    for (process_id, process) in processes {
        // A map of maps: the outer map is keyed by region and year; the inner one by time slice
        let map_for_process = all_availabilities
            .get(process_id)
            .with_context(|| format!("Missing availabilities for process {process_id}"))?;

        check_missing_milestone_years(process, map_for_process, base_year)?;
        check_missing_time_slices(process, map_for_process, time_slice_info)?;
    }

    Ok(())
}

/// Check every milestone year in which the process can be commissioned has availabilities.
///
/// Entries for non-milestone years in which the process can be commissioned (which are only
/// required for pre-defined assets, if at all) are not required and will be checked lazily when
/// assets requiring them are constructed.
fn check_missing_milestone_years(
    process: &Process,
    map_for_process: &ProcessActivityLimitsMap,
    base_year: u32,
) -> Result<()> {
    let process_milestone_years = process
        .years
        .iter()
        .copied()
        .filter(|&year| year >= base_year);
    let mut missing = Vec::new();
    for (region_id, year) in iproduct!(&process.regions, process_milestone_years) {
        if !map_for_process.contains_key(&(region_id.clone(), year)) {
            missing.push((region_id, year));
        }
    }

    ensure!(
        missing.is_empty(),
        "Process {} is missing availabilities for the following regions and milestone years: {}",
        &process.id,
        format_items_with_cap(&missing)
    );

    Ok(())
}

/// Check that entries for all time slices are provided for any process/region/year combo for which
/// we have any entries at all
fn check_missing_time_slices(
    process: &Process,
    map_for_process: &ProcessActivityLimitsMap,
    time_slice_info: &TimeSliceInfo,
) -> Result<()> {
    let mut missing = Vec::new();
    for (region_id, &year) in iproduct!(&process.regions, &process.years) {
        if let Some(map_for_region_year) = map_for_process.get(&(region_id.clone(), year)) {
            // There are at least some entries for this region/year combo; check if there are
            // any time slices not covered
            missing.extend(
                time_slice_info
                    .iter_ids()
                    .filter(|ts| !map_for_region_year.contains_key(ts))
                    .map(|ts| (region_id, year, ts)),
            );
        }
    }

    ensure!(
        missing.is_empty(),
        "Availabilities supplied for some, but not all time slices, for process {}. The following \
        regions, years and time slices are missing: {}",
        &process.id,
        format_items_with_cap(&missing)
    );

    Ok(())
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
