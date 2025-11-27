//! Code for reading process availabilities CSV file
use super::super::{input_err_msg, read_csv, try_insert};
use crate::process::{ProcessActivityLimitsMap, ProcessAvailabilities, ProcessID, ProcessMap};
use crate::region::parse_region_str;
use crate::time_slice::TimeSliceInfo;
use crate::units::{Dimensionless, Year};
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use itertools::iproduct;
use serde::Deserialize;
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
    limits: String,
}

impl ProcessAvailabilityRaw {
    /// Calculate fraction of annual energy as availability multiplied by time slice length.
    ///
    /// The resulting limits are max/min energy produced/consumed in each time slice per
    /// `capacity_to_activity` units of capacity.
    fn to_bounds(&self, ts_length: Year) -> Result<RangeInclusive<Dimensionless>> {
        // Parse availability_range string
        let availability_range = parse_availabilities_string(&self.limits)?;

        // We know ts_length also represents a fraction of a year, so this is ok.
        let ts_frac = ts_length / Year(1.0);
        let start = *availability_range.start() * ts_frac;
        let end = *availability_range.end() * ts_frac;
        Ok(start..=end)
    }
}

fn parse_availabilities_string(s: &str) -> Result<RangeInclusive<Dimensionless>> {
    // Disallow empty string
    ensure!(!s.trim().is_empty(), "Availability range cannot be empty");

    // Require exactly one ".." separator so only forms lower..upper, lower.. or ..upper are allowed.
    let parts: Vec<&str> = s.split("..").collect();
    ensure!(
        parts.len() == 2,
        "Availability range must be of the form 'lower..upper', 'lower..' or '..upper'. Invalid: {s}"
    );
    let left = parts[0].trim();
    let right = parts[1].trim();

    // Create range
    let start = if left.is_empty() {
        Dimensionless(0.0)
    } else {
        left.parse::<f64>()
            .ok()
            .with_context(|| format!("Invalid lower availability limit: {left}"))?
            .into()
    };
    let end = if right.is_empty() {
        Dimensionless(1.0)
    } else {
        right
            .parse::<f64>()
            .ok()
            .with_context(|| format!("Invalid upper availability limit: {right}"))?
            .into()
    };

    // Validation checks
    ensure!(
        end >= start,
        "Upper availability limit must be greater than or equal to lower limit. Invalid: {s}"
    );
    ensure!(
        start >= Dimensionless(0.0),
        "Lower availability limit must be >= 0. Invalid: {s}"
    );
    ensure!(
        end <= Dimensionless(1.0),
        "Upper availability limit must be <= 1. Invalid: {s}"
    );

    Ok(start..=end)
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
/// * `milestone_years` - Milestone years of simulation
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
    // Collect all entries
    let mut entries: HashMap<ProcessID, _> = processes
        .iter()
        .map(|(id, _)| (id.clone(), HashMap::new()))
        .collect();

    for record in iter {
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
        let process_years: Vec<u32> = process.years.clone().collect();
        let record_years =
            parse_year_str(&record.commission_years, &process_years).with_context(|| {
                format!("Invalid year for process {id}. Valid years are {process_years:?}")
            })?;

        // Get time slices
        let ts_selection = time_slice_info.get_selection(&record.time_slice)?;

        // Store the activity limit for each region/year
        let entries_for_process = entries.get_mut(id).unwrap();
        for (region_id, year) in iproduct!(&record_regions, &record_years) {
            let entries_for_process_region_year = entries_for_process
                .entry((region_id.clone(), *year))
                .or_default();
            let length = time_slice_info.length_for_selection(&ts_selection)?;
            try_insert(
                entries_for_process_region_year,
                &ts_selection,
                record.to_bounds(length)?,
            )?;
        }
    }

    // Create `ProcessActivityLimitsMap`s
    let mut map: HashMap<ProcessID, ProcessActivityLimitsMap> = HashMap::new();
    for (process_id, process) in processes {
        let mut inner_map = HashMap::new();
        let entries_for_process = &entries[process_id];
        for (region_id, year) in iproduct!(&process.regions, process.years.clone()) {
            let limits = entries_for_process
                .get(&(region_id.clone(), year))
                .cloned()
                .unwrap_or_default();
            let availabilities = ProcessAvailabilities::new_from_limits(&limits, time_slice_info);
            inner_map.insert((region_id.clone(), year), Rc::new(availabilities));
        }
        map.insert(process_id.clone(), inner_map);
    }

    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use float_cmp::assert_approx_eq;
    use rstest::rstest;

    fn create_process_availability_raw(limits: String) -> ProcessAvailabilityRaw {
        ProcessAvailabilityRaw {
            process_id: "process".into(),
            regions: "region".into(),
            commission_years: "2010".into(),
            time_slice: "day".into(),
            limits,
        }
    }

    #[rstest]
    #[case("0.1..0.9", Dimensionless(0.1)..=Dimensionless(0.9))]
    #[case("..0.9", Dimensionless(0.0)..=Dimensionless(0.9))] // Empty lower
    #[case("0.1..", Dimensionless(0.1)..=Dimensionless(1.0))] // Empty upper
    #[case("0.5..0.5", Dimensionless(0.5)..=Dimensionless(0.5))] // Equality
    fn test_parse_availabilities_string_valid(
        #[case] input: &str,
        #[case] expected: RangeInclusive<Dimensionless>,
    ) {
        assert_eq!(parse_availabilities_string(input).unwrap(), expected);
    }

    #[rstest]
    #[case("", "Availability range cannot be empty")]
    #[case(
        "0.6..0.5",
        "Upper availability limit must be greater than or equal to lower limit. Invalid: 0.6..0.5"
    )]
    #[case(
        "..0.1..0.9",
        "Availability range must be of the form 'lower..upper', 'lower..' or '..upper'. Invalid: ..0.1..0.9"
    )]
    #[case("0.1...0.9", "Invalid upper availability limit: .0.9")]
    #[case(
        "-0.1..0.5",
        "Lower availability limit must be >= 0. Invalid: -0.1..0.5"
    )]
    #[case("0.1..1.5", "Upper availability limit must be <= 1. Invalid: 0.1..1.5")]
    #[case("abc..0.5", "Invalid lower availability limit: abc")]
    #[case(
        "0.5",
        "Availability range must be of the form 'lower..upper', 'lower..' or '..upper'. Invalid: 0.5"
    )]
    fn test_parse_availabilities_string_invalid(#[case] input: &str, #[case] error_msg: &str) {
        assert_error!(parse_availabilities_string(input), error_msg);
    }

    #[rstest]
    #[case("0.1..", Year(0.1), Dimensionless(0.01)..=Dimensionless(0.1))] // Lower bound
    #[case("..0.5", Year(0.1), Dimensionless(0.0)..=Dimensionless(0.05))] // Upper bound
    #[case("0.5..0.5", Year(0.1), Dimensionless(0.05)..=Dimensionless(0.05))] // Equality
    fn test_to_bounds(
        #[case] limits: &str,
        #[case] ts_length: Year,
        #[case] expected: RangeInclusive<Dimensionless>,
    ) {
        let raw = create_process_availability_raw(limits.into());
        let bounds = raw.to_bounds(ts_length).unwrap();
        assert_approx_eq!(Dimensionless, *bounds.start(), *expected.start());
        assert_approx_eq!(Dimensionless, *bounds.end(), *expected.end());
    }
}
