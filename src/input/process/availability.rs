//! Code for reading process availabilities CSV file
use super::super::{format_items_with_cap, input_err_msg, read_csv, try_insert};
use crate::process::{ActivityLimits, Process, ProcessActivityLimitsMap, ProcessID, ProcessMap};
use crate::region::parse_region_str;
use crate::time_slice::{TimeSliceInfo, TimeSliceSelection};
use crate::units::{Dimensionless, Year};
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use indexmap::IndexMap;
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
    years: String,
    time_slice: String,
    limit_type: LimitType,
    value: Dimensionless,
}

impl ProcessAvailabilityRaw {
    fn validate(&self) -> Result<()> {
        // Check availability value
        ensure!(
            self.value >= Dimensionless(0.0) && self.value <= Dimensionless(1.0),
            "Value for availability must be between 0 and 1 inclusive"
        );

        Ok(())
    }

    /// Calculate fraction of annual energy as availability multiplied by duration.
    ///
    /// The resulting limits are max/min energy produced/consumed in each time slice per
    /// `capacity_to_activity` units of capacity.
    fn to_bounds(&self, duration: Year) -> RangeInclusive<Dimensionless> {
        assert!(duration >= Year(0.0) && duration <= Year(1.0));

        // We know duration also represents a fraction of a year, so this is ok.
        let fraction = duration / Year(1.0);
        let value = self.value * fraction;
        match self.limit_type {
            LimitType::LowerBound => value..=fraction,
            LimitType::UpperBound => Dimensionless(0.0)..=value,
            LimitType::Equality => value..=value,
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
        let record_years = parse_year_str(&record.years, process_years).with_context(|| {
            format!("Invalid year for process {id}. Valid years are {process_years:?}")
        })?;

        // Get time slices
        let ts_selection = time_slice_info.get_selection(&record.time_slice)?;

        // Insert the activity limit into the map
        let limits_map = map.entry(id.clone()).or_default();
        for (region_id, year) in iproduct!(&record_regions, &record_years) {
            let limits_map_inner = limits_map.entry((region_id.clone(), *year)).or_default();
            let limits_map_inner = Rc::get_mut(limits_map_inner).unwrap();
            let bounds = record.to_bounds(time_slice_info.get_duration(&ts_selection));
            try_insert(limits_map_inner, &ts_selection, bounds.clone())?;
        }
    }

    add_fallback_limits(&mut map, processes);

    Ok(map)
}

fn compute_limits_full(
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

            infer_limits(map_for_region_year);
            // map_for_region_year
            //     .entry(TimeSliceSelection::Annual)
            //     .or_insert(Dimensionless(0.0)..=Dimensionless(1.0));
        }
    }
}

// const DEFAULT_LIMITS: &RangeInclusive<>

fn infer_limits(
    time_slice_info: &TimeSliceInfo,
    map: &mut IndexMap<TimeSliceSelection, ActivityLimits>,
    ts_selection: &TimeSliceSelection,
    limits: &RangeInclusive<Dimensionless>,
) {
    let cur_limits = if let Some(limits) = map.get(ts_selection) {
        limits.limits
    } else {
        Dimensionless(0.0)..=Dimensionless(1.0)
    };

    let mut max = Dimensionless(1.0);
    let mut min = Dimensionless(0.0);
    for child in ts_selection.iter_children(time_slice_info) {}
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
                .or_insert(Dimensionless(0.0)..=Dimensionless(1.0));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_process_availability_raw(
        limit_type: LimitType,
        value: Dimensionless,
    ) -> ProcessAvailabilityRaw {
        ProcessAvailabilityRaw {
            process_id: "process".into(),
            regions: "region".into(),
            years: "2010".into(),
            time_slice: "day".into(),
            limit_type,
            value,
        }
    }

    #[test]
    fn test_validate() {
        // Valid
        let valid = create_process_availability_raw(LimitType::LowerBound, Dimensionless(0.5));
        assert!(valid.validate().is_ok());
        let valid = create_process_availability_raw(LimitType::LowerBound, Dimensionless(0.0));
        assert!(valid.validate().is_ok());
        let valid = create_process_availability_raw(LimitType::LowerBound, Dimensionless(1.0));
        assert!(valid.validate().is_ok());

        // Invalid: negative value
        let invalid = create_process_availability_raw(LimitType::LowerBound, Dimensionless(-0.5));
        assert!(invalid.validate().is_err());

        // Invalid: value greater than 1
        let invalid = create_process_availability_raw(LimitType::LowerBound, Dimensionless(1.5));
        assert!(invalid.validate().is_err());

        // Invalid: infinity value
        let invalid =
            create_process_availability_raw(LimitType::LowerBound, Dimensionless(f64::INFINITY));
        assert!(invalid.validate().is_err());

        // Invalid: negative infinity value
        let invalid = create_process_availability_raw(
            LimitType::LowerBound,
            Dimensionless(f64::NEG_INFINITY),
        );
        assert!(invalid.validate().is_err());

        // Invalid: NaN value
        let invalid =
            create_process_availability_raw(LimitType::LowerBound, Dimensionless(f64::NAN));
        assert!(invalid.validate().is_err());
    }

    #[test]
    fn test_to_bounds() {
        let ts_length = Year(0.1);

        // Lower bound
        let raw = create_process_availability_raw(LimitType::LowerBound, Dimensionless(0.5));
        let bounds = raw.to_bounds(ts_length);
        assert_eq!(bounds, Dimensionless(0.05)..=Dimensionless(0.1));

        // Upper bound
        let raw = create_process_availability_raw(LimitType::UpperBound, Dimensionless(0.5));
        let bounds = raw.to_bounds(ts_length);
        assert_eq!(bounds, Dimensionless(0.0)..=Dimensionless(0.05));

        // Equality
        let raw = create_process_availability_raw(LimitType::Equality, Dimensionless(0.5));
        let bounds = raw.to_bounds(ts_length);
        assert_eq!(bounds, Dimensionless(0.05)..=Dimensionless(0.05));
    }
}
