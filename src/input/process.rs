//! Code for reading process-related information from CSV files.
use super::{input_err_msg, read_csv};
use crate::commodity::CommodityMap;
use crate::id::IDCollection;
use crate::process::{
    Process, ProcessActivityLimitsMap, ProcessFlowsMap, ProcessID, ProcessMap, ProcessParameterMap,
};
use crate::region::{RegionID, parse_region_str};
use crate::time_slice::TimeSliceInfo;
use crate::units::ActivityPerCapacity;
use anyhow::{Context, Ok, Result, ensure};
use indexmap::IndexSet;
use itertools::chain;
use serde::Deserialize;
use std::path::Path;
use std::rc::Rc;

mod availability;
use availability::read_process_availabilities;
mod flow;
use flow::read_process_flows;
mod parameter;
use crate::id::define_id_getter;
use parameter::read_process_parameters;

const PROCESSES_FILE_NAME: &str = "processes.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct ProcessRaw {
    id: ProcessID,
    description: String,
    regions: String,
    primary_output: Option<String>,
    start_year: Option<u32>,
    end_year: Option<u32>,
    capacity_to_activity: Option<ActivityPerCapacity>,
}
define_id_getter! {ProcessRaw, ProcessID}

/// Read process information from the specified CSV files.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `commodities` - Commodities for the model
/// * `region_ids` - All possible region IDs
/// * `time_slice_info` - Information about seasons and times of day
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// This function returns a map of processes, with the IDs as keys.
pub fn read_processes(
    model_dir: &Path,
    commodities: &CommodityMap,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<ProcessMap> {
    let mut processes = read_processes_file(model_dir, milestone_years, region_ids, commodities)?;
    let mut activity_limits = read_process_availabilities(model_dir, &processes, time_slice_info)?;
    let mut flows = read_process_flows(model_dir, &mut processes, commodities)?;
    let mut parameters = read_process_parameters(model_dir, &processes, milestone_years[0])?;

    // Add data to Process objects
    for (id, process) in &mut processes {
        // This will always succeed as we know there will only be one reference to the process here
        let process = Rc::get_mut(process).unwrap();

        // We have already checked that there are maps for every process so this will succeed
        process.activity_limits = activity_limits.remove(id).unwrap();
        process.flows = flows.remove(id).unwrap();
        process.parameters = parameters.remove(id).unwrap();
    }

    Ok(processes)
}

fn read_processes_file(
    model_dir: &Path,
    milestone_years: &[u32],
    region_ids: &IndexSet<RegionID>,
    commodities: &CommodityMap,
) -> Result<ProcessMap> {
    let file_path = model_dir.join(PROCESSES_FILE_NAME);
    let processes_csv = read_csv(&file_path)?;
    read_processes_file_from_iter(processes_csv, milestone_years, region_ids, commodities)
        .with_context(|| input_err_msg(&file_path))
}

fn read_processes_file_from_iter<I>(
    iter: I,
    milestone_years: &[u32],
    region_ids: &IndexSet<RegionID>,
    commodities: &CommodityMap,
) -> Result<ProcessMap>
where
    I: Iterator<Item = ProcessRaw>,
{
    let mut processes = ProcessMap::new();
    for process_raw in iter {
        let start_year = process_raw.start_year.unwrap_or(milestone_years[0]);
        let end_year = process_raw
            .end_year
            .unwrap_or(*milestone_years.last().unwrap());

        // Check year range is valid
        ensure!(
            start_year <= end_year,
            "Error in parameter for process {}: start_year > end_year",
            process_raw.id
        );

        // Select process years. It is possible for assets to have been commissioned before the
        // simulation's time horizon, so assume that all years >=start_year and <base year are valid
        // too.
        let years = chain(
            start_year..milestone_years[0],
            milestone_years
                .iter()
                .copied()
                .filter(|year| (start_year..=end_year).contains(year)),
        )
        .collect();

        // Parse region ID
        let regions = parse_region_str(&process_raw.regions, region_ids)?;

        // Check whether primary output is valid
        let primary_output = process_raw
            .primary_output
            .map(|id| {
                let id = commodities.get_id(id.trim())?;
                Ok(id.clone())
            })
            .transpose()?;

        let capacity_to_activity = process_raw
            .capacity_to_activity
            .unwrap_or(ActivityPerCapacity(1.0));

        // Validate capacity_to_activity
        ensure!(
            capacity_to_activity >= ActivityPerCapacity(0.0),
            "Error in process {}: capacity_to_activity must be >= 0",
            process_raw.id
        );

        let process = Process {
            id: process_raw.id.clone(),
            description: process_raw.description,
            years,
            activity_limits: ProcessActivityLimitsMap::new(),
            flows: ProcessFlowsMap::new(),
            parameters: ProcessParameterMap::new(),
            regions,
            primary_output,
            capacity_to_activity,
        };

        ensure!(
            processes.insert(process_raw.id, process.into()).is_none(),
            "Duplicate process ID"
        );
    }

    Ok(processes)
}
