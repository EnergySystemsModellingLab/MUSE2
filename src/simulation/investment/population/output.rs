//! Debug output for the population asset selection algorithm.
use super::metrics::AppraisalOutput;
use crate::asset::AssetID;
use crate::output::DebugDataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::investment::DemandMap;
use crate::time_slice::{TimeSliceID, TimeSliceLevel, TimeSliceSelection};
use crate::units::{ActivityPerCapacity, Capacity, Flow};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Summary appraisal results, with one row per feasible option.
const APPRAISAL_RESULTS_FILE_NAME: &str = "debug_asset_selection_population.csv";
/// Per-time-slice fitted activity per unit capacity for each feasible option.
const APPRAISAL_RESULTS_TIME_SLICE_FILE_NAME: &str =
    "debug_asset_selection_population_time_slices.csv";

/// One row in the summary appraisal results file.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AppraisalResultsRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    capacity: Capacity,
    metric: f64,
}

/// One row in the time-slice appraisal results file.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AppraisalResultsTimeSliceRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    activity_per_capacity: ActivityPerCapacity,
    time_slice_level: TimeSliceLevel,
    demand_for_selection: Flow,
}

/// Write the one-time appraisal results for all feasible options.
///
/// The summary file contains the full-demand capacity and optimisation metric. The time-slice file
/// records the fitted activity per unit capacity alongside demand at the commodity's balance
/// level. There is no unmet-demand column because each retained appraisal matches the full profile.
pub fn write_debug_info(
    writer: &mut DebugDataWriter,
    milestone_year: u32,
    run_description: &str,
    appraisal_results: &[AppraisalOutput],
    demand: &DemandMap,
    time_slice_level: TimeSliceLevel,
) -> Result<()> {
    let run_description = writer.debug_run_description(run_description);
    let output_writer = writer.get_custom_writer(APPRAISAL_RESULTS_FILE_NAME)?;
    for result in appraisal_results {
        output_writer.serialize(AppraisalResultsRow {
            milestone_year,
            run_description: run_description.to_string(),
            asset_id: result.asset.id(),
            process_id: result.asset.process_id().clone(),
            region_id: result.asset.region_id().clone(),
            capacity: result.capacity,
            metric: result.metric,
        })?;
    }

    let output_writer = writer.get_custom_writer(APPRAISAL_RESULTS_TIME_SLICE_FILE_NAME)?;
    for result in appraisal_results {
        for (time_slice, activity_per_capacity) in &result.activity_per_capacity {
            // Map the individual time slice to its containing demand selection at the commodity's
            // balance level.
            let selection = match time_slice_level {
                TimeSliceLevel::Annual => TimeSliceSelection::Annual,
                TimeSliceLevel::Season => TimeSliceSelection::Season(time_slice.season.clone()),
                TimeSliceLevel::DayNight => TimeSliceSelection::Single(time_slice.clone()),
            };
            output_writer.serialize(AppraisalResultsTimeSliceRow {
                milestone_year,
                run_description: run_description.to_string(),
                asset_id: result.asset.id(),
                process_id: result.asset.process_id().clone(),
                region_id: result.asset.region_id().clone(),
                time_slice: time_slice.clone(),
                activity_per_capacity: *activity_per_capacity,
                time_slice_level,
                demand_for_selection: demand[&selection],
            })?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::{Asset, AssetRef};
    use crate::fixture::{asset, time_slice};
    use crate::output::DataWriter;
    use crate::units::ActivityPerCapacity;
    use indexmap::indexmap;
    use itertools::{Itertools, assert_equal};
    use rstest::rstest;
    use std::iter;
    use std::path::Path;
    use tempfile::tempdir;

    fn read_rows<T: for<'de> Deserialize<'de>>(dir: &Path, file_name: &str) -> Vec<T> {
        csv::Reader::from_path(dir.join(file_name))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap()
    }

    #[rstest]
    fn write_debug_info_writes_profile_without_unmet_demand(asset: Asset, time_slice: TimeSliceID) {
        let run_description = "test_run";
        let dir = tempdir().unwrap();
        let demand = indexmap! { TimeSliceSelection::Single(time_slice.clone()) => Flow(100.0) };
        let appraisal = AppraisalOutput {
            asset: AssetRef::from(asset.clone()),
            capacity: Capacity(2.0),
            activity_per_capacity: indexmap! {
                time_slice.clone() => ActivityPerCapacity(5.0)
            },
            metric: 4.14,
        };
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_debug_info(
                writer.debug_writer().unwrap(),
                2020,
                run_description,
                &[appraisal],
                &demand,
                TimeSliceLevel::DayNight,
            )
            .unwrap();
            writer.flush().unwrap();
        }

        assert_equal(
            read_rows::<AppraisalResultsRow>(dir.path(), APPRAISAL_RESULTS_FILE_NAME),
            iter::once(AppraisalResultsRow {
                milestone_year: 2020,
                run_description: run_description.to_string(),
                asset_id: None,
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                capacity: Capacity(2.0),
                metric: 4.14,
            }),
        );
        assert_equal(
            read_rows::<AppraisalResultsTimeSliceRow>(
                dir.path(),
                APPRAISAL_RESULTS_TIME_SLICE_FILE_NAME,
            ),
            iter::once(AppraisalResultsTimeSliceRow {
                milestone_year: 2020,
                run_description: run_description.to_string(),
                asset_id: None,
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                time_slice: time_slice.clone(),
                activity_per_capacity: ActivityPerCapacity(5.0),
                time_slice_level: TimeSliceLevel::DayNight,
                demand_for_selection: Flow(100.0),
            }),
        );
    }
}
