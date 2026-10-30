//! Debug output files for the portfolio algorithm.
use super::optimisation::ResultsMap;
use crate::asset::AssetID;
use crate::output::DataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// The output file name for optimisation results
const RESULTS_FILE_NAME: &str = "debug_appraisal_results.csv";

/// The output file name for optimisation time slice results
const RESULTS_TIME_SLICE_FILE_NAME: &str = "debug_appraisal_results_time_slices.csv";

/// Represents the capacity of an asset in a row of the results CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ResultsRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    capacity: Capacity,
}

/// Represents the activity of an asset in a row of the time slice results CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ResultsTimeSliceRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    activity: Activity,
}

/// Write the capacities and activities from the optimisation to the debug files
pub fn write_debug_info(
    writer: &mut DataWriter,
    milestone_year: u32,
    run_description: &str,
    results: &ResultsMap,
) -> Result<()> {
    if !writer.debug_enabled() {
        return Ok(());
    }

    let run_description = writer.debug_run_description(run_description);
    writer.write_debug_rows(
        RESULTS_FILE_NAME,
        results
            .capacities
            .iter()
            .map(|(asset, capacity)| ResultsRow {
                milestone_year,
                run_description: run_description.clone(),
                asset_id: asset.id(),
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                capacity: *capacity,
            }),
    )?;
    writer.write_debug_rows(
        RESULTS_TIME_SLICE_FILE_NAME,
        results
            .activities
            .iter()
            .map(|((asset, time_slice), activity)| ResultsTimeSliceRow {
                milestone_year,
                run_description: run_description.clone(),
                asset_id: asset.id(),
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                time_slice: time_slice.clone(),
                activity: *activity,
            }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::AssetPool;
    use crate::fixture::{assets, time_slice};
    use indexmap::indexmap;
    use itertools::{Itertools, assert_equal};
    use rstest::rstest;
    use std::iter;
    use tempfile::tempdir;

    #[rstest]
    fn write_results(assets: AssetPool, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let run_description = "test_run";
        let capacity = Capacity(12.5);
        let activity = Activity(100.5);
        let dir = tempdir().unwrap();
        let asset = assets.iter().next().unwrap();
        let results = ResultsMap {
            capacities: indexmap! { asset.clone() => capacity },
            activities: indexmap! { (asset.clone(), time_slice.clone()) => activity },
        };

        // Write results
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            writer.set_debug_context("context".to_string());
            write_debug_info(&mut writer, milestone_year, run_description, &results).unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let run_description = "context; test_run".to_string();
        let expected_capacity = ResultsRow {
            milestone_year,
            run_description: run_description.clone(),
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            capacity,
        };
        let records: Vec<ResultsRow> = csv::Reader::from_path(dir.path().join(RESULTS_FILE_NAME))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap();
        assert_equal(records, iter::once(expected_capacity));

        let expected_activity = ResultsTimeSliceRow {
            milestone_year,
            run_description,
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice,
            activity,
        };
        let records: Vec<ResultsTimeSliceRow> =
            csv::Reader::from_path(dir.path().join(RESULTS_TIME_SLICE_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected_activity));
    }
}
