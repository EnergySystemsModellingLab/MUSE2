//! Debug output for the competitive asset selection algorithm.
use super::metrics::AppraisalOutput;
use crate::asset::AssetID;
use crate::output::DebugDataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::investment::DemandMap;
use crate::time_slice::{TimeSliceID, TimeSliceLevel, TimeSliceSelection};
use crate::units::{Activity, Capacity, Flow, MoneyPerActivity};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// The output file name for appraisal results
const APPRAISAL_RESULTS_FILE_NAME: &str = "debug_asset_selection_competitive.csv";

/// The output file name for appraisal time slice results
const APPRAISAL_RESULTS_TIME_SLICE_FILE_NAME: &str =
    "debug_asset_selection_competitive_time_slices.csv";

/// Represents the appraisal results in a row of the appraisal results CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AppraisalResultsRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    capacity: Capacity,
    metric: Option<f64>,
}

/// Represents the appraisal results in a row of the appraisal results time slice CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AppraisalResultsTimeSliceRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    activity: Activity,
    activity_coefficient: MoneyPerActivity,
    time_slice_level: TimeSliceLevel,
    demand_for_selection: Flow,
    unmet_demand_for_selection: Flow,
}

/// Write debug info about a round of investment appraisal
pub fn write_debug_info(
    writer: Option<&mut DebugDataWriter>,
    milestone_year: u32,
    run_description: &str,
    appraisal_results: &[AppraisalOutput],
    demand: &DemandMap,
    time_slice_level: TimeSliceLevel,
) -> Result<()> {
    let Some(writer) = writer else {
        return Ok(());
    };
    let run_description = writer.debug_run_description(run_description);

    let rows = appraisal_results.iter().map(|result| AppraisalResultsRow {
        milestone_year,
        run_description: run_description.to_string(),
        asset_id: result.asset.id(),
        process_id: result.asset.process_id().clone(),
        region_id: result.asset.region_id().clone(),
        capacity: result.asset.total_capacity(),
        metric: result.metric.as_ref().map(|m| m.value()),
    });
    let output_writer = writer.get_custom_writer(APPRAISAL_RESULTS_FILE_NAME)?;
    for row in rows {
        output_writer.serialize(row)?;
    }

    let time_slice_rows = appraisal_results.iter().flat_map(|result| {
        let run_description = &run_description;
        result.activity.iter().map(move |(time_slice, activity)| {
            // Map the individual time slice back to its containing selection so we can look
            // up selection-level demand and unmet demand.
            let selection = match time_slice_level {
                TimeSliceLevel::Annual => TimeSliceSelection::Annual,
                TimeSliceLevel::Season => TimeSliceSelection::Season(time_slice.season.clone()),
                TimeSliceLevel::DayNight => TimeSliceSelection::Single(time_slice.clone()),
            };
            AppraisalResultsTimeSliceRow {
                milestone_year,
                run_description: run_description.to_string(),
                asset_id: result.asset.id(),
                process_id: result.asset.process_id().clone(),
                region_id: result.asset.region_id().clone(),
                time_slice: time_slice.clone(),
                activity: *activity,
                activity_coefficient: result.coefficients.activity_coefficients[time_slice],
                time_slice_level,
                demand_for_selection: demand[&selection],
                unmet_demand_for_selection: result.unmet_demand[&selection],
            }
        })
    });
    let output_writer = writer.get_custom_writer(APPRAISAL_RESULTS_TIME_SLICE_FILE_NAME)?;
    for row in time_slice_rows {
        output_writer.serialize(row)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::coefficients::ObjectiveCoefficients;
    use super::super::metrics::LCOXMetric;
    use super::*;
    use crate::asset::{Asset, AssetRef};
    use crate::fixture::{asset, time_slice};
    use crate::output::DataWriter;
    use indexmap::indexmap;
    use itertools::{Itertools, assert_equal};
    use rstest::{fixture, rstest};
    use std::iter;
    use std::path::Path;
    use std::sync::Arc;
    use tempfile::tempdir;

    #[fixture]
    fn appraisal_output(asset: Asset, time_slice: TimeSliceID) -> AppraisalOutput {
        let activity_coefficients = indexmap! { time_slice.clone() => MoneyPerActivity(0.5) };
        let market_costs = indexmap! { time_slice.clone() => MoneyPerActivity(0.4) };
        let activity = indexmap! { time_slice.clone() => Activity(10.0) };
        let unmet_demand =
            indexmap! { TimeSliceSelection::Single(time_slice.clone()) => Flow(5.0) };
        AppraisalOutput {
            asset: AssetRef::from(asset),
            coefficients: Arc::new(ObjectiveCoefficients {
                activity_coefficients,
                market_costs,
            }),
            activity,
            unmet_demand,
            metric: Some(Box::new(LCOXMetric::new(MoneyPerActivity(4.14)))),
        }
    }

    fn read_rows<T: for<'de> Deserialize<'de>>(dir: &Path, file_name: &str) -> Vec<T> {
        csv::Reader::from_path(dir.join(file_name))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap()
    }

    #[rstest]
    fn write_debug_info_works(
        asset: Asset,
        appraisal_output: AppraisalOutput,
        time_slice: TimeSliceID,
    ) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let dir = tempdir().unwrap();
        let demand = indexmap! { TimeSliceSelection::Single(time_slice.clone()) => Flow(100.0) };

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_debug_info(
                writer.debug_writer(),
                milestone_year,
                &run_description,
                &[appraisal_output],
                &demand,
                TimeSliceLevel::DayNight,
            )
            .unwrap();
            writer.flush().unwrap();
        }

        let expected = AppraisalResultsRow {
            milestone_year,
            run_description: run_description.clone(),
            asset_id: None,
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            capacity: Capacity(2.0),
            metric: Some(4.14),
        };
        let records: Vec<AppraisalResultsRow> = read_rows(dir.path(), APPRAISAL_RESULTS_FILE_NAME);
        assert_equal(records, iter::once(expected));

        let expected = AppraisalResultsTimeSliceRow {
            milestone_year,
            run_description,
            asset_id: None,
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice: time_slice.clone(),
            activity: Activity(10.0),
            activity_coefficient: MoneyPerActivity(0.5),
            time_slice_level: TimeSliceLevel::DayNight,
            demand_for_selection: Flow(100.0),
            unmet_demand_for_selection: Flow(5.0),
        };
        let records: Vec<AppraisalResultsTimeSliceRow> =
            read_rows(dir.path(), APPRAISAL_RESULTS_TIME_SLICE_FILE_NAME);
        assert_equal(records, iter::once(expected));
    }
}
