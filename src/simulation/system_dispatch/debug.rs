//! Debug output files for the dispatch optimisation.
use super::optimisation::Solution;
use crate::asset::{AssetID, AssetRef};
use crate::commodity::CommodityID;
use crate::output::DataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Flow, Money, MoneyPerActivity, MoneyPerFlow};
use anyhow::Result;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::iter;

/// Debug output file for asset dispatch
const ACTIVITY_ASSET_DISPATCH: &str = "debug_dispatch_assets.csv";

/// The output file name for commodity balance duals
const COMMODITY_BALANCE_DUALS_FILE_NAME: &str = "debug_commodity_balance_duals.csv";

/// The output file name for unmet demand values
const UNMET_DEMAND_FILE_NAME: &str = "debug_unmet_demand.csv";

/// The output file name for extra solver output values
const SOLVER_VALUES_FILE_NAME: &str = "debug_solver.csv";

/// Represents the activity in a row of the dispatch CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct DispatchRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    activity: Option<Activity>,
    activity_dual: Option<MoneyPerActivity>,
    column_dual: Option<MoneyPerActivity>,
}

/// Represents the commodity balance duals data in a row of the commodity balance duals CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct CommodityBalanceDualsRow {
    milestone_year: u32,
    run_description: String,
    commodity_id: CommodityID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    value: MoneyPerFlow,
}

/// Represents the unmet demand data in a row of the unmet demand CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct UnmetDemandRow {
    milestone_year: u32,
    run_description: String,
    commodity_id: CommodityID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    value: Flow,
}

/// Represents solver output values
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct SolverValuesRow {
    milestone_year: u32,
    run_description: String,
    objective_value: Money,
}

/// Create the debug files which are always present when writing debug output
///
/// The unmet demand file is not included, as it is only created if there is unmet demand.
pub fn create_debug_files(writer: &mut DataWriter) -> Result<()> {
    writer.write_debug_rows(ACTIVITY_ASSET_DISPATCH, iter::empty::<DispatchRow>())?;
    writer.write_debug_rows(
        COMMODITY_BALANCE_DUALS_FILE_NAME,
        iter::empty::<CommodityBalanceDualsRow>(),
    )?;
    writer.write_debug_rows(SOLVER_VALUES_FILE_NAME, iter::empty::<SolverValuesRow>())
}

/// Write debug info about the dispatch optimisation
pub fn write_dispatch_debug_info(
    writer: &mut DataWriter,
    milestone_year: u32,
    run_description: &str,
    solution: &Solution,
) -> Result<()> {
    if !writer.debug_enabled() {
        return Ok(());
    }

    let run_description = writer.debug_run_description(run_description);
    write_dispatch(
        writer,
        milestone_year,
        &run_description,
        solution.iter_activity(),
        solution.iter_activity_duals(),
        solution.iter_column_duals(),
    )?;
    write_commodity_balance_duals(
        writer,
        milestone_year,
        &run_description,
        solution.iter_commodity_balance_duals(),
    )?;
    write_unmet_demand(
        writer,
        milestone_year,
        &run_description,
        solution.iter_unmet_demand(),
    )?;
    write_solver_values(
        writer,
        milestone_year,
        &run_description,
        solution.objective_value,
    )
}

/// Write activity to file
///
/// `run_description` should already include any debug context.
fn write_dispatch<'a, I, J, K>(
    writer: &mut DataWriter,
    milestone_year: u32,
    run_description: &str,
    iter_activity: I,
    iter_activity_duals: J,
    iter_column_duals: K,
) -> Result<()>
where
    I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    J: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
    K: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
{
    // To account for different order of entries or missing ones, we first compile data in hash map
    type CompiledActivityData = (
        Option<Activity>,
        Option<MoneyPerActivity>,
        Option<MoneyPerActivity>,
    );
    let mut map: IndexMap<(&AssetRef, &TimeSliceID), CompiledActivityData> = IndexMap::new();

    // For the activities
    for (asset, time_slice, activity) in iter_activity {
        map.entry((asset, time_slice)).or_default().0 = Some(activity);
    }
    // The activity duals
    for (asset, time_slice, activity_dual) in iter_activity_duals {
        map.entry((asset, time_slice)).or_default().1 = Some(activity_dual);
    }
    // And the column duals
    for (asset, time_slice, column_dual) in iter_column_duals {
        map.entry((asset, time_slice)).or_default().2 = Some(column_dual);
    }

    let rows = map.into_iter().map(
        |((asset, time_slice), (activity, activity_dual, column_dual))| DispatchRow {
            milestone_year,
            run_description: run_description.to_string(),
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice: time_slice.clone(),
            activity,
            activity_dual,
            column_dual,
        },
    );
    writer.write_debug_rows(ACTIVITY_ASSET_DISPATCH, rows)
}

/// Write commodity balance duals to file
///
/// `run_description` should already include any debug context.
fn write_commodity_balance_duals<'a, I>(
    writer: &mut DataWriter,
    milestone_year: u32,
    run_description: &str,
    iter: I,
) -> Result<()>
where
    I: Iterator<Item = (&'a CommodityID, &'a RegionID, &'a TimeSliceID, MoneyPerFlow)>,
{
    let rows = iter.map(
        |(commodity_id, region_id, time_slice, value)| CommodityBalanceDualsRow {
            milestone_year,
            run_description: run_description.to_string(),
            commodity_id: commodity_id.clone(),
            region_id: region_id.clone(),
            time_slice: time_slice.clone(),
            value,
        },
    );
    writer.write_debug_rows(COMMODITY_BALANCE_DUALS_FILE_NAME, rows)
}

/// Write unmet demand values to file
///
/// The file is only created if there is unmet demand. `run_description` should already include any
/// debug context.
fn write_unmet_demand<'a, I>(
    writer: &mut DataWriter,
    milestone_year: u32,
    run_description: &str,
    iter: I,
) -> Result<()>
where
    I: Iterator<Item = (&'a CommodityID, &'a RegionID, &'a TimeSliceID, Flow)>,
{
    let mut iter = iter.peekable();
    if iter.peek().is_none() {
        return Ok(());
    }

    let rows = iter.map(
        |(commodity_id, region_id, time_slice, value)| UnmetDemandRow {
            milestone_year,
            run_description: run_description.to_string(),
            commodity_id: commodity_id.clone(),
            region_id: region_id.clone(),
            time_slice: time_slice.clone(),
            value,
        },
    );
    writer.write_debug_rows(UNMET_DEMAND_FILE_NAME, rows)
}

/// Write additional solver output values to file
///
/// `run_description` should already include any debug context.
fn write_solver_values(
    writer: &mut DataWriter,
    milestone_year: u32,
    run_description: &str,
    objective_value: Money,
) -> Result<()> {
    let row = SolverValuesRow {
        milestone_year,
        run_description: run_description.to_string(),
        objective_value,
    };
    writer.write_debug_rows(SOLVER_VALUES_FILE_NAME, iter::once(row))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::AssetPool;
    use crate::fixture::{assets, commodity_id, region_id, time_slice};
    use itertools::{Itertools, assert_equal};
    use rstest::rstest;
    use serde::de::DeserializeOwned;
    use std::fmt::Debug;
    use std::path::Path;
    use tempfile::tempdir;

    /// Read back the rows of a CSV file
    fn read_rows<T: DeserializeOwned>(dir: &Path, file_name: &str) -> Vec<T> {
        csv::Reader::from_path(dir.join(file_name))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap()
    }

    /// Check that a single row is written to the given file
    fn assert_single_row<T: DeserializeOwned + Debug + PartialEq>(
        dir: &Path,
        file_name: &str,
        expected: T,
    ) {
        assert_equal(read_rows::<T>(dir, file_name), iter::once(expected));
    }

    #[rstest]
    fn write_commodity_balance_duals_works(
        commodity_id: CommodityID,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let value = MoneyPerFlow(0.5);
        let dir = tempdir().unwrap();

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_commodity_balance_duals(
                &mut writer,
                milestone_year,
                &run_description,
                iter::once((&commodity_id, &region_id, &time_slice, value)),
            )
            .unwrap();
            writer.flush().unwrap();
        }

        let expected = CommodityBalanceDualsRow {
            milestone_year,
            run_description,
            commodity_id,
            region_id,
            time_slice,
            value,
        };
        assert_single_row(dir.path(), COMMODITY_BALANCE_DUALS_FILE_NAME, expected);
    }

    #[rstest]
    fn write_unmet_demand_works(
        commodity_id: CommodityID,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let value = Flow(0.5);
        let dir = tempdir().unwrap();

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_unmet_demand(
                &mut writer,
                milestone_year,
                &run_description,
                iter::once((&commodity_id, &region_id, &time_slice, value)),
            )
            .unwrap();
            writer.flush().unwrap();
        }

        let expected = UnmetDemandRow {
            milestone_year,
            run_description,
            commodity_id,
            region_id,
            time_slice,
            value,
        };
        assert_single_row(dir.path(), UNMET_DEMAND_FILE_NAME, expected);
    }

    #[test]
    fn write_unmet_demand_skips_file_if_empty() {
        let dir = tempdir().unwrap();

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_unmet_demand(&mut writer, 2020, "test_run", iter::empty()).unwrap();
            writer.flush().unwrap();
        }

        assert!(!dir.path().join(UNMET_DEMAND_FILE_NAME).exists());
    }

    #[rstest]
    #[case(Some(MoneyPerActivity(-1.5)), Some(MoneyPerActivity(5.0)))]
    #[case(None, None)]
    fn write_dispatch_works(
        assets: AssetPool,
        time_slice: TimeSliceID,
        #[case] activity_dual: Option<MoneyPerActivity>,
        #[case] column_dual: Option<MoneyPerActivity>,
    ) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let activity = Activity(100.5);
        let dir = tempdir().unwrap();
        let asset = assets.iter().next().unwrap();

        // Missing duals should result in empty fields
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_dispatch(
                &mut writer,
                milestone_year,
                &run_description,
                iter::once((asset, &time_slice, activity)),
                activity_dual
                    .map(|dual| (asset, &time_slice, dual))
                    .into_iter(),
                column_dual
                    .map(|dual| (asset, &time_slice, dual))
                    .into_iter(),
            )
            .unwrap();
            writer.flush().unwrap();
        }

        let expected = DispatchRow {
            milestone_year,
            run_description,
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice,
            activity: Some(activity),
            activity_dual,
            column_dual,
        };
        assert_single_row(dir.path(), ACTIVITY_ASSET_DISPATCH, expected);
    }

    #[test]
    fn write_solver_values_works() {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let objective_value = Money(1234.56);
        let dir = tempdir().unwrap();

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            write_solver_values(
                &mut writer,
                milestone_year,
                &run_description,
                objective_value,
            )
            .unwrap();
            writer.flush().unwrap();
        }

        let expected = SolverValuesRow {
            milestone_year,
            run_description,
            objective_value,
        };
        assert_single_row(dir.path(), SOLVER_VALUES_FILE_NAME, expected);
    }

    #[test]
    fn create_debug_files_creates_expected_files() {
        let dir = tempdir().unwrap();

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), true).unwrap();
            create_debug_files(&mut writer).unwrap();
            writer.flush().unwrap();
        }

        for file_name in [
            ACTIVITY_ASSET_DISPATCH,
            COMMODITY_BALANCE_DUALS_FILE_NAME,
            SOLVER_VALUES_FILE_NAME,
        ] {
            assert!(dir.path().join(file_name).exists());
        }
        assert!(!dir.path().join(UNMET_DEMAND_FILE_NAME).exists());
    }
}
