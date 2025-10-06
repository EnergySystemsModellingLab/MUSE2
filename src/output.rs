//! The module responsible for writing output data to disk.
use crate::agent::AgentID;
use crate::asset::{Asset, AssetID, AssetRef};
use crate::commodity::CommodityID;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::CommodityPrices;
use crate::simulation::investment::appraisal::AppraisalOutput;
use crate::simulation::optimisation::{FlowMap, Solution};
use crate::simulation::prices::ReducedCosts;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity, Flow, Money, MoneyPerActivity, MoneyPerFlow};
use anyhow::{Context, Result, ensure};
use csv;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};

pub mod metadata;
use metadata::write_metadata;

/// The root folder in which model-specific output folders will be created
const OUTPUT_DIRECTORY_ROOT: &str = "muse2_results";

/// The output file name for commodity flows
const COMMODITY_FLOWS_FILE_NAME: &str = "commodity_flows.csv";

/// The output file name for commodity prices
const COMMODITY_PRICES_FILE_NAME: &str = "commodity_prices.csv";

/// The output file name for assets
const ASSETS_FILE_NAME: &str = "assets.csv";

/// The output file name for raw activity
const ACTIVITY_FILE_NAME: &str = "debug_activity.csv";

/// The output file name for commodity balance duals
const COMMODITY_BALANCE_DUALS_FILE_NAME: &str = "debug_commodity_balance_duals.csv";

/// The output file name for activity duals
const ACTIVITY_DUALS_FILE_NAME: &str = "debug_activity_duals.csv";

/// The output file name for extra solver output values
const SOLVER_VALUES_FILE_NAME: &str = "debug_solver.csv";

/// The output file name for appraisal results
const APPRAISAL_RESULTS_FILE_NAME: &str = "debug_appraisal_results.csv";

/// The output file name for reduced costs
const REDUCED_COSTS_FILE_NAME: &str = "debug_reduced_costs.csv";

/// Get the model name from the specified directory path
pub fn get_output_dir(model_dir: &Path) -> Result<PathBuf> {
    // Get the model name from the dir path. This ends up being convoluted because we need to check
    // for all possible errors. Ugh.
    let model_dir = model_dir
        .canonicalize() // canonicalise in case the user has specified "."
        .context("Could not resolve path to model")?;

    let model_name = model_dir
        .file_name()
        .context("Model cannot be in root folder")?
        .to_str()
        .context("Invalid chars in model dir name")?;

    // Construct path
    Ok([OUTPUT_DIRECTORY_ROOT, model_name].iter().collect())
}

/// Create a new output directory for the model, optionally overwriting existing data
///
/// # Arguments
///
/// * `output_dir` - The output directory to create/overwrite
/// * `allow_overwrite` - Whether to delete and recreate the folder if it is non-empty
///
/// # Returns
///
/// True if the output dir contained existing data that was deleted, false if not, or an error.
pub fn create_output_directory(output_dir: &Path, allow_overwrite: bool) -> Result<bool> {
    // If the folder already exists, then delete it
    let overwrite = if let Ok(mut it) = fs::read_dir(output_dir) {
        if it.next().is_none() {
            // Folder exists and is empty: nothing to do
            return Ok(false);
        }

        ensure!(
            allow_overwrite,
            "Output folder already exists and is not empty. \
            Please delete the folder or pass the --overwrite command-line option."
        );

        fs::remove_dir_all(output_dir).context("Could not delete folder")?;
        true
    } else {
        false
    };

    // Try to create the directory, with parents
    fs::create_dir_all(output_dir)?;

    Ok(overwrite)
}

/// Represents a row in the assets output CSV file.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AssetRow {
    asset_id: AssetID,
    process_id: ProcessID,
    region_id: RegionID,
    agent_id: AgentID,
    commission_year: u32,
    decommission_year: Option<u32>,
    capacity: Capacity,
}

impl AssetRow {
    /// Create a new [`AssetRow`]
    fn new(asset: &Asset) -> Self {
        Self {
            asset_id: asset.id().unwrap(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            agent_id: asset.agent_id().unwrap().clone(),
            commission_year: asset.commission_year(),
            decommission_year: asset.decommission_year(),
            capacity: asset.capacity(),
        }
    }
}

/// Represents the flow-related data in a row of the commodity flows CSV file.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct CommodityFlowRow {
    milestone_year: u32,
    asset_id: AssetID,
    commodity_id: CommodityID,
    time_slice: TimeSliceID,
    flow: Flow,
}

/// Represents a row in the commodity prices CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct CommodityPriceRow {
    milestone_year: u32,
    commodity_id: CommodityID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    price: MoneyPerFlow,
}

/// Represents the activity in a row of the activity CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ActivityRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    activity: Activity,
}

/// Represents the activity duals data in a row of the activity duals CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ActivityDualsRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    value: MoneyPerActivity,
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

/// Represents solver output values
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct SolverValuesRow {
    milestone_year: u32,
    run_description: String,
    objective_value: Money,
}

/// Represents the appraisal results in a row of the appraisal results CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AppraisalResultsRow {
    milestone_year: u32,
    run_description: String,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    capacity: Capacity,
    unmet_demand: Flow,
    metric: f64,
}

/// Represents the reduced costs in a row of the reduced costs CSV file
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ReducedCostsRow {
    milestone_year: u32,
    asset_id: Option<AssetID>,
    process_id: ProcessID,
    region_id: RegionID,
    time_slice: TimeSliceID,
    reduced_cost: MoneyPerActivity,
}

/// For writing extra debug information about the model
struct DebugDataWriter {
    context: Option<String>,
    activity_writer: csv::Writer<File>,
    commodity_balance_duals_writer: csv::Writer<File>,
    activity_duals_writer: csv::Writer<File>,
    solver_values_writer: csv::Writer<File>,
    appraisal_results_writer: csv::Writer<File>,
    reduced_costs_writer: csv::Writer<File>,
}

impl DebugDataWriter {
    /// Open CSV files to write debug info to
    ///
    /// # Arguments
    ///
    /// * `output_path` - Folder where files will be saved
    fn create(output_path: &Path) -> Result<Self> {
        let new_writer = |file_name| {
            let file_path = output_path.join(file_name);
            csv::Writer::from_path(file_path)
        };

        Ok(Self {
            context: None,
            activity_writer: new_writer(ACTIVITY_FILE_NAME)?,
            commodity_balance_duals_writer: new_writer(COMMODITY_BALANCE_DUALS_FILE_NAME)?,
            activity_duals_writer: new_writer(ACTIVITY_DUALS_FILE_NAME)?,
            solver_values_writer: new_writer(SOLVER_VALUES_FILE_NAME)?,
            appraisal_results_writer: new_writer(APPRAISAL_RESULTS_FILE_NAME)?,
            reduced_costs_writer: new_writer(REDUCED_COSTS_FILE_NAME)?,
        })
    }

    /// Prepend the current context to the run description
    fn with_context(&self, run_description: &str) -> String {
        if let Some(context) = &self.context {
            format!("{context}; {run_description}")
        } else {
            run_description.to_string()
        }
    }

    /// Write debug info about the dispatch optimisation
    fn write_dispatch_debug_info(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        solution: &Solution,
    ) -> Result<()> {
        self.write_activity(milestone_year, run_description, solution.iter_activity())?;
        self.write_activity_duals(
            milestone_year,
            run_description,
            solution.iter_activity_duals(),
        )?;
        self.write_commodity_balance_duals(
            milestone_year,
            run_description,
            solution.iter_commodity_balance_duals(),
        )?;
        self.write_solver_values(milestone_year, run_description, solution.objective_value)?;
        Ok(())
    }

    // Write activity to file
    fn write_activity<'a, I>(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        iter: I,
    ) -> Result<()>
    where
        I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
    {
        for (asset, time_slice, activity) in iter {
            let row = ActivityRow {
                milestone_year,
                run_description: self.with_context(run_description),
                asset_id: asset.id(),
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                time_slice: time_slice.clone(),
                activity,
            };
            self.activity_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Write activity duals to file
    fn write_activity_duals<'a, I>(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        iter: I,
    ) -> Result<()>
    where
        I: Iterator<Item = (&'a AssetRef, &'a TimeSliceID, MoneyPerActivity)>,
    {
        for (asset, time_slice, value) in iter {
            let row = ActivityDualsRow {
                milestone_year,
                run_description: self.with_context(run_description),
                asset_id: asset.id(),
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                time_slice: time_slice.clone(),
                value,
            };
            self.activity_duals_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Write commodity balance duals to file
    fn write_commodity_balance_duals<'a, I>(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        iter: I,
    ) -> Result<()>
    where
        I: Iterator<Item = (&'a CommodityID, &'a RegionID, &'a TimeSliceID, MoneyPerFlow)>,
    {
        for (commodity_id, region_id, time_slice, value) in iter {
            let row = CommodityBalanceDualsRow {
                milestone_year,
                run_description: self.with_context(run_description),
                commodity_id: commodity_id.clone(),
                region_id: region_id.clone(),
                time_slice: time_slice.clone(),
                value,
            };
            self.commodity_balance_duals_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Write additional solver output values to file
    fn write_solver_values(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        objective_value: Money,
    ) -> Result<()> {
        let row = SolverValuesRow {
            milestone_year,
            run_description: self.with_context(run_description),
            objective_value,
        };
        self.solver_values_writer.serialize(row)?;
        self.solver_values_writer.flush()?;

        Ok(())
    }

    /// Write appraisal results to file
    fn write_appraisal_results(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        appraisal_results: &[AppraisalOutput],
    ) -> Result<()> {
        for result in appraisal_results {
            let row = AppraisalResultsRow {
                milestone_year,
                run_description: self.with_context(run_description),
                asset_id: result.asset.id(),
                process_id: result.asset.process_id().clone(),
                region_id: result.asset.region_id().clone(),
                capacity: result.capacity,
                unmet_demand: result.unmet_demand.values().copied().sum(),
                metric: result.metric,
            };
            self.appraisal_results_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Write reduced costs to file
    fn write_reduced_costs(
        &mut self,
        milestone_year: u32,
        reduced_costs: &ReducedCosts,
    ) -> Result<()> {
        for ((asset, time_slice), reduced_cost) in reduced_costs.iter() {
            let row = ReducedCostsRow {
                milestone_year,
                asset_id: asset.id(),
                process_id: asset.process_id().clone(),
                region_id: asset.region_id().clone(),
                time_slice: time_slice.clone(),
                reduced_cost: *reduced_cost,
            };
            self.reduced_costs_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Flush the underlying streams
    fn flush(&mut self) -> Result<()> {
        self.activity_writer.flush()?;
        self.commodity_balance_duals_writer.flush()?;
        self.activity_duals_writer.flush()?;
        self.solver_values_writer.flush()?;
        self.appraisal_results_writer.flush()?;
        self.reduced_costs_writer.flush()?;

        Ok(())
    }
}

/// An object for writing commodity prices to file
pub struct DataWriter {
    assets_path: PathBuf,
    flows_writer: csv::Writer<File>,
    prices_writer: csv::Writer<File>,
    debug_writer: Option<DebugDataWriter>,
}

impl DataWriter {
    /// Open CSV files to write output data to
    ///
    /// # Arguments
    ///
    /// * `output_path` - Folder where files will be saved
    /// * `model_path` - Path to input model
    /// * `save_debug_info` - Whether to include extra CSV files for debugging model
    pub fn create(output_path: &Path, model_path: &Path, save_debug_info: bool) -> Result<Self> {
        write_metadata(output_path, model_path).context("Failed to save metadata")?;

        let new_writer = |file_name| {
            let file_path = output_path.join(file_name);
            csv::Writer::from_path(file_path)
        };

        let debug_writer = if save_debug_info {
            // Create debug CSV files
            Some(DebugDataWriter::create(output_path)?)
        } else {
            None
        };

        Ok(Self {
            assets_path: output_path.join(ASSETS_FILE_NAME),
            flows_writer: new_writer(COMMODITY_FLOWS_FILE_NAME)?,
            prices_writer: new_writer(COMMODITY_PRICES_FILE_NAME)?,
            debug_writer,
        })
    }

    /// Write debug info about the dispatch optimisation
    pub fn write_dispatch_debug_info(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        solution: &Solution,
    ) -> Result<()> {
        if let Some(wtr) = &mut self.debug_writer {
            wtr.write_dispatch_debug_info(milestone_year, run_description, solution)?;
        }

        Ok(())
    }

    /// Write debug info about the investment appraisal
    pub fn write_appraisal_debug_info(
        &mut self,
        milestone_year: u32,
        run_description: &str,
        appraisal_results: &[AppraisalOutput],
    ) -> Result<()> {
        if let Some(wtr) = &mut self.debug_writer {
            wtr.write_appraisal_results(milestone_year, run_description, appraisal_results)?;
        }

        Ok(())
    }

    /// Write assets to a CSV file.
    ///
    /// The whole file is written at once and is overwritten with subsequent invocations. This is
    /// done so that partial results will be written in the case of errors and so that the user can
    /// see the results while the simulation is still running.
    ///
    /// The file is sorted by asset ID.
    ///
    /// # Panics
    ///
    /// Panics if any of the assets has not yet been commissioned (decommissioned assets are fine).
    pub fn write_assets<'a, I>(&mut self, assets: I) -> Result<()>
    where
        I: Iterator<Item = &'a AssetRef>,
    {
        let mut writer = csv::Writer::from_path(&self.assets_path)?;
        for asset in assets.sorted() {
            let row = AssetRow::new(asset);
            writer.serialize(row)?;
        }
        writer.flush()?;

        Ok(())
    }

    /// Write commodity flows to a CSV file
    pub fn write_flows(&mut self, milestone_year: u32, flow_map: &FlowMap) -> Result<()> {
        for ((asset, commodity_id, time_slice), flow) in flow_map {
            let row = CommodityFlowRow {
                milestone_year,
                asset_id: asset.id().unwrap(),
                commodity_id: commodity_id.clone(),
                time_slice: time_slice.clone(),
                flow: *flow,
            };
            self.flows_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Write commodity prices to a CSV file
    pub fn write_prices(&mut self, milestone_year: u32, prices: &CommodityPrices) -> Result<()> {
        for (commodity_id, region_id, time_slice, price) in prices.iter() {
            let row = CommodityPriceRow {
                milestone_year,
                commodity_id: commodity_id.clone(),
                region_id: region_id.clone(),
                time_slice: time_slice.clone(),
                price,
            };
            self.prices_writer.serialize(row)?;
        }

        Ok(())
    }

    /// Write reduced costs to a CSV file
    pub fn write_debug_reduced_costs(
        &mut self,
        milestone_year: u32,
        reduced_costs: &ReducedCosts,
    ) -> Result<()> {
        if let Some(wtr) = &mut self.debug_writer {
            wtr.write_reduced_costs(milestone_year, reduced_costs)?;
        }
        Ok(())
    }

    /// Flush the underlying streams
    pub fn flush(&mut self) -> Result<()> {
        self.flows_writer.flush()?;
        self.prices_writer.flush()?;
        if let Some(wtr) = &mut self.debug_writer {
            wtr.flush()?;
        }

        Ok(())
    }

    /// Add context to the debug writer
    pub fn set_debug_context(&mut self, context: String) {
        if let Some(wtr) = &mut self.debug_writer {
            wtr.context = Some(context);
        }
    }

    /// Clear context from the debug writer
    pub fn clear_debug_context(&mut self) {
        if let Some(wtr) = &mut self.debug_writer {
            wtr.context = None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::AssetPool;
    use crate::fixture::{assets, commodity_id, region_id, time_slice};
    use crate::time_slice::TimeSliceID;
    use indexmap::indexmap;
    use itertools::{Itertools, assert_equal};
    use rstest::rstest;
    use std::iter;
    use tempfile::tempdir;

    #[rstest]
    fn test_write_assets(assets: AssetPool) {
        let dir = tempdir().unwrap();

        // Write an asset
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
            writer.write_assets(assets.iter_active()).unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let asset = assets.iter_active().next().unwrap();
        let expected = AssetRow::new(asset);
        let records: Vec<AssetRow> = csv::Reader::from_path(dir.path().join(ASSETS_FILE_NAME))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_flows(assets: AssetPool, commodity_id: CommodityID, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let asset = assets.iter_active().next().unwrap();
        let flow_map = indexmap! {
            (asset.clone(), commodity_id.clone(), time_slice.clone()) => Flow(42.0)
        };

        // Write a flow
        let dir = tempdir().unwrap();
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
            writer.write_flows(milestone_year, &flow_map).unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = CommodityFlowRow {
            milestone_year,
            asset_id: asset.id().unwrap(),
            commodity_id,
            time_slice,
            flow: Flow(42.0),
        };
        let records: Vec<CommodityFlowRow> =
            csv::Reader::from_path(dir.path().join(COMMODITY_FLOWS_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_prices(commodity_id: CommodityID, region_id: RegionID, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let price = MoneyPerFlow(42.0);
        let mut prices = CommodityPrices::default();
        prices.insert(&commodity_id, &region_id, &time_slice, price);

        let dir = tempdir().unwrap();

        // Write a price
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
            writer.write_prices(milestone_year, &prices).unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = CommodityPriceRow {
            milestone_year,
            commodity_id,
            region_id,
            time_slice,
            price,
        };
        let records: Vec<CommodityPriceRow> =
            csv::Reader::from_path(dir.path().join(COMMODITY_PRICES_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_commodity_balance_duals(
        commodity_id: CommodityID,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let value = MoneyPerFlow(0.5);
        let dir = tempdir().unwrap();

        // Write commodity balance dual
        {
            let mut writer = DebugDataWriter::create(dir.path()).unwrap();
            writer
                .write_commodity_balance_duals(
                    milestone_year,
                    &run_description,
                    iter::once((&commodity_id, &region_id, &time_slice, value)),
                )
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = CommodityBalanceDualsRow {
            milestone_year,
            run_description,
            commodity_id,
            region_id,
            time_slice,
            value,
        };
        let records: Vec<CommodityBalanceDualsRow> =
            csv::Reader::from_path(dir.path().join(COMMODITY_BALANCE_DUALS_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_activity_duals(assets: AssetPool, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let value = MoneyPerActivity(0.5);
        let dir = tempdir().unwrap();
        let asset = assets.iter_active().next().unwrap();

        // Write activity dual
        {
            let mut writer = DebugDataWriter::create(dir.path()).unwrap();
            writer
                .write_activity_duals(
                    milestone_year,
                    &run_description,
                    iter::once((asset, &time_slice, value)),
                )
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = ActivityDualsRow {
            milestone_year,
            run_description,
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice,
            value,
        };
        let records: Vec<ActivityDualsRow> =
            csv::Reader::from_path(dir.path().join(ACTIVITY_DUALS_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_activity(assets: AssetPool, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let activity = Activity(100.5);
        let dir = tempdir().unwrap();
        let asset = assets.iter_active().next().unwrap();

        // Write activity
        {
            let mut writer = DebugDataWriter::create(dir.path()).unwrap();
            writer
                .write_activity(
                    milestone_year,
                    &run_description,
                    iter::once((asset, &time_slice, activity)),
                )
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = ActivityRow {
            milestone_year,
            run_description,
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice,
            activity,
        };
        let records: Vec<ActivityRow> = csv::Reader::from_path(dir.path().join(ACTIVITY_FILE_NAME))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_solver_values() {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let objective_value = Money(1234.56);
        let dir = tempdir().unwrap();

        // Write solver values
        {
            let mut writer = DebugDataWriter::create(dir.path()).unwrap();
            writer
                .write_solver_values(milestone_year, &run_description, objective_value)
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = SolverValuesRow {
            milestone_year,
            run_description,
            objective_value,
        };
        let records: Vec<SolverValuesRow> =
            csv::Reader::from_path(dir.path().join(SOLVER_VALUES_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_appraisal_results(assets: AssetPool) {
        let milestone_year = 2020;
        let run_description = "test_run".to_string();
        let dir = tempdir().unwrap();
        let asset = assets.iter_active().next().unwrap();

        // Write appraisal results
        {
            let mut writer = DebugDataWriter::create(dir.path()).unwrap();
            let appraisal = AppraisalOutput {
                asset: asset.clone(),
                capacity: Capacity(42.0),
                unmet_demand: Default::default(),
                metric: 4.14,
            };
            writer
                .write_appraisal_results(milestone_year, &run_description, &[appraisal])
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = AppraisalResultsRow {
            milestone_year,
            run_description,
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            capacity: Capacity(42.0),
            unmet_demand: Flow(0.0),
            metric: 4.14,
        };
        let records: Vec<AppraisalResultsRow> =
            csv::Reader::from_path(dir.path().join(APPRAISAL_RESULTS_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn test_write_reduced_costs(assets: AssetPool, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let dir = tempdir().unwrap();
        let asset = assets.iter_active().next().unwrap();

        // Write reduced costs
        {
            let mut writer = DebugDataWriter::create(dir.path()).unwrap();
            let reduced_costs = indexmap! {
                (asset.clone(), time_slice.clone()) => MoneyPerActivity(0.5)
            }
            .into();
            writer
                .write_reduced_costs(milestone_year, &reduced_costs)
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let expected = ReducedCostsRow {
            milestone_year,
            asset_id: asset.id(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            time_slice,
            reduced_cost: MoneyPerActivity(0.5),
        };
        let records: Vec<ReducedCostsRow> =
            csv::Reader::from_path(dir.path().join(REDUCED_COSTS_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[test]
    fn test_create_output_directory_new_directory() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("new_output");

        // Create a new directory should succeed and return false (no overwrite)
        let result = create_output_directory(&output_dir, false).unwrap();
        assert!(!result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
    }

    #[test]
    fn test_create_output_directory_existing_empty_directory() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("empty_output");

        // Create the directory first
        fs::create_dir(&output_dir).unwrap();

        // Creating again should succeed and return false (no overwrite needed)
        let result = create_output_directory(&output_dir, false).unwrap();
        assert!(!result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
    }

    #[test]
    fn test_create_output_directory_existing_with_files_no_overwrite() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("output_with_files");

        // Create directory with a file
        fs::create_dir(&output_dir).unwrap();
        fs::write(output_dir.join("existing_file.txt"), "some content").unwrap();

        // Should fail when allow_overwrite is false
        let result = create_output_directory(&output_dir, false);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Output folder already exists")
        );
    }

    #[test]
    fn test_create_output_directory_existing_with_files_allow_overwrite() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("output_with_files");

        // Create directory with a file
        fs::create_dir(&output_dir).unwrap();
        let file_path = output_dir.join("existing_file.txt");
        fs::write(&file_path, "some content").unwrap();

        // Should succeed when allow_overwrite is true and return true (overwrite occurred)
        let result = create_output_directory(&output_dir, true).unwrap();
        assert!(result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
        assert!(!file_path.exists()); // File should be gone
    }

    #[test]
    fn test_create_output_directory_nested_path() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("nested").join("path").join("output");

        // Should create nested directories and return false (no overwrite)
        let result = create_output_directory(&output_dir, false).unwrap();
        assert!(!result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
    }

    #[test]
    fn test_create_output_directory_existing_subdirs_with_files_allow_overwrite() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("output_with_subdirs");

        // Create directory structure with files
        fs::create_dir_all(&output_dir.join("subdir")).unwrap();
        fs::write(output_dir.join("file1.txt"), "content1").unwrap();
        fs::write(output_dir.join("subdir").join("file2.txt"), "content2").unwrap();

        // Should succeed when allow_overwrite is true and return true (overwrite occurred)
        let result = create_output_directory(&output_dir, true).unwrap();
        assert!(result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
        // All previous content should be gone
        assert!(!output_dir.join("file1.txt").exists());
        assert!(!output_dir.join("subdir").exists());
    }
}
