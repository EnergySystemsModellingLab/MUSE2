//! The module responsible for writing output data to disk.
use crate::agent::AgentID;
use crate::asset::{Asset, AssetID, AssetRef};
use crate::commodity::CommodityID;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::activity::FlowMap;
use crate::simulation::prices::PriceMap;
use crate::time_slice::TimeSliceID;
use crate::units::{Capacity, Flow, MoneyPerFlow};
use anyhow::{Context, Result, ensure};
use csv;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};

pub mod metadata;
use metadata::write_metadata;

/// The output file name for commodity flows
const COMMODITY_FLOWS_FILE_NAME: &str = "commodity_flows.csv";

/// The output file name for commodity prices
const COMMODITY_PRICES_FILE_NAME: &str = "commodity_prices.csv";

/// The output file name for assets
const ASSETS_FILE_NAME: &str = "assets.csv";

/// The output file name for asset capacities
const ASSET_CAPACITIES_FILE_NAME: &str = "asset_capacities.csv";

/// Get the default output directory for the model
pub fn get_output_dir(model_dir: &Path, results_root: PathBuf) -> Result<PathBuf> {
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
    Ok([results_root, model_name.into()].iter().collect())
}

/// Get the default output directory for commodity flow graphs for the model
pub fn get_graphs_dir(model_dir: &Path, graph_results_root: PathBuf) -> Result<PathBuf> {
    let model_dir = model_dir
        .canonicalize() // canonicalise in case the user has specified "."
        .context("Could not resolve path to model")?;
    let model_name = model_dir
        .file_name()
        .context("Model cannot be in root folder")?
        .to_str()
        .context("Invalid chars in model dir name")?;
    Ok([graph_results_root, model_name.into()].iter().collect())
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

/// Copy input files to output directory
pub fn copy_input_files(model_dir: &Path, output_dir: &Path, model_name: &str) -> Result<()> {
    // Get the model name from the dir path.
    let mut input_copy_dir = output_dir.to_path_buf();
    input_copy_dir.extend(["input", model_name]);

    fs::create_dir_all(&input_copy_dir).context("Could not create input copy directory")?;

    for entry in fs::read_dir(model_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            let file_name = path.file_name().unwrap();
            fs::copy(&path, input_copy_dir.join(file_name))?;
        }
    }
    Ok(())
}

/// Represents a row in the assets output CSV file.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AssetRow {
    asset_id: AssetID,
    process_id: ProcessID,
    region_id: RegionID,
    agent_id: AgentID,
    commission_year: u32,
}

impl AssetRow {
    /// Create a new [`AssetRow`] for the given asset
    fn new(asset: &Asset) -> Self {
        Self {
            asset_id: asset.id().unwrap(),
            process_id: asset.process_id().clone(),
            region_id: asset.region_id().clone(),
            agent_id: asset.agent_id().unwrap().clone(),
            commission_year: asset.commission_year(),
        }
    }
}

/// Represents a row in the asset capacities output CSV file.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct AssetCapacityRow {
    milestone_year: u32,
    asset_id: AssetID,
    capacity: Capacity,
    /// Empty if the capacity is continuous
    num_tranches: Option<u32>,
    mothballed_capacity: Capacity,
    /// Empty if the capacity is continuous
    mothballed_tranches: Option<u32>,
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

/// For writing extra debug information about the model
struct DebugDataWriter {
    context: Option<String>,
    output_path: PathBuf,
    /// Writers for debug files owned by other modules, created on first use
    custom_writers: HashMap<&'static str, csv::Writer<File>>,
}

impl DebugDataWriter {
    /// Set up debug output, creating CSV files on first use
    ///
    /// # Arguments
    ///
    /// * `output_path` - Folder where files will be saved
    fn create(output_path: &Path) -> Self {
        Self {
            context: None,
            output_path: output_path.to_path_buf(),
            custom_writers: HashMap::new(),
        }
    }

    /// Prepend the current context to the run description
    fn with_context(&self, run_description: &str) -> String {
        if let Some(context) = &self.context {
            format!("{context}; {run_description}")
        } else {
            run_description.to_string()
        }
    }

    /// Write rows to a debug file owned by another module
    ///
    /// The file is created on first use, even if `rows` is empty.
    fn write_custom_rows<R: Serialize>(
        &mut self,
        file_name: &'static str,
        rows: impl IntoIterator<Item = R>,
    ) -> Result<()> {
        let writer = match self.custom_writers.entry(file_name) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                entry.insert(csv::Writer::from_path(self.output_path.join(file_name))?)
            }
        };
        for row in rows {
            writer.serialize(row)?;
        }

        Ok(())
    }

    /// Flush the underlying streams
    fn flush(&mut self) -> Result<()> {
        for wrt in self.custom_writers.values_mut() {
            wrt.flush()?;
        }

        Ok(())
    }
}

/// An object for writing output data to file
pub struct DataWriter {
    assets: csv::Writer<File>,
    asset_capacities: csv::Writer<File>,
    flows: csv::Writer<File>,
    prices: csv::Writer<File>,
    debug: Option<DebugDataWriter>,
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

        let debug_writer = save_debug_info.then(|| DebugDataWriter::create(output_path));

        Ok(Self {
            assets: new_writer(ASSETS_FILE_NAME)?,
            asset_capacities: new_writer(ASSET_CAPACITIES_FILE_NAME)?,
            flows: new_writer(COMMODITY_FLOWS_FILE_NAME)?,
            prices: new_writer(COMMODITY_PRICES_FILE_NAME)?,
            debug: debug_writer,
        })
    }

    /// Whether debug files are being written
    pub fn debug_enabled(&self) -> bool {
        self.debug.is_some()
    }

    /// Prepend the current debug context (if any) to a run description
    pub fn debug_run_description(&self, run_description: &str) -> String {
        match &self.debug {
            Some(wtr) => wtr.with_context(run_description),
            None => run_description.to_string(),
        }
    }

    /// Write rows to a custom debug CSV file in the output directory
    ///
    /// This allows other modules (e.g. asset selection algorithms) to own their debug files. The
    /// file is created the first time it is written to (even if `rows` is empty). Does nothing if
    /// debug output is disabled.
    ///
    /// Files for asset selection algorithms should be named
    /// `debug_asset_selection_<agent_type>[_<suffix>].csv`.
    pub fn write_debug_rows<R: Serialize>(
        &mut self,
        file_name: &'static str,
        rows: impl IntoIterator<Item = R>,
    ) -> Result<()> {
        if let Some(wtr) = &mut self.debug {
            wtr.write_custom_rows(file_name, rows)?;
        }

        Ok(())
    }

    /// Append newly commissioned asset definitions to the assets CSV file
    pub fn write_assets(&mut self, assets: &[AssetRef]) -> Result<()> {
        for asset in assets {
            self.assets.serialize(AssetRow::new(asset))?;
        }

        Ok(())
    }

    /// Write asset capacities for the current milestone year to a CSV file
    pub fn write_asset_capacities(
        &mut self,
        milestone_year: u32,
        assets: &[AssetRef],
    ) -> Result<()> {
        for asset in assets {
            let row = AssetCapacityRow {
                milestone_year,
                asset_id: asset.id().unwrap(),
                capacity: asset.total_capacity(),
                num_tranches: asset.capacity().num_tranches(),
                mothballed_capacity: asset.mothballed_capacity().total_capacity(),
                mothballed_tranches: asset.mothballed_capacity().num_tranches(),
            };
            self.asset_capacities.serialize(row)?;
        }

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
            self.flows.serialize(row)?;
        }

        Ok(())
    }

    /// Write commodity prices to a CSV file
    pub fn write_prices(&mut self, milestone_year: u32, prices: &PriceMap) -> Result<()> {
        for (commodity_id, region_id, time_slice, price) in prices.iter() {
            let row = CommodityPriceRow {
                milestone_year,
                commodity_id: commodity_id.clone(),
                region_id: region_id.clone(),
                time_slice: time_slice.clone(),
                price,
            };
            self.prices.serialize(row)?;
        }

        Ok(())
    }

    /// Flush the underlying streams
    pub fn flush(&mut self) -> Result<()> {
        self.assets.flush()?;
        self.asset_capacities.flush()?;
        self.flows.flush()?;
        self.prices.flush()?;
        if let Some(wtr) = &mut self.debug {
            wtr.flush()?;
        }

        Ok(())
    }

    /// Add context to the debug writer
    pub fn set_debug_context(&mut self, context: String) {
        if let Some(wtr) = &mut self.debug {
            wtr.context = Some(context);
        }
    }

    /// Clear context from the debug writer
    pub fn clear_debug_context(&mut self) {
        if let Some(wtr) = &mut self.debug {
            wtr.context = None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::{AssetCapacity, AssetPool};
    use crate::fixture::{assets, commodity_id, multi_tranche_asset, region_id, time_slice};
    use crate::time_slice::TimeSliceID;
    use indexmap::indexmap;
    use itertools::{Itertools, assert_equal};
    use rstest::rstest;
    use std::iter;
    use tempfile::tempdir;

    #[rstest]
    fn write_assets(assets: AssetPool) {
        let dir = tempdir().unwrap();

        // Write an asset
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
            writer.write_assets(&assets).unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let asset = assets.iter().next().unwrap();
        let expected = AssetRow::new(asset);
        let records: Vec<AssetRow> = csv::Reader::from_path(dir.path().join(ASSETS_FILE_NAME))
            .unwrap()
            .into_deserialize()
            .try_collect()
            .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn write_asset_capacities(assets: AssetPool) {
        let milestone_year = 2020;
        let dir = tempdir().unwrap();

        // Write asset capacities
        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
            writer
                .write_asset_capacities(milestone_year, &assets)
                .unwrap();
            writer.flush().unwrap();
        }

        // Read back and compare
        let asset = assets.iter().next().unwrap();
        let expected = AssetCapacityRow {
            milestone_year,
            asset_id: asset.id().unwrap(),
            capacity: asset.total_capacity(),
            num_tranches: Some(1),
            mothballed_capacity: Capacity(0.0),
            mothballed_tranches: Some(0),
        };
        let records: Vec<AssetCapacityRow> =
            csv::Reader::from_path(dir.path().join(ASSET_CAPACITIES_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn write_asset_capacities_with_mothballed_capacity(multi_tranche_asset: Asset) {
        let milestone_year = 2020;
        let dir = tempdir().unwrap();
        let mut assets = AssetPool::new();
        assets.commission_new(2010, &mut vec![multi_tranche_asset.into()]);
        let asset = assets
            .iter()
            .next()
            .unwrap()
            .clone()
            .with_mothballed_capacity(AssetCapacity::discrete(1, Capacity(4.0)), milestone_year);

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
            writer
                .write_asset_capacities(milestone_year, std::slice::from_ref(&asset))
                .unwrap();
            writer.flush().unwrap();
        }

        let expected = AssetCapacityRow {
            milestone_year,
            asset_id: asset.id().unwrap(),
            capacity: Capacity(12.0),
            num_tranches: Some(3),
            mothballed_capacity: Capacity(4.0),
            mothballed_tranches: Some(1),
        };
        let records: Vec<AssetCapacityRow> =
            csv::Reader::from_path(dir.path().join(ASSET_CAPACITIES_FILE_NAME))
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
        assert_equal(records, iter::once(expected));
    }

    #[rstest]
    fn write_flows(assets: AssetPool, commodity_id: CommodityID, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let asset = assets.iter().next().unwrap();
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
    fn write_prices(commodity_id: CommodityID, region_id: RegionID, time_slice: TimeSliceID) {
        let milestone_year = 2020;
        let price = MoneyPerFlow(42.0);
        let mut prices = PriceMap::default();
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
    #[case(true)]
    #[case(false)]
    fn write_debug_rows(#[case] debug: bool) {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct TestRow {
            run_description: String,
            value: u32,
        }

        let dir = tempdir().unwrap();
        let file_name = "debug_test.csv";
        let rows = [
            TestRow {
                run_description: "a".into(),
                value: 1,
            },
            TestRow {
                run_description: "b".into(),
                value: 2,
            },
        ];

        {
            let mut writer = DataWriter::create(dir.path(), dir.path(), debug).unwrap();
            assert_eq!(writer.debug_enabled(), debug);

            // Rows written across multiple calls end up in the same file
            writer.write_debug_rows(file_name, &rows[..1]).unwrap();
            writer.write_debug_rows(file_name, &rows[1..]).unwrap();
            writer.flush().unwrap();
        }

        let debug_files = fs::read_dir(dir.path())
            .unwrap()
            .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
            .filter(|name| name.starts_with("debug_"))
            .collect::<Vec<_>>();
        assert_eq!(
            debug_files,
            if debug { vec![file_name] } else { Vec::new() }
        );

        let path = dir.path().join(file_name);
        if debug {
            let records: Vec<TestRow> = csv::Reader::from_path(path)
                .unwrap()
                .into_deserialize()
                .try_collect()
                .unwrap();
            assert_equal(records, rows);
        } else {
            assert!(!path.exists());
        }
    }

    #[test]
    fn create_output_directory_new_directory() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("new_output");

        // Create a new directory should succeed and return false (no overwrite)
        let result = create_output_directory(&output_dir, false).unwrap();
        assert!(!result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
    }

    #[test]
    fn create_output_directory_existing_empty_directory() {
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
    fn create_output_directory_existing_with_files_no_overwrite() {
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
    fn create_output_directory_existing_with_files_allow_overwrite() {
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
    fn create_output_directory_nested_path() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("nested").join("path").join("output");

        // Should create nested directories and return false (no overwrite)
        let result = create_output_directory(&output_dir, false).unwrap();
        assert!(!result);
        assert!(output_dir.exists());
        assert!(output_dir.is_dir());
    }

    #[test]
    fn create_output_directory_existing_subdirs_with_files_allow_overwrite() {
        let temp_dir = tempdir().unwrap();
        let output_dir = temp_dir.path().join("output_with_subdirs");

        // Create directory structure with files
        fs::create_dir_all(output_dir.join("subdir")).unwrap();
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
