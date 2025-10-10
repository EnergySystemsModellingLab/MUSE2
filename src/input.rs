//! Common routines for handling input data.
use crate::asset::AssetPool;
use crate::graph::build_and_validate_commodity_graphs_for_model;
use crate::id::{HasID, IDLike};
use crate::model::{Model, ModelParameters};
use crate::units::UnitType;
use anyhow::{Context, Result, bail, ensure};
use float_cmp::approx_eq;
use indexmap::IndexMap;
use itertools::Itertools;
use serde::de::{Deserialize, DeserializeOwned, Deserializer};
use std::collections::HashMap;
use std::fs;
use std::hash::Hash;
use std::path::Path;

mod agent;
use agent::read_agents;
mod asset;
use asset::read_assets;
mod commodity;
use commodity::read_commodities;
mod process;
use process::read_processes;
mod region;
use region::read_regions;
mod time_slice;
use time_slice::read_time_slice_info;

/// Read a series of type `T`s from a CSV file.
///
/// Will raise an error if the file is empty.
///
/// # Arguments
///
/// * `file_path` - Path to the CSV file
pub fn read_csv<'a, T: DeserializeOwned + 'a>(
    file_path: &'a Path,
) -> Result<impl Iterator<Item = T> + 'a> {
    let vec = read_csv_internal(file_path)?;
    if vec.is_empty() {
        bail!("CSV file {} cannot be empty", file_path.display());
    }
    Ok(vec.into_iter())
}

/// Read a series of type `T`s from a CSV file.
///
/// # Arguments
///
/// * `file_path` - Path to the CSV file
pub fn read_csv_optional<'a, T: DeserializeOwned + 'a>(
    file_path: &'a Path,
) -> Result<impl Iterator<Item = T> + 'a> {
    let vec = read_csv_internal(file_path)?;
    Ok(vec.into_iter())
}

fn read_csv_internal<'a, T: DeserializeOwned + 'a>(file_path: &'a Path) -> Result<Vec<T>> {
    let vec = csv::Reader::from_path(file_path)
        .with_context(|| input_err_msg(file_path))?
        .into_deserialize()
        .process_results(|iter| iter.collect_vec())
        .with_context(|| input_err_msg(file_path))?;

    Ok(vec)
}

/// Parse a TOML file at the specified path.
///
/// # Arguments
///
/// * `file_path` - Path to the TOML file
///
/// # Returns
///
/// * The deserialised TOML data or an error if the file could not be read or parsed.
pub fn read_toml<T: DeserializeOwned>(file_path: &Path) -> Result<T> {
    let toml_str = fs::read_to_string(file_path).with_context(|| input_err_msg(file_path))?;
    let toml_data = toml::from_str(&toml_str).with_context(|| input_err_msg(file_path))?;
    Ok(toml_data)
}

/// Read a Dimensionless float, checking that it is between 0 and 1
pub fn deserialise_proportion_nonzero<'de, D, T>(deserialiser: D) -> Result<T, D::Error>
where
    T: UnitType,
    D: Deserializer<'de>,
{
    let value = f64::deserialize(deserialiser)?;
    if !(value > 0.0 && value <= 1.0) {
        Err(serde::de::Error::custom("Value must be > 0 and <= 1"))?;
    }

    Ok(T::new(value))
}

/// Format an error message to include the file path. To be used with `anyhow::Context`.
pub fn input_err_msg<P: AsRef<Path>>(file_path: P) -> String {
    format!("Error reading {}", file_path.as_ref().display())
}

/// Read a CSV file of items with IDs.
///
/// As this function is only ever used for top-level CSV files (i.e. the ones which actually define
/// the IDs for a given type), we use an ordered map to maintain the order in the input files.
fn read_csv_id_file<T, ID: IDLike>(file_path: &Path) -> Result<IndexMap<ID, T>>
where
    T: HasID<ID> + DeserializeOwned,
{
    fn fill_and_validate_map<T, ID: IDLike>(file_path: &Path) -> Result<IndexMap<ID, T>>
    where
        T: HasID<ID> + DeserializeOwned,
    {
        let mut map = IndexMap::new();
        for record in read_csv::<T>(file_path)? {
            let id = record.get_id().clone();
            let existing = map.insert(id.clone(), record).is_some();
            ensure!(!existing, "Duplicate ID found: {id}");
        }
        ensure!(!map.is_empty(), "CSV file is empty");

        Ok(map)
    }

    fill_and_validate_map(file_path).with_context(|| input_err_msg(file_path))
}

/// Check that fractions sum to (approximately) one
fn check_values_sum_to_one_approx<I, T>(fractions: I) -> Result<()>
where
    T: UnitType,
    I: Iterator<Item = T>,
{
    let sum = fractions.sum();
    ensure!(
        approx_eq!(T, sum, T::new(1.0), epsilon = 1e-5),
        "Sum of fractions does not equal one (actual: {sum})"
    );

    Ok(())
}

/// Check whether an iterator contains values that are sorted and unique
pub fn is_sorted_and_unique<T, I>(iter: I) -> bool
where
    T: PartialOrd + Clone,
    I: IntoIterator<Item = T>,
{
    iter.into_iter().tuple_windows().all(|(a, b)| a < b)
}

/// Inserts a key-value pair into a `HashMap` if the key does not already exist.
///
/// If the key already exists, it returns an error with a message indicating the key's existence.
pub fn try_insert<K, V>(map: &mut HashMap<K, V>, key: &K, value: V) -> Result<()>
where
    K: Eq + Hash + Clone + std::fmt::Debug,
{
    let existing = map.insert(key.clone(), value).is_some();
    ensure!(!existing, "Key {key:?} already exists in the map");
    Ok(())
}

/// Format a list of items with a cap on display count for error messages
pub fn format_items_with_cap<T: std::fmt::Debug>(items: &[T]) -> String {
    const MAX_DISPLAY: usize = 10;
    if items.len() <= MAX_DISPLAY {
        format!("{items:?}")
    } else {
        format!(
            "{:?} and {} more",
            &items[..MAX_DISPLAY],
            items.len() - MAX_DISPLAY
        )
    }
}

/// Read a model from the specified directory.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
///
/// # Returns
///
/// The static model data ([`Model`]) and an [`AssetPool`] struct or an error.
pub fn load_model<P: AsRef<Path>>(model_dir: P) -> Result<(Model, AssetPool)> {
    let model_params = ModelParameters::from_path(&model_dir)?;

    let time_slice_info = read_time_slice_info(model_dir.as_ref())?;
    let regions = read_regions(model_dir.as_ref())?;
    let region_ids = regions.keys().cloned().collect();
    let years = &model_params.milestone_years;

    let commodities = read_commodities(model_dir.as_ref(), &region_ids, &time_slice_info, years)?;
    let processes = read_processes(
        model_dir.as_ref(),
        &commodities,
        &region_ids,
        &time_slice_info,
        years,
    )?;
    let agents = read_agents(
        model_dir.as_ref(),
        &commodities,
        &processes,
        &region_ids,
        years,
    )?;
    let agent_ids = agents.keys().cloned().collect();
    let assets = read_assets(model_dir.as_ref(), &agent_ids, &processes, &region_ids)?;

    // Build and validate commodity graphs for all regions and years
    // This gives us the commodity order for each region/year which is passed to the model
    let commodity_order = build_and_validate_commodity_graphs_for_model(
        &processes,
        &commodities,
        &region_ids,
        years,
        &time_slice_info,
    )?;

    let model_path = model_dir
        .as_ref()
        .canonicalize()
        .context("Could not parse path to model")?;
    let model = Model {
        model_path,
        parameters: model_params,
        agents,
        commodities,
        processes,
        time_slice_info,
        regions,
        commodity_order,
    };
    Ok((model, AssetPool::new(assets)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::GenericID;
    use crate::units::Dimensionless;
    use rstest::rstest;
    use serde::Deserialize;
    use serde::de::IntoDeserializer;
    use serde::de::value::{Error as ValueError, F64Deserializer};
    use std::fs::File;
    use std::io::Write;
    use std::path::PathBuf;
    use tempfile::tempdir;

    #[derive(Debug, PartialEq, Deserialize)]
    struct Record {
        id: GenericID,
        value: u32,
    }

    impl HasID<GenericID> for Record {
        fn get_id(&self) -> &GenericID {
            &self.id
        }
    }

    /// Create an example CSV file in dir_path
    fn create_csv_file(dir_path: &Path, contents: &str) -> PathBuf {
        let file_path = dir_path.join("test.csv");
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "{contents}").unwrap();
        file_path
    }

    /// Test a normal read
    #[test]
    fn test_read_csv() {
        let dir = tempdir().unwrap();
        let file_path = create_csv_file(dir.path(), "id,value\nhello,1\nworld,2\n");
        let records: Vec<Record> = read_csv(&file_path).unwrap().collect();
        assert_eq!(
            records,
            &[
                Record {
                    id: "hello".into(),
                    value: 1,
                },
                Record {
                    id: "world".into(),
                    value: 2,
                }
            ]
        );

        // File with no data (only column headers)
        let file_path = create_csv_file(dir.path(), "id,value\n");
        assert!(read_csv::<Record>(&file_path).is_err());
        assert!(
            read_csv_optional::<Record>(&file_path)
                .unwrap()
                .next()
                .is_none()
        );
    }

    #[test]
    fn test_read_toml() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.toml");
        {
            let mut file = File::create(&file_path).unwrap();
            writeln!(file, "id = \"hello\"\nvalue = 1").unwrap();
        }

        assert_eq!(
            read_toml::<Record>(&file_path).unwrap(),
            Record {
                id: "hello".into(),
                value: 1,
            }
        );

        {
            let mut file = File::create(&file_path).unwrap();
            writeln!(file, "bad toml syntax").unwrap();
        }

        assert!(read_toml::<Record>(&file_path).is_err());
    }

    /// Deserialise value with deserialise_proportion_nonzero()
    fn deserialise_f64(value: f64) -> Result<Dimensionless, ValueError> {
        let deserialiser: F64Deserializer<ValueError> = value.into_deserializer();
        deserialise_proportion_nonzero(deserialiser)
    }

    #[test]
    fn test_deserialise_proportion_nonzero() {
        // Valid inputs
        assert_eq!(deserialise_f64(0.01), Ok(Dimensionless(0.01)));
        assert_eq!(deserialise_f64(0.5), Ok(Dimensionless(0.5)));
        assert_eq!(deserialise_f64(1.0), Ok(Dimensionless(1.0)));

        // Invalid inputs
        assert!(deserialise_f64(0.0).is_err());
        assert!(deserialise_f64(-1.0).is_err());
        assert!(deserialise_f64(2.0).is_err());
        assert!(deserialise_f64(f64::NAN).is_err());
        assert!(deserialise_f64(f64::INFINITY).is_err());
    }

    #[test]
    fn test_check_values_sum_to_one_approx() {
        // Single input, valid
        assert!(check_values_sum_to_one_approx([Dimensionless(1.0)].into_iter()).is_ok());

        // Multiple inputs, valid
        assert!(
            check_values_sum_to_one_approx([Dimensionless(0.4), Dimensionless(0.6)].into_iter())
                .is_ok()
        );

        // Single input, invalid
        assert!(check_values_sum_to_one_approx([Dimensionless(0.5)].into_iter()).is_err());

        // Multiple inputs, invalid
        assert!(
            check_values_sum_to_one_approx([Dimensionless(0.4), Dimensionless(0.3)].into_iter())
                .is_err()
        );

        // Edge cases
        assert!(
            check_values_sum_to_one_approx([Dimensionless(f64::INFINITY)].into_iter()).is_err()
        );
        assert!(check_values_sum_to_one_approx([Dimensionless(f64::NAN)].into_iter()).is_err());
    }

    #[rstest]
    #[case(&[], true)]
    #[case(&[1], true)]
    #[case(&[1,2], true)]
    #[case(&[1,2,3,4], true)]
    #[case(&[2,1],false)]
    #[case(&[1,1],false)]
    #[case(&[1,3,2,4], false)]
    fn test_is_sorted_and_unique(#[case] values: &[u32], #[case] expected: bool) {
        assert_eq!(is_sorted_and_unique(values), expected)
    }

    #[test]
    fn test_format_items_with_cap() {
        let items = vec!["a", "b", "c"];
        assert_eq!(format_items_with_cap(&items), r#"["a", "b", "c"]"#);

        // Test with more than 10 items to trigger the cap
        let many_items = vec!["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"];
        assert_eq!(
            format_items_with_cap(&many_items),
            r#"["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] and 2 more"#
        );
    }
}
