//! Code for loading program settings.
use crate::get_muse2_config_dir;
use crate::input::read_toml;
use crate::log::DEFAULT_LOG_LEVEL;
use anyhow::Result;
use documented::DocumentedFields;
use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::path::{Path, PathBuf};

const SETTINGS_FILE_NAME: &str = "settings.toml";

const DEFAULT_SETTINGS_FILE_HEADER: &str = concat!(
    "# This file contains the program settings for MUSE2.
#
# The default options for MUSE2 v",
    env!("CARGO_PKG_VERSION"),
    " are shown below, commented out. To change an option, uncomment it and set the value
# appropriately.
#
# To show the default options for the current version of MUSE2, run:
# \tmuse2 settings show-default
#
# For information about the possible settings, visit:
# \thttps://energysystemsmodellinglab.github.io/MUSE2/file_formats/program_settings.html
"
);

/// Get the path to where the settings file will be read from
pub fn get_settings_file_path() -> PathBuf {
    let mut path = get_muse2_config_dir();
    path.push(SETTINGS_FILE_NAME);

    path
}

/// Program settings from config file
///
/// NOTE: If you add or change a field in this struct, you must also update the schema in
/// `schemas/settings.yaml`.
#[derive(Debug, DocumentedFields, Serialize, Deserialize, PartialEq)]
#[serde(default)]
pub struct Settings {
    /// The default program log level
    pub log_level: String,
    /// Whether to overwrite output files by default
    pub overwrite: bool,
    /// Whether to write additional information to CSV files
    pub debug_model: bool,
    /// Results root path to save MUSE2 results. Defaults to `muse2_results`.
    pub results_root: PathBuf,
    /// Results root path to save MUSE2 graph outputs. Defaults to `muse2_graphs`.
    pub graph_results_root: PathBuf,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            log_level: DEFAULT_LOG_LEVEL.to_string(),
            overwrite: false,
            debug_model: false,
            results_root: PathBuf::from("muse2_results"),
            graph_results_root: PathBuf::from("muse2_graphs"),
        }
    }
}

impl Settings {
    /// Read the contents of a settings file from the model directory.
    ///
    /// If the file is not present, default settings will be used.
    ///
    /// # Returns
    ///
    /// The program settings as a `Settings` struct or an error if loading fails.
    pub fn load() -> Result<Settings> {
        Self::load_from_path(&get_settings_file_path())
    }

    /// Read from the specified path, returning
    fn load_from_path(file_path: &Path) -> Result<Settings> {
        if !file_path.is_file() {
            return Ok(Settings::default());
        }

        read_toml(file_path)
    }

    /// The contents of the default settings file.
    pub fn default_file_contents() -> String {
        // Settings object with default values for params
        let settings = Settings::default();

        // Convert to TOML
        let settings_raw = toml::to_string(&settings).expect("Could not convert settings to TOML");

        // Iterate through the generated TOML, commenting out parameter lines and inserting
        // their documentation comments
        let mut out = DEFAULT_SETTINGS_FILE_HEADER.to_string();
        for line in settings_raw.split('\n') {
            if let Some((field, _)) = line.split_once('=') {
                // Add documentation from doc comments
                let field = field.trim();

                // Use doc comment to document parameter. All fields should have doc comments.
                let docs = Settings::get_field_docs(field).expect("Missing doc comment for field");
                for line in docs.split('\n') {
                    write!(&mut out, "\n# # {}\n", line.trim()).unwrap();
                }

                writeln!(&mut out, "# {}", line.trim()).unwrap();
            }
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn settings_load_from_path_no_file() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join(SETTINGS_FILE_NAME); // NB: doesn't exist
        assert_eq!(
            Settings::load_from_path(&file_path).unwrap(),
            Settings::default()
        );
    }

    #[test]
    fn settings_load_from_path() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join(SETTINGS_FILE_NAME);

        {
            let mut file = File::create(&file_path).unwrap();
            writeln!(file, "log_level = \"warn\"").unwrap();
        }

        assert_eq!(
            Settings::load_from_path(&file_path).unwrap(),
            Settings {
                log_level: "warn".to_string(),
                ..Settings::default()
            }
        );
    }

    #[test]
    fn default_file_contents() {
        assert!(!Settings::default_file_contents().is_empty());
    }
}
