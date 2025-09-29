//! Code for loading program settings.
use crate::input::read_toml;
use crate::log::DEFAULT_LOG_LEVEL;
use anyhow::Result;
use documented::DocumentedFields;
use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::path::Path;

const SETTINGS_FILE_NAME: &str = "settings.toml";

const DEFAULT_SETTINGS_FILE_HEADER: &str = "# This file contains the program settings for MUSE 2.0
# For more information, visit:
# \thttps://energysystemsmodellinglab.github.io/MUSE_2.0/file_formats/program_settings.html
";

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
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            log_level: DEFAULT_LOG_LEVEL.to_string(),
            overwrite: false,
            debug_model: false,
        }
    }
}

impl Settings {
    /// Read the contents of a settings file from the model directory.
    ///
    /// If the file is not present, default values for settings will be used
    ///
    /// # Arguments
    ///
    /// * `model_dir` - Folder containing model configuration files
    ///
    /// # Returns
    ///
    /// The program settings as a `Settings` struct or an error if the file is invalid
    pub fn load() -> Result<Settings> {
        let file_path = Path::new(SETTINGS_FILE_NAME);
        if !file_path.is_file() {
            return Ok(Settings::default());
        }

        read_toml(file_path)
    }

    /// The contents of the default settings file
    pub fn default_file_contents() -> String {
        // Settings object with default values for params
        let settings = Settings::default();

        // Convert to TOML
        let settings_raw = toml::to_string(&settings).expect("Could not convert settings to TOML");

        // Iterate through the generated TOML, commenting out lines and adding docs
        let mut out = DEFAULT_SETTINGS_FILE_HEADER.to_string();
        for line in settings_raw.split('\n') {
            if let Some(last) = line.find('=') {
                // Add documentation from doc comments
                let field = line[..last].trim();

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
    use current_dir::Cwd;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn test_settings_from_path_no_file() {
        let dir = tempdir().unwrap();
        let mut cwd = Cwd::mutex().lock().unwrap();
        cwd.set(dir.path()).unwrap();
        assert_eq!(Settings::load().unwrap(), Settings::default());
    }

    #[test]
    fn test_settings_from_path() {
        let dir = tempdir().unwrap();
        let mut cwd = Cwd::mutex().lock().unwrap();
        cwd.set(dir.path()).unwrap();

        {
            let mut file = File::create(Path::new(SETTINGS_FILE_NAME)).unwrap();
            writeln!(file, "log_level = \"warn\"").unwrap();
        }

        assert_eq!(
            Settings::load().unwrap(),
            Settings {
                log_level: "warn".to_string(),
                debug_model: false,
                overwrite: false
            }
        );
    }

    #[test]
    fn test_default_file_contents() {
        assert!(!Settings::default_file_contents().is_empty());
    }
}
