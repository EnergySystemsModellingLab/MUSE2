//! Code for loading program settings.
use crate::get_muse2_config_dir;
use crate::input::read_toml;
use crate::log::DEFAULT_LOG_LEVEL;
use anyhow::Result;
use clap::Args;
use documented::DocumentedFields;
use serde::{Deserialize, Serialize};
use std::env;
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

/// Defines the program [`Settings`] together with their defaults and which of them can be
/// overridden from the command line.
///
/// Each setting is declared exactly once: its doc comment, type and default value. Appending
/// `, cli` to a setting generates a matching `Option<T>` field on [`SettingsOverrides`] (reusing
/// the doc comment as the flag's help text) and a merge step in [`Settings::apply_overrides`].
/// Settings without `cli` can only be set via `settings.toml`.
///
/// Both `bool` and `String` settings can be marked `cli`. A `bool` flag may be passed bare
/// (`--flag`, meaning `true`) or with an explicit value (`--flag=false`), whereas a `String` flag
/// always takes a value (`--flag value`).
macro_rules! settings {
    (
        $(
            $(#[doc = $doc:literal])+
            $field:ident: $ty:tt = $default:expr $(, $cli:ident)? ;
        )+
    ) => {
        /// Program settings, resolved from the defaults, the settings file and CLI overrides.
        ///
        /// NOTE: If you add or change a field here, you must also update the schema in
        /// `schemas/settings.yaml`.
        #[derive(Debug, DocumentedFields, Serialize, Deserialize, PartialEq)]
        #[serde(default)]
        pub struct Settings {
            $(
                $(#[doc = $doc])+
                pub $field: $ty,
            )+
        }

        impl Default for Settings {
            fn default() -> Self {
                Self {
                    $( $field: $default, )+
                }
            }
        }

        settings!(@overrides { } { }
            $( [ $field: $ty { $(#[doc = $doc])+ } $($cli)? ] )+
        );
    };

    // Overridable `bool` setting: add an `Option<bool>` flag and a merge step. The flag may be
    // passed bare (`--flag` => `true`) or with an explicit value (`--flag=false`).
    (@overrides
        { $($fields:tt)* }
        { $($applies:tt)* }
        [ $field:ident: bool { $(#[doc = $doc:literal])+ } cli ]
        $($rest:tt)*
    ) => {
        settings!(@overrides
            {
                $($fields)*
                $(#[doc = $doc])+
                #[arg(long, value_name = "BOOL", num_args = 0..=1, require_equals = true, default_missing_value = "true")]
                pub $field: Option<bool>,
            }
            { $($applies)* ($field) }
            $($rest)*
        );
    };

    // Overridable `String` setting: add an `Option<String>` flag and a merge step. The flag always
    // takes a value (`--flag value`).
    (@overrides
        { $($fields:tt)* }
        { $($applies:tt)* }
        [ $field:ident: String { $(#[doc = $doc:literal])+ } cli ]
        $($rest:tt)*
    ) => {
        settings!(@overrides
            {
                $($fields)*
                $(#[doc = $doc])+
                #[arg(long)]
                pub $field: Option<String>,
            }
            { $($applies)* ($field) }
            $($rest)*
        );
    };

    // File-only setting: skip it.
    (@overrides
        { $($fields:tt)* }
        { $($applies:tt)* }
        [ $field:ident: $ty:tt { $(#[doc = $doc:literal])+ } ]
        $($rest:tt)*
    ) => {
        settings!(@overrides { $($fields)* } { $($applies)* } $($rest)*);
    };

    // No settings left: emit the overrides struct and the merge method.
    (@overrides { $($fields:tt)* } { $( ($afield:ident) )* }) => {
        /// Settings that can be overridden via command-line arguments.
        ///
        /// A field is `Some` only when the corresponding flag was passed; otherwise the value from
        /// the settings file (or default) is kept.
        #[derive(Debug, Default, Args)]
        pub struct SettingsOverrides {
            $($fields)*
        }

        impl Settings {
            /// Apply command-line `overrides` on top of these settings.
            pub fn apply_overrides(&mut self, overrides: &SettingsOverrides) {
                $(
                    if let Some(value) = &overrides.$afield {
                        self.$afield = value.clone();
                    }
                )*
            }
        }
    };
}

settings! {
    /// The default program log level
    log_level: String = DEFAULT_LOG_LEVEL.to_string();

    /// Results root path to save MUSE2 results. Defaults to `muse2_results`.
    results_root: PathBuf = PathBuf::from("muse2_results");

    /// Results root path to save MUSE2 graph outputs. Defaults to `muse2_graphs`.
    graph_results_root: PathBuf = PathBuf::from("muse2_graphs");

    /// Whether to overwrite output files
    overwrite: bool = false, cli;

    /// Whether to write additional information to CSV files
    debug_model: bool = false, cli;

    /// Whether to copy input files to the output folder
    copy_input_files: bool = true, cli;
}

impl Settings {
    /// Read the contents of a settings file from the global MUSE2 configuration directory.
    ///
    /// If the file is not present or the user has set the `MUSE2_USE_DEFAULT_SETTINGS` environment
    /// variable to 1, then the default settings will be used.
    ///
    /// # Returns
    ///
    /// The program settings as a `Settings` struct or an error if loading fails.
    pub fn load_or_default() -> Result<Settings> {
        if env::var("MUSE2_USE_DEFAULT_SETTINGS").is_ok_and(|v| v == "1") {
            Ok(Settings::default())
        } else {
            Self::from_path_or_default(&get_settings_file_path())
        }
    }

    /// Try to read settings from the specified path, returning `Settings::default()` if it doesn't
    /// exist
    fn from_path_or_default(file_path: &Path) -> Result<Settings> {
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
    use rstest::rstest;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn settings_from_path_or_default_no_file() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join(SETTINGS_FILE_NAME); // NB: doesn't exist
        assert_eq!(
            Settings::from_path_or_default(&file_path).unwrap(),
            Settings::default()
        );
    }

    #[test]
    fn settings_from_path_or_default() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join(SETTINGS_FILE_NAME);

        {
            let mut file = File::create(&file_path).unwrap();
            writeln!(file, "log_level = \"warn\"").unwrap();
        }

        assert_eq!(
            Settings::from_path_or_default(&file_path).unwrap(),
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

    /// A CLI override should always take precedence over the settings-file value, including the
    /// case where the user explicitly opts out of an option that is enabled in the file.
    #[rstest]
    #[case(false, None, false)] // no flag passed: keep the file value
    #[case(true, None, true)] // no flag passed: keep the file value
    #[case(false, Some(true), true)] // --overwrite (=true): opt in
    #[case(true, Some(false), false)] // --overwrite=false: opt out
    fn apply_overrides_takes_precedence(
        #[case] file_value: bool,
        #[case] cli_value: Option<bool>,
        #[case] expected: bool,
    ) {
        let mut settings = Settings {
            overwrite: file_value,
            ..Settings::default()
        };
        settings.apply_overrides(&SettingsOverrides {
            overwrite: cli_value,
            ..Default::default()
        });
        assert_eq!(settings.overwrite, expected);
    }
}
