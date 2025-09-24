//! Code related to CLI interface for managing the settings file
use crate::settings::{Settings, get_settings_file_path};
use anyhow::Result;
use clap::Subcommand;

/// Subcommands for settings
#[derive(Subcommand)]
pub enum SettingsSubcommands {
    /// Get the path to where the settings file is read from
    Path,
    /// Write the contents of a placeholder `settings.toml` to the console
    DumpDefault,
}

impl SettingsSubcommands {
    /// Execute the supplied settings subcommand
    pub fn execute(self) -> Result<()> {
        match self {
            Self::Path => handle_path_command(),
            Self::DumpDefault => handle_dump_default_command(),
        }

        Ok(())
    }
}

/// Handle the `path` command
fn handle_path_command() {
    println!("{}", get_settings_file_path().display());
}

/// Handle the `dump-default` command
fn handle_dump_default_command() {
    print!("{}", Settings::default_file_contents());
}
