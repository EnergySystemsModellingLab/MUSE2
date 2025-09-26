//! Code related to CLI interface for managing the settings file
use crate::settings::Settings;
use anyhow::Result;
use clap::Subcommand;

/// Subcommands for settings
#[derive(Subcommand)]
pub enum SettingsSubcommands {
    /// Write the contents of a placeholder `settings.toml` to the console
    DumpDefault,
}

impl SettingsSubcommands {
    /// Execute the supplied settings subcommand
    pub fn execute(self) -> Result<()> {
        match self {
            Self::DumpDefault => handle_dump_default_command(),
        }

        Ok(())
    }
}

/// Handle the `dump-default` command
fn handle_dump_default_command() {
    print!("{}", Settings::default_file_contents());
}
