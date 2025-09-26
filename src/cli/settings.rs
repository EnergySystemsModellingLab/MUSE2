//! Code related to CLI interface for managing the settings file
use crate::settings::{Settings, get_settings_file_path};
use anyhow::{Context, Result};
use clap::Subcommand;
use std::fs;
use std::path::Path;

/// Subcommands for settings
#[derive(Subcommand)]
pub enum SettingsSubcommands {
    /// Edit the program settings file
    Edit,
    /// Get the path to where the settings file is read from
    Path,
    /// Write the contents of a placeholder `settings.toml` to the console
    DumpDefault,
}

impl SettingsSubcommands {
    /// Execute the supplied settings subcommand
    pub fn execute(self) -> Result<()> {
        match self {
            Self::Edit => handle_edit_command()?,
            Self::Path => handle_path_command(),
            Self::DumpDefault => handle_dump_default_command(),
        }

        Ok(())
    }
}

/// Get the path to the settings file, creating it if it doesn't exist
fn ensure_settings_file_exists(file_path: &Path) -> Result<()> {
    if file_path.is_file() {
        // File already exists
        return Ok(());
    }

    if let Some(dir_path) = file_path.parent() {
        // Create parent directory
        fs::create_dir_all(dir_path)
            .with_context(|| format!("Failed to create directory: {}", dir_path.display()))?;
    }

    // Create placeholder settings file
    fs::write(file_path, Settings::default_file_contents())?;

    Ok(())
}

/// Handle the `edit` command
fn handle_edit_command() -> Result<()> {
    let file_path = get_settings_file_path();
    ensure_settings_file_exists(&file_path)?;

    // Allow user to edit in text editor
    println!("Opening settings file for editing: {}", file_path.display());
    edit::edit_file(&file_path)?;

    Ok(())
}

/// Handle the `path` command
fn handle_path_command() {
    println!("{}", get_settings_file_path().display());
}

/// Handle the `dump-default` command
fn handle_dump_default_command() {
    print!("{}", Settings::default_file_contents());
}
