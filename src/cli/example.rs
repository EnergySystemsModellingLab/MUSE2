//! Code related to the example models and the CLI commands for interacting with them.
use super::{RunOpts, handle_run_command};
use crate::example::{Example, get_example_names};
use crate::settings::Settings;
use anyhow::{Context, Result};
use clap::Subcommand;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// The available subcommands for managing example models.
#[derive(Subcommand)]
pub enum ExampleSubcommands {
    /// List available examples.
    List,
    /// Provide information about the specified example.
    Info {
        /// The name of the example.
        name: String,
    },
    /// Extract an example model configuration to a new directory.
    Extract {
        /// The name of the example to extract.
        name: String,
        /// The destination folder for the example.
        new_path: Option<PathBuf>,
    },
    /// Run an example.
    Run {
        /// The name of the example to run.
        name: String,
        /// Other run options
        #[command(flatten)]
        opts: RunOpts,
    },
}

impl ExampleSubcommands {
    /// Execute the supplied example subcommand
    pub fn execute(self) -> Result<()> {
        match self {
            Self::List => handle_example_list_command(),
            Self::Info { name } => handle_example_info_command(&name)?,
            Self::Extract {
                name,
                new_path: dest,
            } => handle_example_extract_command(&name, dest.as_deref())?,
            Self::Run { name, opts } => handle_example_run_command(&name, &opts, None)?,
        }

        Ok(())
    }
}

/// Handle the `example list` command.
fn handle_example_list_command() {
    for name in get_example_names() {
        println!("{name}");
    }
}

/// Handle the `example info` command.
fn handle_example_info_command(name: &str) -> Result<()> {
    // If we can't load it, it's a bug, hence why we panic
    let info = Example::from_name(name)?
        .get_readme()
        .unwrap_or_else(|_| panic!("Could not load README.txt for '{name}' example"));
    print!("{info}");

    Ok(())
}

/// Handle the `example extract` command
fn handle_example_extract_command(name: &str, dest: Option<&Path>) -> Result<()> {
    let example = Example::from_name(name)?;
    let dest = dest.unwrap_or(Path::new(name));
    example.extract(dest)
}

/// Handle the `example run` command.
pub fn handle_example_run_command(
    name: &str,
    opts: &RunOpts,
    settings: Option<Settings>,
) -> Result<()> {
    let example = Example::from_name(name)?;
    let temp_dir = TempDir::new().context("Failed to create temporary directory.")?;
    let model_path = temp_dir.path().join(name);
    example.extract(&model_path)?;
    handle_run_command(&model_path, opts, settings)
}
