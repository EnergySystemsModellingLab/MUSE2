//! Code related to the example models and the CLI commands for interacting with them.
use super::{RunOpts, handle_run_command};
use crate::settings::Settings;
use anyhow::{Context, Result, ensure};
use clap::Subcommand;
use include_dir::{Dir, DirEntry, include_dir};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// The directory containing the example models.
const EXAMPLES_DIR: Dir = include_dir!("examples");

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
    for entry in EXAMPLES_DIR.dirs() {
        println!("{}", entry.path().display());
    }
}

/// Handle the `example info` command.
fn handle_example_info_command(name: &str) -> Result<()> {
    let path: PathBuf = [name, "README.txt"].iter().collect();
    let readme = EXAMPLES_DIR
        .get_file(path)
        .context("Example not found.")?
        .contents_utf8()
        .expect("README.txt is not UTF-8 encoded");

    print!("{readme}");

    Ok(())
}

/// Handle the `example extract` command
fn handle_example_extract_command(name: &str, dest: Option<&Path>) -> Result<()> {
    let dest = dest.unwrap_or(Path::new(name));
    extract_example(name, dest)
}

/// Extract the specified example to a new directory
fn extract_example(name: &str, new_path: &Path) -> Result<()> {
    // Find the subdirectory in EXAMPLES_DIR whose name matches `name`.
    let sub_dir = EXAMPLES_DIR.get_dir(name).context("Example not found.")?;

    ensure!(
        !new_path.exists(),
        "Destination directory {} already exists",
        new_path.display()
    );

    // Copy the contents of the subdirectory to the destination
    fs::create_dir(new_path)?;
    for entry in sub_dir.entries() {
        match entry {
            DirEntry::Dir(_) => panic!("Subdirectories in examples not supported"),
            DirEntry::File(f) => {
                let file_name = f.path().file_name().unwrap();
                let file_path = new_path.join(file_name);
                fs::write(&file_path, f.contents())?;
            }
        }
    }

    Ok(())
}

/// Handle the `example run` command.
pub fn handle_example_run_command(
    name: &str,
    opts: &RunOpts,
    settings: Option<Settings>,
) -> Result<()> {
    let temp_dir = TempDir::new().context("Failed to create temporary directory.")?;
    let model_path = temp_dir.path().join(name);
    extract_example(name, &model_path)?;
    handle_run_command(&model_path, opts, settings)
}
