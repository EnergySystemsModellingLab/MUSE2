//! Code related to the example models and the CLI commands for interacting with them.
use super::{RunOpts, handle_run_command};
use crate::example::patches::{get_patch_names, get_patches};
use crate::example::{Example, get_example_names};
use crate::patch::ModelPatch;
use crate::settings::Settings;
use anyhow::{Context, Result};
use clap::Subcommand;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// The available subcommands for managing example models.
#[derive(Subcommand)]
pub enum ExampleSubcommands {
    /// List available examples.
    List {
        /// Whether to list patched models.
        #[arg(long, hide = true)]
        patch: bool,
    },
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
        /// Whether the model to extract is a patched model.
        #[arg(long, hide = true)]
        patch: bool,
    },
    /// Run an example.
    Run {
        /// The name of the example to run.
        name: String,
        /// Whether the model to run is a patched model.
        #[arg(long, hide = true)]
        patch: bool,
        /// Other run options
        #[command(flatten)]
        opts: RunOpts,
    },
}

impl ExampleSubcommands {
    /// Execute the supplied example subcommand
    pub fn execute(self) -> Result<()> {
        match self {
            Self::List { patch } => handle_example_list_command(patch),
            Self::Info { name } => handle_example_info_command(&name)?,
            Self::Extract {
                name,
                patch,
                new_path,
            } => handle_example_extract_command(&name, new_path.as_deref(), patch)?,
            Self::Run { name, patch, opts } => {
                handle_example_run_command(&name, patch, &opts, None)?;
            }
        }

        Ok(())
    }
}

/// Handle the `example list` command.
fn handle_example_list_command(patch: bool) {
    if patch {
        for name in get_patch_names() {
            println!("{name}");
        }
    } else {
        for name in get_example_names() {
            println!("{name}");
        }
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
fn handle_example_extract_command(name: &str, dest: Option<&Path>, patch: bool) -> Result<()> {
    extract_example(name, patch, dest.unwrap_or(Path::new(name)))
}

/// Extract the specified example to a new directory.
///
/// If `patch` is `true`, then the corresponding patched example will be extracted.
fn extract_example(name: &str, patch: bool, dest: &Path) -> Result<()> {
    if patch {
        let patches = get_patches(name)?;

        // NB: All patched models are based on `simple`, for now
        let example = Example::from_name("simple").unwrap();

        // First extract the example to a temp dir
        let example_tmp = TempDir::new().context("Failed to create temporary directory")?;
        let example_path = example_tmp.path().join(name);
        example
            .extract(&example_path)
            .context("Could not extract example")?;

        // Patch example and put contents in dest
        fs::create_dir(dest).context("Could not create output directory")?;
        ModelPatch::new(example_path)
            .with_file_patches(patches.to_owned())
            .build(dest)
            .context("Failed to patch example")
    } else {
        // Otherwise it's just a regular example
        let example = Example::from_name(name)?;
        example.extract(dest)
    }
}

/// Handle the `example run` command.
pub fn handle_example_run_command(
    name: &str,
    patch: bool,
    opts: &RunOpts,
    settings: Option<Settings>,
) -> Result<()> {
    let temp_dir = TempDir::new().context("Failed to create temporary directory")?;
    let model_path = temp_dir.path().join(name);
    extract_example(name, patch, &model_path)?;
    handle_run_command(&model_path, opts, settings)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    fn assert_dir_non_empty(path: &Path) {
        assert!(
            path.read_dir().unwrap().next().is_some(),
            "Directory is empty"
        );
    }

    #[rstest]
    #[case("muse1_default", false)]
    #[case("simple_divisible", true)]
    fn check_extract_example(#[case] name: &str, #[case] patch: bool) {
        let tmp = TempDir::new().unwrap();
        let dest = tmp.path().join("out");
        extract_example(name, patch, &dest).unwrap();
        assert_dir_non_empty(&dest);
    }
}
