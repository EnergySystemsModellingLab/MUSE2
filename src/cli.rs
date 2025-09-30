//! The command line interface for the simulation.
use crate::input::load_model;
use crate::log;
use crate::output::{create_output_directory, get_output_dir};
use crate::settings::Settings;
use ::log::{info, warn};
use anyhow::{Context, Result};
use clap::{Args, CommandFactory, Parser, Subcommand};
use std::path::{Path, PathBuf};

pub mod example;
use example::ExampleSubcommands;

pub mod settings;
use settings::SettingsSubcommands;

/// The command line interface for the simulation.
#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// The available commands.
    #[command(subcommand)]
    command: Option<Commands>,
    /// Flag to provide the CLI docs as markdown
    #[arg(long, hide = true)]
    markdown_help: bool,
}

/// Options for the run command
#[derive(Args)]
pub struct RunOpts {
    /// Directory for output files
    #[arg(short, long)]
    pub output_dir: Option<PathBuf>,
    /// Whether to overwrite the output directory if it already exists
    #[arg(long)]
    pub overwrite: bool,
    /// Whether to write additional information to CSV files
    #[arg(long)]
    pub debug_model: bool,
}

/// The available commands.
#[derive(Subcommand)]
enum Commands {
    /// Run a simulation model.
    Run {
        /// Path to the model directory.
        model_dir: PathBuf,
        /// Other run options
        #[command(flatten)]
        opts: RunOpts,
    },
    /// Manage example models.
    Example {
        /// The available subcommands for managing example models.
        #[command(subcommand)]
        subcommand: ExampleSubcommands,
    },
    /// Validate a model.
    Validate {
        /// The path to the model directory.
        model_dir: PathBuf,
    },
    /// Manage settings file.
    Settings {
        /// The subcommands for managing the settings file.
        #[command(subcommand)]
        subcommand: SettingsSubcommands,
    },
}

impl Commands {
    /// Execute the supplied CLI command
    fn execute(self) -> Result<()> {
        match self {
            Self::Run { model_dir, opts } => handle_run_command(&model_dir, &opts, None),
            Self::Example { subcommand } => subcommand.execute(),
            Self::Validate { model_dir } => handle_validate_command(&model_dir, None),
            Self::Settings { subcommand } => subcommand.execute(),
        }
    }
}

/// Parse CLI arguments and start MUSE2
pub fn run_cli() -> Result<()> {
    let cli = Cli::parse();

    // Invoked as: `$ muse2 --markdown-help`
    if cli.markdown_help {
        clap_markdown::print_help_markdown::<Cli>();
        return Ok(());
    }

    let Some(command) = cli.command else {
        // Output program help in markdown format
        let help_str = Cli::command().render_long_help().to_string();
        println!("{help_str}");
        return Ok(());
    };

    command.execute()
}

/// Handle the `run` command.
pub fn handle_run_command(
    model_path: &Path,
    opts: &RunOpts,
    settings: Option<Settings>,
) -> Result<()> {
    // Load program settings, if not provided
    let mut settings = if let Some(settings) = settings {
        settings
    } else {
        Settings::load().context("Failed to load settings.")?
    };

    // These settings can be overridden by command-line arguments
    if opts.debug_model {
        settings.debug_model = true;
    }
    if opts.overwrite {
        settings.overwrite = true;
    }

    // Get path to output folder
    let pathbuf: PathBuf;
    let output_path = if let Some(p) = opts.output_dir.as_deref() {
        p
    } else {
        pathbuf = get_output_dir(model_path)?;
        &pathbuf
    };

    let overwrite =
        create_output_directory(output_path, settings.overwrite).with_context(|| {
            format!(
                "Failed to create output directory: {}",
                output_path.display()
            )
        })?;

    // Initialise program logger
    log::init(&settings.log_level, Some(output_path)).context("Failed to initialise logging.")?;

    // Load the model to run
    let (model, assets) = load_model(model_path).context("Failed to load model.")?;
    info!("Loaded model from {}", model_path.display());
    info!("Output folder: {}", output_path.display());

    // NB: We have to wait until the logger is initialised to display this warning
    if overwrite {
        warn!("Output folder will be overwritten");
    }

    // Run the simulation
    crate::simulation::run(&model, assets, output_path, settings.debug_model)?;
    info!("Simulation complete!");

    Ok(())
}

/// Handle the `validate` command.
pub fn handle_validate_command(model_path: &Path, settings: Option<Settings>) -> Result<()> {
    // Load program settings, if not provided
    let settings = if let Some(settings) = settings {
        settings
    } else {
        Settings::load().context("Failed to load settings.")?
    };

    // Initialise program logger (we won't save log files when running the validate command)
    log::init(&settings.log_level, None).context("Failed to initialise logging.")?;

    // Load/validate the model
    load_model(model_path).context("Failed to validate model.")?;
    info!("Model validation successful!");

    Ok(())
}
