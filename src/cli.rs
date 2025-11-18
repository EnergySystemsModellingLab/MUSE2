//! The command line interface for the simulation.
use crate::graph::save_commodity_graphs_for_model;
use crate::input::{load_commodity_graphs, load_model};
use crate::log;
use crate::output::{create_output_directory, get_graphs_dir, get_output_dir};
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

/// Options for the `run` command
#[derive(Args)]
pub struct RunOpts {
    /// Directory for output files
    #[arg(short, long)]
    pub output_dir: Option<PathBuf>,
    /// Whether to overwrite the output directory if it already exists
    #[arg(long)]
    pub overwrite: bool,
    /// Whether to write additional information to CSV files
    #[arg(long, value_name = "BOOL", num_args = 0..=1, default_missing_value = "true")]
    pub debug_model: Option<bool>,
}

/// Options for the `graph` command
#[derive(Args)]
pub struct GraphOpts {
    /// Directory for graph files
    #[arg(short, long)]
    pub output_dir: Option<PathBuf>,
    /// Whether to overwrite the output directory if it already exists
    #[arg(long)]
    pub overwrite: bool,
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
    /// Build and output commodity flow graphs for a model.
    SaveGraphs {
        /// The path to the model directory.
        model_dir: PathBuf,
        /// Other options
        #[command(flatten)]
        opts: GraphOpts,
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
            Self::SaveGraphs { model_dir, opts } => {
                handle_save_graphs_command(&model_dir, &opts, None)
            }
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

    if let Some(command) = cli.command {
        command.execute()?;
    } else {
        // No command provided. Show help.
        Cli::command().print_long_help()?;
    }

    Ok(())
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
    if let Some(opt) = opts.debug_model {
        settings.debug_model = opt;
    }
    if opts.overwrite {
        settings.overwrite = true;
    }

    // Get path to output folder
    let pathbuf: PathBuf;
    let output_path = if let Some(p) = opts.output_dir.as_deref() {
        p
    } else {
        pathbuf = get_output_dir(model_path, settings.results_root)?;
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

    info!("Starting MUSE2 v{}", env!("CARGO_PKG_VERSION"));

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

/// Handle the `save-graphs` command.
pub fn handle_save_graphs_command(
    model_path: &Path,
    opts: &GraphOpts,
    settings: Option<Settings>,
) -> Result<()> {
    // Load program settings, if not provided
    let mut settings = if let Some(settings) = settings {
        settings
    } else {
        Settings::load().context("Failed to load settings.")?
    };

    if opts.overwrite {
        settings.overwrite = true;
    }

    // Get path to output folder
    let pathbuf: PathBuf;
    let output_path = if let Some(p) = opts.output_dir.as_deref() {
        p
    } else {
        pathbuf = get_graphs_dir(model_path, settings.graph_results_root)?;
        &pathbuf
    };

    let overwrite =
        create_output_directory(output_path, settings.overwrite).with_context(|| {
            format!(
                "Failed to create graphs directory: {}",
                output_path.display()
            )
        })?;

    // Initialise program logger (we won't save log files when running this command)
    log::init(&settings.log_level, None).context("Failed to initialise logging.")?;

    // NB: We have to wait until the logger is initialised to display this warning
    if overwrite {
        warn!("Graphs directory will be overwritten");
    }

    // Load commodity flow graphs and save to file
    let commodity_graphs = load_commodity_graphs(model_path).context("Failed to build graphs.")?;
    save_commodity_graphs_for_model(&commodity_graphs, output_path)?;
    info!("Graphs saved to: {}", output_path.display());

    Ok(())
}
