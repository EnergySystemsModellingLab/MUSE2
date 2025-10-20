//! The `log` module provides initialisation and configuration of the application's logging system.
//!
//! This module sets up logging with various levels (error, warn, info, debug, trace) and optional
//! colourisation based on terminal support. It also allows configuration of the log level through
//! environment variables.
use anyhow::{Result, bail};
use chrono::Local;
use fern::colors::{Color, ColoredLevelConfig};
use fern::{Dispatch, FormatCallback};
use log::{LevelFilter, Record};
use std::env;
use std::fmt::{Arguments, Display};
use std::fs::OpenOptions;
use std::io::IsTerminal;
use std::path::Path;
use std::sync::OnceLock;

/// A flag indicating whether the logger has been initialised
static LOGGER_INIT: OnceLock<()> = OnceLock::new();

/// The default log level for the program.
///
/// Used as a fallback if the user hasn't specified something else with the `MUSE2_LOG_LEVEL`
/// environment variable or the settings.toml file.
pub const DEFAULT_LOG_LEVEL: &str = "info";

/// The file name for the log file containing messages about the ordinary operation of MUSE2
const LOG_INFO_FILE_NAME: &str = "muse2_info.log";

/// The file name for the log file containing warnings and error messages
const LOG_ERROR_FILE_NAME: &str = "muse2_error.log";

/// Whether the program logger has been initialised
pub fn is_logger_initialised() -> bool {
    LOGGER_INIT.get().is_some()
}

/// Initialise the program logger using the `fern` logging library with colourised output.
///
/// The user can specify their preferred logging level via the `settings.toml` file (defaulting to
/// `info` if not present) or with the `MUSE2_LOG_LEVEL` environment variable. If both are provided,
/// the environment variable takes precedence.
///
/// Possible log level options are:
///
/// * `error`
/// * `warn`
/// * `info`
/// * `debug`
/// * `trace`
///
/// # Arguments
///
/// * `log_level_from_settings`: The log level specified in `settings.toml`
/// * `log_file_path`: The location to save log files (if Some, log files will be created)
pub fn init(log_level_from_settings: &str, log_file_path: Option<&Path>) -> Result<()> {
    // Retrieve the log level from the environment variable or settings, or use the default
    let log_level =
        env::var("MUSE2_LOG_LEVEL").unwrap_or_else(|_| log_level_from_settings.to_string());

    // Convert the log level string to a log::LevelFilter
    let log_level = match log_level.to_lowercase().as_str() {
        "off" => LevelFilter::Off,
        "error" => LevelFilter::Error,
        "warn" => LevelFilter::Warn,
        "info" => LevelFilter::Info,
        "debug" => LevelFilter::Debug,
        "trace" => LevelFilter::Trace,
        unknown => bail!("Unknown log level: {unknown}"),
    };

    // Set up colours for log levels
    let colours = ColoredLevelConfig::new()
        .error(Color::Red)
        .warn(Color::Yellow)
        .info(Color::Green)
        .debug(Color::Blue)
        .trace(Color::Magenta);

    // Automatically apply colours only if the output is a terminal
    let use_colour_stdout = std::io::stdout().is_terminal();
    let use_colour_stderr = std::io::stderr().is_terminal();

    // Create log files if log file path is available
    let (info_log_file, err_log_file) = if let Some(log_file_path) = log_file_path {
        let new_log_file = |file_name| {
            OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(log_file_path.join(file_name))
        };
        (
            Some(new_log_file(LOG_INFO_FILE_NAME)?),
            Some(new_log_file(LOG_ERROR_FILE_NAME)?),
        )
    } else {
        (None, None)
    };

    // Configure the logger
    let mut dispatch = Dispatch::new()
        .chain(
            // Write non-error messages to stdout
            Dispatch::new()
                .filter(|metadata| metadata.level() > LevelFilter::Warn)
                .format(move |out, message, record| {
                    write_log_colour(out, message, record, use_colour_stdout, &colours);
                })
                .level(log_level)
                .chain(std::io::stdout()),
        )
        .chain(
            // Write error messages to stderr
            Dispatch::new()
                .format(move |out, message, record| {
                    write_log_colour(out, message, record, use_colour_stderr, &colours);
                })
                .level(log_level.min(LevelFilter::Warn))
                .chain(std::io::stderr()),
        );

    // Add log file chains if log files were created
    if let Some(info_log_file) = info_log_file {
        dispatch = dispatch.chain(
            // Write non-error messages to log file
            Dispatch::new()
                .filter(|metadata| metadata.level() > LevelFilter::Warn)
                .format(write_log_plain)
                .level(log_level.max(LevelFilter::Info))
                .chain(info_log_file),
        );
    }

    if let Some(err_log_file) = err_log_file {
        dispatch = dispatch.chain(
            // Write error messages to a different log file
            Dispatch::new()
                .format(write_log_plain)
                .level(LevelFilter::Warn)
                .chain(err_log_file),
        );
    }

    // Apply the logger configuration
    dispatch.apply().expect("Logger already initialised");

    // Set a flag to indicate that the logger has been initialised
    LOGGER_INIT.set(()).unwrap();

    Ok(())
}

/// Write to the log in the format we want for MUSE2
fn write_log<T: Display>(out: FormatCallback, level: T, target: &str, message: &Arguments) {
    let timestamp = Local::now().format("%H:%M:%S");

    out.finish(format_args!("[{timestamp} {level} {target}] {message}"));
}

/// Write to the log with no colours
fn write_log_plain(out: FormatCallback, message: &Arguments, record: &Record) {
    write_log(out, record.level(), record.target(), message);
}

/// Write to the log with optional colours
fn write_log_colour(
    out: FormatCallback,
    message: &Arguments,
    record: &Record,
    use_colour: bool,
    colours: &ColoredLevelConfig,
) {
    // Format output with or without colour based on `use_colour`
    if use_colour {
        write_log(out, colours.color(record.level()), record.target(), message);
    } else {
        write_log_plain(out, message, record);
    }
}
