//! Integration tests for the `run` command.
use muse2::cli::{RunOpts, handle_run_command};
use muse2::log::is_logger_initialised;
use muse2::settings::Settings;
use std::path::PathBuf;
use tempfile::tempdir;

/// Get the path to the example model.
fn get_model_dir() -> PathBuf {
    PathBuf::from("examples/simple")
}

/// An integration test for the `run` command.
///
/// We also check that the logger is initialised after it is run.
#[test]
fn handle_run_command_works() {
    unsafe { std::env::set_var("MUSE2_LOG_LEVEL", "off") };

    assert!(!is_logger_initialised());

    // Save results to non-existent directory to check that directory creation works
    let tempdir = tempdir().unwrap();
    let output_dir = tempdir.path().join("results");
    let opts = RunOpts {
        output_dir: Some(output_dir),
        overwrite: false,
        debug_model: false,
    };
    handle_run_command(&get_model_dir(), &opts, Some(Settings::default())).unwrap();

    assert!(is_logger_initialised());
}
