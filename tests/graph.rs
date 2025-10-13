//! Integration tests for the `graph` command.
use muse2::cli::{GraphOpts, handle_save_graphs_command};
use muse2::log::is_logger_initialised;
use muse2::settings::Settings;
use std::path::PathBuf;
use tempfile::tempdir;

/// Get the path to the example model.
fn get_model_dir() -> PathBuf {
    PathBuf::from("examples/simple")
}

/// An integration test for the `graph` command.
///
/// We also check that the logger is initialised after it is run.
#[test]
fn test_handle_graph_command() {
    unsafe { std::env::set_var("MUSE2_LOG_LEVEL", "off") };

    assert!(!is_logger_initialised());

    // Save results to non-existent directory to check that directory creation works
    let tempdir = tempdir().unwrap();
    let output_dir = tempdir.path().join("graphs");
    let opts = GraphOpts {
        output_dir: Some(output_dir.clone()),
        overwrite: false,
    };
    handle_save_graphs_command(&get_model_dir(), &opts, Some(Settings::default())).unwrap();
    assert!(is_logger_initialised());

    // Check that at least one DOT file was created
    assert!(output_dir.join("GBR_2020.dot").exists());
}
