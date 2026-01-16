//! Integration tests for the `run` command.
use tempfile::tempdir;

mod common;
use common::assert_muse2_runs;

const MODEL_DIR: &str = "examples/simple";

/// Test the `run` command
#[test]
fn check_run_command() {
    // Save results to non-existent directory to check that directory creation works
    let tempdir = tempdir().unwrap();
    let output_dir = tempdir.path().join("results");
    assert_muse2_runs(&[
        "run",
        MODEL_DIR,
        "--output-dir",
        &output_dir.to_string_lossy(),
    ]);
}

/// Test the `graph` command
#[test]
fn check_save_graphs_command() {
    // Save results to non-existent directory to check that directory creation works
    let tempdir = tempdir().unwrap();
    let output_dir = tempdir.path().join("graphs");
    assert_muse2_runs(&[
        "save-graphs",
        MODEL_DIR,
        "--output-dir",
        &output_dir.to_string_lossy(),
    ]);

    // Check that at least one DOT file was created
    assert!(output_dir.join("GBR_2020.dot").exists());
}

/// Test the `validate` command
#[test]
fn check_validate_command() {
    assert_muse2_runs(&["validate", MODEL_DIR]);
}
