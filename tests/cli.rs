//! Integration tests for CLI commands.
use rstest::rstest;

use itertools::Itertools;
use tempfile::tempdir;

mod common;
use common::{assert_muse2_runs, get_muse2_stdout};

const EXAMPLE_NAME: &str = "simple";
const MODEL_DIR: &str = "examples/simple";
const PATCH_EXAMPLE_NAME: &str = "simple_divisible";

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

/// Test the `example list` command
#[rstest]
#[case(true)]
#[case(false)]
fn check_example_list_command(#[case] patch: bool) {
    let mut args = vec!["example", "list"];
    if patch {
        args.push("--patch");
    }

    let stdout = get_muse2_stdout(&args);
    let lines = stdout.split('\n').collect_vec();
    assert!(lines.first().is_some_and(|s| !s.is_empty()));
    assert!(lines.last().is_some_and(|s| s.is_empty()));
}

/// Test the `example info` command
#[test]
fn check_example_info_command() {
    assert!(!get_muse2_stdout(&["example", "info", EXAMPLE_NAME]).is_empty());
}

/// Test the `example extract`
#[rstest]
#[case(true)]
#[case(false)]
fn check_example_extract_command(#[case] patch: bool) {
    let tmp = tempdir().unwrap();
    let output_dir = tmp.path().join("out");
    let output_dir_str = output_dir.to_string_lossy();
    let mut args = vec!["example", "extract"];
    if patch {
        args.extend(["--patch", PATCH_EXAMPLE_NAME]);
    } else {
        args.push(EXAMPLE_NAME);
    }
    args.push(&output_dir_str);

    assert_muse2_runs(&args);
    assert!(
        output_dir.read_dir().unwrap().next().is_some(),
        "Output dir is empty"
    );
}

// NB: `example run` is covered by regression tests
