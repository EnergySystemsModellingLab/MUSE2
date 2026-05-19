//! Common code for running regression tests.
use anyhow::Result;
use float_cmp::approx_eq;
use itertools::Itertools;
use similar::{ChangeTag, TextDiff};
use std::env;
use std::fmt::Write as _;
use std::fs::{File, read_dir};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use tempfile::{TempDir, tempdir};

mod common;
use common::*;

// ------ BEGIN: regression tests ------

// We only check the debug files for the `simple` example at present
define_regression_test_with_debug_files!(simple);

// Other example models
define_regression_test!(missing_commodity);
define_regression_test!(muse1_default);
define_regression_test!(two_outputs);
define_regression_test!(circularity);
define_regression_test!(two_regions);

// Patched examples
define_regression_test_with_patches!(simple_divisible);
define_regression_test_with_patches!(simple_npv);
define_regression_test_with_patches!(simple_marginal);
define_regression_test_with_patches!(simple_marginal_average);
define_regression_test_with_patches!(simple_full);
define_regression_test_with_patches!(simple_full_average);
define_regression_test_with_patches!(simple_ironing_out);

// ------  END: regression tests  ------

/// The tolerance when comparing floating-point values in CSV files
const FLOAT_CMP_TOLERANCE: f64 = 1e-10;

/// Run a regression test for the given example with optional extra arguments to `muse2 run`.
fn run_regression_test(example: &str, extra_args: &[&str]) {
    // Allow user to set output dir for regression tests so they can examine results. This is
    // principally intended for use by CI.
    let tmp: TempDir;
    let output_dir = if let Ok(dir) = env::var("MUSE2_TEST_OUTPUT_DIR") {
        [&dir, example].iter().collect()
    } else {
        tmp = tempdir().unwrap();
        tmp.path().to_path_buf()
    };

    // Invoke muse2
    let output_dir_str = output_dir.to_string_lossy();
    let mut args = vec!["example", "run", example, "--output-dir", &output_dir_str];
    args.extend(extra_args);
    assert_muse2_runs(&args);

    // Check that the output files match (approximately)
    let test_data_dir = PathBuf::from(format!("tests/data/{example}"));
    compare_output_dirs(
        &output_dir,
        &test_data_dir,
        extra_args.contains(&"--debug-model"),
    );
}

fn compare_output_dirs(cur_output_dir1: &Path, test_data_dir: &Path, debug_model: bool) {
    let mut file_names1 = get_csv_file_names(cur_output_dir1);
    if !debug_model {
        file_names1.retain(|name| !name.starts_with("debug_"));
    }
    let file_names2 = get_csv_file_names(test_data_dir);

    // Check that output files haven't been added/removed
    assert!(file_names1 == file_names2);

    let mut errors = Vec::new();
    for file_name in file_names1 {
        compare_lines(cur_output_dir1, test_data_dir, &file_name, &mut errors);
    }

    assert!(
        errors.is_empty(),
        "The following errors occurred:\n  * {}",
        errors.join("\n  * ")
    );
}

fn compare_lines(
    output_dir1: &Path,
    output_dir2: &Path,
    file_name: &str,
    errors: &mut Vec<String>,
) {
    let lines1 = read_lines(&output_dir1.join(file_name));
    let lines2 = read_lines(&output_dir2.join(file_name));

    // check number of lines equal
    let mut has_mismatch = lines1.len() != lines2.len();

    // check each line is the same within numerical tolerance
    if !has_mismatch {
        has_mismatch = lines1
            .iter()
            .zip(&lines2)
            .any(|(line1, line2)| !compare_line(line1, line2));
    }

    if has_mismatch {
        let diff = render_diff(&lines1, &lines2);
        errors.push(format!("{file_name}: output differs\n{diff}"));
    }
}

fn compare_line(line1: &str, line2: &str) -> bool {
    let fields1 = line1.split(',').collect_vec();
    let fields2 = line2.split(',').collect_vec();
    if fields1.len() != fields2.len() {
        return false;
    }

    // Check every field matches
    fields1.into_iter().zip(fields2).all(|(f1, f2)| {
        // First try to compare fields as floating-point values, falling back on string comparison
        try_compare_floats(f1, f2).unwrap_or_else(|| f1 == f2)
    })
}

/// Given two lists of lines which don't match, render a diff showing
/// which lines were added/removed, with line numbers from the original files.
fn render_diff(lines1: &[String], lines2: &[String]) -> String {
    let text1 = lines1.join("\n");
    let text2 = lines2.join("\n");
    let diff = TextDiff::from_lines(&text1, &text2);

    let mut out = String::new();
    let mut line_num1 = 1;
    let mut line_num2 = 1;
    for change in diff.iter_all_changes() {
        let line = change.to_string();
        let line = line.trim_end_matches('\n');
        match change.tag() {
            ChangeTag::Delete => {
                let _ = writeln!(out, "-L{line_num1}: {line}");
                line_num1 += 1;
            }
            ChangeTag::Insert => {
                let _ = writeln!(out, "+L{line_num2}: {line}");
                line_num2 += 1;
            }
            ChangeTag::Equal => {
                line_num1 += 1;
                line_num2 += 1;
            }
        }
    }

    out
}

/// Parse a string into an `f64`, returning `None` if parsing fails or value is infinite/NaN
fn parse_finite(s: &str) -> Option<f64> {
    s.parse().ok().filter(|f: &f64| f.is_finite())
}

fn try_compare_floats(s1: &str, s2: &str) -> Option<bool> {
    let float1 = parse_finite(s1)?;
    let float2 = parse_finite(s2)?;

    Some(approx_eq!(
        f64,
        float1,
        float2,
        epsilon = FLOAT_CMP_TOLERANCE
    ))
}

/// Get the names of CSV files expected to appear in the given folder
fn get_csv_file_names(dir_path: &Path) -> Vec<String> {
    let entries = read_dir(dir_path).unwrap();
    let mut file_names = Vec::new();
    for entry in entries {
        let file_name = entry.unwrap().file_name();
        let file_name = file_name.to_str().unwrap();
        if Path::new(file_name)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("csv"))
        {
            file_names.push(file_name.to_string());
        }
    }

    file_names.sort();
    file_names
}

// Read all lines from a file into a `Vec`
fn read_lines(path: &Path) -> Vec<String> {
    let file1 = File::open(path).unwrap();
    BufReader::new(file1)
        .lines()
        .map_while(Result::ok)
        .collect()
}
