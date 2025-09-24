//! Common code for running regression tests.
use float_cmp::approx_eq;
use itertools::Itertools;
use muse2::cli::RunOpts;
use muse2::cli::example::handle_example_run_command;
use muse2::settings::Settings;
use std::fs::{File, read_dir};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use tempfile::tempdir;

const FLOAT_CMP_TOLERANCE: f64 = 1e-10;

// The two functions below give spurious warnings about being unused because of the multiple `mod
// regression` declarations in different test files, so we suppress the warnings manually

/// Run a regression test for an example model
#[allow(dead_code)]
pub fn run_regression_test(example_name: &str) {
    run_regression_test_debug_opt(example_name, false);
}

/// Run a regression test for an example model
#[allow(dead_code)]
pub fn run_regression_test_with_debug_files(example_name: &str) {
    run_regression_test_debug_opt(example_name, true);
}

fn run_regression_test_debug_opt(example_name: &str, debug_model: bool) {
    unsafe { std::env::set_var("MUSE2_LOG_LEVEL", "off") };

    let tempdir = tempdir().unwrap();
    let opts = RunOpts {
        output_dir: Some(tempdir.path().to_path_buf()),
        debug_model,
    };
    let output_dir = tempdir.path();
    handle_example_run_command(example_name, &opts, Some(Settings::default())).unwrap();

    let test_data_dir = PathBuf::from(format!("tests/data/{example_name}"));
    compare_output_dirs(output_dir, &test_data_dir);
}

fn compare_output_dirs(output_dir1: &Path, output_dir2: &Path) {
    let file_names1 = get_csv_file_names(output_dir1);
    let file_names2 = get_csv_file_names(output_dir2);

    // Check that output files haven't been added/removed
    assert!(file_names1 == file_names2);

    let mut errors = Vec::new();
    for file_name in file_names1 {
        compare_lines(output_dir1, output_dir2, &file_name, &mut errors);
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

    // Check for different number of lines
    if lines1.len() != lines2.len() {
        errors.push(format!(
            "{}: Different number of lines: {} vs {}",
            file_name,
            lines1.len(),
            lines2.len()
        ));
    }

    // Compare each line
    for (num, (line1, line2)) in lines1.into_iter().zip(lines2).enumerate() {
        if !compare_line(num, &line1, &line2, file_name, errors) {
            errors.push(format!(
                "{file_name}: line {num}:\n    + \"{line1}\"\n    - \"{line2}\""
            ))
        }
    }
}

fn compare_line(
    num: usize,
    line1: &str,
    line2: &str,
    file_name: &str,
    errors: &mut Vec<String>,
) -> bool {
    let fields1 = line1.split(",").collect_vec();
    let fields2 = line2.split(",").collect_vec();
    if fields1.len() != fields2.len() {
        errors.push(format!(
            "{}: line {}: Different number of fields: {} vs {}",
            file_name,
            num,
            fields1.len(),
            fields2.len()
        ));
    }

    // Check every field matches
    fields1.into_iter().zip(fields2).all(|(f1, f2)| {
        // First try to compare fields as floating-point values, falling back on string comparison
        try_compare_floats(f1, f2).unwrap_or_else(|| f1 == f2)
    })
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
        if file_name.ends_with(".csv") {
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
