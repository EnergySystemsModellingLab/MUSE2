//! Common code for running regression tests.
use anyhow::Result;
use float_cmp::approx_eq;
use itertools::Itertools;
use ordered_float::NotNan;
use similar::{Algorithm, DiffOp, DiffTag, capture_diff_slices};
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

    // Check whether files differ using the existing field-by-field tolerance rules.
    let mut has_mismatch = lines1.len() != lines2.len();
    if !has_mismatch {
        has_mismatch = lines1
            .iter()
            .zip(&lines2)
            .any(|(line1, line2)| !compare_line(line1, line2));
    }

    if has_mismatch {
        let diff_ops = capture_csv_diff_ops(&lines1, &lines2);
        let diff = render_diff(&diff_ops, &lines1, &lines2);
        errors.push(format!("{file_name}: output differs\n{diff}"));
    }
}

fn compare_line(line1: &str, line2: &str) -> bool {
    let fields1 = line1.split(',').collect_vec();
    let fields2 = line2.split(',').collect_vec();
    if fields1.len() != fields2.len() {
        return false;
    }

    fields1
        .into_iter()
        .zip(fields2)
        .all(|(f1, f2)| try_compare_floats(f1, f2).unwrap_or_else(|| f1 == f2))
}

fn capture_csv_diff_ops(lines1: &[String], lines2: &[String]) -> Vec<DiffOp> {
    let parsed1 = parse_csv_lines(lines1);
    let parsed2 = parse_csv_lines(lines2);
    capture_diff_slices(Algorithm::Myers, &parsed1, &parsed2)
}

fn has_non_equal_diff_ops(diff_ops: &[DiffOp]) -> bool {
    diff_ops.iter().any(|op| op.tag() != DiffTag::Equal)
}

/// Render a line-based diff from `DiffOp`s, including old/new line numbers.
/// For replaced lines, pairs that are equal under `compare_line` are omitted.
fn render_diff(diff_ops: &[DiffOp], lines1: &[String], lines2: &[String]) -> String {
    let mut out = String::new();
    for op in diff_ops {
        let (tag, old_range, new_range) = op.as_tag_tuple();
        match tag {
            DiffTag::Equal => {}
            DiffTag::Delete => {
                for old_idx in old_range {
                    let _ = writeln!(out, "-L{}: {}", old_idx + 1, lines1[old_idx]);
                }
            }
            DiffTag::Insert => {
                for new_idx in new_range {
                    let _ = writeln!(out, "+L{}: {}", new_idx + 1, lines2[new_idx]);
                }
            }
            DiffTag::Replace => {
                let old_start = old_range.start;
                let new_start = new_range.start;
                let paired_len = old_range.len().min(new_range.len());

                for idx in 0..paired_len {
                    let old_idx = old_start + idx;
                    let new_idx = new_start + idx;
                    if !compare_line(&lines1[old_idx], &lines2[new_idx]) {
                        let _ = writeln!(out, "-L{}: {}", old_idx + 1, lines1[old_idx]);
                        let _ = writeln!(out, "+L{}: {}", new_idx + 1, lines2[new_idx]);
                    }
                }

                for (old_idx, line) in lines1
                    .iter()
                    .enumerate()
                    .take(old_range.end)
                    .skip(old_start + paired_len)
                {
                    let _ = writeln!(out, "-L{}: {}", old_idx + 1, line);
                }
                for (new_idx, line) in lines2
                    .iter()
                    .enumerate()
                    .take(new_range.end)
                    .skip(new_start + paired_len)
                {
                    let _ = writeln!(out, "+L{}: {}", new_idx + 1, line);
                }
            }
        }
    }

    out
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CsvLine(Vec<CsvField>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CsvField {
    Float(NotNan<f64>),
    Text(String),
}

fn parse_csv_lines(lines: &[String]) -> Vec<CsvLine> {
    lines.iter().map(|line| parse_csv_line(line)).collect()
}

fn parse_csv_line(line: &str) -> CsvLine {
    CsvLine(line.split(',').map(parse_csv_field).collect())
}

fn parse_csv_field(field: &str) -> CsvField {
    if let Some(float) = parse_finite(field) {
        CsvField::Float(quantise_float(float))
    } else {
        CsvField::Text(field.to_string())
    }
}

fn quantise_float(value: f64) -> NotNan<f64> {
    let scaled = value / FLOAT_CMP_TOLERANCE;
    let quantised = if scaled.is_finite() {
        scaled.round() * FLOAT_CMP_TOLERANCE
    } else {
        value
    };

    let quantised = if quantised == -0.0 { 0.0 } else { quantised };
    NotNan::new(quantised).expect("quantised float should always be finite")
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

#[test]
fn tolerated_float_change_yields_no_diff_ops() {
    let old_lines = vec!["asset,1.00000000001".to_string()];
    let new_lines = vec!["asset,1.00000000002".to_string()];

    let diff_ops = capture_csv_diff_ops(&old_lines, &new_lines);
    assert!(!has_non_equal_diff_ops(&diff_ops));
}

#[test]
fn parse_csv_lines_normalises_floats_within_tolerance() {
    let lines1 = vec![
        "asset_a,1.000000000001,region_1".to_string(),
        "asset_b,2.5,region_2".to_string(),
    ];
    let lines2 = vec![
        "asset_a,1.000000000002,region_1".to_string(),
        "asset_b,2.5,region_2".to_string(),
    ];

    let parsed1 = parse_csv_lines(&lines1);
    let parsed2 = parse_csv_lines(&lines2);

    assert_eq!(parsed1, parsed2);
}

#[test]
fn render_diff_ignores_tolerated_float_changes() {
    let old_lines = vec![
        "asset_a,1.00000000001".to_string(),
        "asset_b,2.0".to_string(),
    ];
    let new_lines = vec![
        "asset_a,1.00000000002".to_string(),
        "asset_b,3.0".to_string(),
    ];

    let diff_ops = capture_csv_diff_ops(&old_lines, &new_lines);
    let diff = render_diff(&diff_ops, &old_lines, &new_lines);

    assert!(!diff.contains("asset_a"));
    assert!(diff.contains("asset_b,2.0"));
    assert!(diff.contains("asset_b,3.0"));
}

#[test]
fn render_diff_ignores_tolerated_float_differences_one_line_missing() {
    let old_lines = vec![
        "asset_a,1.000000000001".to_string(),
        "asset_b,2.000000000001".to_string(),
        "asset_c,3.0".to_string(),
        "asset_d,4.000000000001".to_string(),
        "asset_e,5.000000000001".to_string(),
        "asset_f,6.000000000001".to_string(),
    ];
    let new_lines = vec![
        "asset_a,1.000000000002".to_string(),
        "asset_b,2.000000000002".to_string(),
        "asset_d,4.000000000002".to_string(),
        "asset_e,5.000000000002".to_string(),
        "asset_f,6.000000000002".to_string(),
    ];

    let diff_ops = capture_csv_diff_ops(&old_lines, &new_lines);
    let diff = render_diff(&diff_ops, &old_lines, &new_lines);

    assert!(!diff.contains("asset_a"));
    assert!(!diff.contains("asset_b"));
    assert!(!diff.contains("asset_d"));
    assert!(!diff.contains("asset_e"));
    assert!(!diff.contains("asset_f"));
    assert!(diff.contains("-L3: asset_c,3.0"));
    assert!(!diff.contains("asset_c,9.0"));
}

#[test]
fn render_diff_ignores_quantisation_boundary_tolerance_case() {
    let old_lines = vec![
        "asset_a,25.852906323049822".to_string(),
        "asset_b,2.0".to_string(),
    ];
    let new_lines = vec![
        "asset_a,25.852906323050078".to_string(),
        "asset_b,3.0".to_string(),
    ];

    // `asset_a` differs only within tolerance, while `asset_b` is a real change.
    let diff_ops = capture_csv_diff_ops(&old_lines, &new_lines);
    let diff = render_diff(&diff_ops, &old_lines, &new_lines);

    assert!(!diff.contains("asset_a"));
    assert!(diff.contains("-L2: asset_b,2.0"));
    assert!(diff.contains("+L2: asset_b,3.0"));
}
