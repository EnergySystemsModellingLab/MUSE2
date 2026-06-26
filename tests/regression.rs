//! Common code for running regression tests.
use anyhow::Result;
use colored::Colorize;
use similar::{Algorithm, ChangeTag, TextDiff};
use std::env;
use std::fmt::Write;
use std::fs::{File, read_dir};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use tempfile::{TempDir, tempdir};

mod common;
use common::*;

/// Define regression tests
mod regression {
    use super::*;

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
    define_regression_test_with_patches!(simple_shadow);
    define_regression_test_with_patches!(simple_ironing_out);
    define_regression_test_with_patches!(circularity_npv);
}

/// Tolerance for comparing floating-point values in CSV lines.
const FLOAT_CMP_TOLERANCE: f64 = 1e-10;

/// Run a regression test for the given example with optional extra arguments to `muse2 example run`.
///
/// The `--debug-model` flag is always used so the debug files are available to examine. The debug
/// files are only tested when the `debug_model` parameter is true.
fn run_regression_test(example: &str, extra_args: &[&str], debug_model: bool) {
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
    let mut args = vec![
        "example",
        "run",
        example,
        "--debug-model",
        "--output-dir",
        &output_dir_str,
    ];
    args.extend(extra_args);
    assert_muse2_runs(&args);

    // Check that the output files match (approximately)
    let test_data_dir = PathBuf::from(format!("tests/data/{example}"));
    compare_output_dirs(&output_dir, &test_data_dir, debug_model);
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
        if let Some(diff) = diff_csv_file(cur_output_dir1, test_data_dir, &file_name) {
            errors.push(format!("{file_name}: output differs\n{diff}"));
        }
    }

    assert!(
        errors.is_empty(),
        "The following errors occurred:\n  * {}",
        errors.join("\n  * ")
    );
}

fn diff_csv_file(output_dir1: &Path, output_dir2: &Path, file_name: &str) -> Option<String> {
    let lines1 = read_lines(&output_dir1.join(file_name));
    let lines2 = read_lines(&output_dir2.join(file_name));

    compute_normalised_diff(&lines1, &lines2)
}

/// Compute a line diff after replacing tolerance-equivalent rows with a shared canonical row.
fn compute_normalised_diff(lines1: &[String], lines2: &[String]) -> Option<String> {
    if files_match_with_tolerance(lines1, lines2) {
        return None;
    }

    let (normalised_lines1, normalised_lines2) = normalise_tolerant_equivalents(lines1, lines2);

    (normalised_lines1 != normalised_lines2)
        .then(|| render_normalised_diff(&normalised_lines1, &normalised_lines2))
}

/// Quick pre-check to skip normalisation/diff rendering when rows already match within tolerance.
fn files_match_with_tolerance(lines1: &[String], lines2: &[String]) -> bool {
    lines1.len() == lines2.len()
        && lines1
            .iter()
            .zip(lines2)
            .all(|(line1, line2)| lines_match_with_tolerance(line1, line2))
}

/// Render a human-readable diff from already-normalised lines.
///
/// Reported line numbers are relative to the normalised views and may differ from the original
/// unnormalised row positions when tolerance-based replacements occur.
fn render_normalised_diff(old_lines: &[String], new_lines: &[String]) -> String {
    let old = old_lines.join("\n");
    let new = new_lines.join("\n");
    let diff = TextDiff::configure()
        .algorithm(Algorithm::Myers)
        .diff_lines(&old, &new);
    let mut out = String::new();
    let mut old_line_number = 1;
    let mut new_line_number = 1;
    let mut old_index = 0usize;
    let mut new_index = 0usize;

    for change in diff.iter_all_changes() {
        let line = match change.tag() {
            ChangeTag::Equal => {
                old_line_number += 1;
                new_line_number += 1;
                old_index += 1;
                new_index += 1;
                continue;
            }
            ChangeTag::Delete => {
                let value = &old_lines[old_index];
                let line = format!("-L{old_line_number}: {value}").red().to_string();
                old_line_number += 1;
                old_index += 1;
                line
            }
            ChangeTag::Insert => {
                let value = &new_lines[new_index];
                let line = format!("+L{new_line_number}: {value}").green().to_string();
                new_line_number += 1;
                new_index += 1;
                line
            }
        };
        out.push_str(&line);
        out.push('\n');
    }

    writeln!(
        out,
        "\nNote: line numbers and floats are approximate within tolerance: {FLOAT_CMP_TOLERANCE}."
    )
    .unwrap();
    writeln!(
        out,
        "Small discrepancies are possible between the diff and the actual files."
    )
    .unwrap();

    out
}

/// Replace rows that are equal within `FLOAT_CMP_TOLERANCE` with a shared canonical row.
///
/// We build bins keyed by canonical line values. A line is added to the first existing bin only
/// if it matches every row already in that bin; otherwise a new bin is created with that line as
/// the canonical value.
///
/// We process `lines1` first and then `lines2` so canonical keys are stable with respect to the
/// old output ordering.
fn normalise_tolerant_equivalents(
    lines1: &[String],
    lines2: &[String],
) -> (Vec<String>, Vec<String>) {
    let mut bins = Vec::new();

    let mut bins1 = Vec::with_capacity(lines1.len());
    for line in lines1 {
        let bin = assign_line_to_bin(line, &mut bins);
        bins1.push(bin);
    }

    let mut bins2 = Vec::with_capacity(lines2.len());
    for line in lines2 {
        let bin = assign_line_to_bin(line, &mut bins);
        bins2.push(bin);
    }

    let normalised1 = bins1
        .into_iter()
        .map(|bin| bins[bin].canonical.clone())
        .collect();
    let normalised2 = bins2
        .into_iter()
        .map(|bin| bins[bin].canonical.clone())
        .collect();

    (normalised1, normalised2)
}

/// Assign a line to the first bin it can join without breaking complete-link tolerance.
fn assign_line_to_bin(line: &str, bins: &mut Vec<LineBin>) -> usize {
    if let Some(bin) = find_matching_bin(line, bins) {
        bins[bin].members.push(line.to_string());
        return bin;
    }

    bins.push(LineBin::new(line));
    bins.len() - 1
}

/// Find the first bin whose current members all match the candidate line.
fn find_matching_bin(line: &str, bins: &[LineBin]) -> Option<usize> {
    bins.iter().position(|bin| line_matches_bin(line, bin))
}

/// Check whether a candidate line is within tolerance of every line already in a bin.
fn line_matches_bin(line: &str, bin: &LineBin) -> bool {
    bin.members
        .iter()
        .all(|member| lines_match_with_tolerance(line, member))
}

#[derive(Debug)]
struct LineBin {
    canonical: String,
    members: Vec<String>,
}

impl LineBin {
    /// Create a new bin seeded with the provided line.
    fn new(line: &str) -> Self {
        Self {
            canonical: line.to_string(),
            members: vec![line.to_string()],
        }
    }
}

/// Parse a string into an `f64`, returning `None` if parsing fails or value is infinite/NaN.
fn parse_finite(s: &str) -> Option<f64> {
    s.parse().ok().filter(|f: &f64| f.is_finite())
}

fn lines_match_with_tolerance(line1: &str, line2: &str) -> bool {
    let fields1: Vec<_> = line1.split(',').collect();
    let fields2: Vec<_> = line2.split(',').collect();
    if fields1.len() != fields2.len() {
        return false;
    }

    fields1.into_iter().zip(fields2).all(|(field1, field2)| {
        match (parse_finite(field1), parse_finite(field2)) {
            (Some(float1), Some(float2)) => (float1 - float2).abs() <= FLOAT_CMP_TOLERANCE,
            _ => field1 == field2,
        }
    })
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

mod tests {
    use super::*;

    /// Return a value offset from the base by a fraction of the tolerance.
    fn offset_from(base: f64, multiplier: f64) -> f64 {
        base + FLOAT_CMP_TOLERANCE * multiplier
    }

    /// Build a CSV line from a name and floating-point value.
    fn csv_line(name: &str, value: f64) -> String {
        format!("{name},{value}")
    }

    #[test]
    fn tolerated_float_change_is_normalised_away() {
        let old_lines = vec![csv_line("asset", offset_from(1.0, 0.25))];
        let new_lines = vec![csv_line("asset", offset_from(1.0, 0.75))];

        let (normalised_old, normalised_new) =
            normalise_tolerant_equivalents(&old_lines, &new_lines);
        assert_eq!(normalised_old, normalised_new);
    }

    #[test]
    fn normalise_lines_rounds_float_fields() {
        let lines1 = vec![
            csv_line("asset_a", offset_from(1.0, 0.1)) + ",region_1",
            "asset_b,2.5,region_2".to_string(),
        ];
        let expected = vec![
            csv_line("asset_a", offset_from(1.0, 0.2)) + ",region_1",
            "asset_b,2.5,region_2".to_string(),
        ];

        let (normalised_lines, normalised_expected) =
            normalise_tolerant_equivalents(&lines1, &expected);
        assert_eq!(normalised_lines, normalised_expected);
    }

    #[test]
    fn render_diff_ignores_tolerated_float_changes() {
        let old_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.25)),
            "asset_b,2.0".to_string(),
        ];
        let new_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.75)),
            "asset_b,3.0".to_string(),
        ];

        let diff = compute_normalised_diff(&old_lines, &new_lines)
            .expect("asset_b changed so diff is expected");

        assert!(!diff.contains("asset_a"));
        assert!(diff.contains("asset_b,2.0"));
        assert!(diff.contains("asset_b,3.0"));
    }

    #[test]
    fn render_diff_ignores_tolerated_float_differences_one_line_missing() {
        let old_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.1)),
            csv_line("asset_b", offset_from(2.0, 0.1)),
            "asset_c,3.0".to_string(),
            csv_line("asset_d", offset_from(4.0, 0.1)),
            csv_line("asset_e", offset_from(5.0, 0.1)),
            csv_line("asset_f", offset_from(6.0, 0.1)),
        ];
        let new_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.2)),
            csv_line("asset_b", offset_from(2.0, 0.2)),
            csv_line("asset_d", offset_from(4.0, 0.2)),
            csv_line("asset_e", offset_from(5.0, 0.2)),
            csv_line("asset_f", offset_from(6.0, 0.2)),
        ];

        let diff = compute_normalised_diff(&old_lines, &new_lines)
            .expect("asset_c is missing so this should still report a diff");

        assert!(!diff.contains("asset_a"));
        assert!(!diff.contains("asset_b"));
        assert!(!diff.contains("asset_d"));
        assert!(!diff.contains("asset_e"));
        assert!(!diff.contains("asset_f"));
        assert!(diff.contains("-L3: asset_c,3.0"));
        assert!(!diff.contains("asset_c,9.0"));
    }

    #[test]
    fn render_diff_keeps_deleted_line_text_for_almost_duplicate_rows() {
        let old_lines = vec![
            csv_line("asset_dup", offset_from(1.0, 0.1)),
            csv_line("asset_dup", offset_from(1.0, 0.2)),
        ];
        let new_lines = vec![csv_line("asset_dup", offset_from(1.0, 0.2))];

        let diff = compute_normalised_diff(&old_lines, &new_lines)
            .expect("A deleted duplicate-ish row should produce a reported diff");

        let expected_deleted_line = csv_line("asset_dup", offset_from(1.0, 0.1));
        assert!(
            diff.contains(&format!("-L1: {expected_deleted_line}")),
            "Deleted line should be shown with its original text, but diff was:\n{diff}"
        );
    }

    #[test]
    fn diff_computation_correctly_splits_bins_when_rows_exceed_tolerance() {
        let old_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.9)),
            csv_line("asset_dup", offset_from(2.0, 0.0)),
            "asset_z,9.0".to_string(),
            csv_line("asset_dup", offset_from(2.0, 0.9)),
            "asset_z,9.0".to_string(),
        ];
        let new_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.9)),
            csv_line("asset_dup", offset_from(2.0, 0.0)),
            "asset_z,9.0".to_string(),
            csv_line("asset_dup", offset_from(2.0, 2.1)),
            "asset_z,9.0".to_string(),
        ];

        let diff = compute_normalised_diff(&old_lines, &new_lines)
            .expect("A deleted row should produce a reported diff");

        // 2.0000000009 is within tolerance of 2.0000000000 so they are binned together.
        assert!(
            diff.contains(&format!(
                "-L4: {}",
                csv_line("asset_dup", offset_from(2.0, 0.0))
            )),
            "The row that stays in the original bin should still be shown, but diff was:\n{diff}"
        );
        // 2.0000000021 is outside tolerance of that bin so it must split into a new bin.
        assert!(
            diff.contains(&format!(
                "+L4: {}",
                csv_line("asset_dup", offset_from(2.0, 2.1))
            )),
            "The out-of-tolerance row should split into its own bin, but diff was:\n{diff}"
        );
    }

    // Not really a test, just demonstrates the approximation behaviour in a corner case
    // where there are duplicates, the diff output can only show the actual number in the file
    // within the tolerance
    #[test]
    fn diff_computation_is_approximate_and_order_dependent_part_1() {
        let old_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.1)),
            csv_line("asset_dup", offset_from(2.0, 0.1)),
            csv_line("asset_dup", offset_from(2.0, 0.2)),
            "asset_z,9.0".to_string(),
        ];
        let new_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.2)),
            csv_line("asset_dup", offset_from(2.0, 0.2)),
            "asset_z,9.0".to_string(),
        ];

        let diff = compute_normalised_diff(&old_lines, &new_lines)
            .expect("A deleted row should produce a reported diff");

        assert!(
            diff.contains(&format!(
                "-L3: {}",
                csv_line("asset_dup", offset_from(2.0, 0.1))
            )),
            "A deleted duplicate-ish row should still be shown, but diff was:\n{diff}"
        );
    }

    // Not really a test, just demonstrates the approximation behaviour in a corner case
    // where there are duplicates, the diff output can only show the actual number in the file
    // within the tolerance
    #[test]
    fn diff_computation_is_approximate_and_order_dependent_part_2() {
        let old_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.25)),
            csv_line("asset_dup", offset_from(2.0, 0.2)),
            csv_line("asset_dup", offset_from(2.0, 0.1)),
            "asset_z,9.0".to_string(),
        ];
        let new_lines = vec![
            csv_line("asset_a", offset_from(1.0, 0.75)),
            csv_line("asset_dup", offset_from(2.0, 0.2)),
            "asset_z,9.0".to_string(),
        ];

        let diff = compute_normalised_diff(&old_lines, &new_lines)
            .expect("A deleted row should produce a reported diff");

        assert!(
            diff.contains(&format!(
                "-L3: {}",
                csv_line("asset_dup", offset_from(2.0, 0.2))
            )),
            "A deleted duplicate-ish row should still be shown, but diff was:\n{diff}"
        );
    }
}
