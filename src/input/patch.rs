//! Code for applying patches/diffs to model input files.
use super::input_err_msg;

use anyhow::{Context, Result, bail, ensure};
use csv::{ReaderBuilder, Trim};
use indexmap::IndexSet;
use log::info;
use std::fs;
use std::path::Path;
use tempfile::{TempDir, tempdir};

/// Structure to hold diffs from a diff file
#[derive(Debug)]
struct FileDiffs {
    /// The column headers from the diff file
    headers: Vec<String>,
    /// Rows to delete (normalized to remove whitespace)
    to_delete: IndexSet<String>,
    /// Rows to add (normalized to remove whitespace)
    to_add: IndexSet<String>,
}

/// Read diffs from a diff file.
///
/// Diff files follow the same format as base files, but with an extra column on the left with
/// '+' or '-' indicators to indicate whether that row should be added to or deleted from the base
/// file.
fn read_diffs(file_path: &Path) -> Result<FileDiffs> {
    // Read the diff file from the given path, trimming any whitespace
    let mut reader = ReaderBuilder::new()
        .trim(Trim::All)
        .from_path(file_path)
        .with_context(|| input_err_msg(file_path))?;

    // Read header
    let diff_header = reader.headers().with_context(|| input_err_msg(file_path))?;
    ensure!(!diff_header.is_empty(), "Diff file header cannot be empty");

    // Colect column headers (skip first column which is the diff indicator)
    let headers: Vec<String> = diff_header
        .iter()
        .skip(1)
        .map(ToString::to_string)
        .collect();
    ensure!(
        !headers.is_empty(),
        "Diff file must have at least one data column"
    );

    // Collect additions and deletions
    let mut to_delete = IndexSet::new();
    let mut to_add = IndexSet::new();
    for (line_num, result) in reader.records().enumerate() {
        let record = result.with_context(|| {
            format!("Error reading record at line {} in diff file", line_num + 2)
        })?;

        ensure!(
            !record.is_empty(),
            "Empty row at line {} in diff file",
            line_num + 2
        );

        // First column is the diff indicator
        let diff_indicator = record.get(0).context("Missing diff indicator column")?;

        // Build normalized row string by joining trimmed fields with commas
        let row_str = record.iter().skip(1).collect::<Vec<_>>().join(",");

        match diff_indicator {
            "-" => {
                ensure!(
                    to_delete.insert(row_str.clone()),
                    "Duplicate deletion entry at line {}: {}",
                    line_num + 2,
                    row_str
                );
            }
            "+" => {
                ensure!(
                    to_add.insert(row_str.clone()),
                    "Duplicate addition entry at line {}: {}",
                    line_num + 2,
                    row_str
                );
            }
            _ => {
                bail!(
                    "Invalid diff indicator at line {}: '{}'. Must be '+' or '-'",
                    line_num + 2,
                    diff_indicator
                );
            }
        }
    }

    // Disallow overlap between deletions and additions
    for del_row in &to_delete {
        ensure!(
            !to_add.contains(del_row),
            "Row appears in both deletions and additions: {del_row}"
        );
    }

    Ok(FileDiffs {
        headers,
        to_delete,
        to_add,
    })
}

/// Modify a base CSV file by applying diffs: removing rows and adding rows.
/// Preserves the order of rows from the base file, with new rows appended at the end.
fn modify_base_with_diffs(base: &str, diffs: &FileDiffs) -> Result<String> {
    // Read base file from string, trimming whitespace
    let mut reader = ReaderBuilder::new()
        .trim(Trim::All)
        .from_reader(base.as_bytes());

    // Read and validate header
    let base_header = reader
        .headers()
        .context("Failed to read base file header")?;

    let base_header_vec: Vec<String> = base_header.iter().map(ToString::to_string).collect();

    ensure!(
        base_header_vec == diffs.headers,
        "Header mismatch: base file has [{}], diff file expects [{}]",
        base_header_vec.join(", "),
        diffs.headers.join(", ")
    );

    // Read all rows from base file, preserving order and checking for duplicates
    let mut base_rows = IndexSet::new();
    for (line_num, result) in reader.records().enumerate() {
        let record = result.with_context(|| {
            format!("Error reading record at line {} in base file", line_num + 2)
        })?;

        // Create normalized row string by joining trimmed fields with commas
        let row_str = record.iter().collect::<Vec<_>>().join(",");

        // Check for duplicates
        ensure!(
            base_rows.insert(row_str.clone()),
            "Duplicate row at line {} in base file: {}",
            line_num + 2,
            row_str
        );
    }

    // Apply deletions
    base_rows.retain(|row| !diffs.to_delete.contains(row));

    // Apply additions (append to end, checking for duplicates)
    for add_row in &diffs.to_add {
        ensure!(
            base_rows.insert(add_row.clone()),
            "Addition already present in base file: {add_row}"
        );
    }

    // Rebuild CSV output
    let mut output = String::new();

    // Write header
    output.push_str(&base_header_vec.join(","));

    // Write rows
    if !base_rows.is_empty() {
        output.push('\n');
        output.push_str(&base_rows.iter().cloned().collect::<Vec<_>>().join("\n"));
    }

    Ok(output)
}

pub fn patch_model<P: AsRef<Path>>(base_model_dir: P, diffs_dir: P) -> Result<TempDir> {
    info!(
        "Patching model at '{}' with diffs from '{}'",
        base_model_dir.as_ref().display(),
        diffs_dir.as_ref().display()
    );

    // Copy contents of `base_model_dir` to a temporary directory
    let temp_dir = tempdir().context("Failed to create temporary directory")?;
    let temp_path = temp_dir.path();

    for entry in fs::read_dir(base_model_dir.as_ref())? {
        let entry = entry?;
        let src_path = entry.path();

        // Only copy files (skip any subdirectories if present)
        if src_path.is_file() {
            let dst_path = temp_path.join(entry.file_name());
            fs::copy(&src_path, &dst_path)
                .with_context(|| format!("Failed to copy file: {}", src_path.display()))?;
        }
    }

    // Patch the new model.toml file un the temporary directory
    let base_toml_path = temp_path.join("model.toml");
    let patch_toml_path = diffs_dir.as_ref().join("model.toml");
    patch_model_toml(&base_toml_path, &patch_toml_path)?;

    // Patch CSV files based on diffs in diffs_dir
    for entry in fs::read_dir(diffs_dir.as_ref())? {
        let entry = entry?;
        let diff_path = entry.path();

        // Only process files (skip any subdirectories if present)
        if diff_path.is_file() {
            let diff_filename = diff_path
                .file_name()
                .and_then(|name| name.to_str())
                .context("Invalid filename encoding")?;

            // Ignore non-CSV files
            if !std::path::Path::new(diff_filename)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("csv"))
            {
                continue;
            }

            // Check that the filename ends with "_diff.csv"
            ensure!(
                diff_filename.to_lowercase().ends_with("_diff.csv"),
                "Diff file must end with '_diff.csv': {diff_filename}"
            );

            // Extract the base filename (e.g., "agents" from "agents_diff.csv")
            let base_name = &diff_filename[..diff_filename.len() - "_diff.csv".len()];
            let target_filename = format!("{base_name}.csv");
            let base_path = base_model_dir.as_ref().join(&target_filename);

            // Read base file and apply patch
            let patched_str =
                read_base_file_with_patch(&base_path, &diff_path).with_context(|| {
                    format!(
                        "Error applying patch from {} to {}",
                        diff_path.display(),
                        base_path.display(),
                    )
                })?;

            // Save to the temporary directory
            fs::write(temp_path.join(&target_filename), patched_str)?;
        }
    }

    info!(
        "Patching complete. Patched model saved to temporary path '{}'",
        temp_path.display()
    );

    // Return the temporary directory
    Ok(temp_dir)
}

fn read_base_file_with_patch(base_file_path: &Path, diff_path: &Path) -> Result<String> {
    // Read base file
    if !base_file_path.exists() {
        bail!(
            "Base file for patching does not exist: {}",
            base_file_path.display()
        );
    }
    let base = fs::read_to_string(base_file_path).with_context(|| input_err_msg(base_file_path))?;

    // Read diff file
    if !diff_path.exists() {
        bail!(
            "Diff file for patching does not exist: {}",
            diff_path.display()
        );
    }
    let diffs = read_diffs(diff_path).with_context(|| input_err_msg(diff_path))?;

    // Apply diffs to base file
    let modified = modify_base_with_diffs(&base, &diffs)?;
    Ok(modified)
}

fn patch_model_toml(base_path: &Path, patch_path: &Path) -> Result<()> {
    // Read original TOML file
    let base_str = fs::read_to_string(base_path).with_context(|| input_err_msg(base_path))?;
    let mut base_data: toml::Value =
        toml::from_str(&base_str).with_context(|| input_err_msg(base_path))?;

    // Read patch TOML file
    let patch_str = fs::read_to_string(patch_path).with_context(|| input_err_msg(patch_path))?;
    let patch_data: toml::Value =
        toml::from_str(&patch_str).with_context(|| input_err_msg(patch_path))?;

    // Merge patch into base (only top-level fields allowed)
    let base_table = base_data.as_table_mut().expect("Base TOML must be a table");
    let patch_table = patch_data.as_table().expect("Patch TOML must be a table");

    for (key, patch_val) in patch_table {
        // Skip `base_model` field
        if key == "base_model" {
            continue;
        }

        // Overwrite or add the field from the patch
        base_table.insert(key.clone(), patch_val.clone());
    }

    // Save modified TOML back to original file
    let modified_str = toml::to_string_pretty(&base_data)?;
    fs::write(base_path, modified_str)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use std::io::Write;

    #[test]
    fn test_read_diffs_basic() {
        let temp_dir = tempdir().unwrap();
        let diff_file = temp_dir.path().join("test_diff.csv");

        let content = "diff,col1,col2\n-,val1,val2\n+,val3,val4\n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        let diffs = read_diffs(&diff_file).unwrap();

        assert_eq!(diffs.headers, vec!["col1", "col2"]);
        assert_eq!(diffs.to_delete.len(), 1);
        assert_eq!(diffs.to_add.len(), 1);

        let del_row = "val1,val2".to_string();
        let add_row = "val3,val4".to_string();
        assert!(diffs.to_delete.contains(&del_row));
        assert!(diffs.to_add.contains(&add_row));
    }

    #[test]
    fn test_read_diffs_with_whitespace() {
        let temp_dir = tempdir().unwrap();
        let diff_file = temp_dir.path().join("test_diff.csv");

        let content = " diff , col1 , col2 \n-,  item1  ,  item2  \n+,  another1  ,  another2  \n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        let diffs = read_diffs(&diff_file).unwrap();

        // Headers should be trimmed
        assert_eq!(diffs.headers, vec!["col1", "col2"]);
        // Rows should be normalized (whitespace trimmed)
        assert_eq!(diffs.to_delete.len(), 1);
        assert_eq!(diffs.to_add.len(), 1);

        // Check that whitespace is normalized
        let del_row = "item1,item2".to_string();
        assert!(diffs.to_delete.contains(&del_row));
    }

    #[test]
    fn test_modify_base_with_diffs_preserves_order() {
        let base = "col1,col2\nrow1,row2\nrow3,row4\nrow5,row6\n";

        let mut to_delete = IndexSet::new();
        to_delete.insert("row3,row4".to_string());
        let mut to_add = IndexSet::new();
        to_add.insert("row7,row8".to_string());

        let diffs = FileDiffs {
            headers: vec!["col1".to_string(), "col2".to_string()],
            to_delete,
            to_add,
        };

        let modified = modify_base_with_diffs(base, &diffs).unwrap();

        // Should preserve order: row1,row2 -> row5,row6 -> row7,row8
        let lines: Vec<&str> = modified.lines().collect();
        assert_eq!(lines[0], "col1,col2");
        assert_eq!(lines[1], "row1,row2");
        assert_eq!(lines[2], "row5,row6");
        assert_eq!(lines[3], "row7,row8");
        assert!(!modified.contains("row3,row4"));
    }

    #[test]
    fn test_modify_base_with_diffs_mismatched_header() {
        let base = "col1,col2\nrow1,row2\n";
        let diffs = FileDiffs {
            headers: vec!["col1".to_string(), "col3".to_string()],
            to_delete: IndexSet::new(),
            to_add: IndexSet::new(),
        };

        let result = modify_base_with_diffs(base, &diffs);
        assert_error!(
            result,
            "Header mismatch: base file has [col1, col2], diff file expects [col1, col3]"
        );
    }
}
