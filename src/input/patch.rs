//! Code for applying patches/diffs to model input files.
use super::input_err_msg;

use anyhow::{Context, Result, bail, ensure};
use indexmap::IndexSet;
use log::info;
use std::fs;
use std::path::Path;
use tempfile::{TempDir, tempdir};

/// Structure to hold diffs from a diff file
#[derive(Debug)]
struct FileDiffs {
    /// The header line from the diff file
    header_line: String,
    /// Lines to delete from the original file
    to_delete: IndexSet<String>,
    /// Lines to add to the original file
    to_add: IndexSet<String>,
}

/// Read diffs from a diff file.
///
/// Reads a diff file where the first line is a header, and subsequent lines start with "-," for
/// deletions and "+," for additions.
fn read_diffs(file_path: &Path) -> Result<FileDiffs> {
    // Read the entire file as a string
    let content = fs::read_to_string(file_path).with_context(|| input_err_msg(file_path))?;
    let content = content.trim();

    // Read header line
    // This is saved to ensure that diffs are applied to a base file with the same header
    let header_line = match content.lines().next() {
        Some(line) => line.trim().to_string(),
        None => bail!("Diff file cannot be empty"),
    };

    // Collect additions and deletions
    let mut to_delete = IndexSet::new();
    let mut to_add = IndexSet::new();
    for line in content.lines().skip(1) {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some(body) = line.strip_prefix("-,") {
            let v = body.trim().to_string();
            ensure!(to_delete.insert(v.clone()), "Duplicate deletion entry: {v}",);
        } else if let Some(body) = line.strip_prefix("+,") {
            let v = body.trim().to_string();
            ensure!(to_add.insert(v.clone()), "Duplicate addition entry: {v}");
        } else {
            bail!("Invalid row in diff file: {line}. Must start with '-,' or '+,'");
        }
    }

    // Disallow overlap
    if let Some(dup) = to_delete.iter().find(|d| to_add.contains(*d)) {
        bail!("Line appears in both deletions and additions: {dup}");
    }

    Ok(FileDiffs {
        header_line,
        to_delete,
        to_add,
    })
}

/// Modify a string representation of a file by applying diffs: removing lines and adding lines.
fn modify_base_with_diffs(base: &str, diffs: &FileDiffs) -> Result<String> {
    ensure!(!base.trim().is_empty(), "Base file is empty");

    // Split into lines while preserving order; keep header intact
    let lines: Vec<&str> = base.lines().collect();
    let original_header = lines.first().unwrap().trim();
    ensure!(
        original_header == diffs.header_line,
        "Header line in diff file does not match base file: expected '{}', found '{}'",
        original_header,
        diffs.header_line
    );

    // Build a unique set from the body
    let mut body = IndexSet::new();
    for &line in lines.iter().skip(1) {
        let l = line.strip_suffix('\r').unwrap_or(line);
        ensure!(body.insert(l), "Duplicate line found in base file: {l}");
    }

    // Deletions
    for d in &diffs.to_delete {
        ensure!(
            body.shift_remove(d.as_str()),
            "Row to delete not found in base file: {d}"
        );
    }

    // Additions
    for a in &diffs.to_add {
        ensure!(
            body.insert(a.as_str()),
            "Addition already present in base file: {a}"
        );
    }

    // Rebuild
    let mut out = String::new();
    out.push_str(original_header);
    if !body.is_empty() {
        out.push('\n');
        out.push_str(&body.iter().copied().collect::<Vec<_>>().join("\n"));
    }

    Ok(out)
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

    // Patch the model.toml file
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
                .unwrap();

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

        let content = "header\n-,line_to_delete\n+,line_to_add\n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        let diffs = read_diffs(&diff_file).unwrap();

        assert_eq!(diffs.header_line, "header");
        assert_eq!(diffs.to_delete, vec!["line_to_delete"]);
        assert_eq!(diffs.to_add, vec!["line_to_add"]);
    }

    #[test]
    fn test_read_diffs_with_whitespace() {
        let temp_dir = tempdir().unwrap();
        let diff_file = temp_dir.path().join("test_diff.csv");

        let content = " header \n-,  item_with_spaces  \n+,  another_item  \n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        let diffs = read_diffs(&diff_file).unwrap();

        // Whitespace should be trimmed
        assert_eq!(diffs.header_line, "header");
        assert_eq!(diffs.to_delete, vec!["item_with_spaces"]);
        assert_eq!(diffs.to_add, vec!["another_item"]);
    }

    #[test]
    fn test_read_diffs_invalid_line() {
        let temp_dir = tempdir().unwrap();
        let diff_file = temp_dir.path().join("test_diff.csv");

        let content = "header\ninvalid_line\n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        let result = read_diffs(&diff_file);
        assert_error!(result, "Invalid line in diff file: invalid_line");
    }

    #[test]
    fn test_modify_string_with_diffs_basic() {
        let original = "header\nline1\nline2\nline3\n";
        let diffs = FileDiffs {
            header_line: "header".to_string(),
            to_delete: vec!["line2".to_string()],
            to_add: vec!["line_new".to_string()],
        };

        let modified = modify_base_with_diffs(original, &diffs).unwrap();
        assert!(!modified.contains("line2"));
        assert!(modified.contains("line_new"));
    }

    #[test]
    fn test_modify_string_with_diffs_mismatched_header() {
        let original = "header1\nline1\n";
        let diffs = FileDiffs {
            header_line: "header2".to_string(),
            to_delete: vec![],
            to_add: vec![],
        };

        let result = modify_base_with_diffs(original, &diffs);
        assert_error!(
            result,
            "Header line in diff file does not match original file"
        );
    }

    #[test]
    fn test_modify_string_with_diffs_missing_item() {
        let original = "header\nline1\n";
        let diffs = FileDiffs {
            header_line: "header".to_string(),
            to_delete: vec!["nonexistent".to_string()],
            to_add: vec![],
        };

        let result = modify_base_with_diffs(original, &diffs);
        assert_error!(
            result,
            "Item to delete not found in original file: nonexistent"
        );
    }
}
