//! Code for applying patches/diffs to model input files.
use super::input_err_msg;

use anyhow::{Context, Result, bail, ensure};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::tempdir;

/// Structure to hold diffs from a diff f
struct FileDiffs {
    /// The header line from the diff file
    header_line: String,
    /// Lines to delete from the original file
    to_delete: Vec<String>,
    /// Lines to add to the original file
    to_add: Vec<String>,
}

/// Read diffs from a diff file.
///
/// Reads a diff file where the first line is a header, and subsequent lines start with "-," for
/// deletions and "+," for additions.
fn read_diffs(file_path: &Path) -> Result<FileDiffs> {
    // Read the entire file as a string
    let content = fs::read_to_string(file_path).with_context(|| input_err_msg(file_path))?;

    // Read header line
    // This is saved to ensure that diffs are applied to a base file with the same header
    let header_line = content
        .lines()
        .next()
        .expect("Diff file cannot be empty")
        .to_string();

    // Collect additions and deletions
    let mut to_delete = Vec::new();
    let mut to_add = Vec::new();
    for line in content.lines().skip(1) {
        let line = line.trim();
        if let Some(stripped) = line.strip_prefix("-,") {
            to_delete.push(stripped.trim().to_string());
        } else if let Some(stripped) = line.strip_prefix("+,") {
            to_add.push(stripped.trim().to_string());
        } else {
            bail!(
                "Invalid line in diff file {}: {}",
                file_path.display(),
                line
            );
        }
    }

    Ok(FileDiffs {
        header_line,
        to_delete,
        to_add,
    })
}

/// Modify a string by applying diffs: removing lines and adding lines.
fn modify_string_with_diffs(original: &str, diffs: &FileDiffs) -> Result<String> {
    let mut modified = original.to_string();

    // Check that the headers match
    let original_header = original
        .lines()
        .next()
        .expect("Original string cannot be empty");
    ensure!(
        original_header == diffs.header_line,
        "Header line in diff file does not match original string"
    );

    // Apply deletions
    for item in &diffs.to_delete {
        ensure!(
            modified.contains(item),
            "Item to delete not found in original string: {item}"
        );
        modified = modified.replace(item, "");
    }

    // Apply additions
    for item in &diffs.to_add {
        modified.push_str(item);
    }

    Ok(modified)
}

pub fn patch_model<P: AsRef<Path>>(model_dir: P, diffs_dir: P) -> Result<PathBuf> {
    // Copy contents of model_dir to a teporary directory
    let temp_dir = tempdir().context("Failed to create temporary directory")?;
    let temp_path = temp_dir.path();

    for entry in fs::read_dir(model_dir.as_ref()).with_context(|| {
        format!(
            "Failed to read model directory: {}",
            model_dir.as_ref().display()
        )
    })? {
        let entry = entry?;
        let src_path = entry.path();

        // Only copy files (skip any subdirectories if present)
        if src_path.is_file() {
            let dst_path = temp_path.join(entry.file_name());
            fs::copy(&src_path, &dst_path)
                .with_context(|| format!("Failed to copy file: {}", src_path.display()))?;
        }
    }

    // Apply each patch from diffs_dir to the corresponding file in the temporary directory
    for entry in fs::read_dir(diffs_dir.as_ref()).with_context(|| {
        format!(
            "Failed to read diffs directory: {}",
            diffs_dir.as_ref().display()
        )
    })? {
        let entry = entry?;
        let diff_path = entry.path();

        // Only process files (skip any subdirectories if present)
        if diff_path.is_file() {
            let diff_filename = diff_path
                .file_name()
                .and_then(|name| name.to_str())
                .context("Failed to get diff filename")?;

            // Check that the filename ends with "_diff.csv"
            ensure!(
                diff_filename.ends_with("_diff.csv"),
                "Diff file must end with '_diff.csv': {diff_filename}"
            );

            // Extract the base filename (e.g., "agents" from "agents_diff.csv")
            let base_name = &diff_filename[..diff_filename.len() - "_diff.csv".len()];
            let target_filename = format!("{base_name}.csv");
            let file_path = temp_path.join(&target_filename);

            apply_patch_to_file(&file_path, &diff_path)?;
        }
    }

    // Return the path to the temporary directory
    Ok(temp_path.to_path_buf())
}

fn apply_patch_to_file(file_path: &Path, diff_path: &Path) -> Result<()> {
    let diffs = read_diffs(diff_path)?;
    let original = fs::read_to_string(file_path).with_context(|| input_err_msg(file_path))?;
    let modified = modify_string_with_diffs(&original, &diffs)?;
    fs::write(file_path, modified)?;
    Ok(())
}
