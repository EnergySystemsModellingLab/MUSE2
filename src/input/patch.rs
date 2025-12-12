//! Code for applying patches/diffs to model input files.
use super::input_err_msg;

use anyhow::{Context, Result, bail, ensure};
use csv::{ReaderBuilder, Trim, Writer};
use indexmap::IndexSet;
use log::info;
use std::fs;
use std::path::Path;

/// Structure to hold diffs from a diff file
#[derive(Debug)]
pub struct FilePatch {
    /// The target base filename that this patch applies to (e.g. "agents.csv")
    base_filename: String,
    /// The header row (optional). If `None`, the header is not checked against base files.
    header_row: Option<Vec<String>>,
    /// Rows to delete (each row is a vector of canonicalized fields)
    to_delete: IndexSet<Vec<String>>,
    /// Rows to add (each row is a vector of canonicalized fields)
    to_add: IndexSet<Vec<String>>,
}

/// Build a canonical comma-joined string from an iterator of field strings.
fn canonicalize_fields<I, S>(fields: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    fields
        .into_iter()
        .map(|s| s.as_ref().trim().to_string())
        .collect::<Vec<_>>()
        .join(",")
}

/// Build a canonical vector of trimmed strings from an iterator of field strings.
fn canonicalize_vec<'a, I>(fields: I) -> Vec<String>
where
    I: IntoIterator<Item = &'a str>,
{
    fields.into_iter().map(|s| s.trim().to_string()).collect()
}

impl FilePatch {
    /// Create a new empty `Patch` with the given `base_filename`.
    pub fn new(base_filename: impl Into<String>) -> Self {
        let base_filename = base_filename.into();
        FilePatch {
            base_filename,
            header_row: None,
            to_delete: IndexSet::new(),
            to_add: IndexSet::new(),
        }
    }

    /// Set the header row for this patch (`header` should be a comma-joined string, e.g. "a,b,c").
    pub fn with_header(mut self, header: impl Into<String>) -> Self {
        let s = header.into();
        let v = s.split(',').map(|s| s.trim().to_string()).collect();
        self.header_row = Some(v);
        self
    }

    /// Add a row to the patch (row is a canonical comma-joined string, e.g. "a,b,c").
    /// This consumes and returns the `Patch` so calls can be chained.
    pub fn add_row(mut self, row: impl Into<String>) -> Self {
        let s = row.into();
        let v = s.split(',').map(|s| s.trim().to_string()).collect();
        self.to_add.insert(v);
        self
    }

    /// Mark a row for deletion from the base (row is a canonical comma-joined string).
    /// This consumes and returns the `Patch` so calls can be chained.
    pub fn delete_row(mut self, row: impl Into<String>) -> Self {
        let s = row.into();
        let v = s.split(',').map(|s| s.trim().to_string()).collect();
        self.to_delete.insert(v);
        self
    }

    /// Read a diff file and construct a `Patch`.
    pub fn from_file(file_path: &Path) -> Result<FilePatch> {
        // Extract the base filename by removing the `_diff.csv` suffix
        let file_name = file_path
            .file_name()
            .and_then(|n| n.to_str())
            .context("Invalid filename encoding")?;
        ensure!(
            file_name.to_lowercase().ends_with("_diff.csv"),
            "Diff file must end with '_diff.csv': {file_name}"
        );
        let base_name = &file_name[..file_name.len() - "_diff.csv".len()];
        let base_filename = format!("{base_name}.csv");

        // Read diff CSV file
        let mut reader = ReaderBuilder::new()
            .trim(Trim::All)
            .from_path(file_path)
            .with_context(|| input_err_msg(file_path))?;

        // Read header
        let diff_header = reader
            .headers()
            .with_context(|| input_err_msg(Path::new(&base_filename)))?;
        ensure!(!diff_header.is_empty(), "Diff file header cannot be empty");

        // Collect column headers (skip first column which is the diff indicator)
        let headers_vec: Vec<String> = diff_header
            .iter()
            .skip(1)
            .map(ToString::to_string)
            .collect();
        ensure!(
            !headers_vec.is_empty(),
            "Diff file must have at least one data column"
        );

        // Collect additions and deletions
        let mut to_delete: IndexSet<Vec<String>> = IndexSet::new();
        let mut to_add: IndexSet<Vec<String>> = IndexSet::new();
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

            // Build normalized row vector from the csv record
            let row_vec = canonicalize_vec(record.iter().skip(1));
            let row_str = canonicalize_fields(&row_vec);

            match diff_indicator {
                "-" => {
                    ensure!(
                        to_delete.insert(row_vec.clone()),
                        "Duplicate deletion entry: {row_str}",
                    );
                }
                "+" => {
                    ensure!(
                        to_add.insert(row_vec.clone()),
                        "Duplicate addition entry: {row_str}",
                    );
                }
                _ => {
                    bail!("Invalid diff indicator: '{diff_indicator}'. Must be '+' or '-'");
                }
            }
        }

        // Create Patch object
        Ok(FilePatch {
            base_filename,
            header_row: Some(headers_vec),
            to_delete,
            to_add,
        })
    }

    /// Apply this patch to a base model and return the modified CSV as a string.
    fn apply(&self, base_model_dir: &Path) -> Result<String> {
        // Read the base file to string
        let base_path = base_model_dir.join(&self.base_filename);
        if !base_path.exists() {
            bail!(
                "Base file for patching does not exist: {}",
                base_path.display()
            );
        }
        let base = fs::read_to_string(&base_path).with_context(|| input_err_msg(&base_path))?;

        // Apply the patch
        let modified = modify_base_with_patch(&base, self)?;
        Ok(modified)
    }

    /// Apply this patch to a base model and save the modified CSV to another directory.
    pub fn apply_and_save(&self, base_model_dir: &Path, new_model_dir: &Path) -> Result<()> {
        let modified = self.apply(base_model_dir)?;
        let new_path = new_model_dir.join(&self.base_filename);
        fs::write(&new_path, modified)
            .with_context(|| format!("Failed to write patched file: {}", new_path.display()))?;
        Ok(())
    }
}

/// Modify a base CSV file by applying diffs: removing rows and adding rows.
/// Preserves the order of rows from the base file, with new rows appended at the end.
fn modify_base_with_patch(base: &str, patch: &FilePatch) -> Result<String> {
    // Read base file from string, trimming whitespace
    let mut reader = ReaderBuilder::new()
        .trim(Trim::All)
        .from_reader(base.as_bytes());

    // Read and validate header
    let base_header = reader
        .headers()
        .context("Failed to read base file header")?;

    let base_header_vec: Vec<String> = base_header.iter().map(ToString::to_string).collect();

    // If the patch contains a header, compare it with the base file header.
    if let Some(ref header_row_vec) = patch.header_row {
        ensure!(
            base_header_vec == *header_row_vec,
            "Header mismatch: base file has [{}], diff file expects [{}]",
            base_header_vec.join(", "),
            header_row_vec.join(", ")
        );
    }

    // Read all rows from base file, preserving order and checking for duplicates
    let mut base_rows: IndexSet<Vec<String>> = IndexSet::new();
    for (line_num, result) in reader.records().enumerate() {
        let record = result.with_context(|| {
            format!("Error reading record at line {} in base file", line_num + 2)
        })?;

        // Create normalized row vector by trimming fields
        let row_vec = canonicalize_vec(record.iter());
        let row_str = canonicalize_fields(&row_vec);

        // Check for duplicates
        ensure!(
            base_rows.insert(row_vec.clone()),
            "Duplicate row at line {} in base file: {}",
            line_num + 2,
            row_str
        );
    }

    // Check that there's no overlap between additions and deletions
    for del_row in &patch.to_delete {
        ensure!(
            !patch.to_add.contains(del_row),
            "Row appears in both deletions and additions: {}",
            canonicalize_fields(del_row)
        );
    }

    // Apply deletions
    base_rows.retain(|row| !patch.to_delete.contains(row));

    // Apply additions (append to end, checking for duplicates)
    for add_row in &patch.to_add {
        ensure!(
            base_rows.insert(add_row.clone()),
            "Addition already present in base file: {add_row:?}"
        );
    }

    // Serialize CSV output using csv::Writer to ensure correct quoting/escaping
    let mut wtr = Writer::from_writer(vec![]);
    wtr.write_record(base_header_vec.iter())?;
    for row in &base_rows {
        let row_iter = row.iter().map(String::as_str);
        wtr.write_record(row_iter)?;
    }
    wtr.flush()?;
    let inner = wtr.into_inner()?;
    let output = String::from_utf8(inner)?;
    Ok(output)
}

/// Patch a base model directory with diffs from another directory and save to an output directory.
pub fn patch_model_to_path<P: AsRef<Path>, O: AsRef<Path>>(
    base_model_dir: P,
    diffs_dir: P,
    out_dir: O,
) -> Result<()> {
    info!(
        "Patching model at '{}' with diffs from '{}'",
        base_model_dir.as_ref().display(),
        diffs_dir.as_ref().display()
    );
    let out_path = out_dir.as_ref();

    // Copy all CSV files from the base model directory to the temporary directory
    // Some of these will be overwritten by the patched versions later.
    for entry in fs::read_dir(base_model_dir.as_ref())? {
        let entry = entry?;
        let src_path = entry.path();
        if src_path.is_file()
            && src_path
                .extension()
                .and_then(|e| e.to_str())
                .is_some_and(|ext| ext.eq_ignore_ascii_case("csv"))
        {
            let dst_path = out_path.join(entry.file_name());
            fs::copy(&src_path, &dst_path)
                .with_context(|| format!("Failed to copy file: {}", src_path.display()))?;
        }
    }

    // Read and merge `model.toml` from the base model and the diffs directory, then
    // write the merged result into the output directory.
    let base_toml_src = base_model_dir.as_ref().join("model.toml");
    let patch_toml_path = diffs_dir.as_ref().join("model.toml");
    let merged_toml = read_toml_with_patch(&base_toml_src, &patch_toml_path)?;
    let merged_str = toml::to_string_pretty(&merged_toml)?;
    fs::write(out_path.join("model.toml"), merged_str)?;

    // Read all patch files into memory first. CSV diffs are parsed into `FilePatch` entries;
    // any non-CSV files (e.g. README.txt) are copied from the diffs directory into the
    // temporary model directory so they override/add to the patched model.
    let mut patches = Vec::new();
    for entry in fs::read_dir(diffs_dir.as_ref())? {
        let entry = entry?;
        let diff_path = entry.path();

        if !diff_path.is_file() {
            continue;
        }

        // If the file is a CSV, parse it as a diff. Otherwise, copy the file into the temp dir.
        match diff_path.extension().and_then(|e| e.to_str()) {
            Some(ext) if ext.eq_ignore_ascii_case("csv") => {
                // Read diffs and push to vector (FilePatch::from_file validates `_diff.csv` suffix)
                let patch = FilePatch::from_file(&diff_path).with_context(|| {
                    format!("Failed to read diff file: {}", diff_path.display())
                })?;
                patches.push(patch);
            }
            _ => {
                // Copy non-CSV file (e.g., README) into the temporary patched model directory
                let dst_path = out_path.join(entry.file_name());
                fs::copy(&diff_path, &dst_path).with_context(|| {
                    format!("Failed to copy diff asset: {}", diff_path.display())
                })?;
            }
        }
    }

    // Apply each patch to its corresponding base file and write to temp dir
    for patch in &patches {
        patch
            .apply_and_save(base_model_dir.as_ref(), out_path)
            .with_context(|| format!("Failed to apply patch to file: {}", patch.base_filename))?;
    }

    info!(
        "Patching complete. Patched model saved to '{}'",
        out_path.display()
    );

    Ok(())
}

/// Read `base_path` and `patch_path` TOML files, merge top-level fields from the patch
/// into the base
fn read_toml_with_patch(base_path: &Path, patch_path: &Path) -> Result<toml::Value> {
    // Read base TOML
    let base_str = fs::read_to_string(base_path).with_context(|| input_err_msg(base_path))?;
    let mut base_data: toml::Value =
        toml::from_str(&base_str).with_context(|| input_err_msg(base_path))?;

    // Read patch TOML
    let patch_str = fs::read_to_string(patch_path).with_context(|| input_err_msg(patch_path))?;
    let patch_data: toml::Value =
        toml::from_str(&patch_str).with_context(|| input_err_msg(patch_path))?;

    let base_table = base_data.as_table_mut().expect("Base TOML must be a table");
    let patch_table = patch_data.as_table().expect("Patch TOML must be a table");

    // Merge the patch into the base, skipping `base_model`, and prioritizing patch values
    for (key, patch_val) in patch_table {
        if key == "base_model" {
            continue;
        }
        base_table.insert(key.clone(), patch_val.clone());
    }

    Ok(base_data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn test_read_diffs_basic() {
        // Create diff file
        let temp_dir = tempdir().unwrap();
        let diff_file = temp_dir.path().join("test_diff.csv");
        let content = "diff,col1,col2\n-,val1,val2\n+,val3,val4\n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        // Parse from the file
        let diffs = FilePatch::from_file(&diff_file).unwrap();
        assert_eq!(
            diffs.header_row.as_ref().map(|v| v.join(",")),
            Some("col1,col2".to_string())
        );
        assert_eq!(diffs.to_delete.len(), 1);
        assert_eq!(diffs.to_add.len(), 1);
        let del_row = vec!["val1".to_string(), "val2".to_string()];
        let add_row = vec!["val3".to_string(), "val4".to_string()];
        assert!(diffs.to_delete.contains(&del_row));
        assert!(diffs.to_add.contains(&add_row));
    }

    #[test]
    fn test_read_diffs_with_whitespace() {
        // Create diff file with extra whitespace
        let temp_dir = tempdir().unwrap();
        let diff_file = temp_dir.path().join("test_diff.csv");
        let content = " diff , col1 , col2 \n-,  item1  ,  item2  \n+,  another1  ,  another2  \n";
        let mut file = fs::File::create(&diff_file).unwrap();
        file.write_all(content.as_bytes()).unwrap();

        // Parse from the file
        let diffs_from_file = FilePatch::from_file(&diff_file).unwrap();
        assert_eq!(
            diffs_from_file.header_row.as_ref().map(|v| v.join(",")),
            Some("col1,col2".to_string())
        );
        assert_eq!(diffs_from_file.to_delete.len(), 1);
        assert_eq!(diffs_from_file.to_add.len(), 1);
        let del_row = vec!["item1".to_string(), "item2".to_string()];
        let add_row = vec!["another1".to_string(), "another2".to_string()];
        assert!(diffs_from_file.to_delete.contains(&del_row));
        assert!(diffs_from_file.to_add.contains(&add_row));
    }

    #[test]
    fn test_modify_base_with_diffs_preserves_order() {
        let base = "col1,col2\nrow1,row2\nrow3,row4\nrow5,row6\n";

        let patch = FilePatch::new("test.csv")
            .with_header("col1,col2")
            .delete_row("row3,row4")
            .add_row("row7,row8");

        let modified = modify_base_with_patch(base, &patch).unwrap();

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
        let patch = FilePatch::new("test.csv").with_header("col1,col3");

        let result = modify_base_with_patch(base, &patch);
        assert_error!(
            result,
            "Header mismatch: base file has [col1, col2], diff file expects [col1, col3]"
        );
    }
}
