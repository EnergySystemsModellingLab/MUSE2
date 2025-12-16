//! Code for applying patches to model input files.
use anyhow::{Context, Result, ensure};
use csv::{ReaderBuilder, Trim, Writer};
use indexmap::IndexSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Struct to hold a set of patches to apply to a base model.
pub struct ModelPatch {
    // The base model directory path
    base_model_dir: PathBuf,
    // The list of file patches to apply
    file_patches: Vec<FilePatch>,
    // Optional patch for model.toml (TOML table)
    toml_patch: Option<toml::value::Table>,
}

impl ModelPatch {
    /// Create a new empty `ModelPatch` for a base model at the given directory.
    pub fn new<P: Into<PathBuf>>(base_model_dir: P) -> Self {
        ModelPatch {
            base_model_dir: base_model_dir.into(),
            file_patches: Vec::new(),
            toml_patch: None,
        }
    }

    /// Add a single `FilePatch` to this `ModelPatch`.
    pub fn with_file_patch(mut self, patch: FilePatch) -> Self {
        self.file_patches.push(patch);
        self
    }

    /// Add multiple `FilePatch` entries to this `ModelPatch`.
    pub fn with_file_patches<I>(mut self, patches: I) -> Self
    where
        I: IntoIterator<Item = FilePatch>,
    {
        self.file_patches.extend(patches);
        self
    }

    /// Add a TOML patch (provided as a string) to this `ModelPatch`.
    /// The string will be parsed into a `toml::value::Table`.
    pub fn with_toml_patch(mut self, patch_str: impl AsRef<str>) -> Self {
        assert!(
            self.toml_patch.is_none(),
            "TOML patch already set for this ModelPatch"
        );
        let s = patch_str.as_ref();
        let patch: toml::value::Table =
            toml::from_str(s).expect("Failed to parse string passed to with_toml_patch");
        assert!(
            !patch.contains_key("base_model"),
            "TOML patch must not contain a `base_model` field"
        );
        self.toml_patch = Some(patch);
        self
    }

    /// Build this `ModelPatch` into `out_dir` (creating/overwriting files there).
    fn build<O: AsRef<Path>>(&self, out_dir: O) -> Result<()> {
        let base_dir = self.base_model_dir.as_path();
        let out_path = out_dir.as_ref();

        // Apply toml patch (if any), or copy model.toml unchanged from the base model
        let base_toml_path = base_dir.join("model.toml");
        let out_toml_path = out_path.join("model.toml");
        if let Some(toml_patch) = &self.toml_patch {
            let toml_content = fs::read_to_string(&base_toml_path)?;
            let merged_toml = merge_model_toml(&toml_content, toml_patch)?;
            fs::write(&out_toml_path, merged_toml)?;
        } else {
            fs::copy(&base_toml_path, &out_toml_path)?;
        }

        // Copy all CSV files from the base model into the output directory
        // Any files with associated patches will be overwritten later
        for entry in fs::read_dir(base_dir)? {
            let entry = entry?;
            let src_path = entry.path();
            if src_path.is_file()
                && src_path
                    .extension()
                    .and_then(|e| e.to_str())
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("csv"))
            {
                let dst_path = out_path.join(entry.file_name());
                fs::copy(&src_path, &dst_path)?;
            }
        }

        // Apply file patches
        for patch in &self.file_patches {
            patch.apply_and_save(base_dir, out_path)?;
        }

        Ok(())
    }

    /// Build the patched model into a temporary directory and return the `TempDir`.
    pub fn build_to_tempdir(&self) -> Result<tempfile::TempDir> {
        let temp_dir = tempfile::tempdir()?;
        self.build(temp_dir.path())?;
        Ok(temp_dir)
    }
}

/// Structure to hold patches for a model csv file.
#[derive(Debug)]
pub struct FilePatch {
    /// The file that this patch applies to (e.g. "agents.csv")
    filename: String,
    /// The header row (optional). If `None`, the header is not checked against base files.
    header_row: Option<Vec<String>>,
    /// Rows to delete (each row is a vector of fields)
    to_delete: IndexSet<Vec<String>>,
    /// Rows to add (each row is a vector of fields)
    to_add: IndexSet<Vec<String>>,
}

impl FilePatch {
    /// Create a new empty `Patch` for the given file.
    pub fn new(filename: impl Into<String>) -> Self {
        FilePatch {
            filename: filename.into(),
            header_row: None,
            to_delete: IndexSet::new(),
            to_add: IndexSet::new(),
        }
    }

    /// Set the header row for this patch (header should be a comma-joined string, e.g. "a,b,c").
    pub fn with_header(mut self, header: impl Into<String>) -> Self {
        assert!(
            self.header_row.is_none(),
            "Header already set for this FilePatch",
        );
        let s = header.into();
        let v = s.split(',').map(|s| s.trim().to_string()).collect();
        self.header_row = Some(v);
        self
    }

    /// Add a row to the patch (row should be a comma-joined string, e.g. "a,b,c").
    pub fn add_row(mut self, row: impl Into<String>) -> Self {
        let s = row.into();
        let v = s.split(',').map(|s| s.trim().to_string()).collect();
        self.to_add.insert(v);
        self
    }

    /// Mark a row for deletion from the base (row should be a comma-joined string, e.g. "a,b,c").
    pub fn delete_row(mut self, row: impl Into<String>) -> Self {
        let s = row.into();
        let v = s.split(',').map(|s| s.trim().to_string()).collect();
        self.to_delete.insert(v);
        self
    }

    /// Apply this patch to a base model and return the modified CSV as a string.
    fn apply(&self, base_model_dir: &Path) -> Result<String> {
        // Read the base file to string
        let base_path = base_model_dir.join(&self.filename);
        ensure!(
            base_path.exists() && base_path.is_file(),
            "Base file for patching does not exist: {}",
            base_path.display()
        );
        let base = fs::read_to_string(&base_path)?;

        // Apply the patch
        let modified = modify_base_with_patch(&base, self)
            .with_context(|| format!("Error applying patch to file: {}", self.filename))?;
        Ok(modified)
    }

    /// Apply this patch to a base model and save the modified CSV to another directory.
    pub fn apply_and_save(&self, base_model_dir: &Path, out_model_dir: &Path) -> Result<()> {
        let modified = self.apply(base_model_dir)?;
        let new_path = out_model_dir.join(&self.filename);
        fs::write(&new_path, modified)?;
        Ok(())
    }
}

/// Merge a TOML patch into a base TOML string and return the merged TOML.
fn merge_model_toml(base_toml: &str, patch: &toml::value::Table) -> Result<String> {
    ensure!(
        !patch.contains_key("base_model"),
        "TOML patch must not contain a `base_model` field"
    );

    // Parse base TOML into a table
    let mut base_val: toml::Value = toml::from_str(base_toml)?;
    let base_tbl = base_val
        .as_table_mut()
        .context("Base model TOML must be a table")?;

    // Apply patch entries
    for (k, v) in patch {
        base_tbl.insert(k.clone(), v.clone());
    }

    // Serialize merged TOML back to string
    let out = toml::to_string_pretty(&base_val)?;
    Ok(out)
}

/// Modify a string representation of a base CSV file by applying a `FilePatch`.
/// Preserves the order of rows from the base file, with new rows appended at the end.
fn modify_base_with_patch(base: &str, patch: &FilePatch) -> Result<String> {
    // Read base string, trimming whitespace
    let mut reader = ReaderBuilder::new()
        .trim(Trim::All)
        .from_reader(base.as_bytes());

    // Extract header from the base string
    let base_header = reader
        .headers()
        .context("Failed to read base file header")?;
    let base_header_vec: Vec<String> = base_header.iter().map(ToString::to_string).collect();

    // If the patch contains a header, compare it with the base header.
    if let Some(ref header_row_vec) = patch.header_row {
        ensure!(
            base_header_vec == *header_row_vec,
            "Header mismatch: base file has [{}], patch has [{}]",
            base_header_vec.join(", "),
            header_row_vec.join(", ")
        );
    }

    // Read all rows from the base, preserving order and checking for duplicates
    let mut base_rows: IndexSet<Vec<String>> = IndexSet::new();
    for result in reader.records() {
        let record = result?;

        // Create normalized row vector by trimming fields
        let row_vec = record
            .iter()
            .map(|s| s.trim().to_string())
            .collect::<Vec<_>>();

        // Check for duplicates
        ensure!(
            base_rows.insert(row_vec.clone()),
            "Duplicate row in base file: {row_vec:?}",
        );
    }

    // Check that there's no overlap between additions and deletions
    for del_row in &patch.to_delete {
        ensure!(
            !patch.to_add.contains(del_row),
            "Row appears in both deletions and additions: {del_row:?}",
        );
    }

    // Ensure every row requested for deletion actually exists in the base file.
    for del_row in &patch.to_delete {
        ensure!(
            base_rows.contains(del_row),
            "Row to delete not present in base file: {del_row:?}"
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

    // Serialize CSV output using csv::Writer
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;

    #[test]
    fn test_modify_base_with_patch() {
        let base = "col1,col2\nvalue1,value2\nvalue3,value4\nvalue5,value6\n";

        // Create a patch to delete row3,row4 and add row7,row8
        let patch = FilePatch::new("test.csv")
            .with_header("col1,col2")
            .delete_row("value3,value4")
            .add_row("value7,value8");

        let modified = modify_base_with_patch(base, &patch).unwrap();

        let lines: Vec<&str> = modified.lines().collect();
        assert_eq!(lines[0], "col1,col2"); // header is present
        assert_eq!(lines[1], "value1,value2"); // unchanged row
        assert_eq!(lines[2], "value5,value6"); // unchanged row
        assert_eq!(lines[3], "value7,value8"); // added row
        assert!(!modified.contains("value3,value4")); // deleted row
    }

    #[test]
    fn test_modify_base_with_patch_mismatched_header() {
        let base = "col1,col2\nvalue1,value2\n";

        // Create a patch with a mismatched header
        let patch = FilePatch::new("test.csv").with_header("col1,col3");

        assert_error!(
            modify_base_with_patch(base, &patch),
            "Header mismatch: base file has [col1, col2], patch has [col1, col3]"
        );
    }

    #[test]
    fn test_merge_model_toml_basic() {
        let base = r#"
            field = "data"
            [section]
            a = 1
        "#;

        // Create a TOML patch
        let mut patch = toml::value::Table::new();
        patch.insert(
            "field".to_string(),
            toml::Value::String("patched".to_string()),
        );
        patch.insert(
            "new_field".to_string(),
            toml::Value::String("added".to_string()),
        );

        // Apply patch with `merge_model_toml`
        // Should overwrite field and add new_field, but keep section.a
        let merged = merge_model_toml(base, &patch).unwrap();
        assert!(merged.contains("field = \"patched\""));
        assert!(merged.contains("[section]"));
        assert!(merged.contains("new_field = \"added\""));
    }

    #[test]
    fn test_merge_rejects_base_model_key() {
        let base = r#"field = "data""#;

        // Create a TOML patch with a base_model key
        let mut patch = toml::value::Table::new();
        patch.insert(
            "base_model".to_string(),
            toml::Value::String("..".to_string()),
        );

        // `merge_model_toml` should return an error
        assert_error!(
            merge_model_toml(base, &patch),
            "TOML patch cannot contain a `base_model` field"
        );
    }
}
