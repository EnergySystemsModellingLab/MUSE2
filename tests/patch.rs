//! Integration tests for the `validate` command.
use anyhow::Result;
use muse2::cli::handle_validate_command;
use muse2::model::ModelParameters;
use muse2::patch::{FilePatch, ModelPatch};
use muse2::settings::Settings;
use std::path::PathBuf;
use tempfile::TempDir;

/// Patch of the "simple" model with a change to the `assets.csv` file.
fn get_model_dir_file_patch() -> Result<TempDir> {
    let base_model_dir = PathBuf::from("examples/simple");

    // Small change to an asset capacity
    let assets_patch = FilePatch::new("assets.csv")
        .delete_row("GASDRV,GBR,A0_GEX,4002.26,2020")
        .add_row("GASDRV,GBR,A0_GEX,4003.26,2020");
    let model_patch = ModelPatch::new(&base_model_dir).with_file_patch(assets_patch);

    let temp_dir = model_patch.build_to_tempdir()?;
    Ok(temp_dir)
}

/// Patch of the "simple" model with a change to the `model.toml` file.
fn get_model_dir_toml_patch() -> Result<TempDir> {
    let base_model_dir = PathBuf::from("examples/simple");

    // Add an extra milestone year (2050)
    let mut toml_patch = toml::value::Table::new();
    toml_patch.insert(
        "milestone_years".to_string(),
        toml::Value::Array(vec![
            toml::Value::Integer(2020),
            toml::Value::Integer(2030),
            toml::Value::Integer(2040),
            toml::Value::Integer(2050),
        ]),
    );
    let model_patch = ModelPatch::new(&base_model_dir).with_toml_patch(toml_patch);
    let temp_dir = model_patch.build_to_tempdir()?;
    Ok(temp_dir)
}

#[test]
fn test_file_patch_and_validate() {
    unsafe { std::env::set_var("MUSE2_LOG_LEVEL", "off") };

    // Model is patched successfully
    let model_dir = get_model_dir_file_patch().unwrap();

    // The appropriate change has been made
    let assets_path = model_dir.path().join("assets.csv");
    let assets_content = std::fs::read_to_string(assets_path).unwrap();
    assert!(!assets_content.contains("GASDRV,GBR,A0_GEX,4002.26,2020"));
    assert!(assets_content.contains("GASDRV,GBR,A0_GEX,4003.26,2020"));

    // Validation passes
    handle_validate_command(&model_dir.path(), Some(Settings::default())).unwrap();
}

#[test]
fn test_toml_patch_and_validate() {
    unsafe { std::env::set_var("MUSE2_LOG_LEVEL", "off") };

    // Model is patched successfully
    let model_dir = get_model_dir_toml_patch().unwrap();

    // The appropriate change has been made
    let model_params = ModelParameters::from_path(&model_dir).unwrap();
    assert_eq!(model_params.milestone_years, vec![2020, 2030, 2040, 2050]);

    // Validation should fail
    let val = handle_validate_command(&model_dir.path(), Some(Settings::default()));
    assert!(val.is_err());
}
