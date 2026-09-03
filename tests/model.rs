//! Integration tests for model loading and simulation.
use muse2::input::load_model;
use muse2::patch::{FilePatch, ModelPatch};
use muse2::simulation;
use tempfile::tempdir;

#[test]
fn commodity_constraints_infeasibility_is_reported() {
    // The `missing_commodity` model has no BIOPRD-producing assets in the base year, so
    // enforcing positive production of BIOPRD should make the model infeasible.
    let model_dir = ModelPatch::from_example("missing_commodity")
        .with_toml_patch("please_give_me_broken_results = true")
        .with_file_patch(
            FilePatch::new("commodity_constraints.csv").with_replacement(&[
                "commodity_id,region_id,balance_type,years,time_slice,limits",
                "BIOPRD,GBR,prod,2020,annual,0.0001..",
            ]),
        )
        .build_to_tempdir()
        .unwrap();
    let model = load_model(model_dir.path()).unwrap();
    let output_dir = tempdir().unwrap();

    let error = simulation::run(&model, output_dir.path(), true).unwrap_err();
    let message = format!("{error:#}");

    assert!(
        message.contains("The infeasibility is likely caused by one or more constraints defined in `commodity_constraints.csv`"),
        "{message}"
    );
}
