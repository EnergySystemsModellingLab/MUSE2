//! Functionality for running the MUSE2 simulation across milestone years.
use crate::asset::{Asset, AssetRef};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessMap;
use crate::units::Capacity;
use anyhow::Result;
use std::path::Path;
use std::sync::Arc;

pub mod activity;
pub mod investment;
pub mod market;
pub mod prices;
pub use prices::PriceMap;
pub mod system_dispatch;

/// Run the simulation.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `output_path` - The folder to which output files will be written
/// * `debug_model` - Whether to write additional information (e.g. duals) to output files
pub fn run(model: &Model, output_path: &Path, debug_model: bool) -> Result<()> {
    let mut writer = DataWriter::create(output_path, &model.model_path, debug_model)?;
    system_dispatch::run(model, &mut writer)?;
    writer.flush()
}

/// Create candidate assets for all potential processes in a specified year
pub fn candidate_assets_for_next_year(
    processes: &ProcessMap,
    next_year: Option<u32>,
    candidate_asset_capacity: Capacity,
) -> Vec<AssetRef> {
    let mut candidates = Vec::new();
    let Some(next_year) = next_year else {
        return candidates;
    };

    for process in processes
        .values()
        .filter(move |process| process.active_for_year(next_year))
    {
        for region_id in &process.regions {
            candidates.push(
                Asset::new_candidate(
                    Arc::clone(process),
                    region_id.clone(),
                    candidate_asset_capacity,
                    next_year,
                )
                .unwrap()
                .into(),
            );
        }
    }

    candidates
}
