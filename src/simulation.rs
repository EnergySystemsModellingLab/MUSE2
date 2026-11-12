//! Functionality for running the MUSE2 simulation across milestone years.
//!
//! How each milestone year is simulated is determined by the [`SimulationProtocol`], selected with
//! the `simulation_protocol` setting in `model.toml`. Each protocol lives in its own submodule
//! (e.g. [`system_dispatch`]) and provides a `run` function which loops over the milestone years.
//! Protocols share the agent investment code in [`investment`] and [`market`], and the price
//! calculation code in [`prices`]. The only protocol-specific part of the agent investment loop is
//! how the flows of newly selected assets are determined, which protocols supply by implementing
//! [`investment::FlowEstimator`].
//!
//! # Adding a new protocol
//!
//! 1. Add a variant to [`SimulationProtocol`], and a match arm in [`run`]
//! 2. Create a submodule with a `pub fn run(model: &Model, writer: &mut DataWriter) -> Result<()>`
//!    function, and implement [`investment::FlowEstimator`] if it uses [`investment::run_agents_for_year`]
//! 3. If the protocol has its own debug output files, write them with
//!    [`DataWriter::write_debug_rows`]
//! 4. Add the new value to the `simulation_protocol` entry in `schemas/input/model.yaml`, noting any
//!    settings which are only used by the new protocol
//! 5. Document the protocol in `docs/model/README.md`, and add a release note
//! 6. Add regression tests for the protocol
use crate::asset::{Asset, AssetRef};
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessMap;
use crate::units::Capacity;
use anyhow::Result;
use log::info;
use serde::Deserialize;
use std::path::Path;
use std::sync::Arc;

pub mod activity;
pub mod investment;
pub mod market;
pub mod prices;
pub use prices::PriceMap;
pub mod system_dispatch;

/// The protocol used to simulate each milestone year, selected with `simulation_protocol` in
/// `model.toml`
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize, strum::Display)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum SimulationProtocol {
    /// Agents choose capacity; flows and prices come from a system-wide cost-minimising dispatch
    #[default]
    SystemDispatch,
}

/// Run the simulation.
///
/// # Arguments:
///
/// * `model` - The model to run
/// * `output_path` - The folder to which output files will be written
/// * `debug_model` - Whether to write additional information (e.g. duals) to output files
pub fn run(model: &Model, output_path: &Path, debug_model: bool) -> Result<()> {
    let mut writer = DataWriter::create(output_path, &model.model_path, debug_model)?;
    let protocol = model.parameters.simulation_protocol;
    info!("Simulation protocol: {protocol}");
    match protocol {
        SimulationProtocol::SystemDispatch => system_dispatch::run(model, &mut writer)?,
    }
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
