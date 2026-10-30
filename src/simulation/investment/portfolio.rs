//! Asset selection for portfolio agents
use super::DemandMap;
use crate::agent::Agent;
use crate::asset::AssetRef;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::prices::Prices;
use crate::units::Capacity;
use anyhow::Result;
use std::collections::HashMap;

mod constraints;
mod costs;
mod optimisation;
pub use optimisation::ResultsMap;
use optimisation::perform_optimisation;

/// Get the best assets for meeting demand for the given commodity
#[allow(clippy::too_many_arguments)]
pub fn select_best_assets(
    model: &Model,
    opt_assets: &[AssetRef],
    agent_addition_limits: &HashMap<ProcessID, Capacity>,
    agent_total_limits: &HashMap<ProcessID, Capacity>,
    commodity: &Commodity,
    agent: &Agent,
    region_id: &RegionID,
    prices: &Prices,
    demand: &DemandMap,
    year: u32,
    writer: &mut DataWriter,
) -> Result<Vec<AssetRef>> {
    // Perform optimisation
    let results = perform_optimisation(
        model,
        opt_assets,
        commodity,
        prices,
        year,
        agent_addition_limits,
        agent_total_limits,
        demand,
    )?;

    // Write optimisation results
    let commodity_id = &commodity.id;
    let agent_id = &agent.id;
    writer.write_appraisal_debug_info(
        year,
        &format!("{commodity_id} {region_id} {agent_id}"),
        &results,
    )?;

    // Assemble assets from optimisation result
    let mut selected_assets = Vec::new();
    for (asset, result) in results.capacities {
        // Solver noise can give tiny negative values
        let capacity = asset
            .capacity()
            .with_total_rounded_up(result.max(Capacity(0.0)));
        if capacity.is_zero() {
            continue; // existing assets left out here are mothballed by the pool
        }

        // Selected assets are unmothballed. This must happen before `set_capacity`.
        let mut selected = asset.with_no_mothballed_capacity();
        let inner = selected.make_mut();
        if inner.is_candidate() {
            inner.select_candidate_for_investment(agent.id.clone());
        }
        inner.set_capacity(capacity);
        selected_assets.push(selected);
    }

    Ok(selected_assets)
}
