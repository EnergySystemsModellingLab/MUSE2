//! The portfolio asset selection algorithm.
//!
//! Rather than selecting assets one at a time, a single optimisation chooses the capacities of all
//! existing and candidate assets at once so as to meet demand at least cost.
use crate::agent::Agent;
use crate::asset::{Asset, AssetCapacity, AssetIterator, AssetRef};
use crate::commodity::Commodity;
use crate::model::Model;
use crate::output::DebugDataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::investment::DemandMap;
use crate::simulation::prices::Prices;
use crate::units::Capacity;
use anyhow::Result;
use indexmap::IndexSet;
use itertools::chain;
use std::collections::HashMap;

mod constraints;
mod costs;
mod debug;
mod optimisation;
use debug::write_debug_info;
use optimisation::perform_optimisation;

/// Select assets for an agent to meet its share of demand for a commodity in a region.
///
/// Existing assets of the agent which produce the commodity and one candidate for each process in
/// the agent's search space are considered together in a single optimisation. Processes in
/// `excluded_processes` are not considered.
///
/// # Arguments
///
/// * `model` - The model
/// * `agent` - The agent selecting assets
/// * `commodity` - The commodity for which demand must be met
/// * `region_id` - The region
/// * `demand` - The agent's share of demand for the commodity in the region
/// * `existing_assets` - The asset pool (commissioned and otherwise)
/// * `excluded_processes` - Processes which may not be selected
/// * `agent_addition_limits` - The agent's share of addition limits for candidate processes
/// * `agent_total_limits` - The agent's share of total capacity limits for processes
/// * `prices` - Commodity prices from the previous full system dispatch
/// * `year` - Current milestone year
/// * `debug_writer` - Optional debug writer
///
/// # Returns
///
/// The assets selected (including retained commissioned assets), or an error.
#[allow(clippy::too_many_arguments)]
pub fn select_assets(
    model: &Model,
    agent: &Agent,
    commodity: &Commodity,
    region_id: &RegionID,
    demand: &DemandMap,
    existing_assets: &[AssetRef],
    excluded_processes: &IndexSet<ProcessID>,
    agent_addition_limits: &HashMap<ProcessID, Capacity>,
    agent_total_limits: &HashMap<ProcessID, Capacity>,
    prices: &Prices,
    year: u32,
    writer: Option<&mut DebugDataWriter>,
) -> Result<Vec<AssetRef>> {
    // Existing and candidate assets from which to choose
    let mut opt_assets =
        get_asset_options(existing_assets, agent, commodity, region_id, year).collect::<Vec<_>>();

    // Exclude certain processes from the options
    opt_assets.retain(|asset| !excluded_processes.contains(asset.process_id()));

    // Perform optimisation
    let results = perform_optimisation(
        model,
        &opt_assets,
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
    write_debug_info(
        writer,
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

/// Get options from existing and potential assets for the given parameters
fn get_asset_options<'a>(
    all_existing_assets: &'a [AssetRef],
    agent: &'a Agent,
    commodity: &'a Commodity,
    region_id: &'a RegionID,
    year: u32,
) -> impl Iterator<Item = AssetRef> + 'a {
    // Get existing assets which produce the commodity of interest
    let existing_assets = all_existing_assets
        .iter()
        .filter_agent(&agent.id)
        .filter_region(region_id)
        .filter_primary_producers_of(&commodity.id)
        .cloned();

    // Get candidates assets which produce the commodity of interest
    let candidate_assets = get_candidate_assets(agent, region_id, commodity, year);

    chain(existing_assets, candidate_assets)
}

/// Get candidate assets which produce a particular commodity for a given agent
///
/// Candidates have zero capacity, as the investment optimisation determines how much to build.
/// Capacity is discrete (in tranches of `tranche_size`) for processes with a defined
/// `tranche_size` and continuous otherwise.
fn get_candidate_assets<'a>(
    agent: &'a Agent,
    region_id: &'a RegionID,
    commodity: &'a Commodity,
    year: u32,
) -> impl Iterator<Item = AssetRef> + 'a {
    agent
        .iter_search_space(region_id, &commodity.id, year)
        .map(move |process| {
            let tranche_size = process.tranche_size.unwrap_or(Capacity(0.0));
            let mut asset =
                Asset::new_candidate(process.clone(), region_id.clone(), tranche_size, year)
                    .unwrap();
            if process.tranche_size.is_none() {
                asset.set_capacity(AssetCapacity::continuous(Capacity(0.0)));
            }
            asset.into()
        })
}
