//! The portfolio agent algorithm.
//!
//! Rather than selecting assets one at a time, a single optimisation chooses the capacities of all
//! existing and candidate assets at once so as to meet demand at least cost.
use crate::agent::Agent;
use crate::asset::{Asset, AssetCapacity, AssetRef};
use crate::commodity::Commodity;
use crate::model::Model;
use crate::output::DebugDataWriter;
use crate::process::{Process, ProcessID};
use crate::region::RegionID;
use crate::simulation::activity::Selection;
use crate::simulation::investment::{AssetOptions, DemandMap};
use crate::simulation::prices::Prices;
use crate::units::Capacity;
use anyhow::Result;
use itertools::chain;
use std::collections::HashMap;
use std::sync::Arc;

mod constraints;
mod costs;
mod optimisation;
mod output;
use optimisation::perform_optimisation;
use output::write_debug_info;

/// Run the portfolio algorithm for an agent to meet its share of demand for a commodity in a
/// region.
///
/// Existing assets of the agent which produce the commodity and one candidate for each available
/// process are considered together in a single optimisation.
///
/// # Arguments
///
/// * `model` - The model
/// * `agent` - The agent selecting assets
/// * `commodity` - The commodity for which demand must be met
/// * `region_id` - The region
/// * `demand` - The agent's share of demand for the commodity in the region
/// * `options` - Existing assets and processes available to the agent
/// * `agent_addition_limits` - The agent's share of addition limits for candidate processes
/// * `agent_total_limits` - The agent's share of total capacity limits for processes
/// * `prices` - Commodity prices from the previous full system dispatch
/// * `year` - Current milestone year
/// * `debug_writer` - Optional debug writer
///
/// # Returns
///
/// The assets selected (including retained commissioned assets) along with their activities from
/// the optimisation, or an error.
#[allow(clippy::too_many_arguments)]
pub fn run_agent(
    model: &Model,
    agent: &Agent,
    commodity: &Commodity,
    region_id: &RegionID,
    demand: &DemandMap,
    options: &AssetOptions,
    agent_addition_limits: &HashMap<ProcessID, Capacity>,
    agent_total_limits: &HashMap<ProcessID, Capacity>,
    prices: &Prices,
    year: u32,
    writer: Option<&mut DebugDataWriter>,
) -> Result<Selection> {
    debug_assert_eq!(options.committed.len(), 0);

    // Existing and candidate assets from which to choose
    let opt_assets = chain(
        options.existing.iter().cloned(),
        get_candidate_assets(&options.processes, region_id, year),
    )
    .collect::<Vec<_>>();

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
    let mut selected = Vec::new();
    for (asset, result) in &results.capacities {
        // Solver noise can give tiny negative values
        let capacity = asset
            .capacity()
            .with_total_rounded_up(result.max(Capacity(0.0)));
        if capacity.is_zero() {
            continue; // existing assets left out here are mothballed by the pool
        }

        // Selected assets are unmothballed. This must happen before `set_capacity`.
        let mut selected_asset = asset.clone().with_no_mothballed_capacity();
        let inner = selected_asset.make_mut();
        if inner.is_candidate() {
            inner.select_candidate_for_investment(agent.id.clone());
        }
        inner.set_capacity(capacity);
        selected.push((asset.clone(), selected_asset));
    }

    Ok(Selection::from_optimisation(selected, &results.activities))
}

/// Get candidate assets which produce a particular commodity from the given processes.
///
/// Candidates have zero capacity, as the investment optimisation determines how much to build.
/// Capacity is discrete (in tranches of `tranche_size`) for processes with a defined
/// `tranche_size` and continuous otherwise.
fn get_candidate_assets<'a>(
    processes: &'a [Arc<Process>],
    region_id: &'a RegionID,
    year: u32,
) -> impl Iterator<Item = AssetRef> + 'a {
    processes.iter().map(move |process| {
        let tranche_size = process.tranche_size.unwrap_or(Capacity(0.0));
        let mut asset =
            Asset::new_candidate(process.clone(), region_id.clone(), tranche_size, year).unwrap();
        if process.tranche_size.is_none() {
            asset.set_capacity(AssetCapacity::continuous(Capacity(0.0)));
        }
        asset.into()
    })
}
