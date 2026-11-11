//! The portfolio agent algorithm.
//!
//! Rather than selecting assets one at a time, a single optimisation chooses the capacities of all
//! existing and candidate assets at once so as to meet demand at least cost.
use crate::agent::Agent;
use crate::asset::{Asset, AssetCapacity, AssetRef};
use crate::commodity::Commodity;
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::{Process, ProcessID};
use crate::region::RegionID;
use crate::simulation::activity::Selection;
use crate::simulation::investment::{AssetOptions, DemandMap};
use crate::simulation::prices::Prices;
use crate::units::Capacity;
use anyhow::{Context, Result, ensure};
use itertools::chain;
use std::collections::HashMap;
use std::sync::Arc;

mod constraints;
mod costs;
mod debug;
mod optimisation;
use debug::write_debug_info;
use optimisation::perform_optimisation;

/// Run the portfolio algorithm for an agent to meet its share of demand for a commodity in a
/// region.
///
/// Committed assets (with fixed capacity), existing assets of the agent which produce the commodity
/// and one candidate for each available process are considered together in a single optimisation.
/// Committed assets are always returned unchanged.
///
/// # Arguments
///
/// * `model` - The model
/// * `agent` - The agent selecting assets
/// * `commodity` - The commodity for which demand must be met
/// * `region_id` - The region
/// * `demand` - The agent's share of demand for the commodity in the region
/// * `options` - Committed assets, existing assets and processes available to the agent
/// * `agent_addition_limits` - The agent's share of addition limits for candidate processes
/// * `agent_total_limits` - The agent's share of total capacity limits for processes
/// * `prices` - Commodity prices calculated from the previous agent activities
/// * `year` - Current milestone year
/// * `writer` - Data writer
///
/// # Returns
///
/// The assets selected (including committed assets and retained commissioned assets) along with
/// their activities from the optimisation, or an error. An error is also returned if demand cannot
/// be met.
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
    writer: &mut DataWriter,
) -> Result<Selection> {
    // Committed assets come first, followed by existing and candidate assets
    let num_committed = options.committed.len();
    let opt_assets = chain!(
        options.committed.iter().cloned(),
        options.existing.iter().cloned(),
        get_candidate_assets(&options.processes, region_id, year),
    )
    .collect::<Vec<_>>();

    let agent_id = &agent.id;
    let commodity_id = &commodity.id;

    // With no assets there is nothing to optimise (the LP would be empty)
    if opt_assets.is_empty() {
        let tolerance = model.parameters.remaining_demand_absolute_tolerance;
        ensure!(
            demand.values().all(|flow| *flow <= tolerance),
            "Agent '{agent_id}' cannot meet its demand for commodity '{commodity_id}' in region \
            '{region_id}' in year {year} with its existing assets"
        );
        return Ok(Selection::default());
    }

    // Perform optimisation
    let results = perform_optimisation(
        model,
        &opt_assets,
        num_committed,
        commodity,
        prices,
        year,
        agent_addition_limits,
        agent_total_limits,
        demand,
    )
    .with_context(|| {
        if options.processes.is_empty() {
            format!(
                "Agent '{agent_id}' cannot meet its demand for commodity '{commodity_id}' in \
                region '{region_id}' in year {year} with its existing assets"
            )
        } else {
            format!(
                "Portfolio optimisation failed for agent '{agent_id}', commodity \
                '{commodity_id}', region '{region_id}' in year {year}"
            )
        }
    })?;

    // Write optimisation results
    write_debug_info(
        writer,
        year,
        &format!("{commodity_id} {region_id} {agent_id}"),
        &results,
    )?;

    // Assemble assets from optimisation result
    // Committed assets are returned unchanged, even with zero activity
    let mut selected = options
        .committed
        .iter()
        .map(|asset| (asset.clone(), asset.clone()))
        .collect::<Vec<_>>();
    for (asset, result) in results.capacities.iter().skip(num_committed) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentID;
    use crate::asset::AssetPool;
    use crate::commodity::CommodityID;
    use crate::fixture::build_patched_simple_tempdir;
    use crate::input::load_model;
    use crate::units::Flow;
    use itertools::assert_equal;
    use tempfile::tempdir;

    /// Run the portfolio algorithm with only committed assets for the residential consumer in the
    /// simple example, with the given demand in each time slice. Returns the model's committed
    /// assets alongside the result.
    fn run_committed(demand_per_time_slice: Flow) -> (Vec<AssetRef>, Result<Selection>) {
        let model_dir = build_patched_simple_tempdir(Vec::new());
        let model = load_model(model_dir.path()).unwrap();
        let year = model.parameters.milestone_years[0];
        let mut pool = AssetPool::new();
        pool.commission_new(year, &mut model.user_assets.clone());

        let agent = &model.agents[&AgentID::from("A0_RES")];
        let commodity = &model.commodities[&CommodityID::from("RSHEAT")];
        let region_id = RegionID::from("GBR");
        let committed = pool
            .iter()
            .filter(|asset| asset.agent_id() == Some(&agent.id))
            .cloned()
            .collect::<Vec<_>>();
        let options = AssetOptions {
            committed: committed.clone(),
            ..AssetOptions::default()
        };
        let demand = model
            .time_slice_info
            .iter_selections_at_level(commodity.time_slice_level)
            .map(|selection| (selection, demand_per_time_slice))
            .collect();

        let dir = tempdir().unwrap();
        let mut writer = DataWriter::create(dir.path(), dir.path(), false).unwrap();
        let result = run_agent(
            &model,
            agent,
            commodity,
            &region_id,
            &demand,
            &options,
            &HashMap::new(),
            &HashMap::new(),
            &Prices::default(),
            year,
            &mut writer,
        );
        (committed, result)
    }

    #[test]
    fn run_agent_returns_committed_assets_unchanged() {
        let (committed, result) = run_committed(Flow(1.0));
        let selection = result.unwrap();

        assert_ne!(committed.len(), 0);
        assert_equal(&selection.assets, &committed);
        for asset in &committed {
            assert!(
                selection
                    .activity
                    .keys()
                    .any(|(activity_asset, _)| activity_asset == asset)
            );
        }
    }

    #[test]
    fn run_agent_errors_if_committed_assets_cannot_meet_demand() {
        let (_, result) = run_committed(Flow(1e12));
        let error = result.err().unwrap();
        assert_eq!(
            error.to_string(),
            "Agent 'A0_RES' cannot meet its demand for commodity 'RSHEAT' in region 'GBR' in \
            year 2020 with its existing assets"
        );
        assert!(error.chain().count() > 1);
    }
}
