//! Placeholder for the population agent algorithm.
use crate::agent::Agent;
use crate::commodity::Commodity;
use crate::model::Model;
use crate::output::DebugDataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::investment::{AssetOptions, DemandMap, Selection};
use crate::simulation::prices::Prices;
use crate::units::Capacity;
use anyhow::{Result, bail};
use std::collections::HashMap;

/// Run a population agent.
///
/// Population agents are not yet implemented, so this always returns an error.
#[allow(clippy::too_many_arguments, clippy::needless_pass_by_value)]
pub fn run_agent(
    _model: &Model,
    agent: &Agent,
    _commodity: &Commodity,
    _region_id: &RegionID,
    _demand: &DemandMap,
    _options: &AssetOptions,
    _agent_addition_limits: HashMap<ProcessID, Capacity>,
    _agent_total_limits: HashMap<ProcessID, Capacity>,
    _prices: &Prices,
    _year: u32,
    _debug_writer: Option<&mut DebugDataWriter>,
) -> Result<Selection> {
    bail!("Population agent '{}' is not yet implemented", agent.id)
}
