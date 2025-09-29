//! Code for reading the agent search space CSV file.
use super::super::{input_err_msg, read_csv_optional, try_insert};
use crate::agent::{Agent, AgentID, AgentMap, AgentSearchSpaceMap};
use crate::commodity::CommodityID;
use crate::id::IDCollection;
use crate::process::{Process, ProcessMap};
use crate::year::parse_year_str;
use anyhow::{Context, Result};
use itertools::Itertools;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

const AGENT_SEARCH_SPACE_FILE_NAME: &str = "agent_search_space.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct AgentSearchSpaceRaw {
    /// The agent to apply the search space to.
    agent_id: String,
    /// The commodity to apply the search space to.
    commodity_id: String,
    /// The year(s) to apply the search space to.
    years: String,
    /// The processes that the agent will consider investing in. Expressed as process IDs separated
    /// by semicolons or `None`, meaning all processes.
    search_space: String,
}

/// Search space for an agent
#[derive(Debug)]
struct AgentSearchSpace {
    /// The agent to which this search space applies
    agent_id: AgentID,
    /// The commodity to apply the search space to
    commodity_id: CommodityID,
    /// The year(s) the objective is relevant for
    years: Vec<u32>,
    /// The agent's search space
    search_space: Rc<Vec<Rc<Process>>>,
}

impl AgentSearchSpaceRaw {
    fn into_agent_search_space(
        self,
        agents: &AgentMap,
        processes: &ProcessMap,
        commodity_ids: &HashSet<CommodityID>,
        milestone_years: &[u32],
    ) -> Result<AgentSearchSpace> {
        // Parse search_space string
        let search_space = Rc::new(parse_search_space_str(&self.search_space, processes)?);

        // Get commodity
        let commodity_id = commodity_ids.get_id(&self.commodity_id)?;

        // Check that the year is a valid milestone year
        let year = parse_year_str(&self.years, milestone_years)?;

        let agent_id = agents.get_id(&self.agent_id)?;

        Ok(AgentSearchSpace {
            agent_id: agent_id.clone(),
            commodity_id: commodity_id.clone(),
            years: year,
            search_space,
        })
    }
}

/// Parse a string representing the processes the agent will invest in.
///
/// This string can either be:
///  * Empty, meaning all processes
///  * "all", meaning the same
///  * A list of process IDs separated by semicolons
fn parse_search_space_str(search_space: &str, processes: &ProcessMap) -> Result<Vec<Rc<Process>>> {
    let search_space = search_space.trim();
    if search_space.is_empty() || search_space.eq_ignore_ascii_case("all") {
        Ok(processes.values().cloned().collect())
    } else {
        search_space
            .split(';')
            .map(|id| {
                let process = processes
                    .get(id.trim())
                    .with_context(|| format!("Invalid process '{id}'"))?;
                Ok(process.clone())
            })
            .try_collect()
    }
}

/// Read agent search space info from the `agent_search_space.csv` file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `agents` - Map of agents
/// * `processes` - Map of processes
/// * `commodity_ids` - All possible valid commodity IDs
/// * `milestone_years` - The milestone years of the simulation
///
/// # Returns
///
/// A map of Agents, with the agent ID as the key
pub fn read_agent_search_space(
    model_dir: &Path,
    agents: &AgentMap,
    processes: &ProcessMap,
    commodity_ids: &HashSet<CommodityID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentSearchSpaceMap>> {
    let file_path = model_dir.join(AGENT_SEARCH_SPACE_FILE_NAME);
    let iter = read_csv_optional::<AgentSearchSpaceRaw>(&file_path)?;
    read_agent_search_space_from_iter(iter, agents, processes, commodity_ids, milestone_years)
        .with_context(|| input_err_msg(&file_path))
}

fn read_agent_search_space_from_iter<I>(
    iter: I,
    agents: &AgentMap,
    processes: &ProcessMap,
    commodity_ids: &HashSet<CommodityID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentSearchSpaceMap>>
where
    I: Iterator<Item = AgentSearchSpaceRaw>,
{
    let mut search_spaces = HashMap::new();
    for search_space_raw in iter {
        let search_space = search_space_raw.into_agent_search_space(
            agents,
            processes,
            commodity_ids,
            milestone_years,
        )?;

        // Get or create search space map
        let map = search_spaces
            .entry(search_space.agent_id)
            .or_insert_with(AgentSearchSpaceMap::new);

        // Store process IDs
        for year in search_space.years {
            try_insert(
                map,
                &(search_space.commodity_id.clone(), year),
                search_space.search_space.clone(),
            )?;
        }
    }

    for (agent_id, agent) in agents {
        // Get or create search space map
        let search_space = search_spaces
            .entry(agent_id.clone())
            .or_insert_with(AgentSearchSpaceMap::new);

        // Add missing entries for commodities/years
        fill_missing_search_space_entries(agent, processes, search_space);
    }

    Ok(search_spaces)
}

/// Fill missing entries for the search space map for all commodities/milestone years.
///
/// The entries are filled will all producers of the given commodity in the given year. Only
/// producers which operate in at least one of the same regions as the agent are considered.
fn fill_missing_search_space_entries(
    agent: &Agent,
    processes: &ProcessMap,
    search_space: &mut AgentSearchSpaceMap,
) {
    // Agents all have commodity portions and this field should have been assigned already
    assert!(!agent.commodity_portions.is_empty());

    for (commodity_id, year) in agent.commodity_portions.keys() {
        let key = (commodity_id.clone(), *year);
        search_space.entry(key).or_insert_with(|| {
            Rc::new(get_all_producers(processes, commodity_id, *year).collect())
        });
    }
}

/// Get all processes active in the relevant year and regions which produce the given commodity
fn get_all_producers<'a>(
    processes: &'a ProcessMap,
    commodity_id: &'a CommodityID,
    year: u32,
) -> impl Iterator<Item = Rc<Process>> + 'a {
    processes
        .values()
        .filter(move |process| {
            process.active_for_year(year) && process.primary_output.as_ref() == Some(commodity_id)
        })
        .cloned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{agents, assert_error, region_ids};
    use crate::process::{
        ProcessActivityLimitsMap, ProcessFlowsMap, ProcessID, ProcessParameterMap,
    };
    use crate::region::RegionID;
    use crate::units::ActivityPerCapacity;
    use indexmap::IndexSet;
    use rstest::{fixture, rstest};
    use std::iter;

    #[fixture]
    pub fn processes(region_ids: IndexSet<RegionID>) -> ProcessMap {
        ["A", "B", "C"]
            .map(|id| {
                let id: ProcessID = id.into();
                let process = Process {
                    id: id.clone(),
                    description: "Description".into(),
                    years: vec![2010, 2020],
                    activity_limits: ProcessActivityLimitsMap::new(),
                    flows: ProcessFlowsMap::new(),
                    parameters: ProcessParameterMap::new(),
                    regions: region_ids.clone(),
                    primary_output: None,
                    capacity_to_activity: ActivityPerCapacity(1.0),
                };
                (id, process.into())
            })
            .into_iter()
            .collect()
    }

    #[fixture]
    fn commodity_ids() -> HashSet<CommodityID> {
        iter::once("commodity1".into()).collect()
    }

    #[rstest]
    fn test_search_space_raw_into_search_space_valid(
        agents: AgentMap,
        processes: ProcessMap,
        commodity_ids: HashSet<CommodityID>,
    ) {
        // Valid search space
        let raw = AgentSearchSpaceRaw {
            agent_id: "agent1".into(),
            commodity_id: "commodity1".into(),
            years: "2020".into(),
            search_space: "A;B".into(),
        };
        assert!(
            raw.into_agent_search_space(&agents, &processes, &commodity_ids, &[2020])
                .is_ok()
        );
    }

    #[rstest]
    fn test_search_space_raw_into_search_space_invalid_commodity_id(
        agents: AgentMap,
        processes: ProcessMap,
        commodity_ids: HashSet<CommodityID>,
    ) {
        // Invalid commodity ID
        let raw = AgentSearchSpaceRaw {
            agent_id: "agent1".into(),
            commodity_id: "invalid_commodity".into(),
            years: "2020".into(),
            search_space: "A;B".into(),
        };
        assert_error!(
            raw.into_agent_search_space(&agents, &processes, &commodity_ids, &[2020]),
            "Unknown ID invalid_commodity found"
        );
    }

    #[rstest]
    fn test_search_space_raw_into_search_space_invalid_process_id(
        agents: AgentMap,
        processes: ProcessMap,
        commodity_ids: HashSet<CommodityID>,
    ) {
        // Invalid process ID
        let raw = AgentSearchSpaceRaw {
            agent_id: "agent1".into(),
            commodity_id: "commodity1".into(),
            years: "2020".into(),
            search_space: "A;D".into(),
        };
        assert_error!(
            raw.into_agent_search_space(&agents, &processes, &commodity_ids, &[2020]),
            "Invalid process 'D'"
        );
    }
}
