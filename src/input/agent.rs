//! Code for reading in agent-related data from CSV files.
use super::{input_err_msg, read_csv};
use crate::agent::{
    Agent, AgentCommodityPortionsMap, AgentCostLimitsMap, AgentID, AgentMap, AgentObjectiveMap,
    AgentSearchSpaceMap, DecisionRule,
};
use crate::commodity::CommodityMap;
use crate::process::ProcessMap;
use crate::region::{RegionID, parse_region_str};
use anyhow::{Context, Result, bail, ensure};
use indexmap::IndexSet;
use serde::Deserialize;
use std::path::Path;

mod objective;
use objective::read_agent_objectives;
mod search_space;
use search_space::read_agent_search_space;
mod commodity_portion;
use commodity_portion::read_agent_commodity_portions;
mod cost_limit;
use cost_limit::read_agent_cost_limits;

const AGENT_FILE_NAME: &str = "agents.csv";

/// An agent in the simulation
#[derive(Debug, Deserialize, PartialEq, Clone)]
struct AgentRaw {
    /// A unique identifier for the agent.
    id: String,
    /// A text description of the agent.
    description: String,
    /// The region(s) in which the agent operates.
    regions: String,
    /// The decision rule that the agent uses to decide investment.
    decision_rule: String,
    /// The tolerance around the main objective to consider secondary objectives.
    decision_lexico_tolerance: Option<f64>,
}

/// Read agents info from various CSV files.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `commodities` - Commodities for the model
/// * `process_ids` - The possible valid process IDs
/// * `region_ids` - The possible valid region IDs
///
/// # Returns
///
/// A map of Agents, with the agent ID as the key
pub fn read_agents(
    model_dir: &Path,
    commodities: &CommodityMap,
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
    milestone_years: &[u32],
) -> Result<AgentMap> {
    let mut agents = read_agents_file(model_dir, region_ids)?;
    let agent_ids = agents.keys().cloned().collect();

    // We read commodity portions first as they are required by `read_agent_search_space`
    let mut agent_commodities = read_agent_commodity_portions(
        model_dir,
        &agents,
        commodities,
        region_ids,
        milestone_years,
    )?;
    for (id, agent) in &mut agents {
        agent.commodity_portions = agent_commodities
            .remove(id)
            .with_context(|| format!("Missing commodity portions for agent {id}"))?;
    }

    let mut objectives = read_agent_objectives(model_dir, &agents, milestone_years)?;
    let commodity_ids = commodities.keys().cloned().collect();
    let mut search_spaces = read_agent_search_space(
        model_dir,
        &agents,
        processes,
        &commodity_ids,
        milestone_years,
    )?;
    let mut cost_limits = read_agent_cost_limits(model_dir, &agent_ids, milestone_years)?;

    for (id, agent) in &mut agents {
        agent.objectives = objectives.remove(id).unwrap();
        agent.search_space = search_spaces.remove(id).unwrap();
        if let Some(cost_limits) = cost_limits.remove(id) {
            agent.cost_limits = cost_limits;
        }
    }

    Ok(agents)
}

/// Read agents info from the agents.csv file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `commodities` - Commodities for the model
/// * `process_ids` - The possible valid process IDs
///
/// # Returns
///
/// A map of Agents, with the agent ID as the key
fn read_agents_file(model_dir: &Path, region_ids: &IndexSet<RegionID>) -> Result<AgentMap> {
    let file_path = model_dir.join(AGENT_FILE_NAME);
    let agents_csv = read_csv(&file_path)?;
    read_agents_file_from_iter(agents_csv, region_ids).with_context(|| input_err_msg(&file_path))
}

/// Read agents info from an iterator.
fn read_agents_file_from_iter<I>(iter: I, region_ids: &IndexSet<RegionID>) -> Result<AgentMap>
where
    I: Iterator<Item = AgentRaw>,
{
    let mut agents = AgentMap::new();
    for agent_raw in iter {
        // Parse region ID
        let regions = parse_region_str(&agent_raw.regions, region_ids)?;

        // Parse decision rule
        let decision_rule = match agent_raw.decision_rule.to_ascii_lowercase().as_str() {
            "single" => DecisionRule::Single,
            "weighted" => DecisionRule::Weighted,
            "lexico" => {
                let tolerance = agent_raw
                    .decision_lexico_tolerance
                    .with_context(|| "Missing tolerance for lexico decision rule")?;
                ensure!(
                    tolerance >= 0.0,
                    "Lexico tolerance must be non-negative, got {tolerance}"
                );
                DecisionRule::Lexicographical { tolerance }
            }
            invalid_rule => bail!("Invalid decision rule: {invalid_rule}"),
        };

        ensure!(
            decision_rule == DecisionRule::Single,
            "Currently only the \"single\" decision rule is supported"
        );

        let agent = Agent {
            id: AgentID(agent_raw.id.into()),
            description: agent_raw.description,
            commodity_portions: AgentCommodityPortionsMap::new(),
            search_space: AgentSearchSpaceMap::new(),
            decision_rule,
            cost_limits: AgentCostLimitsMap::new(),
            regions,
            objectives: AgentObjectiveMap::new(),
        };

        ensure!(
            agents.insert(agent.id.clone(), agent).is_none(),
            "Duplicate agent ID"
        );
    }

    Ok(agents)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::DecisionRule;
    use std::iter;

    #[test]
    fn test_read_agents_file_from_iter() {
        // Valid case
        let region_ids = IndexSet::from(["GBR".into()]);
        let agent = AgentRaw {
            id: "agent".into(),
            description: "".into(),
            decision_rule: "single".into(),
            decision_lexico_tolerance: None,
            regions: "GBR".into(),
        };
        let agent_out = Agent {
            id: "agent".into(),
            description: "".into(),
            commodity_portions: AgentCommodityPortionsMap::new(),
            search_space: AgentSearchSpaceMap::new(),
            decision_rule: DecisionRule::Single,
            cost_limits: AgentCostLimitsMap::new(),
            regions: IndexSet::from(["GBR".into()]),
            objectives: AgentObjectiveMap::new(),
        };
        let expected = AgentMap::from_iter(iter::once(("agent".into(), agent_out)));
        let actual = read_agents_file_from_iter(iter::once(agent), &region_ids).unwrap();
        assert_eq!(actual, expected);

        // Duplicate agent ID
        let agents = [
            AgentRaw {
                id: "agent".into(),
                description: "".into(),
                decision_rule: "single".into(),
                decision_lexico_tolerance: None,
                regions: "GBR".into(),
            },
            AgentRaw {
                id: "agent".into(),
                description: "".into(),
                decision_rule: "single".into(),
                decision_lexico_tolerance: None,
                regions: "GBR".into(),
            },
        ];
        assert!(read_agents_file_from_iter(agents.into_iter(), &region_ids).is_err());

        // Lexico tolerance missing for lexico decision rule
        let agent = AgentRaw {
            id: "agent".into(),
            description: "".into(),
            decision_rule: "lexico".into(),
            decision_lexico_tolerance: None,
            regions: "GBR".into(),
        };
        assert!(read_agents_file_from_iter(iter::once(agent), &region_ids).is_err());
    }
}
