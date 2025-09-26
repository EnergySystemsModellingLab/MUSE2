//! Code for reading the agent cost limits CSV file.
use super::super::{input_err_msg, read_csv_optional};
use crate::agent::{AgentCostLimits, AgentCostLimitsMap, AgentID};
use crate::id::IDCollection;
use crate::units::Money;
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::path::Path;

const AGENT_COST_LIMITS_FILE_NAME: &str = "agent_cost_limits.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct AgentCostLimitsRaw {
    agent_id: String,
    years: String,
    capex_limit: Option<Money>,
    annual_cost_limit: Option<Money>,
}

impl AgentCostLimitsRaw {
    fn to_agent_cost_limits(&self) -> AgentCostLimits {
        AgentCostLimits {
            capex_limit: self.capex_limit,
            annual_cost_limit: self.annual_cost_limit,
        }
    }
}

/// Read agent cost limits info from the `agent_cost_limits.csv` file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
///
/// # Returns
///
/// A map of Agents, with the agent ID as the key
pub fn read_agent_cost_limits(
    model_dir: &Path,
    agent_ids: &HashSet<AgentID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentCostLimitsMap>> {
    let file_path = model_dir.join(AGENT_COST_LIMITS_FILE_NAME);
    let agent_cost_limits_csv = read_csv_optional(&file_path)?;
    read_agent_cost_limits_from_iter(agent_cost_limits_csv, agent_ids, milestone_years)
        .with_context(|| input_err_msg(&file_path))
}

fn read_agent_cost_limits_from_iter<I>(
    iter: I,
    agent_ids: &HashSet<AgentID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentCostLimitsMap>>
where
    I: Iterator<Item = AgentCostLimitsRaw>,
{
    let mut map: HashMap<AgentID, AgentCostLimitsMap> = HashMap::new();
    for agent_cost_limits_raw in iter {
        let cost_limits = agent_cost_limits_raw.to_agent_cost_limits();
        let years = parse_year_str(
            &agent_cost_limits_raw.years,
            milestone_years.iter().copied(),
        )?;

        // Get agent ID
        let agent_id = agent_ids.get_id(&agent_cost_limits_raw.agent_id)?;

        // Get or create entry in the map
        let entry = map.entry(agent_id.clone()).or_default();

        // Insert cost limits for the specified year(s)
        for year in years {
            entry.insert(year, cost_limits.clone());
        }
    }

    // Validation: if cost limits are specified for an agent, they must be present for all years.
    for (id, cost_limits) in &map {
        for year in milestone_years {
            ensure!(
                cost_limits.contains_key(year),
                "Agent {id} is missing cost limits for year {year}"
            );
        }
    }

    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentCostLimits;
    use std::collections::HashSet;

    fn create_agent_cost_limits_raw(
        agent_id: &str,
        year: &str,
        capex_limit: Option<Money>,
        annual_cost_limit: Option<Money>,
    ) -> AgentCostLimitsRaw {
        AgentCostLimitsRaw {
            agent_id: agent_id.to_string(),
            years: year.to_string(),
            capex_limit,
            annual_cost_limit,
        }
    }

    #[test]
    fn test_read_agent_cost_limits_from_iter_all_years() {
        let agent_ids: HashSet<AgentID> = ["Agent1", "Agent2"]
            .iter()
            .map(|&id| AgentID::from(id))
            .collect();
        let milestone_years = [2020, 2025];

        let iter = [
            create_agent_cost_limits_raw("Agent1", "all", Some(Money(100.0)), Some(Money(200.0))),
            create_agent_cost_limits_raw("Agent2", "all", Some(Money(150.0)), Some(Money(250.0))),
        ]
        .into_iter();

        let result = read_agent_cost_limits_from_iter(iter, &agent_ids, &milestone_years).unwrap();

        assert_eq!(result.len(), 2);
        for year in milestone_years {
            assert_eq!(
                result[&AgentID::from("Agent1")][&year],
                AgentCostLimits {
                    capex_limit: Some(Money(100.0)),
                    annual_cost_limit: Some(Money(200.0)),
                }
            );
            assert_eq!(
                result[&AgentID::from("Agent2")][&year],
                AgentCostLimits {
                    capex_limit: Some(Money(150.0)),
                    annual_cost_limit: Some(Money(250.0)),
                }
            );
        }
    }

    #[test]
    fn test_read_agent_cost_limits_from_iter_some_years() {
        let agent_ids: HashSet<AgentID> = ["Agent1"].iter().map(|&id| AgentID::from(id)).collect();
        let milestone_years = [2020, 2025];

        let iter = [create_agent_cost_limits_raw(
            "Agent1",
            "2020;2025",
            Some(Money(100.0)),
            Some(Money(200.0)),
        )]
        .into_iter();

        let result = read_agent_cost_limits_from_iter(iter, &agent_ids, &milestone_years).unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(
            result[&AgentID::from("Agent1")][&2020],
            AgentCostLimits {
                capex_limit: Some(Money(100.0)),
                annual_cost_limit: Some(Money(200.0)),
            }
        );
        assert_eq!(
            result[&AgentID::from("Agent1")][&2025],
            AgentCostLimits {
                capex_limit: Some(Money(100.0)),
                annual_cost_limit: Some(Money(200.0)),
            }
        );
    }

    #[test]
    fn test_read_agent_cost_limits_from_iter_missing_years() {
        let agent_ids: HashSet<AgentID> = ["Agent1"].iter().map(|&id| AgentID::from(id)).collect();
        let milestone_years = [2020, 2025];

        let iter = [create_agent_cost_limits_raw(
            "Agent1",
            "2020",
            Some(Money(100.0)),
            Some(Money(200.0)),
        )]
        .into_iter();

        let result = read_agent_cost_limits_from_iter(iter, &agent_ids, &milestone_years);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Agent Agent1 is missing cost limits for year 2025"
        );
    }
}
