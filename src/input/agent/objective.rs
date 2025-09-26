//! Code for reading the agent objectives CSV file.
use super::super::{input_err_msg, read_csv, try_insert};
use crate::agent::{AgentID, AgentMap, AgentObjectiveMap, DecisionRule, ObjectiveType};
use crate::units::Dimensionless;
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use itertools::Itertools;
use log::warn;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

const AGENT_OBJECTIVES_FILE_NAME: &str = "agent_objectives.csv";

/// An objective for an agent with associated parameters
#[derive(Debug, Clone, Deserialize, PartialEq)]
struct AgentObjectiveRaw {
    /// Unique agent id identifying the agent this objective belongs to
    agent_id: AgentID,
    /// The year(s) the objective is relevant for
    years: String,
    /// Acronym identifying the objective (e.g. LCOX)
    objective_type: ObjectiveType,
    /// For the weighted sum decision rule, the set of weights to apply to each objective.
    decision_weight: Option<Dimensionless>,
    /// For the lexico decision rule, the order in which to consider objectives.
    decision_lexico_order: Option<u32>,
}

/// Read agent objective info from the `agent_objectives.csv` file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
///
/// # Returns
///
/// A map of Agents, with the agent ID as the key
pub fn read_agent_objectives(
    model_dir: &Path,
    agents: &AgentMap,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentObjectiveMap>> {
    let file_path = model_dir.join(AGENT_OBJECTIVES_FILE_NAME);
    let agent_objectives_csv = read_csv(&file_path)?;
    read_agent_objectives_from_iter(agent_objectives_csv, agents, milestone_years)
        .with_context(|| input_err_msg(&file_path))
}

fn read_agent_objectives_from_iter<I>(
    iter: I,
    agents: &AgentMap,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentObjectiveMap>>
where
    I: Iterator<Item = AgentObjectiveRaw>,
{
    let mut all_objectives = HashMap::new();
    for objective in iter {
        let (id, agent) = agents
            .get_key_value(&objective.agent_id)
            .context("Invalid agent ID")?;

        // Check that required parameters are present and others are absent
        check_objective_parameter(&objective, &agent.decision_rule)?;

        let agent_objectives = all_objectives
            .entry(id.clone())
            .or_insert_with(AgentObjectiveMap::new);
        for year in parse_year_str(&objective.years, milestone_years.iter().copied())? {
            try_insert(agent_objectives, &year, objective.objective_type).with_context(|| {
                format!("Duplicate agent objective entry for agent {id} and year {year}")
            })?;
        }
    }

    // Check that agents have one objective per milestone year
    for agent_id in agents.keys() {
        let agent_objectives = all_objectives
            .get(agent_id)
            .with_context(|| format!("Agent {agent_id} has no objectives"))?;

        let missing_years = milestone_years
            .iter()
            .copied()
            .filter(|year| !agent_objectives.contains_key(year))
            .collect_vec();
        ensure!(
            missing_years.is_empty(),
            "Agent {agent_id} is missing objectives for the following milestone years: {missing_years:?}"
        );

        let npv_years = milestone_years
            .iter()
            .copied()
            .filter(|year| agent_objectives[year] == ObjectiveType::NetPresentValue)
            .collect_vec();
        if !npv_years.is_empty() {
            warn!(
                "Agent {agent_id} is using NPV in years {npv_years:?}. \
                Support for NPV is currently experimental and may give bad results."
            );
        }
    }

    Ok(all_objectives)
}

/// Check that required parameters are present and others are absent
fn check_objective_parameter(
    objective: &AgentObjectiveRaw,
    decision_rule: &DecisionRule,
) -> Result<()> {
    // Check that the user hasn't supplied a value for a field we're not using
    macro_rules! check_field_none {
        ($field:ident) => {
            ensure!(
                objective.$field.is_none(),
                "Field {} should be empty for this decision rule",
                stringify!($field)
            )
        };
    }

    // Check that required fields are present
    macro_rules! check_field_some {
        ($field:ident) => {
            ensure!(
                objective.$field.is_some(),
                "Required field {} is empty",
                stringify!($field)
            )
        };
    }

    match decision_rule {
        DecisionRule::Single => {
            check_field_none!(decision_weight);
            check_field_none!(decision_lexico_order);
        }
        DecisionRule::Weighted => {
            check_field_some!(decision_weight);
            check_field_none!(decision_lexico_order);
        }
        DecisionRule::Lexicographical { tolerance: _ } => {
            check_field_none!(decision_weight);
            check_field_some!(decision_lexico_order);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::ObjectiveType;
    use crate::fixture::{agents, assert_error};
    use rstest::{fixture, rstest};
    use std::iter;

    macro_rules! objective {
        ($decision_weight:expr, $decision_lexico_order:expr) => {
            AgentObjectiveRaw {
                agent_id: "agent".into(),
                years: "2020".into(),
                objective_type: ObjectiveType::LevelisedCostOfX,
                decision_weight: $decision_weight,
                decision_lexico_order: $decision_lexico_order,
            }
        };
    }

    #[test]
    fn test_check_objective_parameter_single() {
        // DecisionRule::Single
        let decision_rule = DecisionRule::Single;
        let objective = objective!(None, None);
        assert!(check_objective_parameter(&objective, &decision_rule).is_ok());
        let objective = objective!(Some(Dimensionless(1.0)), None);
        assert!(check_objective_parameter(&objective, &decision_rule).is_err());
        let objective = objective!(None, Some(1));
        assert!(check_objective_parameter(&objective, &decision_rule).is_err());
    }

    #[test]
    fn test_check_objective_parameter_weighted() {
        // DecisionRule::Weighted
        let decision_rule = DecisionRule::Weighted;
        let objective = objective!(Some(Dimensionless(1.0)), None);
        assert!(check_objective_parameter(&objective, &decision_rule).is_ok());
        let objective = objective!(None, None);
        assert!(check_objective_parameter(&objective, &decision_rule).is_err());
        let objective = objective!(None, Some(1));
        assert!(check_objective_parameter(&objective, &decision_rule).is_err());
    }

    #[test]
    fn test_check_objective_parameter_lexico() {
        // DecisionRule::Lexicographical
        let decision_rule = DecisionRule::Lexicographical { tolerance: 1.0 };
        let objective = objective!(None, Some(1));
        assert!(check_objective_parameter(&objective, &decision_rule).is_ok());
        let objective = objective!(None, None);
        assert!(check_objective_parameter(&objective, &decision_rule).is_err());
        let objective = objective!(Some(Dimensionless(1.0)), None);
        assert!(check_objective_parameter(&objective, &decision_rule).is_err());
    }

    #[fixture]
    fn objective_raw() -> AgentObjectiveRaw {
        AgentObjectiveRaw {
            agent_id: "agent1".into(),
            years: "2020".into(),
            objective_type: ObjectiveType::LevelisedCostOfX,
            decision_weight: None,
            decision_lexico_order: None,
        }
    }

    #[rstest]
    fn test_read_agent_objectives_from_iter_valid(
        agents: AgentMap,
        objective_raw: AgentObjectiveRaw,
    ) {
        let milestone_years = [2020];
        let expected = iter::once((
            "agent1".into(),
            iter::once((2020, objective_raw.objective_type)).collect(),
        ))
        .collect();
        let actual = read_agent_objectives_from_iter(
            iter::once(objective_raw.clone()),
            &agents,
            &milestone_years,
        )
        .unwrap();
        assert_eq!(actual, expected);
    }

    #[rstest]
    fn test_read_agent_objectives_from_iter_invalid_no_objective_for_agent(agents: AgentMap) {
        // Missing objective for agent
        assert_error!(
            read_agent_objectives_from_iter(iter::empty(), &agents, &[2020]),
            "Agent agent1 has no objectives"
        );
    }

    #[rstest]
    fn test_read_agent_objectives_from_iter_invalid_no_objective_for_year(
        agents: AgentMap,
        objective_raw: AgentObjectiveRaw,
    ) {
        // Missing objective for milestone year
        assert_error!(
            read_agent_objectives_from_iter(iter::once(objective_raw), &agents, &[2020, 2030]),
            "Agent agent1 is missing objectives for the following milestone years: [2030]"
        );
    }

    #[rstest]
    fn test_read_agent_objectives_from_iter_invalid_bad_param(agents: AgentMap) {
        // Bad parameter
        let bad_objective = AgentObjectiveRaw {
            agent_id: "agent1".into(),
            years: "2020".into(),
            objective_type: ObjectiveType::LevelisedCostOfX,
            decision_weight: Some(Dimensionless(1.0)), // Should only accept None for DecisionRule::Single
            decision_lexico_order: None,
        };
        assert_error!(
            read_agent_objectives_from_iter([bad_objective].into_iter(), &agents, &[2020]),
            "Field decision_weight should be empty for this decision rule"
        );
    }
}
