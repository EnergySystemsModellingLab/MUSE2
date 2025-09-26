//! Code for reading the agent commodities CSV file.
use super::super::{deserialise_proportion_nonzero, input_err_msg, read_csv, try_insert};
use crate::agent::{AgentCommodityPortionsMap, AgentID, AgentMap};
use crate::commodity::{CommodityMap, CommodityType};
use crate::id::IDCollection;
use crate::region::RegionID;
use crate::units::Dimensionless;
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use float_cmp::approx_eq;
use indexmap::IndexSet;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::path::Path;

const AGENT_COMMODITIES_FILE_NAME: &str = "agent_commodity_portions.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct AgentCommodityPortionRaw {
    /// Unique agent id identifying the agent.
    agent_id: String,
    /// The commodity that the agent is responsible for.
    commodity_id: String,
    /// The year(s) the commodity portion applies to.
    years: String,
    /// The proportion of the commodity production that the agent is responsible for.
    #[serde(deserialize_with = "deserialise_proportion_nonzero")]
    commodity_portion: Dimensionless,
}

/// Read agent commodity portions info from the `agent_commodity_portions.csv` file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
///
/// # Returns
///
/// A map of Agents, with the agent ID as the key
pub fn read_agent_commodity_portions(
    model_dir: &Path,
    agents: &AgentMap,
    commodities: &CommodityMap,
    region_ids: &IndexSet<RegionID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentCommodityPortionsMap>> {
    let file_path = model_dir.join(AGENT_COMMODITIES_FILE_NAME);
    let agent_commodity_portions_csv = read_csv(&file_path)?;
    read_agent_commodity_portions_from_iter(
        agent_commodity_portions_csv,
        agents,
        commodities,
        region_ids,
        milestone_years,
    )
    .with_context(|| input_err_msg(&file_path))
}

fn read_agent_commodity_portions_from_iter<I>(
    iter: I,
    agents: &AgentMap,
    commodities: &CommodityMap,
    region_ids: &IndexSet<RegionID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentCommodityPortionsMap>>
where
    I: Iterator<Item = AgentCommodityPortionRaw>,
{
    let mut agent_commodity_portions = HashMap::new();
    for agent_commodity_portion_raw in iter {
        // Get agent ID
        let agent_id_raw = agent_commodity_portion_raw.agent_id.as_str();
        let id = agents.get_id(agent_id_raw)?;

        // Get/create entry for agent
        let entry = agent_commodity_portions
            .entry(id.clone())
            .or_insert_with(AgentCommodityPortionsMap::new);

        // Insert portion for the commodity/year(s)
        let commodity_id_raw = agent_commodity_portion_raw.commodity_id.as_str();
        let commodity_id = commodities.get_id(commodity_id_raw)?;
        let years = parse_year_str(
            &agent_commodity_portion_raw.years,
            milestone_years.iter().copied(),
        )?;
        for year in years {
            try_insert(
                entry,
                &(commodity_id.clone(), year),
                agent_commodity_portion_raw.commodity_portion,
            )?;
        }
    }

    validate_agent_commodity_portions(
        &agent_commodity_portions,
        agents,
        commodities,
        region_ids,
        milestone_years,
    )?;

    Ok(agent_commodity_portions)
}

fn validate_agent_commodity_portions(
    agent_commodity_portions: &HashMap<AgentID, AgentCommodityPortionsMap>,
    agents: &AgentMap,
    commodities: &CommodityMap,
    region_ids: &IndexSet<RegionID>,
    milestone_years: &[u32],
) -> Result<()> {
    // CHECK 1: Each specified commodity must have data for all years
    for (id, portions) in agent_commodity_portions {
        // Colate set of commodities for this agent
        let commodity_ids: HashSet<_> = portions.keys().map(|(id, _)| id).collect();

        // Check that each commodity has data for all milestone years
        for commodity_id in commodity_ids {
            for year in milestone_years {
                ensure!(
                    portions.contains_key(&(commodity_id.clone(), *year)),
                    "Agent {id} does not have data for commodity {commodity_id} in year {year}"
                );
            }
        }
    }

    // CHECK 2: Total portions for each commodity/year/region must sum to 1
    // First step is to create a map with the key as (commodity_id, year, region_id), and the value
    // as the sum of the portions for that key across all agents
    let mut summed_portions = HashMap::new();
    for (id, agent_commodity_portions) in agent_commodity_portions {
        let agent = agents.get(id).context("Invalid agent ID")?;
        for ((commodity_id, year), portion) in agent_commodity_portions {
            for region in region_ids {
                if agent.regions.contains(region) {
                    let key = (commodity_id, year, region);
                    summed_portions
                        .entry(key)
                        .and_modify(|v| *v += *portion)
                        .or_insert(*portion);
                }
            }
        }
    }

    // We then check the map to ensure values for each key are 1
    for (key, portion) in &summed_portions {
        ensure!(
            approx_eq!(Dimensionless, *portion, Dimensionless(1.0), epsilon = 1e-5),
            "Commodity {} in year {} and region {} does not sum to 1.0",
            key.0,
            key.1,
            key.2
        );
    }

    // CHECK 3: All commodities of SVD or SED type must be covered for all regions and years
    // This checks the same summed_portions map as above, just checking the keys
    // We first need to create a list of SVD and SED commodities to check against
    let svd_and_sed_commodities = commodities
        .iter()
        .filter(|(_, commodity)| {
            matches!(
                commodity.kind,
                CommodityType::SupplyEqualsDemand | CommodityType::ServiceDemand
            )
        })
        .map(|(id, _)| id);

    // Check that summed_portions contains all SVD/SED commodities for all regions and milestone
    // years
    for commodity_id in svd_and_sed_commodities {
        for year in milestone_years {
            for region in region_ids {
                let key = (commodity_id, year, region);
                ensure!(
                    summed_portions.contains_key(&key),
                    "Commodity {commodity_id} in year {year} and region {region} is not covered"
                );
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{
        Agent, AgentCostLimitsMap, AgentObjectiveMap, AgentSearchSpaceMap, DecisionRule,
    };
    use crate::commodity::{Commodity, CommodityID, CommodityLevyMap, CommodityType, DemandMap};
    use crate::time_slice::TimeSliceLevel;
    use indexmap::IndexMap;
    use std::rc::Rc;

    #[test]
    fn test_validate_agent_commodity_portions() {
        let region_ids = IndexSet::from([RegionID::new("region1"), RegionID::new("region2")]);
        let milestone_years = [2020];
        let agents = IndexMap::from([(
            AgentID::new("agent1"),
            Agent {
                id: "agent1".into(),
                description: "An agent".into(),
                commodity_portions: AgentCommodityPortionsMap::new(),
                search_space: AgentSearchSpaceMap::new(),
                decision_rule: DecisionRule::Single,
                cost_limits: AgentCostLimitsMap::new(),
                regions: region_ids.clone(),
                objectives: AgentObjectiveMap::new(),
            },
        )]);
        let mut commodities = IndexMap::from([(
            CommodityID::new("commodity1"),
            Rc::new(Commodity {
                id: "commodity1".into(),
                description: "A commodity".into(),
                kind: CommodityType::SupplyEqualsDemand,
                time_slice_level: TimeSliceLevel::Annual,
                levies: CommodityLevyMap::new(),
                demand: DemandMap::new(),
            }),
        )]);

        // Valid case
        let mut map = AgentCommodityPortionsMap::new();
        map.insert(("commodity1".into(), 2020), Dimensionless(1.0));
        let agent_commodity_portions = HashMap::from([("agent1".into(), map)]);
        assert!(
            validate_agent_commodity_portions(
                &agent_commodity_portions,
                &agents,
                &commodities,
                &region_ids,
                &milestone_years
            )
            .is_ok()
        );

        // Invalid case: portions do not sum to 1
        let mut map_v2 = AgentCommodityPortionsMap::new();
        map_v2.insert(("commodity1".into(), 2020), Dimensionless(0.5));
        let agent_commodities_v2 = HashMap::from([("agent1".into(), map_v2)]);
        assert!(
            validate_agent_commodity_portions(
                &agent_commodities_v2,
                &agents,
                &commodities,
                &region_ids,
                &milestone_years
            )
            .is_err()
        );

        // Invalid case: SED commodity without associated commodity portions
        commodities.insert(
            CommodityID::new("commodity2"),
            Rc::new(Commodity {
                id: "commodity2".into(),
                description: "Another commodity".into(),
                kind: CommodityType::SupplyEqualsDemand,
                time_slice_level: TimeSliceLevel::Annual,
                levies: CommodityLevyMap::new(),
                demand: DemandMap::new(),
            }),
        );
        assert!(
            validate_agent_commodity_portions(
                &agent_commodity_portions,
                &agents,
                &commodities,
                &region_ids,
                &milestone_years
            )
            .is_err()
        );
    }
}
