//! Agents drive the economy of the MUSE 2.0 simulation, through relative investment in different
//! assets.
use crate::commodity::CommodityID;
use crate::id::define_id_type;
use crate::process::Process;
use crate::region::RegionID;
use crate::units::{Dimensionless, Money};
use indexmap::{IndexMap, IndexSet};
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::HashMap;
use std::rc::Rc;

define_id_type! {AgentID}

/// A map of [`Agent`]s, keyed by agent ID
pub type AgentMap = IndexMap<AgentID, Agent>;

/// A map of cost limits for an agent, keyed by year
pub type AgentCostLimitsMap = HashMap<u32, AgentCostLimits>;

/// A map of commodity portions for an agent, keyed by commodity and year
pub type AgentCommodityPortionsMap = HashMap<(CommodityID, u32), Dimensionless>;

/// A map for the agent's search space, keyed by commodity and year
pub type AgentSearchSpaceMap = HashMap<(CommodityID, u32), Rc<Vec<Rc<Process>>>>;

/// A map of objectives for an agent, keyed by commodity and year.
///
/// NB: As we currently only support the "single" decision rule, the only parameter we need for
/// objectives is the type.
pub type AgentObjectiveMap = HashMap<u32, ObjectiveType>;

/// An agent in the simulation
#[derive(Debug, Clone, PartialEq)]
pub struct Agent {
    /// A unique identifier for the agent.
    pub id: AgentID,
    /// A text description of the agent.
    pub description: String,
    /// The proportion of the commodity production that the agent is responsible for.
    pub commodity_portions: AgentCommodityPortionsMap,
    /// The processes that the agent will consider investing in.
    pub search_space: AgentSearchSpaceMap,
    /// The decision rule that the agent uses to decide investment.
    pub decision_rule: DecisionRule,
    /// Cost limits (e.g. capital cost, annual operating cost)
    pub cost_limits: AgentCostLimitsMap,
    /// The regions in which this agent operates.
    pub regions: IndexSet<RegionID>,
    /// The agent's objectives.
    pub objectives: AgentObjectiveMap,
}

impl Agent {
    /// Get all the processes in this agent's search space which produce the commodity in the given
    /// year
    pub fn iter_possible_producers_of<'a>(
        &'a self,
        region_id: &RegionID,
        commodity_id: &'a CommodityID,
        year: u32,
    ) -> impl Iterator<Item = &'a Rc<Process>> + use<'a> {
        let flows_key = (region_id.clone(), year);
        self.search_space[&(commodity_id.clone(), year)]
            .iter()
            .filter(move |process| process.flows[&flows_key][commodity_id].is_output())
    }
}

/// The cost limits for an agent in a particular year
#[derive(Debug, Clone, PartialEq)]
pub struct AgentCostLimits {
    /// The maximum capital cost the agent will pay.
    pub capex_limit: Option<Money>,
    /// The maximum annual operating cost (fuel etc.) that the agent will pay.
    pub annual_cost_limit: Option<Money>,
}

/// The decision rule for a particular objective
#[derive(Debug, Clone, PartialEq)]
pub enum DecisionRule {
    /// Used when there is only a single objective
    Single,
    /// A simple weighting of objectives
    Weighted,
    /// Objectives are considered in a specific order
    Lexicographical {
        /// The tolerance around the main objective to consider secondary objectives. This is an absolute value of maximum deviation in the units of the main objective.
        tolerance: f64,
    },
}

/// The type of objective for the agent
#[derive(Debug, Clone, Copy, PartialEq, DeserializeLabeledStringEnum)]
pub enum ObjectiveType {
    /// Average cost of one unit of output commodity over its lifetime
    #[string = "lcox"]
    LevelisedCostOfX,
    /// Net present value
    #[string = "npv"]
    NetPresentValue,
}
