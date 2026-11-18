//! Module for creating and analysing agent-specific commodity graphs
use crate::agent::{AgentID, AgentMap};
use crate::commodity::CommodityID;
use crate::process::{FlowDirection, Process, ProcessFlow, ProcessID, ProcessMap};
use crate::region::RegionID;
use anyhow::Result;
use indexmap::IndexMap;
use itertools::iproduct;
use petgraph::Directed;
use petgraph::dot::Dot;
use petgraph::graph::{EdgeReference, Graph};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write as IoWrite;
use std::path::Path;
use std::sync::Arc;

/// A graph of commodity flows for a given agent, region and year
pub type CommoditiesGraph = Graph<GraphNode, GraphEdge, Directed>;

#[derive(Eq, PartialEq, Clone, Hash, derive_more::Display)]
/// A node in the agent-specific commodity graph
pub enum GraphNode {
    /// A node representing a commodity in a specific region for a specific agent
    #[display("{_0} | {_1} | {_2}")]
    CommodityMarket(CommodityID, RegionID, AgentID),
    /// A source node for processes that have no inputs
    #[display("SOURCE")]
    Source,
    /// A sink node for processes that have no outputs
    #[display("SINK")]
    Sink,
    /// A demand node for commodities with service demands
    #[display("DEMAND")]
    Demand,
}

#[derive(Eq, PartialEq, Clone, Hash, derive_more::Display)]
/// An edge in the agent-specific commodity graph
pub enum GraphEdge {
    /// An edge representing a primary flow of a process installed in the given region for an agent
    #[display("{_0} | {_1} | {_2}")]
    Primary(ProcessID, RegionID, AgentID),
    /// An edge representing a secondary flow of a process installed in the given region for an agent
    #[display("{_0} | {_1} | {_2}")]
    Secondary(ProcessID, RegionID, AgentID),
    /// An edge representing a service demand
    #[display("DEMAND")]
    Demand,
}

/// Helper function to return a possible flow operating in the requested year
///
/// We are only interested in the flow directions, which are constant across years. This
/// function checks whether the process can be operating in the target region and year and, if so,
/// returns its flows. It considers both the commission year and the process lifetime, since a
/// process may operate for years after its commission window. If the process cannot be operating
/// in the target region/year, `None` is returned.
fn get_flow_for_year(
    process: &Process,
    target: (RegionID, u32),
) -> Option<Arc<IndexMap<CommodityID, ProcessFlow>>> {
    // If its already in the map, we return it
    if process.flows.contains_key(&target) {
        return process.flows.get(&target).cloned();
    }

    // Otherwise we try to find one that operates in the target year. It is assumed that
    // parameters are defined for at least the same (region, year) combinations as flows.
    let (target_region, target_year) = target;
    for ((region, year), value) in &process.flows {
        if *region != target_region {
            continue;
        }
        if year + process.parameters[&(region.clone(), *year)].lifetime >= target_year {
            return Some(value.clone());
        }
    }
    None
}

/// Gets market nodes for agents with a share of a commodity in the given region and year.
fn get_commodity_market_nodes(
    agents: &AgentMap,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    year: u32,
) -> Vec<GraphNode> {
    agents
        .values()
        .filter(|agent| {
            agent.regions.contains(region_id)
                && agent
                    .commodity_portions
                    .contains_key(&(commodity_id.clone(), year))
        })
        .map(|agent| {
            GraphNode::CommodityMarket(commodity_id.clone(), region_id.clone(), agent.id.clone())
        })
        .collect()
}

/// Creates a directed graph of commodity flows for a given year, covering all agents and regions.
///
/// The graph contains nodes only for `(commodity, region, agent)` combinations where the agent has
/// a share of the commodity for the specified year and operates in the region. A process is
/// included when it appears in an agent's search space for the given region and year. There will
/// be an edge from `(A, R, G1)` to `(B, R, G2)` if such a process consumes commodity A and produces
/// commodity B in region R, and G1 and G2 have shares of A and B respectively. The edge records
/// the agent whose search space includes the process. Commodities are traded in a common regional
/// market, so a process can consume commodity A from any agent with a share in region R.
///
/// There are also special `Source` and `Sink` nodes, which are used for processes that have no
/// inputs or outputs.
///
/// The graph does not take into account process availabilities or commodity demands, both of which
/// can vary by time slice.
fn create_commodities_graph_for_year(
    processes: &ProcessMap,
    agents: &AgentMap,
    year: u32,
) -> CommoditiesGraph {
    let mut graph = Graph::new();
    let mut node_index = HashMap::new();

    for agent in agents.values() {
        for region_id in &agent.regions {
            // A process may appear in multiple commodity-specific search spaces.
            let search_space_process_ids: HashSet<_> = agent
                .search_space
                .iter()
                .filter(|((_, search_region, search_year), _)| {
                    search_region == region_id && *search_year == year
                })
                .flat_map(|(_, search_space)| search_space.iter().map(|process| &process.id))
                .collect();

            for process in processes
                .values()
                .filter(|process| search_space_process_ids.contains(&process.id))
            {
                let Some(flows) = get_flow_for_year(process, (region_id.clone(), year)) else {
                    // Process doesn't operate in this region/year
                    continue;
                };

                let (input_flows, output_flows): (Vec<_>, Vec<_>) = flows
                    .values()
                    .partition(|flow| flow.direction() == FlowDirection::Input);

                // Get input nodes for the process. Commodities enter a common regional market,
                // so any agent with a share can supply this process's input.
                let mut inputs: Vec<_> = input_flows
                    .iter()
                    .flat_map(|flow| {
                        get_commodity_market_nodes(agents, &flow.commodity.id, region_id, year)
                    })
                    .collect();
                // Get output nodes for the process.
                let mut outputs: Vec<_> = output_flows
                    .iter()
                    .flat_map(|flow| {
                        get_commodity_market_nodes(agents, &flow.commodity.id, region_id, year)
                    })
                    .collect();

                // Use `Source` node if no inputs, `Sink` node if no outputs
                if input_flows.is_empty() {
                    inputs.push(GraphNode::Source);
                }
                if output_flows.is_empty() {
                    outputs.push(GraphNode::Sink);
                }

                // Skip processes when no owned market is available on either side.
                if inputs.is_empty() || outputs.is_empty() {
                    continue;
                }

                // Get primary output for the process
                let primary_output = &process.primary_output;

                // Create edges from all inputs to all outputs.
                // Edges carry the process's region and agent so downstream filters can identify
                // `(process_id, region_id, agent_id)`.
                for (input, output) in iproduct!(inputs, outputs) {
                    let source_node_index = *node_index
                        .entry(input.clone())
                        .or_insert_with(|| graph.add_node(input.clone()));
                    let target_node_index = *node_index
                        .entry(output.clone())
                        .or_insert_with(|| graph.add_node(output.clone()));
                    let is_primary = match &output {
                        GraphNode::CommodityMarket(commodity_id, _, _) => {
                            primary_output.as_ref() == Some(commodity_id)
                        }
                        _ => false,
                    };

                    graph.add_edge(
                        source_node_index,
                        target_node_index,
                        if is_primary {
                            GraphEdge::Primary(
                                process.id.clone(),
                                region_id.clone(),
                                agent.id.clone(),
                            )
                        } else {
                            GraphEdge::Secondary(
                                process.id.clone(),
                                region_id.clone(),
                                agent.id.clone(),
                            )
                        },
                    );
                }
            }
        }
    }

    graph
}

/// Builds base commodity graphs for each year, covering all agents and regions.
///
/// These do not take into account demand and process availability.
pub fn build_commodity_graphs_for_model(
    processes: &ProcessMap,
    agents: &AgentMap,
    years: &[u32],
) -> IndexMap<u32, CommoditiesGraph> {
    years
        .iter()
        .map(|&year| {
            let graph = create_commodities_graph_for_year(processes, agents, year);
            (year, graph)
        })
        .collect()
}

/// Gets custom DOT attributes for edges in an agent-specific commodity graph
fn get_edge_attributes(_: &CommoditiesGraph, edge_ref: EdgeReference<GraphEdge>) -> String {
    match edge_ref.weight() {
        // Use dashed lines for secondary flows
        GraphEdge::Secondary(..) => "style=dashed".to_string(),
        // Other edges use default attributes
        _ => String::new(),
    }
}

/// Saves agent-specific commodity graphs to file
///
/// The graphs are saved as DOT files to the specified output path, one file per year, with a
/// `_v2` suffix.
pub fn save_commodity_graphs_for_model(
    commodity_graphs: &IndexMap<u32, CommoditiesGraph>,
    output_path: &Path,
) -> Result<()> {
    for (year, graph) in commodity_graphs {
        let dot = Dot::with_attr_getters(
            graph,
            &[],
            &get_edge_attributes,  // Custom attributes for edges
            &|_, _| String::new(), // Use default attributes for nodes
        );
        let mut file = File::create(output_path.join(format!("{year}_v2.dot")))?;
        write!(file, "{dot}")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::{Agent, AgentID, AgentSearchSpaceMap};
    use crate::commodity::Commodity;
    use crate::fixture::{agents, svd_commodity};
    use crate::process::{FlowType, ProcessFlow};
    use crate::units::{ActivityPerCapacity, Dimensionless, FlowPerActivity, MoneyPerFlow};
    use indexmap::{IndexSet, indexmap};
    use petgraph::graph::NodeIndex;
    use petgraph::visit::EdgeRef;

    fn commodity(id: &str) -> Arc<Commodity> {
        let mut commodity = svd_commodity();
        commodity.id = id.into();
        Arc::new(commodity)
    }

    fn process_flow(commodity: Arc<Commodity>, coefficient: f64) -> ProcessFlow {
        ProcessFlow {
            commodity,
            coeff: FlowPerActivity(coefficient),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        }
    }

    fn process_with_flows(
        region_id: &RegionID,
        year: u32,
        flows: IndexMap<CommodityID, ProcessFlow>,
        primary_output: CommodityID,
    ) -> Arc<Process> {
        Arc::new(Process {
            id: "producer".into(),
            description: String::new(),
            years: year..=year,
            activity_limits: HashMap::new(),
            flows: HashMap::from([((region_id.clone(), year), Arc::new(flows))]),
            parameters: HashMap::new(),
            regions: IndexSet::from([region_id.clone()]),
            primary_output: Some(primary_output),
            capacity_to_activity: ActivityPerCapacity(1.0),
            tranche_size: None,
        })
    }

    fn agent_with_share(
        template: &Agent,
        id: AgentID,
        commodity: &Commodity,
        region_id: &RegionID,
        year: u32,
        process: Option<Arc<Process>>,
    ) -> Agent {
        let mut agent = template.clone();
        agent.id = id;
        agent.commodity_portions.clear();
        agent
            .commodity_portions
            .insert((commodity.id.clone(), year), Dimensionless(0.5));
        if let Some(process) = process {
            agent.search_space = AgentSearchSpaceMap::from([(
                (commodity.id.clone(), region_id.clone(), year),
                Arc::new(vec![process]),
            )]);
        }
        agent
    }

    fn market_node(
        graph: &CommoditiesGraph,
        commodity: &Commodity,
        region_id: &RegionID,
        agent_id: &AgentID,
    ) -> NodeIndex {
        graph
            .node_indices()
            .find(|&node| {
                graph[node]
                    == GraphNode::CommodityMarket(
                        commodity.id.clone(),
                        region_id.clone(),
                        agent_id.clone(),
                    )
            })
            .unwrap()
    }

    fn has_edge(
        graph: &CommoditiesGraph,
        source: NodeIndex,
        target: NodeIndex,
        expected_edge: &GraphEdge,
    ) -> bool {
        graph.edge_references().any(|edge| {
            edge.source() == source && edge.target() == target && edge.weight() == expected_edge
        })
    }

    #[test]
    fn graph_connects_shared_markets_only_to_agents_with_shares() {
        let region_id: RegionID = "GBR".into();
        let input_commodity = commodity("commodity1");
        let primary_commodity = commodity("commodity2");
        let secondary_commodity = commodity("commodity3");
        let year = 2020;
        let process = process_with_flows(
            &region_id,
            year,
            indexmap! {
                input_commodity.id.clone() => process_flow(input_commodity.clone(), -1.0),
                primary_commodity.id.clone() => process_flow(primary_commodity.clone(), 1.0),
                secondary_commodity.id.clone() => process_flow(secondary_commodity.clone(), 1.0),
            },
            primary_commodity.id.clone(),
        );
        let processes = indexmap! {process.id.clone() => process.clone()};
        let mut agents = agents(IndexSet::from([region_id.clone()]));
        let input_agent_id: AgentID = "agent1".into();
        let process_agent_id: AgentID = "agent2".into();
        let secondary_owner_id: AgentID = "agent3".into();
        agents
            .get_mut(&input_agent_id)
            .unwrap()
            .commodity_portions
            .insert((input_commodity.id.clone(), year), Dimensionless(0.5));
        let template = agents[&input_agent_id].clone();
        let process_agent = agent_with_share(
            &template,
            process_agent_id.clone(),
            &primary_commodity,
            &region_id,
            year,
            Some(process.clone()),
        );
        agents.insert(process_agent_id.clone(), process_agent);
        let secondary_owner = agent_with_share(
            &template,
            secondary_owner_id.clone(),
            &secondary_commodity,
            &region_id,
            year,
            None,
        );
        agents.insert(secondary_owner_id.clone(), secondary_owner);

        let graph = create_commodities_graph_for_year(&processes, &agents, year);

        assert_eq!(graph.node_count(), 3);
        assert_eq!(graph.edge_count(), 2);
        let input_node = market_node(&graph, &input_commodity, &region_id, &input_agent_id);
        let primary_output_node =
            market_node(&graph, &primary_commodity, &region_id, &process_agent_id);
        let secondary_output_node = market_node(
            &graph,
            &secondary_commodity,
            &region_id,
            &secondary_owner_id,
        );

        assert!(has_edge(
            &graph,
            input_node,
            primary_output_node,
            &GraphEdge::Primary(
                "producer".into(),
                region_id.clone(),
                process_agent_id.clone(),
            ),
        ));
        assert!(has_edge(
            &graph,
            input_node,
            secondary_output_node,
            &GraphEdge::Secondary("producer".into(), region_id, process_agent_id,),
        ));
    }
}
