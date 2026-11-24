//! Module for creating and analysing agent-to-agent commodity flow graphs
use crate::agent::{AgentID, AgentMap};
use crate::commodity::CommodityID;
use crate::process::{FlowDirection, Process, ProcessFlow, ProcessID, ProcessMap};
use crate::region::RegionID;
use anyhow::{Result, anyhow};
use indexmap::IndexMap;
use itertools::iproduct;
use petgraph::Directed;
use petgraph::algo::toposort;
use petgraph::dot::Dot;
use petgraph::graph::{EdgeReference, Graph};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write as IoWrite;
use std::path::Path;
use std::sync::Arc;

/// A graph of agent-to-agent commodity flows for a given year.
pub type AgentGraph = Graph<AgentGraphNode, GraphEdge, Directed>;

#[derive(Eq, PartialEq, Clone, Hash, derive_more::Display)]
/// A node in an agent graph.
pub enum AgentGraphNode {
    /// A node representing an agent
    #[display("{_0}")]
    Agent(AgentID),
    /// A source node for processes that have no inputs
    #[display("SOURCE")]
    Source,
    /// A sink node for processes that have no outputs
    #[display("SINK")]
    Sink,
}

#[derive(Eq, PartialEq, Clone, Hash, derive_more::Display)]
/// An edge in the agent graph.
pub enum GraphEdge {
    /// A primary flow of a process installed in the given region for an agent.
    #[display("{_0} | {_1} | {_2}")]
    Primary(ProcessID, RegionID, AgentID),
    /// A secondary flow of a process installed in the given region for an agent.
    #[display("{_0} | {_1} | {_2}")]
    Secondary(ProcessID, RegionID, AgentID),
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

/// Gets graph nodes for agents with a share of a commodity in the given region and year.
fn get_agent_market_nodes(
    agents: &AgentMap,
    commodity_id: &CommodityID,
    region_id: &RegionID,
    year: u32,
) -> Vec<AgentGraphNode> {
    agents
        .values()
        .filter(|agent| {
            agent.regions.contains(region_id)
                && agent
                    .commodity_portions
                    .contains_key(&(commodity_id.clone(), year))
        })
        .map(|agent| AgentGraphNode::Agent(agent.id.clone()))
        .collect()
}

/// Creates an agent-to-agent graph of commodity flows for a given year.
///
/// Each market node is mapped directly to its agent, so an agent is represented by a single node
/// even when it owns shares of multiple commodities. Edges retain the process, region, and agent
/// whose search space includes the process. Processes without inputs or outputs use `Source` and
/// `Sink` nodes, respectively.
fn create_agent_graph_for_year(processes: &ProcessMap, agents: &AgentMap, year: u32) -> AgentGraph {
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

                // Commodities enter a common regional market, so any agent with a share can
                // supply this process's input.
                let mut inputs: Vec<_> = input_flows
                    .iter()
                    .flat_map(|flow| {
                        get_agent_market_nodes(agents, &flow.commodity.id, region_id, year)
                    })
                    .collect();
                // Keep each output's primary/secondary status while mapping its market to agents.
                let mut outputs: Vec<_> = output_flows
                    .iter()
                    .flat_map(|flow| {
                        let is_primary =
                            process.primary_output.as_ref() == Some(&flow.commodity.id);
                        get_agent_market_nodes(agents, &flow.commodity.id, region_id, year)
                            .into_iter()
                            .map(move |node| (node, is_primary))
                    })
                    .collect();

                // Use `Source` node if no inputs, `Sink` node if no outputs
                if input_flows.is_empty() {
                    inputs.push(AgentGraphNode::Source);
                }
                if output_flows.is_empty() {
                    outputs.push((AgentGraphNode::Sink, false));
                }

                // Skip processes when no owned market is available on either side.
                if inputs.is_empty() || outputs.is_empty() {
                    continue;
                }

                // Create edges from all inputs to all outputs.
                // Multiple markets owned by the same agent can create parallel edges.
                for (input, (output, is_primary)) in iproduct!(inputs, outputs) {
                    let source_node_index = *node_index
                        .entry(input.clone())
                        .or_insert_with(|| graph.add_node(input.clone()));
                    let target_node_index = *node_index
                        .entry(output.clone())
                        .or_insert_with(|| graph.add_node(output.clone()));

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

/// Builds agent graphs for each year, covering all agents and regions.
pub fn build_agent_graphs_for_model(
    processes: &ProcessMap,
    agents: &AgentMap,
    years: &[u32],
) -> IndexMap<u32, AgentGraph> {
    years
        .iter()
        .map(|&year| {
            let graph = create_agent_graph_for_year(processes, agents, year);
            (year, graph)
        })
        .collect()
}

/// Returns agents in downstream-first topological order.
///
/// `Source` and `Sink` nodes participate in sorting but are omitted from the result.
/// Returns an error if the graph contains a cycle.
pub fn downstream_first_topological_order(graph: &AgentGraph) -> Result<Vec<AgentID>> {
    let order = toposort(graph, None).map_err(|_| anyhow!("Agent graph contains a cycle"))?;
    Ok(order
        .into_iter()
        .rev()
        .filter_map(|node| match &graph[node] {
            AgentGraphNode::Agent(agent_id) => Some(agent_id.clone()),
            AgentGraphNode::Source | AgentGraphNode::Sink => None,
        })
        .collect())
}

/// Gets custom DOT attributes for edges in an agent graph.
fn get_agent_edge_attributes(_: &AgentGraph, edge_ref: EdgeReference<GraphEdge>) -> String {
    match edge_ref.weight() {
        // Use dashed lines for secondary flows
        GraphEdge::Secondary(..) => "style=dashed".to_string(),
        // Other edges use default attributes
        GraphEdge::Primary(..) => String::new(),
    }
}

/// Saves agent graphs to file
///
/// The graphs are saved as DOT files to the specified output path, one file per year, with a
/// `_v3` suffix.
pub fn save_agent_graphs_for_model(
    agent_graphs: &IndexMap<u32, AgentGraph>,
    output_path: &Path,
) -> Result<()> {
    for (year, graph) in agent_graphs {
        let dot = Dot::with_attr_getters(graph, &[], &get_agent_edge_attributes, &|_, _| {
            String::new()
        });
        let mut file = File::create(output_path.join(format!("{year}_v3.dot")))?;
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
        id: &str,
        region_id: &RegionID,
        year: u32,
        flows: IndexMap<CommodityID, ProcessFlow>,
        primary_output: Option<CommodityID>,
    ) -> Arc<Process> {
        Arc::new(Process {
            id: id.into(),
            description: String::new(),
            years: year..=year,
            activity_limits: HashMap::new(),
            flows: HashMap::from([((region_id.clone(), year), Arc::new(flows))]),
            parameters: HashMap::new(),
            regions: IndexSet::from([region_id.clone()]),
            primary_output,
            capacity_to_activity: ActivityPerCapacity(1.0),
            investment_constraints: HashMap::new(),
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

    fn graph_node(graph: &AgentGraph, expected: &AgentGraphNode) -> NodeIndex {
        graph
            .node_indices()
            .find(|&node| &graph[node] == expected)
            .unwrap()
    }

    fn has_edge<N>(
        graph: &Graph<N, GraphEdge, Directed>,
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
            "producer",
            &region_id,
            year,
            indexmap! {
                input_commodity.id.clone() => process_flow(input_commodity.clone(), -1.0),
                primary_commodity.id.clone() => process_flow(primary_commodity.clone(), 1.0),
                secondary_commodity.id.clone() => process_flow(secondary_commodity.clone(), 1.0),
            },
            Some(primary_commodity.id.clone()),
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
        let mut process_agent = agent_with_share(
            &template,
            process_agent_id.clone(),
            &primary_commodity,
            &region_id,
            year,
            Some(process.clone()),
        );
        process_agent
            .commodity_portions
            .insert((secondary_commodity.id.clone(), year), Dimensionless(0.5));
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

        let graphs = build_agent_graphs_for_model(&processes, &agents, &[year]);
        let graph = &graphs[&year];

        assert_eq!(graph.node_count(), 3);
        assert_eq!(graph.edge_count(), 3);
        let input_node = graph_node(graph, &AgentGraphNode::Agent(input_agent_id.clone()));
        let primary_output_node =
            graph_node(graph, &AgentGraphNode::Agent(process_agent_id.clone()));
        let secondary_output_node =
            graph_node(graph, &AgentGraphNode::Agent(secondary_owner_id.clone()));

        assert!(has_edge(
            graph,
            input_node,
            primary_output_node,
            &GraphEdge::Primary(
                "producer".into(),
                region_id.clone(),
                process_agent_id.clone(),
            ),
        ));
        assert!(has_edge(
            graph,
            input_node,
            primary_output_node,
            &GraphEdge::Secondary(
                "producer".into(),
                region_id.clone(),
                process_agent_id.clone(),
            ),
        ));
        assert!(has_edge(
            graph,
            input_node,
            secondary_output_node,
            &GraphEdge::Secondary("producer".into(), region_id, process_agent_id,),
        ));
    }

    #[test]
    fn graph_preserves_source_and_sink_edges() {
        let region_id: RegionID = "GBR".into();
        let input_commodity = commodity("input");
        let output_commodity = commodity("output");
        let year = 2020;
        let source_process = process_with_flows(
            "source_process",
            &region_id,
            year,
            indexmap! {
                output_commodity.id.clone() => process_flow(output_commodity.clone(), 1.0),
            },
            Some(output_commodity.id.clone()),
        );
        let sink_process = process_with_flows(
            "sink_process",
            &region_id,
            year,
            indexmap! {
                input_commodity.id.clone() => process_flow(input_commodity.clone(), -1.0),
            },
            None,
        );

        let processes = indexmap! {
            source_process.id.clone() => source_process.clone(),
            sink_process.id.clone() => sink_process.clone(),
        };
        let mut agents = agents(IndexSet::from([region_id.clone()]));
        let template = agents.values().next().unwrap().clone();
        agents.clear();
        let input_agent_id: AgentID = "input_agent".into();
        let output_agent_id: AgentID = "output_agent".into();
        let input_agent = agent_with_share(
            &template,
            input_agent_id.clone(),
            &input_commodity,
            &region_id,
            year,
            Some(sink_process),
        );
        let output_agent = agent_with_share(
            &template,
            output_agent_id.clone(),
            &output_commodity,
            &region_id,
            year,
            Some(source_process),
        );
        agents.insert(input_agent_id.clone(), input_agent);
        agents.insert(output_agent_id.clone(), output_agent);

        let graphs = build_agent_graphs_for_model(&processes, &agents, &[year]);
        let graph = &graphs[&year];
        let input_node = graph_node(graph, &AgentGraphNode::Agent(input_agent_id.clone()));
        let output_node = graph_node(graph, &AgentGraphNode::Agent(output_agent_id.clone()));
        let source_node = graph_node(graph, &AgentGraphNode::Source);
        let sink_node = graph_node(graph, &AgentGraphNode::Sink);

        assert_eq!(graph.node_count(), 4);
        assert_eq!(graph.edge_count(), 2);
        assert!(has_edge(
            graph,
            source_node,
            output_node,
            &GraphEdge::Primary("source_process".into(), region_id.clone(), output_agent_id),
        ));
        assert!(has_edge(
            graph,
            input_node,
            sink_node,
            &GraphEdge::Secondary("sink_process".into(), region_id, input_agent_id),
        ));
    }

    #[test]
    fn downstream_first_order_reverses_topological_order() {
        let mut graph = AgentGraph::new();
        let source = graph.add_node(AgentGraphNode::Source);
        let agent1 = graph.add_node(AgentGraphNode::Agent("agent1".into()));
        let agent2 = graph.add_node(AgentGraphNode::Agent("agent2".into()));
        let sink = graph.add_node(AgentGraphNode::Sink);
        graph.add_edge(
            source,
            agent1,
            GraphEdge::Primary("process1".into(), "GBR".into(), "agent1".into()),
        );
        graph.add_edge(
            agent1,
            agent2,
            GraphEdge::Primary("process2".into(), "GBR".into(), "agent2".into()),
        );
        graph.add_edge(
            agent2,
            sink,
            GraphEdge::Primary("process3".into(), "GBR".into(), "agent2".into()),
        );

        let order = downstream_first_topological_order(&graph)
            .unwrap()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        assert_eq!(order, ["agent2", "agent1"]);
    }

    #[test]
    fn downstream_first_order_errors_on_cycles() {
        let mut graph = AgentGraph::new();
        let agent1 = graph.add_node(AgentGraphNode::Agent("agent1".into()));
        let agent2 = graph.add_node(AgentGraphNode::Agent("agent2".into()));
        graph.add_edge(
            agent1,
            agent2,
            GraphEdge::Primary("process1".into(), "GBR".into(), "agent1".into()),
        );
        graph.add_edge(
            agent2,
            agent1,
            GraphEdge::Primary("process2".into(), "GBR".into(), "agent2".into()),
        );

        assert_eq!(
            downstream_first_topological_order(&graph)
                .unwrap_err()
                .to_string(),
            "Agent graph contains a cycle"
        );
    }
}
