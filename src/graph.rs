//! Module for creating and analysing commodity graphs
use crate::commodity::CommodityID;
use crate::process::{FlowDirection, ProcessID, ProcessMap};
use crate::region::RegionID;
use anyhow::Result;
use indexmap::IndexSet;
use itertools::iproduct;
use petgraph::Directed;
use petgraph::dot::Dot;
use petgraph::graph::{EdgeReference, Graph};
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Write as IoWrite;
use std::path::Path;

pub mod investment;
pub mod validate;

/// A graph of commodity flows for a given region and year
pub type CommoditiesGraph = Graph<GraphNode, GraphEdge, Directed>;

#[derive(Eq, PartialEq, Clone, Hash)]
/// A node in the commodity graph
pub enum GraphNode {
    /// A node representing a commodity
    Commodity(CommodityID),
    /// A source node for processes that have no inputs
    Source,
    /// A sink node for processes that have no outputs
    Sink,
    /// A demand node for commodities with service demands
    Demand,
}

impl Display for GraphNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GraphNode::Commodity(id) => write!(f, "{id}"),
            GraphNode::Source => write!(f, "SOURCE"),
            GraphNode::Sink => write!(f, "SINK"),
            GraphNode::Demand => write!(f, "DEMAND"),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash)]
/// An edge in the commodity graph
pub enum GraphEdge {
    /// An edge representing a primary flow of a process
    Primary(ProcessID),
    /// An edge representing a secondary (non-primary) flow of a process
    Secondary(ProcessID),
    /// An edge representing a service demand
    Demand,
}

impl Display for GraphEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GraphEdge::Primary(process_id) | GraphEdge::Secondary(process_id) => {
                write!(f, "{process_id}")
            }
            GraphEdge::Demand => write!(f, "DEMAND"),
        }
    }
}

/// Creates a directed graph of commodity flows for a given region and year.
///
/// The graph contains nodes for all commodities that may be consumed/produced by processes in the
/// specified region/year. There will be an edge from commodity A to B if there exists a process
/// that consumes A and produces B.
///
/// There are also special `Source` and `Sink` nodes, which are used for processes that have no
/// inputs or outputs.
///
/// The graph does not take into account process availabilities or commodity demands, both of which
/// can vary by time slice. See `prepare_commodities_graph_for_validation`.
fn create_commodities_graph_for_region_year(
    processes: &ProcessMap,
    region_id: &RegionID,
    year: u32,
) -> CommoditiesGraph {
    let mut graph = Graph::new();
    let mut commodity_to_node_index = HashMap::new();

    let key = (region_id.clone(), year);
    for process in processes.values() {
        let Some(flows) = process.flows.get(&key) else {
            // Process doesn't operate in this region/year
            continue;
        };

        // Get output nodes for the process
        let mut outputs: Vec<_> = flows
            .values()
            .filter(|flow| flow.direction() == FlowDirection::Output)
            .map(|flow| GraphNode::Commodity(flow.commodity.id.clone()))
            .collect();

        // Get input nodes for the process
        let mut inputs: Vec<_> = flows
            .values()
            .filter(|flow| flow.direction() == FlowDirection::Input)
            .map(|flow| GraphNode::Commodity(flow.commodity.id.clone()))
            .collect();

        // Use `Source` node if no inputs, `Sink` node if no outputs
        if inputs.is_empty() {
            inputs.push(GraphNode::Source);
        }
        if outputs.is_empty() {
            outputs.push(GraphNode::Sink);
        }

        // Get primary output for the process
        let primary_output = &process.primary_output;

        // Create edges from all inputs to all outputs
        // We also create nodes the first time they are encountered
        for (input, output) in iproduct!(inputs, outputs) {
            let source_node_index = *commodity_to_node_index
                .entry(input.clone())
                .or_insert_with(|| graph.add_node(input.clone()));
            let target_node_index = *commodity_to_node_index
                .entry(output.clone())
                .or_insert_with(|| graph.add_node(output.clone()));
            let is_primary = match &output {
                GraphNode::Commodity(commodity_id) => primary_output.as_ref() == Some(commodity_id),
                _ => false,
            };

            graph.add_edge(
                source_node_index,
                target_node_index,
                if is_primary {
                    GraphEdge::Primary(process.id.clone())
                } else {
                    GraphEdge::Secondary(process.id.clone())
                },
            );
        }
    }

    graph
}

/// Builds base commodity graphs for each region and year
///
/// These do not take into account demand and process availability
pub fn build_commodity_graphs_for_model(
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
    years: &[u32],
) -> HashMap<(RegionID, u32), CommoditiesGraph> {
    iproduct!(region_ids, years.iter())
        .map(|(region_id, year)| {
            let graph = create_commodities_graph_for_region_year(processes, region_id, *year);
            ((region_id.clone(), *year), graph)
        })
        .collect()
}

/// Gets custom DOT attributes for edges in a commodity graph
fn get_edge_attributes(_: &CommoditiesGraph, edge_ref: EdgeReference<GraphEdge>) -> String {
    match edge_ref.weight() {
        // Use dashed lines for secondary flows
        GraphEdge::Secondary(_) => "style=dashed".to_string(),
        // Other edges use default attributes
        _ => String::new(),
    }
}

/// Saves commodity graphs to file
///
/// The graphs are saved as DOT files to the specified output path
pub fn save_commodity_graphs_for_model(
    commodity_graphs: &HashMap<(RegionID, u32), CommoditiesGraph>,
    output_path: &Path,
) -> Result<()> {
    for ((region_id, year), graph) in commodity_graphs {
        let dot = Dot::with_attr_getters(
            graph,
            &[],
            &get_edge_attributes,  // Custom attributes for edges
            &|_, _| String::new(), // Use default attributes for nodes
        );
        let mut file = File::create(output_path.join(format!("{region_id}_{year}.dot")))?;
        write!(file, "{dot}")?;
    }
    Ok(())
}
