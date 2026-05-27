//! Module for creating and analysing commodity graphs
use crate::commodity::CommodityID;
use crate::process::{FlowDirection, Process, ProcessFlow, ProcessID, ProcessMap};
use crate::region::RegionID;
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use itertools::iproduct;
use petgraph::Directed;
use petgraph::dot::Dot;
use petgraph::graph::{EdgeReference, Graph};
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Write as IoWrite;
use std::path::Path;
use std::rc::Rc;

pub mod investment;
pub mod validate;

/// A graph of commodity flows for a given region and year
pub type CommoditiesGraph = Graph<GraphNode, GraphEdge, Directed>;

#[derive(Eq, PartialEq, Clone, Hash)]
/// A node in the commodity graph
pub enum GraphNode {
    /// A node representing a commodity in a specific region
    Commodity(CommodityID, RegionID),
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
            GraphNode::Commodity(id, region) => write!(f, "{id}:{region}"),
            GraphNode::Source => write!(f, "SOURCE"),
            GraphNode::Sink => write!(f, "SINK"),
            GraphNode::Demand => write!(f, "DEMAND"),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash)]
/// An edge in the commodity graph
pub enum GraphEdge {
    /// An edge representing a primary flow of a process installed in the given region
    Primary(ProcessID, RegionID),
    /// An edge representing a secondary (non-primary) flow of a process installed in the given region
    Secondary(ProcessID, RegionID),
    /// An edge representing a service demand
    Demand,
}

impl Display for GraphEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GraphEdge::Primary(process_id, _) | GraphEdge::Secondary(process_id, _) => {
                write!(f, "{process_id}")
            }
            GraphEdge::Demand => write!(f, "DEMAND"),
        }
    }
}

/// Helper function to return a possible flow operating in the requested year
///
/// We are only interested in the flow directions, which are constant across years. This
/// function checks whether the process can be operating in the target region and year and, if so,
/// returns its flows. It considers both the commission year and the process lifetime, since a
/// process may operate for years after its commission window. If the process cannot be operating
/// in the target region/year, `None` is returned.
fn get_flow_for_year(process: &Process, target: (RegionID, u32)) -> Option<Rc<Vec<ProcessFlow>>> {
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

/// Creates a directed graph of commodity flows for a given year, covering all regions.
///
/// The graph contains nodes for all `(commodity, region)` pairs that may be consumed/produced
/// by processes in the specified year across all regions. There will be an edge from node
/// `(A, Ra)` to `(B, Rb)` if there exists a process installed in some region that consumes
/// commodity A in region Ra and produces commodity B in region Rb.
///
/// There are also special `Source` and `Sink` nodes, which are used for processes that have no
/// inputs or outputs.
///
/// The graph does not take into account process availabilities or commodity demands, both of which
/// can vary by time slice. See `prepare_commodities_graph_for_validation`.
fn create_commodities_graph_for_year(
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
    year: u32,
) -> CommoditiesGraph {
    let mut graph = Graph::new();
    let mut node_index = HashMap::new();

    for region_id in region_ids {
        let key = (region_id.clone(), year);
        for process in processes.values() {
            let Some(flows) = get_flow_for_year(process, key.clone()) else {
                // Process doesn't operate in this region/year
                continue;
            };

            // Get output nodes for the process (keyed by the flow's target region)
            let mut outputs: Vec<_> = flows
                .iter()
                .filter(|flow| flow.direction() == FlowDirection::Output)
                .map(|flow| GraphNode::Commodity(flow.commodity.id.clone(), flow.region_id.clone()))
                .collect();

            // Get input nodes for the process (keyed by the flow's source region)
            let mut inputs: Vec<_> = flows
                .iter()
                .filter(|flow| flow.direction() == FlowDirection::Input)
                .map(|flow| GraphNode::Commodity(flow.commodity.id.clone(), flow.region_id.clone()))
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

            // Create edges from all inputs to all outputs.
            // Edges carry the asset's installing region so the availability filter in
            // validation can look up `(process_id, asset_region, year)`.
            for (input, output) in iproduct!(inputs, outputs) {
                let source_node_index = *node_index
                    .entry(input.clone())
                    .or_insert_with(|| graph.add_node(input.clone()));
                let target_node_index = *node_index
                    .entry(output.clone())
                    .or_insert_with(|| graph.add_node(output.clone()));
                let is_primary = match &output {
                    GraphNode::Commodity(commodity_id, _) => {
                        primary_output.as_ref() == Some(commodity_id)
                    }
                    _ => false,
                };

                graph.add_edge(
                    source_node_index,
                    target_node_index,
                    if is_primary {
                        GraphEdge::Primary(process.id.clone(), region_id.clone())
                    } else {
                        GraphEdge::Secondary(process.id.clone(), region_id.clone())
                    },
                );
            }
        }
    }

    graph
}

/// Builds base commodity graphs for each year, covering all regions.
///
/// These do not take into account demand and process availability.
pub fn build_commodity_graphs_for_model(
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
    years: &[u32],
) -> IndexMap<u32, CommoditiesGraph> {
    years
        .iter()
        .map(|&year| {
            let graph = create_commodities_graph_for_year(processes, region_ids, year);
            (year, graph)
        })
        .collect()
}

/// Gets custom DOT attributes for edges in a commodity graph
fn get_edge_attributes(_: &CommoditiesGraph, edge_ref: EdgeReference<GraphEdge>) -> String {
    match edge_ref.weight() {
        // Use dashed lines for secondary flows
        GraphEdge::Secondary(..) => "style=dashed".to_string(),
        // Other edges use default attributes
        _ => String::new(),
    }
}

/// Saves commodity graphs to file
///
/// The graphs are saved as DOT files to the specified output path, one file per year.
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
        let mut file = File::create(output_path.join(format!("{year}.dot")))?;
        write!(file, "{dot}")?;
    }
    Ok(())
}
