//! Module for creating and analysing commodity graphs
use crate::commodity::{CommodityID, CommodityMap, CommodityType};
use crate::process::{ProcessID, ProcessMap};
use crate::region::RegionID;
use crate::time_slice::{TimeSliceInfo, TimeSliceLevel, TimeSliceSelection};
use crate::units::{Dimensionless, Flow};
use anyhow::{Context, Result, anyhow, ensure};
use indexmap::IndexSet;
use itertools::{Itertools, iproduct};
use petgraph::Directed;
use petgraph::algo::toposort;
use petgraph::dot::Dot;
use petgraph::graph::{EdgeReference, Graph};
use petgraph::visit::EdgeFiltered;
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::Write as IoWrite;
use std::path::Path;
use strum::IntoEnumIterator;

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
    /// An edge representing a secondary flow of a process
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
            .filter(|flow| flow.is_output())
            .map(|flow| GraphNode::Commodity(flow.commodity.id.clone()))
            .collect();

        // Get input nodes for the process
        let mut inputs: Vec<_> = flows
            .values()
            .filter(|flow| flow.is_input())
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
        let primary_output = process.primary_output.clone();

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
                GraphNode::Commodity(commodity_id) => primary_output == Some(commodity_id.clone()),
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

/// Prepares a graph for validation with [`validate_commodities_graph`].
///
/// It takes a base graph produced by `create_commodities_graph_for_region_year`, and modifies it to
/// account for process availabilities and commodity demands within the given time slice selection,
/// returning a new graph.
///
/// Commodity demands are represented by the `Demand` node. We only add edges to the `Demand` node
/// for commodities with the same `time_slice_level` as the selection. Other demands can be ignored
/// since this graph will only be validated for commodities with the same `time_slice_level` as the
/// selection.
fn prepare_commodities_graph_for_validation(
    base_graph: &CommoditiesGraph,
    processes: &ProcessMap,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
    region_id: &RegionID,
    year: u32,
    time_slice_selection: &TimeSliceSelection,
) -> CommoditiesGraph {
    let mut filtered_graph = base_graph.clone();

    // Filter by process availability
    // We keep edges if the process has availability > 0 in any time slice in the selection
    let key = (region_id.clone(), year);
    filtered_graph.retain_edges(|graph, edge_idx| {
        // Get the process for the edge
        let process_id = match graph.edge_weight(edge_idx).unwrap() {
            GraphEdge::Primary(process_id) | GraphEdge::Secondary(process_id) => process_id,
            GraphEdge::Demand => panic!("Demand edges should not be present in the base graph"),
        };
        let process = &processes[process_id];

        // Check if the process has availability > 0 in any time slice in the selection
        time_slice_selection
            .iter(time_slice_info)
            .any(|(time_slice, _)| {
                let Some(limits_map) = process.activity_limits.get(&key) else {
                    return false;
                };
                limits_map
                    .get(time_slice)
                    .is_some_and(|avail| *avail.end() > Dimensionless(0.0))
            })
    });

    // Add demand edges
    // We add edges to the `Demand` node for commodities that are demanded in the selection
    // NOTE: we only do this for commodities with the same time_slice_level as the selection
    let demand_node_index = filtered_graph.add_node(GraphNode::Demand);
    for (commodity_id, commodity) in commodities {
        if time_slice_selection.level() == commodity.time_slice_level
            && commodity
                .demand
                .get(&(region_id.clone(), year, time_slice_selection.clone()))
                .is_some_and(|&v| v > Flow(0.0))
        {
            let commodity_node = GraphNode::Commodity(commodity_id.clone());
            let commodity_node_index = filtered_graph
                .node_indices()
                .find(|&idx| filtered_graph.node_weight(idx) == Some(&commodity_node))
                .unwrap_or_else(|| {
                    filtered_graph.add_node(GraphNode::Commodity(commodity_id.clone()))
                });
            filtered_graph.add_edge(commodity_node_index, demand_node_index, GraphEdge::Demand);
        }
    }

    filtered_graph
}

/// Validates that the commodity graph follows the rules for different commodity types.
///
/// It takes as input a graph created by `create_commodities_graph_for_validation`, which is built
/// for a specific time slice selection (must match the `time_slice_level` passed to this function).
///
/// The validation is only performed for commodities with the specified time slice level. For full
/// validation of all commodities in the model, we therefore need to run this function for all time
/// slice selections at all time slice levels. This is handled by
/// [`validate_commodity_graphs_for_model`].
fn validate_commodities_graph(
    graph: &CommoditiesGraph,
    commodities: &CommodityMap,
    time_slice_level: TimeSliceLevel,
) -> Result<()> {
    for node_idx in graph.node_indices() {
        // Get the commodity ID for the node
        let graph_node = graph.node_weight(node_idx).unwrap();
        let GraphNode::Commodity(commodity_id) = graph_node else {
            // Skip special nodes
            continue;
        };

        // Only validate commodities with the specified time slice level
        let commodity = &commodities[commodity_id];
        if commodity.time_slice_level != time_slice_level {
            continue;
        }

        // Count the incoming and outgoing edges for the commodity
        let has_incoming = graph
            .edges_directed(node_idx, petgraph::Direction::Incoming)
            .next()
            .is_some();
        let has_outgoing = graph
            .edges_directed(node_idx, petgraph::Direction::Outgoing)
            .next()
            .is_some();

        // Match validation rules to commodity type
        match commodity.kind {
            CommodityType::ServiceDemand => {
                // Cannot have outgoing `Primary`/`Secondary` (non-`Demand`) edges
                let has_non_demand_outgoing = graph
                    .edges_directed(node_idx, petgraph::Direction::Outgoing)
                    .any(|edge| edge.weight() != &GraphEdge::Demand);
                ensure!(
                    !has_non_demand_outgoing,
                    "SVD commodity {commodity_id} cannot be an input to a process"
                );

                // If it has `Demand` edges, it must have at least one producer
                let has_demand_edges = graph
                    .edges_directed(node_idx, petgraph::Direction::Outgoing)
                    .any(|edge| edge.weight() == &GraphEdge::Demand);
                if has_demand_edges {
                    ensure!(
                        has_incoming,
                        "SVD commodity {commodity_id} is demanded but has no producers"
                    );
                }
            }
            CommodityType::SupplyEqualsDemand => {
                // SED: if consumed (outgoing edges), must also be produced (incoming edges)
                ensure!(
                    !has_outgoing || has_incoming,
                    "SED commodity {commodity_id} may be consumed but has no producers"
                );
            }
            CommodityType::Other => {
                // OTH: cannot have both incoming and outgoing edges
                ensure!(
                    !(has_incoming && has_outgoing),
                    "OTH commodity {commodity_id} cannot have both producers and consumers"
                );
            }
        }
    }

    Ok(())
}

/// Performs topological sort on the commodity graph to get the ordering for investments
///
/// The returned Vec only includes SVD and SED commodities.
fn topo_sort_commodities(
    graph: &CommoditiesGraph,
    commodities: &CommodityMap,
) -> Result<Vec<CommodityID>> {
    // We only consider primary edges
    let primary_graph =
        EdgeFiltered::from_fn(graph, |edge| matches!(edge.weight(), GraphEdge::Primary(_)));

    // Perform a topological sort on the graph
    let order = toposort(&primary_graph, None).map_err(|cycle| {
        let cycle_commodity = graph.node_weight(cycle.node_id()).unwrap().clone();
        anyhow!("Cycle detected in commodity graph for commodity {cycle_commodity}")
    })?;

    // We return the order in reverse so that leaf-node commodities are solved first
    // We also filter to only include SVD and SED commodities
    let order: Vec<CommodityID> = order
        .iter()
        .rev()
        .filter_map(|node_idx| {
            // Get the commodity for the node
            let GraphNode::Commodity(commodity_id) = graph.node_weight(*node_idx).unwrap() else {
                // Skip special nodes
                return None;
            };
            let commodity = &commodities[commodity_id];

            // Only include SVD and SED commodities
            matches!(
                commodity.kind,
                CommodityType::ServiceDemand | CommodityType::SupplyEqualsDemand
            )
            .then(|| commodity_id.clone())
        })
        .collect();

    Ok(order)
}

/// Builds base commodity graphs for each region and year
///
/// These do not take into account demand and process availability
pub fn build_commodity_graphs_for_model(
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
    years: &[u32],
) -> Result<HashMap<(RegionID, u32), CommoditiesGraph>> {
    let commodity_graphs: HashMap<(RegionID, u32), CommoditiesGraph> =
        iproduct!(region_ids, years.iter())
            .map(|(region_id, year)| {
                let graph = create_commodities_graph_for_region_year(processes, region_id, *year);
                ((region_id.clone(), *year), graph)
            })
            .collect();

    Ok(commodity_graphs)
}

/// Validates commodity graphs for the entire model.
///
/// This function creates commodity flow graphs for each region/year combination in the model,
/// validates the graph structure against commodity type rules, and determines the optimal
/// investment order for commodities.
///
/// The validation process checks three time slice levels:
/// - **Annual**: Validates annual-level commodities and processes
/// - **Seasonal**: Validates seasonal-level commodities and processes for each season
/// - **Day/Night**: Validates day/night-level commodities and processes for each time slice
///
/// # Arguments
///
/// * `processes` - All processes in the model with their flows and activity limits
/// * `commodities` - All commodities with their types and demand specifications
/// * `region_ids` - Collection of regions to model
/// * `years` - Years to analyse
/// * `time_slice_info` - Time slice configuration (seasons, day/night periods)
///
/// # Returns
///
/// A map from `(region, year)` to the ordered list of commodities for investment decisions. The
/// ordering ensures that leaf-node commodities (those with no outgoing edges) are solved first.
///
/// # Errors
///
/// Returns an error if:
/// - Any commodity graph contains cycles
/// - Commodity type rules are violated (e.g., SVD commodities being consumed)
/// - Demand cannot be satisfied
pub fn validate_commodity_graphs_for_model(
    commodity_graphs: &HashMap<(RegionID, u32), CommoditiesGraph>,
    processes: &ProcessMap,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) -> Result<HashMap<(RegionID, u32), Vec<CommodityID>>> {
    // Determine commodity ordering for each region and year
    let commodity_order: HashMap<(RegionID, u32), Vec<CommodityID>> = commodity_graphs
        .iter()
        .map(|((region_id, year), graph)| -> Result<_> {
            let order = topo_sort_commodities(graph, commodities).with_context(|| {
                format!("Error validating commodity graph for {region_id} in {year}")
            })?;
            Ok(((region_id.clone(), *year), order))
        })
        .try_collect()?;

    // Validate graphs at all time slice levels (taking into account process availability and demand)
    for ((region_id, year), base_graph) in commodity_graphs {
        for ts_level in TimeSliceLevel::iter() {
            for ts_selection in time_slice_info.iter_selections_at_level(ts_level) {
                let graph = prepare_commodities_graph_for_validation(
                    base_graph,
                    processes,
                    commodities,
                    time_slice_info,
                    region_id,
                    *year,
                    &ts_selection,
                );
                validate_commodities_graph(&graph, commodities, ts_level).with_context(|| {
                    format!(
                        "Error validating commodity graph for \
                            {region_id} in {year} in {ts_selection}"
                    )
                })?;
            }
        }
    }

    // If all the validation passes, return the commodity ordering
    Ok(commodity_order)
}

/// Gets DOT attributes for graph edges
fn get_edge_attributes(_: &CommoditiesGraph, edge_ref: EdgeReference<GraphEdge>) -> String {
    match edge_ref.weight() {
        GraphEdge::Primary(_) | GraphEdge::Demand => String::new(),
        GraphEdge::Secondary(_) => "style=dashed".to_string(),
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
        let dot = Dot::with_attr_getters(graph, &[], &get_edge_attributes, &|_, _| String::new());
        let mut file = File::create(output_path.join(format!("{region_id}_{year}.dot")))?;
        write!(file, "{dot}")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{assert_error, other_commodity, sed_commodity, svd_commodity};
    use petgraph::graph::Graph;
    use rstest::rstest;
    use std::rc::Rc;

    #[rstest]
    fn test_topo_sort_linear_graph(sed_commodity: Commodity, svd_commodity: Commodity) {
        // Create a simple linear graph: A -> B -> C
        let mut graph = Graph::new();

        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));
        let node_c = graph.add_node(GraphNode::Commodity("C".into()));

        // Add edges: A -> B -> C
        graph.add_edge(node_a, node_b, GraphEdge::Primary("process1".into()));
        graph.add_edge(node_b, node_c, GraphEdge::Primary("process2".into()));

        // Create commodities map using fixtures
        let mut commodities = CommodityMap::new();
        commodities.insert("A".into(), Rc::new(sed_commodity.clone()));
        commodities.insert("B".into(), Rc::new(sed_commodity));
        commodities.insert("C".into(), Rc::new(svd_commodity));

        let result = topo_sort_commodities(&graph, &commodities).unwrap();

        // Expected order: C, B, A (leaf nodes first)
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "C".into());
        assert_eq!(result[1], "B".into());
        assert_eq!(result[2], "A".into());
    }

    #[rstest]
    fn test_topo_sort_cyclic_graph(sed_commodity: Commodity) {
        // Create a simple cyclic graph: A -> B -> A
        let mut graph = Graph::new();

        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));

        // Add edges creating a cycle: A -> B -> A
        graph.add_edge(node_a, node_b, GraphEdge::Primary("process1".into()));
        graph.add_edge(node_b, node_a, GraphEdge::Primary("process2".into()));

        // Create commodities map using fixtures
        let mut commodities = CommodityMap::new();
        commodities.insert("A".into(), Rc::new(sed_commodity.clone()));
        commodities.insert("B".into(), Rc::new(sed_commodity));

        // This should return an error due to the cycle
        // The error message should flag commodity B
        // Note: A is also involved in the cycle, but B is flagged as it is encountered first
        let result = topo_sort_commodities(&graph, &commodities);
        assert_error!(result, "Cycle detected in commodity graph for commodity B");
    }

    #[rstest]
    fn test_validate_commodities_graph(
        other_commodity: Commodity,
        sed_commodity: Commodity,
        svd_commodity: Commodity,
    ) {
        let mut graph = Graph::new();
        let mut commodities = CommodityMap::new();

        // Add test commodities (all have DayNight time slice level)
        commodities.insert("A".into(), Rc::new(other_commodity));
        commodities.insert("B".into(), Rc::new(sed_commodity));
        commodities.insert("C".into(), Rc::new(svd_commodity));

        // Build valid graph: A(OTH) -> B(SED) -> C(SVD) ->D(DEMAND)
        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));
        let node_c = graph.add_node(GraphNode::Commodity("C".into()));
        let node_d = graph.add_node(GraphNode::Demand);
        graph.add_edge(node_a, node_b, GraphEdge::Primary("process1".into()));
        graph.add_edge(node_b, node_c, GraphEdge::Primary("process2".into()));
        graph.add_edge(node_c, node_d, GraphEdge::Demand);

        // Validate the graph at DayNight level
        let result = validate_commodities_graph(&graph, &commodities, TimeSliceLevel::Annual);
        assert!(result.is_ok());
    }

    #[rstest]
    fn test_validate_commodities_graph_invalid_svd_consumed(
        svd_commodity: Commodity,
        sed_commodity: Commodity,
        other_commodity: Commodity,
    ) {
        let mut graph = Graph::new();
        let mut commodities = CommodityMap::new();

        // Add test commodities (all have DayNight time slice level)
        commodities.insert("A".into(), Rc::new(svd_commodity));
        commodities.insert("B".into(), Rc::new(sed_commodity));
        commodities.insert("C".into(), Rc::new(other_commodity));

        // Build invalid graph: C(OTH) -> A(SVD) -> B(SED) - SVD cannot be consumed
        let node_c = graph.add_node(GraphNode::Commodity("C".into()));
        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));
        graph.add_edge(node_c, node_a, GraphEdge::Primary("process1".into()));
        graph.add_edge(node_a, node_b, GraphEdge::Primary("process2".into()));

        // Validate the graph at DayNight level
        let result = validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight);
        assert_error!(result, "SVD commodity A cannot be an input to a process");
    }

    #[rstest]
    fn test_validate_commodities_graph_invalid_svd_not_produced(svd_commodity: Commodity) {
        let mut graph = Graph::new();
        let mut commodities = CommodityMap::new();

        // Add test commodities (all have DayNight time slice level)
        commodities.insert("A".into(), Rc::new(svd_commodity));

        // Build invalid graph: A(SVD) -> B(DEMAND) - SVD must be produced
        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Demand);
        graph.add_edge(node_a, node_b, GraphEdge::Demand);

        // Validate the graph at DayNight level
        let result = validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight);
        assert_error!(result, "SVD commodity A is demanded but has no producers");
    }

    #[rstest]
    fn test_validate_commodities_graph_invalid_sed(sed_commodity: Commodity) {
        let mut graph = Graph::new();
        let mut commodities = CommodityMap::new();

        // Add test commodities (all have DayNight time slice level)
        commodities.insert("A".into(), Rc::new(sed_commodity.clone()));
        commodities.insert("B".into(), Rc::new(sed_commodity));

        // Build invalid graph: B(SED) -> A(SED)
        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));
        graph.add_edge(node_b, node_a, GraphEdge::Primary("process1".into()));

        // Validate the graph at DayNight level
        let result = validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight);
        assert_error!(
            result,
            "SED commodity B may be consumed but has no producers"
        );
    }

    #[rstest]
    fn test_validate_commodities_graph_invalid_oth(
        other_commodity: Commodity,
        sed_commodity: Commodity,
    ) {
        let mut graph = Graph::new();
        let mut commodities = CommodityMap::new();

        // Add test commodities (all have DayNight time slice level)
        commodities.insert("A".into(), Rc::new(other_commodity));
        commodities.insert("B".into(), Rc::new(sed_commodity.clone()));
        commodities.insert("C".into(), Rc::new(sed_commodity));

        // Build invalid graph: B(SED) -> A(OTH) -> C(SED)
        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));
        let node_c = graph.add_node(GraphNode::Commodity("C".into()));
        graph.add_edge(node_b, node_a, GraphEdge::Primary("process1".into()));
        graph.add_edge(node_a, node_c, GraphEdge::Primary("process2".into()));

        // Validate the graph at DayNight level
        let result = validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight);
        assert_error!(
            result,
            "OTH commodity A cannot have both producers and consumers"
        );
    }
}
