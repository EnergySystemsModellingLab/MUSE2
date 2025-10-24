//! Module for solving the investment order of commodities
use super::{CommoditiesGraph, GraphEdge, GraphNode};
use crate::commodity::{CommodityID, CommodityMap, CommodityType};
use crate::region::RegionID;
use crate::simulation::investment::InvestmentSet;
use petgraph::Directed;
use petgraph::algo::{condensation, toposort};
use petgraph::graph::Graph;
use petgraph::prelude::NodeIndex;
use std::collections::HashMap;

type CompressedCommodityGraph = Graph<CompressedNode, GraphEdge, Directed>;

enum CompressedNode {
    Single(GraphNode),
    Cycle(Vec<GraphNode>),
    _Layer(Vec<GraphNode>),
}

impl CompressedNode {
    fn to_investment_set(&self) -> InvestmentSet {
        match self {
            CompressedNode::Single(node) => {
                if let GraphNode::Commodity(id) = node {
                    InvestmentSet::Single(id.clone())
                } else {
                    unreachable!("CompressedNode::Single must contain a Commodity node");
                }
            }
            CompressedNode::Cycle(nodes) => {
                let commodity_ids: Vec<CommodityID> = nodes
                    .iter()
                    .map(|node| match node {
                        GraphNode::Commodity(id) => id.clone(),
                        _ => {
                            unreachable!("CompressedNode::Cycle must contain only Commodity nodes")
                        }
                    })
                    .collect();
                InvestmentSet::Cycle(commodity_ids)
            }
            CompressedNode::_Layer(nodes) => {
                let commodity_ids: Vec<CommodityID> = nodes
                    .iter()
                    .map(|node| match node {
                        GraphNode::Commodity(id) => id.clone(),
                        _ => {
                            unreachable!("CompressedNode::Layer must contain only Commodity nodes")
                        }
                    })
                    .collect();
                InvestmentSet::Layer(commodity_ids)
            }
        }
    }
}

/// Performs topological sort on the commodity graph to get the ordering for investments
///
/// The returned Vec only includes SVD and SED commodities.
fn solve_investment_order(
    graph: &CommoditiesGraph,
    commodities: &CommodityMap,
) -> Vec<InvestmentSet> {
    // Filter the graph to only include SVD/SED commodities and primary edges
    let graph_filtered = graph.filter_map(
        // Consider only SVD/SED commodities
        |_, node_weight| {
            // Get the commodity for the node
            let GraphNode::Commodity(commodity_id) = node_weight else {
                // Skip special nodes
                return None;
            };
            let commodity = &commodities[commodity_id];
            matches!(
                commodity.kind,
                CommodityType::ServiceDemand | CommodityType::SupplyEqualsDemand
            )
            .then_some(node_weight.clone())
        },
        // Consider only primary edges
        |_, edge_weight| {
            matches!(edge_weight, GraphEdge::Primary(_)).then_some(edge_weight.clone())
        },
    );

    // Condense strongly connected components
    let condensed_graph = compress_cycles(&graph_filtered);

    // Perform a topological sort on the condensed graph
    // We can safely unwrap because `toposort` will only return an error in case of cycles, which
    // should have been detected and compressed with `compress_cycles`
    let order = toposort(&condensed_graph, None).unwrap();

    // Create investment sets (reverse topological order)
    order
        .iter()
        .rev()
        .map(|node_idx| {
            condensed_graph
                .node_weight(*node_idx)
                .unwrap()
                .to_investment_set()
        })
        .collect()
}

fn compress_cycles(graph: &CommoditiesGraph) -> CompressedCommodityGraph {
    // Detect strongly connected components
    let condensed_graph = condensation(graph.clone(), true);

    // Map to a new CompressedCommodityGraph
    condensed_graph.map(
        // Map nodes to CompressedNode
        |_, node_weight| match node_weight.len() {
            0 => unreachable!("Condensed graph node must have at least one member"),
            1 => CompressedNode::Single(node_weight[0].clone()),
            _ => CompressedNode::Cycle(node_weight.clone()),
        },
        // Keep edges the same
        |_, edge_weight| edge_weight.clone(),
    )
}

fn _assign_ranks(
    graph: &CompressedCommodityGraph,
    order: Vec<NodeIndex>,
) -> HashMap<NodeIndex, usize> {
    let mut rank = HashMap::new();

    // Initialize all ranks to 0
    for node in graph.node_indices() {
        rank.insert(node, 0);
    }

    // Traverse in topological order
    for u in order {
        let current_rank = *rank.get(&u).unwrap();
        for v in graph.neighbors_directed(u, petgraph::Direction::Outgoing) {
            let entry = rank.entry(v).or_insert(0);
            *entry = (*entry).max(current_rank + 1);
        }
    }

    rank
}

/// Determine commodity ordering for each region and year
///
/// # Arguments
///
/// * `commodity_graphs` - Commodity graphs for each region and year, outputted from `build_commodity_graphs_for_model`
/// * `commodities` - All commodities with their types and demand specifications
///
/// # Returns
///
/// A map from `(region, year)` to the ordered list of commodities for investment decisions. The
/// ordering ensures that leaf-node commodities (those with no outgoing edges) are solved first.
pub fn solve_investment_order_for_model(
    commodity_graphs: &HashMap<(RegionID, u32), CommoditiesGraph>,
    commodities: &CommodityMap,
) -> HashMap<(RegionID, u32), Vec<InvestmentSet>> {
    commodity_graphs
        .iter()
        .map(|((region_id, year), graph)| {
            (
                (region_id.clone(), *year),
                solve_investment_order(graph, commodities),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{sed_commodity, svd_commodity};
    use petgraph::graph::Graph;
    use rstest::rstest;
    use std::rc::Rc;

    #[rstest]
    fn test_solve_investment_order_linear_graph(
        sed_commodity: Commodity,
        svd_commodity: Commodity,
    ) {
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

        let result = solve_investment_order(&graph, &commodities);

        // Expected order: C, B, A (leaf nodes first)
        // No cycles, so all investment sets should be `Single`
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], InvestmentSet::Single("C".into()));
        assert_eq!(result[1], InvestmentSet::Single("B".into()));
        assert_eq!(result[2], InvestmentSet::Single("A".into()));
    }

    #[rstest]
    fn test_solve_investment_order_cyclic_graph(sed_commodity: Commodity) {
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

        let result = solve_investment_order(&graph, &commodities);

        // Should be a single `Cycle` investment set containing both commodities
        assert_eq!(result.len(), 1);
        assert_eq!(
            result[0],
            InvestmentSet::Cycle(vec!["A".into(), "B".into()])
        );
    }
}
