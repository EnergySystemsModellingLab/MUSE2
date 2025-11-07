//! Module for solving the investment order of commodities
use super::{CommoditiesGraph, GraphEdge, GraphNode};
use crate::commodity::{CommodityMap, CommodityType};
use crate::region::RegionID;
use crate::simulation::investment::InvestmentSet;
use indexmap::IndexMap;
use petgraph::algo::{condensation, toposort};
use petgraph::graph::Graph;
use petgraph::prelude::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::{Directed, Direction};
use std::collections::HashMap;

type InvestmentGraph = Graph<InvestmentSet, GraphEdge, Directed>;

/// Analyse the commodity graphs for a given year to determine the order in which investment
/// decisions should be made.
///
/// Steps:
/// 1. Initialise an `InvestmentGraph` from the set of original `CommodityGraph`s for the given
///    year, filtering to only include SVD/SED commodities and primary edges. `CommodityGraph`s from
///    all regions are combined into a single `InvestmentGraph`. TODO: at present there can be no
///    edges between regions; in future we will want to implement trade as edges between regions,
///    but this will have no impact on the following steps.
/// 2. Condense strongly connected components (cycles) into `InvestmentSet::Cycle` nodes.
/// 3. Perform a topological sort on the condensed graph.
/// 4. Compute layers for investment based on the topological order, grouping independent sets into
///    `InvestmentSet::Layer`s.
///
/// Arguments:
/// * `graphs` - Commodity graphs for each region and year, outputted from `build_commodity_graphs_for_model`
/// * `commodities` - All commodities with their types and demand specifications
/// * `year` - The year to solve the investment order for
///
/// # Returns
/// A Vec of `InvestmentSet`s in the order they should be solved, with cycles grouped into
/// `InvestmentSet::Cycle`s and independent sets grouped into `InvestmentSet::Layer`s.
fn solve_investment_order_for_year(
    graphs: &IndexMap<(RegionID, u32), CommoditiesGraph>,
    commodities: &CommodityMap,
    year: u32,
) -> Vec<InvestmentSet> {
    // Initialise InvestmentGraph for this year from the set of original `CommodityGraph`s
    let mut investment_graph = init_investment_graph_for_year(graphs, year, commodities);

    // TODO: condense sibling commodities (commodities that share at least one producer)

    // Condense strongly connected components
    investment_graph = compress_cycles(investment_graph);

    // Perform a topological sort on the condensed graph
    // We can safely unwrap because `toposort` will only return an error in case of cycles, which
    // should have been detected and compressed with `compress_cycles`
    let order = toposort(&investment_graph, None).unwrap();

    // Compute layers for investment
    compute_layers(&investment_graph, &order)
}

/// Initialise an `InvestmentGraph` for the given year from a set of `CommodityGraph`s
///
/// Commodity graphs for each region are first filtered to only include SVD/SED commodities and
/// primary edges. Each commodity node is then added to a global investment graph as an
/// `InvestmentSet::Single`, with edges preserved from the original commodity graphs.
fn init_investment_graph_for_year(
    graphs: &IndexMap<(RegionID, u32), CommoditiesGraph>,
    year: u32,
    commodities: &CommodityMap,
) -> InvestmentGraph {
    let mut combined = InvestmentGraph::new();

    // Iterate over the graphs for the given year
    for ((region_id, _), graph) in graphs.iter().filter(|((_, y), _)| *y == year) {
        // Filter the graph to only include SVD/SED commodities and primary edges
        let filtered = graph.filter_map(
            |_, n| match n {
                GraphNode::Commodity(cid) => {
                    let kind = &commodities[cid].kind;
                    matches!(
                        kind,
                        CommodityType::ServiceDemand | CommodityType::SupplyEqualsDemand
                    )
                    .then_some(GraphNode::Commodity(cid.clone()))
                }
                _ => None,
            },
            |_, e| matches!(e, GraphEdge::Primary(_)).then_some(e.clone()),
        );

        // Add nodes to the combined graph
        let node_map: HashMap<_, _> = filtered
            .node_indices()
            .map(|ni| {
                let GraphNode::Commodity(cid) = filtered.node_weight(ni).unwrap() else {
                    unreachable!()
                };
                (
                    ni,
                    combined.add_node(InvestmentSet::Single((cid.clone(), region_id.clone()))),
                )
            })
            .collect();

        // Add edges to the combined graph
        for e in filtered.edge_references() {
            combined.add_edge(
                node_map[&e.source()],
                node_map[&e.target()],
                e.weight().clone(),
            );
        }
    }

    combined
}

/// Compresses cycles into `InvestmentSet::Cycle` nodes
fn compress_cycles(graph: InvestmentGraph) -> InvestmentGraph {
    // Detect strongly connected components
    let condensed_graph = condensation(graph, true);

    // Map to a new InvestmentGraph
    condensed_graph.map(
        // Map nodes to InvestmentSet
        // If only one member, keep as-is; if multiple members, create Cycle
        |_, node_weight| match node_weight.len() {
            0 => unreachable!("Condensed graph node must have at least one member"),
            1 => node_weight[0].clone(),
            _ => InvestmentSet::Cycle(
                node_weight
                    .iter()
                    .flat_map(|s| s.iter_markets())
                    .cloned()
                    .collect(),
            ),
        },
        // Keep edges the same
        |_, edge_weight| edge_weight.clone(),
    )
}

/// Compute layers of investment sets from the topological order
///
/// This function works by computing the rank of each node in the graph based on the longest path
/// from any root node to that node. Any nodes with the same rank are independent and can be solved
/// in parallel. Nodes with different rank must be solved in order from highest rank (leaf nodes)
/// to lowest rank (root nodes).
///
/// This function computes the ranks of each node, groups nodes by rank, and then produces a final
/// ordered Vec of `InvestmentSet`s which gives the order in which to solve the investment decisions.
///
/// Investment sets with the same rank (i.e., can be solved in parallel) are grouped into
/// `InvestmentSet::Layer`. Investment sets that are alone in their rank remain as-is (i.e. either
/// `Single` or `Cycle`). `Layer`s can contain a mix of `Single` and `Cycle` investment sets.
///
/// For example, given the following graph:
///
/// ```text
///     A
///    / \
///   B   C
///  / \   \
/// D   E   F
/// ```
///
/// Rank 0: A -> `InvestmentSet::Single`
/// Rank 1: B, C -> `InvestmentSet::Layer`
/// Rank 2: D, E, F -> `InvestmentSet::Layer`
///
/// These are returned as a `Vec<InvestmentSet>` from highest rank to lowest (i.e. the D, E, F layer
/// first, then the B, C layer, then the singleton A).
///
/// Arguments:
/// * `graph` - The investment graph. Any cycles in the graph MUST have already been compressed.
///   This will be necessary anyway as computing a topological sort to obtain the `order` requires
///   an acyclic graph.
/// * `order` - The topological order of the graph nodes. Computed using `petgraph::algo::toposort`.
///
/// Returns:
/// A Vec of `InvestmentSet`s in the order they should be solved, with independent sets grouped into
/// `InvestmentSet::Layer`s.
fn compute_layers(graph: &InvestmentGraph, order: &[NodeIndex]) -> Vec<InvestmentSet> {
    // Initialize all ranks to 0
    let mut ranks: HashMap<_, usize> = graph.node_indices().map(|n| (n, 0)).collect();

    // Calculate the rank of each node by traversing in topological order
    // The algorithm works by iterating through each node in topological order and updating the ranks
    // of its neighbors to be at least one more than the current node's rank.
    for &u in order {
        let current_rank = ranks[&u];
        for v in graph.neighbors_directed(u, Direction::Outgoing) {
            if let Some(r) = ranks.get_mut(&v) {
                *r = (*r).max(current_rank + 1);
            }
        }
    }

    // Group nodes by rank
    let max_rank = ranks.values().copied().max().unwrap_or(0);
    let mut groups: Vec<Vec<InvestmentSet>> = vec![Vec::new(); max_rank + 1];
    for node_idx in order {
        let rank = *ranks.get(node_idx).unwrap();
        let w = graph.node_weight(*node_idx).unwrap().clone();
        groups[rank].push(w);
    }

    // Produce final ordered Vec<InvestmentSet>: ranks descending (leaf-first),
    // compressing equal-rank nodes into an InvestmentSet::Layer.
    let mut result = Vec::new();
    for mut items in groups.into_iter().rev() {
        if items.is_empty() {
            unreachable!("Should be no gaps in the ranking")
        }
        // If only one InvestmentSet in the group, we do not need to compress into a layer, so just
        // push the single item (this item may be a `Single` or `Cycle`).
        if items.len() == 1 {
            result.push(items.remove(0));
        // Otherwise, create a layer. The items within the layer may be a mix of `Single` or `Cycle`.
        } else {
            result.push(InvestmentSet::Layer(items));
        }
    }

    result
}

/// Determine investment ordering for each year
///
/// # Arguments
///
/// * `commodity_graphs` - Commodity graphs for each region and year, outputted from `build_commodity_graphs_for_model`
/// * `commodities` - All commodities with their types and demand specifications
///
/// # Returns
///
/// A map from `year` to the ordered list of `InvestmentSet`s for investment decisions. The
/// ordering ensures that leaf-node `InvestmentSet`s (those with no outgoing edges) are solved
/// first.
pub fn solve_investment_order_for_model(
    commodity_graphs: &IndexMap<(RegionID, u32), CommoditiesGraph>,
    commodities: &CommodityMap,
    years: &[u32],
) -> HashMap<u32, Vec<InvestmentSet>> {
    let mut investment_orders = HashMap::new();
    for year in years {
        let order = solve_investment_order_for_year(commodity_graphs, commodities, *year);
        investment_orders.insert(*year, order);
    }
    investment_orders
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

        let graphs = IndexMap::from([(("GBR".into(), 2020), graph)]);
        let result = solve_investment_order_for_year(&graphs, &commodities, 2020);

        // Expected order: C, B, A (leaf nodes first)
        // No cycles or layers, so all investment sets should be `Single`
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], InvestmentSet::Single(("C".into(), "GBR".into())));
        assert_eq!(result[1], InvestmentSet::Single(("B".into(), "GBR".into())));
        assert_eq!(result[2], InvestmentSet::Single(("A".into(), "GBR".into())));
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

        let graphs = IndexMap::from([(("GBR".into(), 2020), graph)]);
        let result = solve_investment_order_for_year(&graphs, &commodities, 2020);

        // Should be a single `Cycle` investment set containing both commodities
        assert_eq!(result.len(), 1);
        assert_eq!(
            result[0],
            InvestmentSet::Cycle(vec![("A".into(), "GBR".into()), ("B".into(), "GBR".into())])
        );
    }

    #[rstest]
    fn test_solve_investment_order_layered_graph(
        sed_commodity: Commodity,
        svd_commodity: Commodity,
    ) {
        // Create a graph with layers:
        //     A
        //    / \
        //   B   C
        //    \ /
        //     D
        let mut graph = Graph::new();

        let node_a = graph.add_node(GraphNode::Commodity("A".into()));
        let node_b = graph.add_node(GraphNode::Commodity("B".into()));
        let node_c = graph.add_node(GraphNode::Commodity("C".into()));
        let node_d = graph.add_node(GraphNode::Commodity("D".into()));

        // Add edges
        graph.add_edge(node_a, node_b, GraphEdge::Primary("process1".into()));
        graph.add_edge(node_a, node_c, GraphEdge::Primary("process2".into()));
        graph.add_edge(node_b, node_d, GraphEdge::Primary("process3".into()));
        graph.add_edge(node_c, node_d, GraphEdge::Primary("process4".into()));

        // Create commodities map using fixtures
        let mut commodities = CommodityMap::new();
        commodities.insert("A".into(), Rc::new(sed_commodity.clone()));
        commodities.insert("B".into(), Rc::new(sed_commodity.clone()));
        commodities.insert("C".into(), Rc::new(sed_commodity));
        commodities.insert("D".into(), Rc::new(svd_commodity));

        let graphs = IndexMap::from([(("GBR".into(), 2020), graph)]);
        let result = solve_investment_order_for_year(&graphs, &commodities, 2020);

        // Expected order: D, Layer(B, C), A
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], InvestmentSet::Single(("D".into(), "GBR".into())));
        assert_eq!(
            result[1],
            InvestmentSet::Layer(vec![
                InvestmentSet::Single(("B".into(), "GBR".into())),
                InvestmentSet::Single(("C".into(), "GBR".into()))
            ])
        );
        assert_eq!(result[2], InvestmentSet::Single(("A".into(), "GBR".into())));
    }

    #[rstest]
    fn test_solve_investment_order_multiple_regions(
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

        // Duplicate the graph over two regions
        let graphs = IndexMap::from([
            (("GBR".into(), 2020), graph.clone()),
            (("FRA".into(), 2020), graph),
        ]);
        let result = solve_investment_order_for_year(&graphs, &commodities, 2020);

        // Expected order: Should have three layers, each with two commodities (one per region)
        assert_eq!(result.len(), 3);
        assert_eq!(
            result[0],
            InvestmentSet::Layer(vec![
                InvestmentSet::Single(("C".into(), "GBR".into())),
                InvestmentSet::Single(("C".into(), "FRA".into()))
            ])
        );
        assert_eq!(
            result[1],
            InvestmentSet::Layer(vec![
                InvestmentSet::Single(("B".into(), "GBR".into())),
                InvestmentSet::Single(("B".into(), "FRA".into()))
            ])
        );
        assert_eq!(
            result[2],
            InvestmentSet::Layer(vec![
                InvestmentSet::Single(("A".into(), "GBR".into())),
                InvestmentSet::Single(("A".into(), "FRA".into()))
            ])
        );
    }
}
