//! Module for solving the investment order of commodities
use super::{CommoditiesGraph, GraphEdge, GraphNode};
use crate::commodity::{CommodityMap, CommodityType};
use crate::region::RegionID;
use crate::simulation::investment::InvestmentSet;
use highs::{Col, HighsModelStatus, RowProblem, Sense};
use indexmap::IndexMap;
use log::warn;
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
    investment_graph = compress_cycles(&investment_graph);

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
fn compress_cycles(graph: &InvestmentGraph) -> InvestmentGraph {
    // Detect strongly connected components
    let mut condensed_graph = condensation(graph.clone(), true);

    // Order nodes within each strongly connected component
    order_sccs(&mut condensed_graph, graph);

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

/// Order the members of each strongly connected component using a mixed-integer linear program.
///
/// `condensed_graph` contains the SCCs detected in the original investment graph, stored as
/// `Vec<InvestmentSet>` node weights. Single-element components are already acyclic, but components
/// with multiple members require an internal ordering so that the investment algorithm can treat
/// them as near-acyclic chains, minimising potential disruption.
///
/// To rank the members of each multi-node component, we construct a mixed integer linear program
/// (MILP). This ordering MILP is a direct instance of the classical Linear Ordering Problem, using
/// binary precedence variables with antisymmetry and transitivity constraints to enforce an acyclic
/// total order. For background, see:
///
///   Reinelt, G. (1985).
///   *The Linear Ordering Problem: Algorithms and Applications*.
///   Springer, Lecture Notes in Economics and Mathematical Systems, Vol. 237.
///
/// The main features of the MILP are:
/// * Binary variables `x[i][j]` represent whether market `i` should appear before market `j`.
/// * Antisymmetry constraints force each pair `(i, j)` to choose exactly one direction (i.e. if
///   `i` comes before `j`, then `j` cannot be before `i`).
/// * Transitivity constraints prevent 3-cycles, ensuring the resulting relation is acyclic (i.e. if
///   `i` comes before `j` and `j` comes before `k`, then `k` cannot come before `i`).
/// * The objective minimises the number of “forward” edges (edges that would point from an earlier
///   market to a later one), counted within the original SCC and treated as unit penalties. A small
///   bias nudges exporters earlier without outweighing the main objective; a bias >1 would
///   instead prioritise exporters even if it created extra conflicts in the final order.
///
/// Once the MILP is solved, markets are scored by the number of pairwise “wins” (how many other
/// markets they precede). Sorting by this score — using the original index as a tiebreaker to keep
/// relative order stable — yields the final sequence that replaces the SCC in the condensed graph.
/// At least one pairwise mismatch is always inevitable (e.g. where X is solved before Y, but Y may
/// consume X, so the demand for X cannot be guaranteed upfront).
///
/// # Example
///
/// Suppose three markets (A, B and C) form a cycle in the original graph with the following edges:
///
/// ```text
/// A ← B ← C ← A
/// ```
///
/// Additionally, C has an outgoing edge to a node outside the cycle.
///
/// The costs matrix in the MILP is set up to penalise any edge that points “forward” in the final
/// order: if there's an edge from X to Y we prefer to place Y before X so the edge points backwards:
///
/// ```text
///    |   | A | B | C |
///    | A | 0 | 0 | 1 |
///    | B | 1 | 0 | 0 |
///    | C | 0 | 1 | 0 |
/// ```
///
/// On top of this, we give a small preference to markets that export outside the SCC, so nodes with
/// outgoing edges beyond the cycle are pushed earlier. This is done via an `EXTERNAL_BIAS`
/// parameter (B) applied to the cost matrix:
///
/// ```text
///    |   | A | B | C     |
///    | A | 0 | 0 | 1 + B |  i.e. extra penalty for putting A before C
///    | B | 1 | 0 | 0 + B |  i.e. extra penalty for putting B before C
///    | C | 0 | 1 | 0     |
/// ```
///
/// Solving this problem with binary decision variables for each x[i][j], and constraints to enforce
/// antisymmetry and transitivity, yields optimal decision variables of:
///
/// ```text
///    x[A][B] = 1 (A before B)
///    x[A][C] = 0 (C before A)
///    x[B][A] = 0 (A before B)
///    x[B][C] = 0 (C before B)
///    x[C][A] = 1 (C before A)
///    x[C][B] = 1 (C before B)
/// ```
///
/// From these, summing the number of times each market is preferred over another gives an optimal
/// order of:
///
/// ```text
/// C, A, B
/// ```
///
/// * By scheduling C before A before B, the edges C ← A and A ← B incur no cost because their
///   targets appear earlier than their sources.
/// * The preference towards having exporter markets early in the order keeps C at the front.
/// * As with any SCC, at least one pairwise violation is guaranteed. In this ordering, the only
///   pairwise violation is between B and C, as C is solved before B, but B may consume C.
///
/// The resulting order replaces the original `InvestmentSet::Cycle` entry inside the condensed
/// graph, providing a deterministic processing sequence for downstream logic.
#[allow(clippy::too_many_lines)]
fn order_sccs(
    condensed_graph: &mut Graph<Vec<InvestmentSet>, GraphEdge>,
    original_graph: &InvestmentGraph,
) {
    const EXTERNAL_BIAS: f64 = 0.1;

    // Map each investment set back to the node index in the original graph so we can inspect edges.
    let node_lookup: HashMap<InvestmentSet, NodeIndex> = original_graph
        .node_indices()
        .map(|idx| (original_graph.node_weight(idx).unwrap().clone(), idx))
        .collect();

    // Work through each SCC; groups with just one investment set don't need to be ordered.
    for group in condensed_graph.node_indices() {
        let scc = condensed_graph.node_weight_mut(group).unwrap();
        let n = scc.len();
        if n <= 1 {
            continue;
        }

        // Capture current order and resolve each investment set back to its original graph index.
        let original_order = scc.clone();
        let original_indices = original_order
            .iter()
            .map(|set| {
                node_lookup
                    .get(set)
                    .copied()
                    .expect("Condensed SCC node must exist in the original graph")
            })
            .collect::<Vec<_>>();

        // Build a fast lookup from original node index to its position in the SCC slice.
        let mut index_position = HashMap::new();
        for (pos, idx) in original_indices.iter().copied().enumerate() {
            index_position.insert(idx, pos);
        }

        // Record whether any edge inside the original SCC goes from market i to market j; these become penalties.
        let mut penalties = vec![vec![0.0f64; n]; n];
        let mut has_external_outgoing = vec![false; n];
        for (i, &idx) in original_indices.iter().enumerate() {
            // Loop over the edges going out of this node
            for edge in original_graph.edges_directed(idx, Direction::Outgoing) {
                // If the target j is inside this SCC, record a penalty for putting i before j
                if let Some(&j) = index_position.get(&edge.target()) {
                    penalties[i][j] = 1.0;

                // Otherwise, mark that i has an outgoing edge to outside the SCC
                } else {
                    has_external_outgoing[i] = true;
                }
            }
        }

        // Bias: if market j has outgoing edges to nodes outside this SCC, we prefer to place it earlier.
        for (j, has_external) in has_external_outgoing.iter().enumerate() {
            if *has_external {
                for (row_idx, row) in penalties.iter_mut().enumerate() {
                    // Add a small bias to all entries in column j, except the diagonal
                    // i.e. penalise putting any other market before market j
                    if row_idx != j {
                        row[j] += EXTERNAL_BIAS;
                    }
                }
            }
        }

        // Build a MILP whose binary variables x[i][j] indicate "i is ordered before j".
        // Objective: minimise Σ penalty[i][j] · x[i][j], so forward edges (and the export bias) add cost.
        let mut problem = RowProblem::default();
        let mut vars: Vec<Vec<Option<Col>>> = vec![vec![None; n]; n];
        for (i, row) in vars.iter_mut().enumerate() {
            for (j, slot) in row.iter_mut().enumerate() {
                if i == j {
                    continue;
                }
                let cost = penalties[i][j];

                // Create binary variable x[i][j]
                *slot = Some(problem.add_integer_column(cost, 0..=1));
            }
        }

        // Enforce antisymmetry: for each pair (i, j), exactly one of x[i][j] and x[j][i] is 1.
        // i.e. if i comes before j, then j cannot come before i.
        for (i, row) in vars.iter().enumerate() {
            for (j, _) in row.iter().enumerate().skip(i + 1) {
                let Some(x_ij) = vars[i][j] else { continue };
                let Some(x_ji) = vars[j][i] else { continue };
                problem.add_row(1.0..=1.0, [(x_ij, 1.0), (x_ji, 1.0)]);
            }
        }

        // Enforce transitivity to avoid 3-cycles: x[i][j] + x[j][k] + x[k][i] ≤ 2.
        // i.e. if i comes before j and j comes before k, then k cannot come before i.
        for (i, row) in vars.iter().enumerate() {
            for (j, _) in row.iter().enumerate() {
                if i == j {
                    continue;
                }
                for (k, _) in vars.iter().enumerate() {
                    if i == k || j == k {
                        continue;
                    }
                    let Some(x_ij) = vars[i][j] else { continue };
                    let Some(x_jk) = vars[j][k] else { continue };
                    let Some(x_ki) = vars[k][i] else { continue };
                    problem.add_row(..=2.0, [(x_ij, 1.0), (x_jk, 1.0), (x_ki, 1.0)]);
                }
            }
        }

        let mut model = problem.optimise(Sense::Minimise);
        model.make_quiet();
        let solved = match model.try_solve() {
            Ok(solved) => solved,
            Err(status) => {
                warn!("HiGHS failed while ordering an SCC: {status:?}");
                continue;
            }
        };

        if solved.status() != HighsModelStatus::Optimal {
            let status = solved.status();
            warn!("HiGHS returned a non-optimal status while ordering an SCC: {status:?}");
            continue;
        }

        let solution = solved.get_solution();
        // Score each market by the number of "wins" it achieves (times it must precede another).
        let mut wins = vec![0usize; n];
        for (i, row) in vars.iter().enumerate() {
            for (j, var) in row.iter().enumerate() {
                if i == j {
                    continue;
                }
                if var.is_some_and(|col| solution[col] > 0.5) {
                    wins[i] += 1;
                }
            }
        }

        // Sort by descending win count; break ties on the original index so equal-score nodes keep
        // their relative order.
        let mut order: Vec<usize> = (0..n).collect();
        order.sort_by(|&a, &b| wins[b].cmp(&wins[a]).then_with(|| a.cmp(&b)));

        // Rewrite the SCC in the new order
        *scc = order
            .into_iter()
            .map(|idx| original_order[idx].clone())
            .collect();
    }
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

    #[test]
    fn test_order_sccs_simple_cycle() {
        let markets = ["A", "B", "C"].map(|id| InvestmentSet::Single((id.into(), "GBR".into())));

        // Create graph with cycle edges plus an extra dependency B ← D (see doc comment)
        let mut original = InvestmentGraph::new();
        let node_indices: Vec<_> = markets
            .iter()
            .map(|set| original.add_node(set.clone()))
            .collect();
        for &(src, dst) in &[(1, 0), (2, 1), (0, 2)] {
            original.add_edge(
                node_indices[src],
                node_indices[dst],
                GraphEdge::Primary("process1".into()),
            );
        }
        // External market receiving exports from C; encourages C to appear early.
        let external = original.add_node(InvestmentSet::Single(("X".into(), "GBR".into())));
        original.add_edge(
            node_indices[2],
            external,
            GraphEdge::Primary("process2".into()),
        );

        // Single SCC containing all markets.
        let mut condensed: Graph<Vec<InvestmentSet>, GraphEdge> = Graph::new();
        let component = condensed.add_node(markets.to_vec());

        order_sccs(&mut condensed, &original);

        // Expected order corresponds to the example in the doc comment.
        // Note that C should be first, as it has an outgoing edge to the external market.
        let expected = ["C", "A", "B"]
            .map(|id| InvestmentSet::Single((id.into(), "GBR".into())))
            .to_vec();

        assert_eq!(condensed.node_weight(component).unwrap(), &expected);
    }

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
