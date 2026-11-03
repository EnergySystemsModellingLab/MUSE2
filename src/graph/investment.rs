//! Module for solving the investment order of commodities
use super::{CommoditiesGraph, GraphEdge, GraphNode};
use crate::commodity::{CommodityMap, CommodityType};
use crate::simulation::market::MarketSet;
use crate::simulation::optimisation::{ModelError, solve_optimal};
use anyhow::{Context, Result, ensure};
use highs::{Col, RowProblem, Sense};
use indexmap::IndexMap;
use petgraph::algo::{condensation, toposort};
use petgraph::graph::Graph;
use petgraph::prelude::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::{Directed, Direction};
use std::collections::HashMap;

/// A graph of the market sets to invest in, with no cycles
type InvestmentGraph = Graph<MarketSet, GraphEdge, Directed>;

/// Analyse a commodity graph to determine the order in which investment decisions should be made.
///
/// Steps:
/// 1. Filter the commodity graph to only include SVD/SED commodities (all edges between them,
///    primary and secondary, are kept).
/// 2. Condense strongly connected components (cycles) into single nodes, and order the markets
///    within each one (see [`order_scc`]). These become `MarketSet::Cycle`s.
/// 3. Perform a topological sort on the condensed graph.
/// 4. Compute layers for investment based on the topological order, grouping independent sets into
///    `MarketSet::Layer`s.
///
/// Arguments:
/// * `graph` - Commodity graph
/// * `commodities` - All commodities with their types and demand specifications
///
/// # Returns
/// A Vec of `MarketSet`s in the order they should be solved, with cycles grouped into
/// `MarketSet::Cycle`s and independent sets grouped into `MarketSet::Layer`s.
fn solve_investment_order(
    graph: &CommoditiesGraph,
    commodities: &CommodityMap,
) -> Result<Vec<MarketSet>> {
    // Filter to only SVD/SED commodities. Each node's weight is its index in the commodity graph.
    let market_graph = graph.filter_map(
        |idx, node| is_market(node, commodities).then_some(idx),
        |_, edge| Some(edge.clone()),
    );

    // Condense strongly connected components, so that each node holds the commodity graph indices
    // of one or more markets
    let mut condensed_graph = condensation(market_graph, true);

    // Order the markets within each strongly connected component
    for scc in condensed_graph.node_weights_mut() {
        if scc.len() > 1 {
            order_scc(graph, commodities, scc)?;
        }
    }

    // Convert each node to a `MarketSet`
    let investment_graph =
        condensed_graph.map(|_, scc| to_market_set(graph, scc), |_, edge| edge.clone());

    // Perform a topological sort on the condensed graph
    // We can safely unwrap because `toposort` will only return an error in case of cycles, which
    // have been condensed
    let order = toposort(&investment_graph, None).unwrap();

    // Compute layers for investment
    Ok(compute_layers(&investment_graph, &order))
}

/// Whether a commodity graph node is an SVD/SED commodity (i.e. one that is invested in)
fn is_market(node: &GraphNode, commodities: &CommodityMap) -> bool {
    match node {
        GraphNode::CommodityMarket(commodity_id, _) => matches!(
            commodities[commodity_id].kind,
            CommodityType::ServiceDemand | CommodityType::SupplyEqualsDemand
        ),
        _ => false,
    }
}

/// Convert the commodity graph indices of a strongly connected component into a `MarketSet`
///
/// Components with one market become `MarketSet::Single`, and those with multiple markets become
/// `MarketSet::Cycle`.
fn to_market_set(graph: &CommoditiesGraph, scc: &[NodeIndex]) -> MarketSet {
    // Function to look up a node in the commodity graph and get its (commodity, region) pair.
    // All nodes are commodities, because `filter_map` only kept SVD/SED commodity nodes.
    let market = |&idx: &NodeIndex| match &graph[idx] {
        GraphNode::CommodityMarket(commodity_id, region_id) => {
            (commodity_id.clone(), region_id.clone())
        }
        _ => panic!("Strongly connected components should only contain commodities"),
    };
    match scc {
        // Exactly one member: a single market that isn't part of a cycle
        [idx] => MarketSet::Single(market(idx)),
        // Several members: a cycle, already in investment order (from `order_scc`)
        [_, _, ..] => MarketSet::Cycle(scc.iter().map(market).collect()),
        // An SCC must never be empty
        [] => panic!("unexpected empty SCC"),
    }
}

/// Order the markets in a strongly connected component (SCC) using a mixed-integer linear program.
///
/// The markets in an SCC (i.e. a cycle) depend on one another, so they cannot be put in an order
/// that respects every dependency. They still require an internal ordering so that the investment
/// algorithm can treat them as near-acyclic chains, minimising potential disruption.
///
/// To rank the markets, we construct a mixed integer linear program (MILP). This MILP is adapted
/// from the classical Linear Ordering Problem:
///
/// Marti, Rafael, and G Reinelt.
/// The Linear Ordering Problem: Exact and Heuristic Methods in Combinatorial Optimization.
/// 1st ed. 2011. Berlin: Springer-Verlag, 2011. Web.
///
/// The main features of the MILP are:
/// * Binary variables `x[i][j]` represent whether market `i` should appear before market `j`.
/// * Antisymmetry constraints force each pair `(i, j)` to choose exactly one direction (i.e. if
///   `i` comes before `j`, then `j` cannot be before `i`).
/// * Transitivity constraints prevent 3-cycles, ensuring the resulting relation is acyclic (i.e. if
///   `i` comes before `j` and `j` comes before `k`, then `k` cannot come before `i`).
/// * Incoming-edge constraints require every SCC node to retain at least one correctly-ordered
///   incoming edge within the SCC unless it has an external incoming edge.
/// * The objective minimises the number of “forward” edges (edges that would point from an earlier
///   market to a later one), counted within the original SCC and treated as unit penalties. A small
///   bias (<1) is added to nudge exporters earlier without outweighing the main objective (a bias
///   >1 would instead prioritise exporters even if it created extra conflicts in the final order).
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
/// Additionally, C has an outgoing edge to a node downstream of the cycle, and B has an incoming
/// edge from upstream.
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
/// Additionally, each node must retain at least one incoming edge from a node that appears after it
/// in the final investment order. This includes edges within the SCC and any external incoming
/// edges. In this example, since B has an incoming edge from outside the SCC, this constraint
/// implies that only A and C must retain a correctly-ordered incoming edge from within the SCC:
///
/// ```text
///    x[A][B] >= 1
///    x[B][C] >= 0
///    x[C][A] >= 1
/// ```
///
/// Solving this problem with binary decision variables for each `x[i][j]`, and additional
/// constraints to enforce antisymmetry and transitivity, yields optimal decision variables of:
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
/// * This pairwise violation is permitted because B has an additional producer upstream of the SCC.
///
/// `scc` holds the commodity graph indices of the SCC's markets, and is sorted in place into the
/// final order. If the MILP cannot be solved, an error is returned (e.g. if the SCC has no inputs
/// from outside it).
fn order_scc(
    graph: &CommoditiesGraph,
    commodities: &CommodityMap,
    scc: &mut [NodeIndex],
) -> Result<()> {
    const EXTERNAL_BIAS: f64 = 0.1;
    let n = scc.len();
    if n < 2 {
        return Ok(());
    }

    // Build a fast lookup from commodity graph index to position in the SCC slice.
    let index_position: HashMap<_, _> = scc
        .iter()
        .enumerate()
        .map(|(pos, &idx)| (idx, pos))
        .collect();

    // Record whether any edge inside the SCC goes from market i to market j; these become penalties.
    let mut penalties = vec![vec![0.0f64; n]; n];
    let mut has_external_outgoing = vec![false; n];
    let mut has_external_incoming = vec![false; n];
    for (i, &idx) in scc.iter().enumerate() {
        // Loop over the edges going out of this node
        for edge in graph.edges_directed(idx, Direction::Outgoing) {
            // If the target j is inside this SCC, record a penalty for putting i before j
            if let Some(&j) = index_position.get(&edge.target()) {
                penalties[i][j] = 1.0;

            // Otherwise, if the target is a market outside the SCC, mark that i has an outgoing
            // edge to outside the SCC
            } else if is_market(&graph[edge.target()], commodities) {
                has_external_outgoing[i] = true;
            }
        }

        // Check whether this node has any incoming edges from outside the SCC, including from
        // SOURCE nodes
        has_external_incoming[i] = graph
            .neighbors_directed(idx, Direction::Incoming)
            .any(|source| !index_position.contains_key(&source));
    }

    // Make sure the SCC has at least one external incoming edge
    ensure!(
        has_external_incoming.contains(&true),
        "SCC {} has no inputs",
        to_market_set(graph, scc)
    );

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
        for j in 0..row.len() {
            if i == j {
                continue;
            }
            for k in 0..vars.len() {
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

    // Every SCC node must retain at least one correctly-ordered incoming edge.
    for i in 0..n {
        let mut incoming_terms = Vec::new();
        for j in 0..n {
            if i == j {
                continue;
            }

            // Check if the commodity graph contains j -> i.
            if graph.contains_edge(scc[j], scc[i]) {
                // Get the variable saying whether i comes before j (i.e. "correct" ordering
                // for a j -> i edge) and add it to the terms
                incoming_terms.push((vars[i][j].unwrap(), 1.0));
            }
        }

        // If the node has an incoming edge from outside the SCC, then it doesn't need a
        // correctly-ordered internal incoming edge. Otherwise it does (i.e. a least one
        // variable in `incoming_terms` must be 1.0)
        let required = if has_external_incoming[i] { 0.0 } else { 1.0 };
        problem.add_row(required.., incoming_terms);
    }

    // Solve model
    let model = problem.optimise(Sense::Minimise);
    let solved = solve_optimal(model)
        .map_err(ModelError::into_anyhow)
        .with_context(|| {
            format!(
                "Could not find a valid investment order for circular dependency {}",
                to_market_set(graph, scc)
            )
        })?;
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
    let ordered: Vec<_> = order.into_iter().map(|i| scc[i]).collect();
    scc.copy_from_slice(&ordered);

    Ok(())
}

/// Compute layers of market sets from the topological order
///
/// This function works by computing the rank of each node in the graph based on the longest path
/// from any root node to that node. Any nodes with the same rank are independent and can be solved
/// in parallel. Nodes with different rank must be solved in order from highest rank (leaf nodes)
/// to lowest rank (root nodes).
///
/// This function computes the ranks of each node, groups nodes by rank, and then produces a final
/// ordered Vec of `MarketSet`s which gives the order in which to solve the investment decisions.
///
/// Market sets with the same rank (i.e., can be solved in parallel) are grouped into
/// `MarketSet::Layer`. Market sets that are alone in their rank remain as-is (i.e. either
/// `Single` or `Cycle`). `Layer`s can contain a mix of `Single` and `Cycle` market sets.
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
/// Rank 0: A -> `MarketSet::Single`
/// Rank 1: B, C -> `MarketSet::Layer`
/// Rank 2: D, E, F -> `MarketSet::Layer`
///
/// These are returned as a `Vec<MarketSet>` from highest rank to lowest (i.e. the D, E, F layer
/// first, then the B, C layer, then the singleton A).
///
/// Arguments:
/// * `graph` - The investment graph. Any cycles in the graph MUST have already been compressed.
///   This will be necessary anyway as computing a topological sort to obtain the `order` requires
///   an acyclic graph.
/// * `order` - The topological order of the graph nodes. Computed using `petgraph::algo::toposort`.
///
/// Returns:
/// A Vec of `MarketSet`s in the order they should be solved, with independent sets grouped into
/// `MarketSet::Layer`s.
fn compute_layers(graph: &InvestmentGraph, order: &[NodeIndex]) -> Vec<MarketSet> {
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
    let mut groups: Vec<Vec<MarketSet>> = vec![Vec::new(); max_rank + 1];
    for node_idx in order {
        let rank = ranks[node_idx];
        let w = graph.node_weight(*node_idx).unwrap().clone();
        groups[rank].push(w);
    }

    // Produce final ordered Vec<MarketSet>: ranks descending (leaf-first),
    // compressing equal-rank nodes into an MarketSet::Layer.
    let mut result = Vec::new();
    for mut items in groups.into_iter().rev() {
        if items.is_empty() {
            unreachable!("Should be no gaps in the ranking")
        }
        // If only one MarketSet in the group, we do not need to compress into a layer, so just
        // push the single item (this item may be a `Single` or `Cycle`).
        if items.len() == 1 {
            result.push(items.remove(0));
        // Otherwise, create a layer. The items within the layer may be a mix of `Single` or `Cycle`.
        } else {
            result.push(MarketSet::Layer(items));
        }
    }

    result
}

/// Determine investment ordering for each year
///
/// # Arguments
///
/// * `commodity_graphs` - Commodity graphs for each year
/// * `commodities` - All commodities with their types and demand specifications
///
/// # Returns
///
/// A map from `year` to the ordered list of `MarketSet`s for investment decisions. The
/// ordering ensures that leaf-node `MarketSet`s (those with no outgoing edges) are solved
/// first.
pub fn solve_investment_order_for_model(
    commodity_graphs: &IndexMap<u32, CommoditiesGraph>,
    commodities: &CommodityMap,
    years: &[u32],
) -> Result<HashMap<u32, Vec<MarketSet>>> {
    let mut investment_orders = HashMap::new();
    for year in years {
        let graph = &commodity_graphs[year];
        let order = solve_investment_order(graph, commodities)?;
        investment_orders.insert(*year, order);
    }
    Ok(investment_orders)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{sed_commodity, svd_commodity};
    use std::sync::Arc;

    /// Add `(input, output)` edges between commodities in a region to a commodity graph, with a
    /// separate process for each edge. Commodity nodes are added as needed.
    fn add_edges(graph: &mut CommoditiesGraph, region_id: &str, edges: &[(&str, &str)]) {
        for &(input, output) in edges {
            let [source, target] = [input, output].map(|commodity_id| {
                let node = GraphNode::CommodityMarket(commodity_id.into(), region_id.into());
                graph
                    .node_indices()
                    .find(|&idx| graph[idx] == node)
                    .unwrap_or_else(|| graph.add_node(node))
            });
            let process_id = format!("{input}_to_{output}");
            graph.add_edge(
                source,
                target,
                GraphEdge::Primary(process_id.into(), region_id.into()),
            );
        }
    }

    /// Create a commodity map with the given SED and SVD commodities
    fn commodity_map(sed_ids: &[&str], svd_ids: &[&str]) -> CommodityMap {
        let sed = Arc::new(sed_commodity());
        let svd = Arc::new(svd_commodity());
        let sed_entries = sed_ids.iter().map(|&id| (id.into(), sed.clone()));
        let svd_entries = svd_ids.iter().map(|&id| (id.into(), svd.clone()));
        sed_entries.chain(svd_entries).collect()
    }

    /// A `MarketSet::Single` for a commodity in GBR
    fn single(commodity_id: &str) -> MarketSet {
        MarketSet::Single((commodity_id.into(), "GBR".into()))
    }

    /// A `MarketSet::Cycle` for commodities in GBR
    fn cycle(commodity_ids: &[&str]) -> MarketSet {
        MarketSet::Cycle(
            commodity_ids
                .iter()
                .map(|&id| (id.into(), "GBR".into()))
                .collect(),
        )
    }

    #[test]
    fn order_scc_simple_cycle() {
        // Create a cycle A ← B ← C ← A (see doc comment for `order_scc`), where C is also consumed
        // to produce X, which is downstream of the cycle. B is also produced from Y, which is
        // upstream of the cycle (without at least one upstream input, SCC ordering will fail).
        let mut graph = CommoditiesGraph::new();
        add_edges(
            &mut graph,
            "GBR",
            &[("B", "A"), ("C", "B"), ("A", "C"), ("C", "X"), ("Y", "B")],
        );
        let commodities = commodity_map(&["A", "B", "C", "X", "Y"], &[]);

        let result = solve_investment_order(&graph, &commodities).unwrap();

        // X is downstream of the cycle, so comes first, and Y is upstream, so comes last. The order
        // within the cycle corresponds to the example in the doc comment. Note that C should be
        // first, as it has an outgoing edge to the external market. B is last because it receives
        // inputs from upstream.
        assert_eq!(result, [single("X"), cycle(&["C", "A", "B"]), single("Y")]);
    }

    #[test]
    fn solve_investment_order_linear_graph() {
        // Create a simple linear graph: A -> B -> C
        let mut graph = CommoditiesGraph::new();
        add_edges(&mut graph, "GBR", &[("A", "B"), ("B", "C")]);
        let commodities = commodity_map(&["A", "B"], &["C"]);

        let result = solve_investment_order(&graph, &commodities).unwrap();

        // Expected order: C, B, A (leaf nodes first)
        // No cycles or layers, so all market sets should be `Single`
        assert_eq!(result, [single("C"), single("B"), single("A")]);
    }

    #[test]
    fn solve_investment_order_cyclic_graph() {
        // Create a simple cyclic graph: X -> A -> B -> A
        let mut graph = CommoditiesGraph::new();
        add_edges(&mut graph, "GBR", &[("X", "A"), ("A", "B"), ("B", "A")]);
        let commodities = commodity_map(&["X", "A", "B"], &[]);

        let result = solve_investment_order(&graph, &commodities).unwrap();

        // Should be a `Cycle` market set containing both commodities, followed by a `Single`
        assert_eq!(result, [cycle(&["B", "A"]), single("X")]);
    }

    #[test]
    fn solve_investment_order_layered_graph() {
        // Create a graph with layers:
        //     A
        //    / \
        //   B   C
        //    \ /
        //     D
        let mut graph = CommoditiesGraph::new();
        add_edges(
            &mut graph,
            "GBR",
            &[("A", "B"), ("A", "C"), ("B", "D"), ("C", "D")],
        );
        let commodities = commodity_map(&["A", "B", "C"], &["D"]);

        let result = solve_investment_order(&graph, &commodities).unwrap();

        // Expected order: D, Layer(B, C), A
        assert_eq!(
            result,
            [
                single("D"),
                MarketSet::Layer(vec![single("B"), single("C")]),
                single("A")
            ]
        );
    }

    #[test]
    fn solve_investment_order_multiple_regions() {
        // Create a single graph spanning two regions, each with a linear chain A -> B -> C
        let mut graph = CommoditiesGraph::new();
        add_edges(&mut graph, "GBR", &[("A", "B"), ("B", "C")]);
        add_edges(&mut graph, "FRA", &[("A", "B"), ("B", "C")]);
        let commodities = commodity_map(&["A", "B"], &["C"]);

        let result = solve_investment_order(&graph, &commodities).unwrap();

        // Expected order: Should have three layers, each with two commodities (one per region)
        let layer = |commodity_id: &str| {
            MarketSet::Layer(vec![
                MarketSet::Single((commodity_id.into(), "GBR".into())),
                MarketSet::Single((commodity_id.into(), "FRA".into())),
            ])
        };
        assert_eq!(result, [layer("C"), layer("B"), layer("A")]);
    }
}
