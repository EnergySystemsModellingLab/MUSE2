//! Suggests which processes to mark as `feedback_process` when the non-feedback commodity
//! network is cyclic.
//!
//! When [`validate_non_feedback_commodity_graphs_for_model`](super::validate) fails, the network
//! contains a cycle that cannot be resolved without feedback processes. This module searches for
//! the smallest sets of processes that, if marked as feedback (removing all their edges), leave a
//! network that is both acyclic and structurally valid.
//!
//! Validity is stricter than mere acyclicity: after the feedback edges are removed, a SED commodity
//! must be either fully connected (produced and consumed) or fully disconnected (neither). Breaking
//! a cycle by removing a single edge can leave a SED commodity half-connected (e.g. produced but no
//! longer consumed), which is invalid; resolving such a loop therefore requires marking every
//! process that keeps it half-connected. For the electricity/hydrogen loop this means both the
//! electrolyser and the turbine must be marked.
//!
//! # Algorithm
//!
//! 1. Enumerate every simple cycle in the commodity conversion graph with Johnson's algorithm and
//!    reduce each to the *set of processes* on its edges. Cycles are detected over the full
//!    topology, ignoring existing feedback flags, so a loop that is only *partially* marked is
//!    still found and can be completed (e.g. if the turbine is already marked, we can still suggest
//!    the electrolyser).
//! 2. Search for the smallest sets of processes to mark. A HiGHS MILP proposes a minimum-size set
//!    that touches ("hits") every cycle; each proposal is verified by actually marking those
//!    processes as feedback and re-running
//!    `validate_non_feedback_commodity_graphs_for_region_year`. Reusing the real validator means
//!    the suggestions can never drift from the validation rules. A proposal that fails validation
//!    (for instance because it breaks a cycle but leaves a commodity half-connected) is excluded
//!    with a no-good cut and the search continues.
//!
//! The search reports every minimal valid set (so the user sees genuine alternatives), or an empty
//! result if none exists — for example when the sole producer of an externally-consumed commodity
//! is the only way to break a cycle, which commonly arises with multiple-output processes.
//!
//! Suggestions are generated for the single region/year graph whose validation failed. A suggestion
//! that fixes that graph may break another region/year, which the user resolves iteratively.
use super::validate::validate_non_feedback_commodity_graphs_for_region_year;
use super::{CommoditiesGraph, GraphEdge, GraphNode};
use crate::commodity::CommodityMap;
use crate::process::{ProcessID, ProcessMap};
use crate::region::RegionID;
use crate::time_slice::TimeSliceInfo;
use highs::{HighsModelStatus, RowProblem, Sense};
use petgraph::visit::EdgeRef;
use std::collections::{BTreeSet, HashMap, HashSet};

/// The maximum number of alternative suggestions to report.
const MAX_ALTERNATIVES: usize = 5;

/// A bound on solver iterations, to guard against pathological search spaces.
const MAX_ITERATIONS: usize = 1000;

/// A candidate set of processes to mark as `feedback_process` to make the graph valid.
pub type FeedbackSuggestion = BTreeSet<ProcessID>;

/// Suggests sets of processes to mark as `feedback_process` to make the given graph valid.
///
/// `base_graph` is the failing region/year graph, still carrying the user's existing feedback
/// flags. The remaining arguments are exactly what the validator needs to re-check a candidate.
///
/// Returns every minimal set of processes that resolves the failure (so the caller can present
/// alternatives), or an empty `Vec` if no such set exists.
pub fn suggest_feedback_processes(
    base_graph: &CommoditiesGraph,
    processes: &ProcessMap,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
    region_id: &RegionID,
    year: u32,
) -> Vec<FeedbackSuggestion> {
    // Phase 1: find every cycle, expressed as the set of processes that could break it.
    let mut cycle_sets: HashSet<BTreeSet<ProcessID>> = HashSet::new();
    collect_cycle_sets(base_graph, &mut cycle_sets);
    if cycle_sets.is_empty() {
        // No cycles means the failure is not something feedback processes can fix.
        return Vec::new();
    }

    // Phase 2: search for minimal sets that break every cycle *and* pass full validation.
    search_valid_sets(&cycle_sets, |candidate| {
        candidate_passes_validation(
            base_graph,
            processes,
            commodities,
            time_slice_info,
            region_id,
            year,
            candidate,
        )
    })
}

/// Tests one candidate set by marking its processes as feedback and reusing the real validator.
///
/// The candidate is *added to* the graph's existing feedback flags: `base_graph` already reflects
/// what the user has marked, and here we additionally set `feedback = true` on every edge belonging
/// to a candidate process. A suggestion therefore means "mark these in addition to what you already
/// have". Delegating to `validate_non_feedback_commodity_graphs_for_region_year` keeps this check
/// identical to real validation.
fn candidate_passes_validation(
    base_graph: &CommoditiesGraph,
    processes: &ProcessMap,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
    region_id: &RegionID,
    year: u32,
    candidate: &BTreeSet<ProcessID>,
) -> bool {
    let mut graph = base_graph.clone();
    for weight in graph.edge_weights_mut() {
        // Demand edges carry no process, so they can never be feedback edges; skip them.
        let (GraphEdge::Primary {
            process_id,
            feedback,
        }
        | GraphEdge::Secondary {
            process_id,
            feedback,
        }) = weight
        else {
            continue;
        };
        if candidate.contains(process_id) {
            *feedback = true;
        }
    }
    validate_non_feedback_commodity_graphs_for_region_year(
        &graph,
        region_id,
        year,
        processes,
        commodities,
        time_slice_info,
    )
    .is_ok()
}

/// Formats the suggestions for the error message.
///
/// A single option is rendered as a bare list (`A, B`); multiple options are each wrapped in
/// parentheses and joined with `" or "` (`(A, B) or (C)`) so the alternatives read unambiguously.
pub fn format_feedback_suggestions(suggestions: &[FeedbackSuggestion]) -> String {
    let multiple = suggestions.len() > 1;
    suggestions
        .iter()
        .map(|processes| {
            let ids = processes
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", ");
            if multiple { format!("({ids})") } else { ids }
        })
        .collect::<Vec<String>>()
        .join(" or ")
}

/// Collects, for a single graph, the set of processes on every simple cycle in the non-feedback
/// network. A valid feedback set must remove at least one process from each.
fn collect_cycle_sets(graph: &CommoditiesGraph, cycle_sets: &mut HashSet<BTreeSet<ProcessID>>) {
    // Processes on each commodity-to-commodity, non-feedback edge.
    let mut edge_procs: HashMap<(usize, usize), BTreeSet<ProcessID>> = HashMap::new();
    for edge in graph.edge_references() {
        let Some((process_id, _)) = edge_label(edge.weight()) else {
            continue;
        };
        let is_commodity = |idx| matches!(graph.node_weight(idx), Some(GraphNode::Commodity(_)));
        if is_commodity(edge.source()) && is_commodity(edge.target()) {
            edge_procs
                .entry((edge.source().index(), edge.target().index()))
                .or_default()
                .insert(process_id.clone());
        }
    }

    let mut adj = vec![Vec::new(); graph.node_count()];
    for &(a, b) in edge_procs.keys() {
        adj[a].push(b);
    }
    // Johnson requires deterministic successor order for reproducible output.
    for row in &mut adj {
        row.sort_unstable();
    }

    // Reduce each cycle to the union of processes on its edges. Removing any one of them breaks the
    // cycle, so the cover constraint over this set is "mark at least one".
    for cycle in Johnson::new(adj).elementary_circuits() {
        let procs = (0..cycle.len())
            .flat_map(|i| &edge_procs[&(cycle[i], cycle[(i + 1) % cycle.len()])])
            .cloned()
            .collect();
        cycle_sets.insert(procs);
    }
}

/// Searches for the minimal sets of processes that break every cycle and pass `is_valid`.
///
/// This is a minimum hitting-set problem solved by repeated MILP calls ("verify and cut"):
/// [`solve_once`] returns a smallest process set that hits every cycle, we verify it with
/// `is_valid`, and — valid or not — forbid that exact assignment before solving again. Because the
/// MILP always returns a minimum-size solution, results come out in non-decreasing size, so once we
/// have a valid set of size `n` we can stop as soon as the solver is forced to a larger size. This
/// yields all minimal valid sets (the alternatives) without exploring larger ones.
///
/// Indices are used throughout because the MILP works over integer columns: `candidates[i]` is the
/// process for column `i`, and `cover_constraints`/`forbidden` are expressed in those indices.
fn search_valid_sets(
    cycle_sets: &HashSet<BTreeSet<ProcessID>>,
    is_valid: impl Fn(&BTreeSet<ProcessID>) -> bool,
) -> Vec<FeedbackSuggestion> {
    // The processes on any cycle are the only ones worth marking; number them for the MILP.
    let candidates: Vec<ProcessID> = cycle_sets
        .iter()
        .flatten()
        .cloned()
        .collect::<BTreeSet<ProcessID>>()
        .into_iter()
        .collect();
    let index: HashMap<&ProcessID, usize> =
        candidates.iter().enumerate().map(|(i, p)| (p, i)).collect();

    // One cover constraint per cycle, as candidate indices: at least one of them must be marked.
    let cover_constraints: Vec<Vec<usize>> = cycle_sets
        .iter()
        .map(|set| set.iter().map(|p| index[p]).collect())
        .collect();

    let mut valid: Vec<BTreeSet<ProcessID>> = Vec::new();
    let mut forbidden: Vec<Vec<usize>> = Vec::new();
    let mut target_size: Option<usize> = None;

    // MAX_ITERATIONS bounds validator calls; MAX_ALTERNATIVES bounds how many options we report.
    for _ in 0..MAX_ITERATIONS {
        if valid.len() >= MAX_ALTERNATIVES {
            break;
        }
        let Some(marked) = solve_once(candidates.len(), &cover_constraints, &forbidden) else {
            break; // No further cycle-breaking set exists.
        };
        // Solutions only grow in size, so once we exceed the first valid size we have them all.
        if target_size.is_some_and(|size| marked.len() > size) {
            break;
        }
        // Forbid this exact set next time: if valid we want a *different* alternative, if invalid we
        // must not see it again.
        forbidden.push(marked.clone());

        let candidate: BTreeSet<ProcessID> =
            marked.iter().map(|&i| candidates[i].clone()).collect();
        if is_valid(&candidate) {
            target_size.get_or_insert(marked.len());
            valid.push(candidate);
        }
    }

    valid
}

/// Solves one instance of the hitting-set MILP: pick the fewest candidate processes to mark.
///
/// Each candidate is a binary column (1 = mark as feedback) with objective coefficient 1, so
/// minimising the objective minimises the number of processes marked. Two families of rows
/// constrain the solution:
///
/// * **Cover** — for every cycle, `sum(x_i) >= 1`, forcing at least one of its processes to be
///   marked.
/// * **No-good cuts** — each previously returned assignment `S` is excluded *exactly* (supersets
///   stay reachable, which matters when a small set breaks a cycle but fails validation and a larger
///   one is required). Requiring at least one variable to differ from `S` gives
///   `sum_{i in S}(1 - x_i) + sum_{i not in S}(x_i) >= 1`, which rearranges to a single linear row
///   with coefficient -1 for columns in `S`, +1 for the rest, and lower bound `1 - |S|`.
///
/// Returns the marked candidate indices, or `None` if no assignment satisfies the constraints.
fn solve_once(
    num_candidates: usize,
    cover_constraints: &[Vec<usize>],
    forbidden: &[Vec<usize>],
) -> Option<Vec<usize>> {
    let mut problem = RowProblem::default();
    // One binary column per candidate; cost 1 each so the objective counts marked processes.
    let cols: Vec<_> = (0..num_candidates)
        .map(|_| problem.add_integer_column(1.0, 0..=1))
        .collect();

    // Cover: every cycle must lose at least one process.
    for cycle in cover_constraints {
        let terms: Vec<_> = cycle.iter().map(|&i| (cols[i], 1.0)).collect();
        problem.add_row(1.0.., terms);
    }

    // No-good cut per forbidden assignment: -1 for its members, +1 for the rest, bound 1 - |S|.
    for assignment in forbidden {
        let mut in_assignment = vec![false; num_candidates];
        for &i in assignment {
            in_assignment[i] = true;
        }
        let terms: Vec<_> = (0..num_candidates)
            .map(|i| (cols[i], if in_assignment[i] { -1.0 } else { 1.0 }))
            .collect();
        problem.add_row((1.0 - count_as_f64(assignment.len())).., terms);
    }

    let solved = problem.optimise(Sense::Minimise).try_solve().ok()?;
    if solved.status() != HighsModelStatus::Optimal {
        return None;
    }
    let solution = solved.get_solution();
    // Columns above 0.5 are the marked processes (values are binary, so effectively == 1).
    Some(
        (0..num_candidates)
            .filter(|&i| solution[cols[i]] > 0.5)
            .collect(),
    )
}

/// Converts a small count to an `f64` without triggering precision-loss lints.
fn count_as_f64(count: usize) -> f64 {
    f64::from(u32::try_from(count).expect("count fits in u32"))
}

/// Returns the process and feedback flag for a conversion edge, or `None` for demand edges.
fn edge_label(edge: &GraphEdge) -> Option<(&ProcessID, bool)> {
    match edge {
        GraphEdge::Primary {
            process_id,
            feedback,
        }
        | GraphEdge::Secondary {
            process_id,
            feedback,
        } => Some((process_id, *feedback)),
        GraphEdge::Demand => None,
    }
}

/// Johnson's algorithm for enumerating the elementary circuits (simple cycles) of a directed graph.
///
/// Reference: Donald B. Johnson, "Finding all the elementary circuits of a directed graph" (1975).
/// Each circuit is reported exactly once, canonicalised by its least-indexed vertex: the search is
/// restricted to the subgraph on vertices `>= s` and rooted at `s`, so a circuit is only emitted
/// when `s` is its smallest vertex. The `blocked`/`b` bookkeeping is what keeps the algorithm from
/// being exponential in dead-end exploration — a vertex stays blocked until unblocking it could
/// actually yield a new circuit.
struct Johnson {
    /// Successor lists, indexed by vertex (a commodity node's index).
    adj: Vec<Vec<usize>>,
    /// Whether each vertex is currently blocked from being revisited on the active search.
    blocked: Vec<bool>,
    /// Deferred-unblock lists: `b[w]` holds the vertices to unblock once `w` is unblocked.
    b: Vec<BTreeSet<usize>>,
    /// The vertices on the path currently being explored.
    stack: Vec<usize>,
    /// The circuits found so far, each as a list of vertices.
    circuits: Vec<Vec<usize>>,
}

impl Johnson {
    fn new(adj: Vec<Vec<usize>>) -> Self {
        let n = adj.len();
        Self {
            adj,
            blocked: vec![false; n],
            b: vec![BTreeSet::new(); n],
            stack: Vec::new(),
            circuits: Vec::new(),
        }
    }

    /// Runs the search once from each start vertex `s` and returns every circuit found.
    fn elementary_circuits(mut self) -> Vec<Vec<usize>> {
        for s in 0..self.adj.len() {
            // Reset per-root state; only vertices in the subgraph induced by `>= s` are visited.
            for v in s..self.adj.len() {
                self.blocked[v] = false;
                self.b[v].clear();
            }
            self.stack.clear();
            self.circuit(s, s);
        }
        self.circuits
    }

    /// Depth-first search for circuits rooted at `s`, extending the current path with `v`.
    ///
    /// Returns whether a circuit back to `s` was found through `v`. This drives whether `v` is
    /// unblocked immediately (a circuit closed, so `v` may yield more) or its unblocking is deferred
    /// via `b` (retried only once one of its successors is unblocked).
    fn circuit(&mut self, v: usize, s: usize) -> bool {
        let mut found = false;
        self.stack.push(v);
        self.blocked[v] = true;

        // Only consider the subgraph induced by vertices with index >= s.
        for w in self.adj[v].clone() {
            if w < s {
                continue;
            }
            if w == s {
                // Reaching the root closes an elementary circuit: the current path.
                self.circuits.push(self.stack.clone());
                found = true;
            } else if !self.blocked[w] && self.circuit(w, s) {
                found = true;
            }
        }

        if found {
            self.unblock(v);
        } else {
            // No circuit through `v` yet; defer its unblocking to when a successor is unblocked.
            for w in self.adj[v].clone() {
                if w >= s {
                    self.b[w].insert(v);
                }
            }
        }

        self.stack.pop();
        found
    }

    /// Marks `u` explorable again and recursively unblocks the vertices that were waiting on it.
    fn unblock(&mut self, u: usize) {
        self.blocked[u] = false;
        for w in std::mem::take(&mut self.b[u]) {
            if self.blocked[w] {
                self.unblock(w);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::graph::Graph;

    /// Builds a non-feedback primary edge for the given process.
    fn primary(process_id: &str) -> GraphEdge {
        GraphEdge::Primary {
            process_id: process_id.into(),
            feedback: false,
        }
    }

    #[test]
    fn collect_cycle_sets_finds_loop_processes() {
        // A<->B loop, plus a Source edge that must be ignored (not part of any cycle).
        let mut graph: CommoditiesGraph = Graph::new();
        let src = graph.add_node(GraphNode::Source);
        let a = graph.add_node(GraphNode::Commodity("A".into()));
        let b = graph.add_node(GraphNode::Commodity("B".into()));
        graph.add_edge(src, a, primary("ext"));
        graph.add_edge(a, b, primary("p_ab"));
        graph.add_edge(b, a, primary("p_ba"));

        let mut cycle_sets = HashSet::new();
        collect_cycle_sets(&graph, &mut cycle_sets);

        assert_eq!(
            cycle_sets,
            HashSet::from([BTreeSet::from(["p_ab".into(), "p_ba".into()])])
        );
    }

    #[test]
    fn search_reports_all_minimal_alternatives() {
        // Single 3-cycle where any one process is a valid fix.
        let cycle_sets = HashSet::from([BTreeSet::from(["p1".into(), "p2".into(), "p3".into()])]);

        let options: HashSet<BTreeSet<ProcessID>> =
            search_valid_sets(&cycle_sets, |c| c.len() == 1)
                .into_iter()
                .collect();

        assert_eq!(
            options,
            HashSet::from([
                BTreeSet::from(["p1".into()]),
                BTreeSet::from(["p2".into()]),
                BTreeSet::from(["p3".into()]),
            ])
        );
    }

    #[test]
    fn search_finds_superset_when_singletons_invalid() {
        // Only the whole loop is valid, so singletons are cut and the pair is found.
        let cycle_sets = HashSet::from([BTreeSet::from(["p_ab".into(), "p_ba".into()])]);
        let both: BTreeSet<ProcessID> = BTreeSet::from(["p_ab".into(), "p_ba".into()]);

        let suggestions = search_valid_sets(&cycle_sets, |c| *c == both);

        assert_eq!(suggestions.len(), 1);
        assert_eq!(suggestions[0], both);
    }

    #[test]
    fn search_returns_empty_when_nothing_valid() {
        let cycle_sets = HashSet::from([BTreeSet::from(["p1".into(), "p2".into()])]);
        assert!(search_valid_sets(&cycle_sets, |_| false).is_empty());
    }
}
