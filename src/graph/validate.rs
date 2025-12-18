//! Module for validating commodity graphs
use super::{CommoditiesGraph, GraphEdge, GraphNode};
use crate::commodity::{CommodityMap, CommodityType};
use crate::process::{Process, ProcessMap};
use crate::region::RegionID;
use crate::time_slice::{TimeSliceInfo, TimeSliceLevel, TimeSliceSelection};
use crate::units::{Dimensionless, Flow};
use anyhow::{Context, Result, ensure};
use indexmap::IndexMap;
use strum::IntoEnumIterator;

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
        can_be_active(process, &key, time_slice_selection)
    });

    // Add demand edges
    // We add edges to the `Demand` node for commodities that are demanded in the selection
    // NOTE: we only do this for commodities with the same time_slice_level as the selection
    let demand_node_index = filtered_graph.add_node(GraphNode::Demand);
    for (commodity_id, commodity) in commodities {
        if commodity
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

/// Checks if a process can be active for a particular timeslice in a given year and region
///
/// It considers all commission years that can lead to a running process in the target region and
/// year, accounting for the process lifetime, and then checks if, for any of those, the process
/// is active in the required timeslice. In other words, this checks if there is the _possibility_
/// of having an active process, although there is no guarantee of that happening since it depends
/// on the investment.
fn can_be_active(
    process: &Process,
    target: &(RegionID, u32),
    time_slice_selection: &TimeSliceSelection,
) -> bool {
    let (target_region, target_year) = target;

    for ((region, year), value) in &process.parameters {
        if region != target_region {
            continue;
        }
        if year + value.lifetime >= *target_year {
            let Some(limits_map) = process.activity_limits.get(target) else {
                continue;
            };
            if limits_map.get_limit(time_slice_selection).end() > &Dimensionless(0.0) {
                return true;
            }
        }
    }
    false
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

/// Validates commodity graphs for the entire model.
///
/// The validation process checks three time slice levels:
/// - **Annual**: Validates annual-level commodities and processes
/// - **Seasonal**: Validates seasonal-level commodities and processes for each season
/// - **Day/Night**: Validates day/night-level commodities and processes for each time slice
///
/// # Arguments
///
/// * `commodity_graphs` - Commodity graphs for each region and year, outputted from `build_commodity_graphs_for_model`
/// * `processes` - All processes in the model with their flows and activity limits
/// * `commodities` - All commodities with their types and demand specifications
/// * `region_ids` - Collection of regions to model
/// * `years` - Years to analyse
/// * `time_slice_info` - Time slice configuration (seasons, day/night periods)
///
/// # Errors
///
/// Returns an error if:
/// - Commodity type rules are violated (e.g., SVD commodities being consumed)
/// - Demand cannot be satisfied
pub fn validate_commodity_graphs_for_model(
    commodity_graphs: &IndexMap<(RegionID, u32), CommoditiesGraph>,
    processes: &ProcessMap,
    commodities: &CommodityMap,
    time_slice_info: &TimeSliceInfo,
) -> Result<()> {
    // Validate graphs at all time slice levels (taking into account process availability and demand)
    for ((region_id, year), base_graph) in commodity_graphs {
        for ts_level in TimeSliceLevel::iter() {
            for ts_selection in time_slice_info.iter_selections_at_level(ts_level) {
                let graph = prepare_commodities_graph_for_validation(
                    base_graph,
                    processes,
                    commodities,
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
        assert!(validate_commodities_graph(&graph, &commodities, TimeSliceLevel::Annual).is_ok());
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
        assert_error!(
            validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight),
            "SVD commodity A cannot be an input to a process"
        );
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
        assert_error!(
            validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight),
            "SVD commodity A is demanded but has no producers"
        );
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
        assert_error!(
            validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight),
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
        assert_error!(
            validate_commodities_graph(&graph, &commodities, TimeSliceLevel::DayNight),
            "OTH commodity A cannot have both producers and consumers"
        );
    }
}
