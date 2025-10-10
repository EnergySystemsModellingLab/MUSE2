//! The module responsible for writing commodity flow graphs to file.
use crate::graph::CommoditiesGraph;
use crate::region::RegionID;
use anyhow::Result;
use petgraph::dot::Dot;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write as IoWrite;
use std::path::Path;

/// Saves commodity graphs to file
///
/// The graphs are saved as DOT files to the specified output path
pub fn save_commodity_graphs_for_model(
    commodity_graphs: &HashMap<(RegionID, u32), CommoditiesGraph>,
    output_path: &Path,
) -> Result<()> {
    for ((region_id, year), graph) in commodity_graphs {
        let dot = Dot::new(&graph);
        let mut file = File::create(output_path.join(format!("{region_id}_{year}.dot")))?;
        write!(file, "{dot}")?;
    }
    Ok(())
}
