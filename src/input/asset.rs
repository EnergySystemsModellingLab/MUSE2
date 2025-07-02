//! Code for reading [Asset]s from a CSV file.
use super::*;
use crate::agent::AgentID;
use crate::asset::Asset;
use crate::id::IDCollection;
use crate::process::ProcessMap;
use crate::region::RegionID;
use crate::units::Capacity;
use anyhow::{Context, Result};
use indexmap::IndexSet;
use itertools::Itertools;
use serde::Deserialize;
use std::path::Path;
use std::rc::Rc;

const ASSETS_FILE_NAME: &str = "assets.csv";

#[derive(Deserialize, PartialEq)]
struct AssetRaw {
    process_id: String,
    region_id: String,
    agent_id: String,
    capacity: Capacity,
    commission_year: u32,
}

/// Read assets CSV file from model directory.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `agent_ids` - All possible process IDs
/// * `processes` - The model's processes
/// * `region_ids` - All possible region IDs
///
/// # Returns
///
/// A `HashMap` containing assets grouped by agent ID.
pub fn read_assets(
    model_dir: &Path,
    agent_ids: &IndexSet<AgentID>,
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
) -> Result<Vec<Asset>> {
    let file_path = model_dir.join(ASSETS_FILE_NAME);
    let assets_csv = read_csv(&file_path)?;
    read_assets_from_iter(assets_csv, agent_ids, processes, region_ids)
        .with_context(|| input_err_msg(&file_path))
}

/// Process assets from an iterator.
///
/// # Arguments
///
/// * `iter` - Iterator of `AssetRaw`s
/// * `agent_ids` - All possible process IDs
/// * `processes` - The model's processes
/// * `region_ids` - All possible region IDs
///
/// # Returns
///
/// A [`Vec`] of [`Asset`]s or an error.
fn read_assets_from_iter<I>(
    iter: I,
    agent_ids: &IndexSet<AgentID>,
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
) -> Result<Vec<Asset>>
where
    I: Iterator<Item = AssetRaw>,
{
    iter.map(|asset| -> Result<_> {
        let agent_id = agent_ids.get_id(&asset.agent_id)?;
        let process = processes
            .get(asset.process_id.as_str())
            .with_context(|| format!("Invalid process ID: {}", &asset.process_id))?;
        let region_id = region_ids.get_id(&asset.region_id)?;

        ensure!(
            asset.capacity.is_finite() && asset.capacity > Capacity(0.0),
            "Capacity must be a finite, positive number"
        );

        Asset::new(
            Some(agent_id.clone()),
            Rc::clone(process),
            region_id.clone(),
            asset.capacity,
            asset.commission_year,
        )
    })
    .try_collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::{processes, region_ids};

    use itertools::assert_equal;
    use rstest::{fixture, rstest};
    use std::iter;

    #[fixture]
    fn agent_ids() -> IndexSet<AgentID> {
        IndexSet::from(["agent1".into()])
    }

    #[rstest]
    fn test_read_assets_from_iter_valid(
        agent_ids: IndexSet<AgentID>,
        processes: ProcessMap,
        region_ids: IndexSet<RegionID>,
    ) {
        let asset_in = AssetRaw {
            agent_id: "agent1".into(),
            process_id: "process1".into(),
            region_id: "GBR".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
        };
        let asset_out = Asset::new(
            Some("agent1".into()),
            Rc::clone(processes.values().next().unwrap()),
            "GBR".into(),
            Capacity(1.0),
            2010,
        )
        .unwrap();
        assert_equal(
            read_assets_from_iter(iter::once(asset_in), &agent_ids, &processes, &region_ids)
                .unwrap(),
            iter::once(asset_out),
        );
    }

    #[rstest]
    #[case(AssetRaw { // Bad process ID
            agent_id: "agent1".into(),
            process_id: "process2".into(),
            region_id: "GBR".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
        })]
    #[case(AssetRaw { // Bad agent ID
            agent_id: "agent2".into(),
            process_id: "process1".into(),
            region_id: "GBR".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
        })]
    #[case(AssetRaw { // Bad region ID: not in region_ids
            agent_id: "agent1".into(),
            process_id: "process1".into(),
            region_id: "FRA".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
        })]
    fn test_read_assets_from_iter_invalid(
        #[case] asset: AssetRaw,
        agent_ids: IndexSet<AgentID>,
        processes: ProcessMap,
        region_ids: IndexSet<RegionID>,
    ) {
        assert!(
            read_assets_from_iter(iter::once(asset), &agent_ids, &processes, &region_ids).is_err()
        );
    }
}
