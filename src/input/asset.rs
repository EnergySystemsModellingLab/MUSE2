//! Code for reading [`Asset`]s from a CSV file.
use super::{input_err_msg, read_csv_optional};
use crate::agent::AgentID;
use crate::asset::Asset;
use crate::id::IDCollection;
use crate::process::ProcessMap;
use crate::region::RegionID;
use crate::units::Capacity;
use anyhow::{Context, Result, ensure};
use indexmap::IndexSet;
use itertools::Itertools;
use serde::Deserialize;
use std::path::Path;
use std::rc::Rc;

const ASSETS_FILE_NAME: &str = "assets.csv";

#[derive(Default, Deserialize, PartialEq)]
struct AssetRaw {
    process_id: String,
    region_id: String,
    agent_id: String,
    capacity: Capacity,
    commission_year: u32,
    #[serde(default)]
    max_decommission_year: Option<u32>,
}

/// Read assets CSV file from model directory.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `agent_ids` - All possible agent IDs
/// * `processes` - The model's processes
/// * `region_ids` - All possible region IDs
///
/// # Returns
///
/// A `Vec` of [`Asset`]s or an error.
pub fn read_assets(
    model_dir: &Path,
    agent_ids: &IndexSet<AgentID>,
    processes: &ProcessMap,
    region_ids: &IndexSet<RegionID>,
) -> Result<Vec<Asset>> {
    let file_path = model_dir.join(ASSETS_FILE_NAME);
    let assets_csv = read_csv_optional(&file_path)?;
    read_assets_from_iter(assets_csv, agent_ids, processes, region_ids)
        .with_context(|| input_err_msg(&file_path))
}

/// Process assets from an iterator.
///
/// # Arguments
///
/// * `iter` - Iterator of `AssetRaw`s
/// * `agent_ids` - All possible agent IDs
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

        // Validate commission year. It should be within the process valid range...
        ensure!(
            process.years.contains(&asset.commission_year),
            "Agent {} has asset with commission year {}, not within process {} commission years: {:?}",
            asset.agent_id,
            asset.commission_year,
            asset.process_id,
            process.years
        );
        // ... and also have associated process parameters and flows
        ensure!(
            process.parameters.contains_key(&(region_id.clone(), asset.commission_year)),
            "Parameters for process {} do not contain entry for year {}, required for asset in agent {}",
            asset.process_id,
            asset.commission_year,
            asset.agent_id,
        );
        ensure!(
            process.flows.contains_key(&(region_id.clone(), asset.commission_year)),
            "Flows for process {} do not contain entry for year {}, required for asset in agent {}",
            asset.process_id,
            asset.commission_year,
            asset.agent_id,
        );

        Asset::new_future_with_max_decommission(
            agent_id.clone(),
            Rc::clone(process),
            region_id.clone(),
            asset.capacity,
            asset.commission_year,
            asset.max_decommission_year,
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
    #[case::max_decommission_year_provided(Some(2015))]
    #[case::max_decommission_year_not_provided(None)]
    fn read_assets_from_iter_valid(
        #[case] max_decommission_year: Option<u32>,
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
            max_decommission_year,
        };
        let asset_out = Asset::new_future_with_max_decommission(
            "agent1".into(),
            Rc::clone(processes.values().next().unwrap()),
            "GBR".into(),
            Capacity(1.0),
            2010,
            max_decommission_year,
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
            max_decommission_year: None,
        })]
    #[case(AssetRaw { // Bad agent ID
            agent_id: "agent2".into(),
            process_id: "process1".into(),
            region_id: "GBR".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
            max_decommission_year: None,
        })]
    #[case(AssetRaw { // Bad region ID: not in region_ids
            agent_id: "agent1".into(),
            process_id: "process1".into(),
            region_id: "FRA".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
            max_decommission_year: None,
        })]
    #[case(AssetRaw { // Bad max_decommission_year: before commission_year
            agent_id: "agent1".into(),
            process_id: "process1".into(),
            region_id: "GBR".into(),
            capacity: Capacity(1.0),
            commission_year: 2010,
            max_decommission_year: Some(2005),
        })]
    fn read_assets_from_iter_invalid(
        #[case] asset: AssetRaw,
        agent_ids: IndexSet<AgentID>,
        processes: ProcessMap,
        region_ids: IndexSet<RegionID>,
    ) {
        read_assets_from_iter(iter::once(asset), &agent_ids, &processes, &region_ids).unwrap_err();
    }
}
