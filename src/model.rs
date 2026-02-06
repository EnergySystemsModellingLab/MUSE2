//! The model represents the static input data provided by the user.
use crate::agent::AgentMap;
use crate::asset::AssetRef;
use crate::commodity::{CommodityID, CommodityMap};
use crate::process::ProcessMap;
use crate::region::{Region, RegionID, RegionMap};
use crate::simulation::investment::InvestmentSet;
use crate::time_slice::TimeSliceInfo;
use std::collections::HashMap;
use std::path::PathBuf;

pub mod parameters;
pub use parameters::{ALLOW_BROKEN_OPTION_NAME, ModelParameters, broken_model_options_allowed};

/// Model definition
pub struct Model {
    /// Path to model folder
    pub model_path: PathBuf,
    /// Parameters from the model TOML file
    pub parameters: ModelParameters,
    /// Agents for the simulation
    pub agents: AgentMap,
    /// Commodities for the simulation
    pub commodities: CommodityMap,
    /// Processes for the simulation
    pub processes: ProcessMap,
    /// Information about seasons and time slices
    pub time_slice_info: TimeSliceInfo,
    /// Regions for the simulation
    pub regions: RegionMap,
    /// User-defined assets
    pub user_assets: Vec<AssetRef>,
    /// Commodity ordering for each milestone year
    pub investment_order: HashMap<u32, Vec<InvestmentSet>>,
}

impl Model {
    /// Iterate over the model's milestone years.
    pub fn iter_years(&self) -> impl Iterator<Item = u32> + '_ {
        self.parameters.milestone_years.iter().copied()
    }

    /// Iterate over the model's regions (region IDs).
    pub fn iter_regions(&self) -> indexmap::map::Keys<'_, RegionID, Region> {
        self.regions.keys()
    }

    /// Iterate over all the markets in the model.
    pub fn iter_markets(&self) -> impl Iterator<Item = (CommodityID, RegionID)> + '_ {
        self.commodities.keys().flat_map(move |commodity_id| {
            self.regions
                .keys()
                .map(move |region_id| (commodity_id.clone(), region_id.clone()))
        })
    }
}
