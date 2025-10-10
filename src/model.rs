//! The model represents the static input data provided by the user.
use crate::agent::AgentMap;
use crate::commodity::{CommodityID, CommodityMap};
use crate::process::ProcessMap;
use crate::region::{RegionID, RegionMap};
use crate::time_slice::TimeSliceInfo;
use std::collections::HashMap;
use std::path::PathBuf;

pub mod parameters;
pub use parameters::{ModelParameters, PricingStrategy};

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
    /// Commodity ordering for each region and year
    pub commodity_order: HashMap<(RegionID, u32), Vec<CommodityID>>,
}

impl Model {
    /// Iterate over the model's milestone years.
    pub fn iter_years(&self) -> impl Iterator<Item = u32> + '_ {
        self.parameters.milestone_years.iter().copied()
    }

    /// Iterate over the model's regions (region IDs).
    pub fn iter_regions(&self) -> impl Iterator<Item = &RegionID> + '_ {
        self.regions.keys()
    }
}
