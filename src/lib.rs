//! Common functionality for MUSE 2.0.
#![warn(missing_docs)]

use dirs::config_dir;
use std::path::PathBuf;

pub mod agent;
pub mod asset;
pub mod cli;
pub mod commodity;
pub mod finance;
pub mod graph;
pub mod id;
pub mod input;
pub mod log;
pub mod model;
pub mod output;
pub mod process;
pub mod region;
pub mod settings;
pub mod simulation;
pub mod time_slice;
pub mod units;
pub mod year;

#[cfg(test)]
mod fixture;

/// Get config dir for program.
///
/// In the unlikely event this path cannot be retrieved, the CWD will be returned.
pub fn get_muse2_config_dir() -> PathBuf {
    let Some(mut config_dir) = config_dir() else {
        return PathBuf::default();
    };

    config_dir.push("muse2");
    config_dir
}
