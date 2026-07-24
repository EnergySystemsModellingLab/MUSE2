//! Common functionality for MUSE2.
#![warn(missing_docs)]

/// The main GitHub issues page for MUSE2
pub const ISSUES_URL: &str = concat!(env!("CARGO_PKG_REPOSITORY"), "/issues");

/// A macro to get a url to the most recent version of the documentation
#[macro_export]
macro_rules! docs_url {
    () => {
        docs_url!("")
    };
    ($suffix:literal) => {
        concat!(
            env!("CARGO_PKG_HOMEPAGE"),
            "/v",
            env!("CARGO_PKG_VERSION"),
            "/",
            $suffix
        )
    };
}

use dirs::config_dir;
use std::path::PathBuf;

pub mod agent;
pub mod asset;
pub mod cli;
pub mod commodity;
pub mod example;
pub mod finance;
pub mod graph;
pub mod id;
pub mod input;
pub mod log;
pub mod model;
pub mod output;
pub mod patch;
pub mod process;
pub mod region;
pub mod settings;
pub mod simulation;
pub mod time_slice;
pub mod timeit;
pub mod units;

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
