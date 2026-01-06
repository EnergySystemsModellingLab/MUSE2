//! Code for writing metadata to file
use anyhow::Result;
use chrono::prelude::*;
use platform_info::{PlatformInfo, PlatformInfoAPI, UNameAPI};
use serde::Serialize;
use std::fs;
use std::path::Path;

/// The output file name for metadata
const METADATA_FILE_NAME: &str = "metadata.toml";

/// Information about the program build via `built` crate
#[allow(clippy::doc_markdown)]
#[allow(clippy::needless_raw_strings)]
mod built_info {
    // The file has been placed there by the build script.
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

/// Get information about program version from git
fn get_git_hash() -> String {
    let Some(hash) = built_info::GIT_COMMIT_HASH_SHORT else {
        return "unknown".into();
    };

    if built_info::GIT_DIRTY == Some(true) {
        format!("{hash}-dirty")
    } else {
        hash.into()
    }
}

/// Metadata about the program run, build and version information for MUSE and the user's system
#[derive(Serialize)]
struct Metadata<'a> {
    run: RunMetadata<'a>,
    program: ProgramMetadata<'a>,
    platform: PlatformMetadata,
}

/// Information about the model run
#[derive(Serialize)]
struct RunMetadata<'a> {
    /// Path to the model which was run
    model_path: &'a Path,
    /// The date and time on which the run started
    datetime: String,
}

impl<'a> RunMetadata<'a> {
    fn new(model_path: &'a Path) -> Self {
        let dt = Local::now();
        Self {
            model_path,
            datetime: dt.to_rfc2822(),
        }
    }
}

#[derive(Serialize)]
struct ProgramMetadata<'a> {
    /// The program name
    name: &'a str,
    /// The program version as specified in Cargo.toml
    version: &'a str,
    /// The target architecture for the build (e.g. x86_64-unknown-linux-gnu)
    target: &'a str,
    /// Whether it is a debug build
    is_debug: bool,
    /// The version of rustc used to compile MUSE
    rustc_version: &'a str,
    /// When MUSE was built
    build_time_utc: &'a str,
    /// The git commit hash for the version of MUSE (if known)
    git_commit_hash: String,
}

impl Default for ProgramMetadata<'_> {
    fn default() -> Self {
        Self {
            name: built_info::PKG_NAME,
            version: built_info::PKG_VERSION,
            target: built_info::TARGET,
            is_debug: built_info::DEBUG,
            rustc_version: built_info::RUSTC_VERSION,
            build_time_utc: built_info::BUILT_TIME_UTC,
            git_commit_hash: get_git_hash(),
        }
    }
}

/// Information about the platform on which MUSE is running.
///
/// The fields correspond to different data available from the [`PlatformInfo`] struct.
#[derive(Serialize)]
struct PlatformMetadata {
    sysname: String,
    nodename: String,
    release: String,
    version: String,
    machine: String,
    osname: String,
}

impl Default for PlatformMetadata {
    fn default() -> Self {
        let info = PlatformInfo::new().expect("Unable to determine platform info");
        Self {
            sysname: info.sysname().to_string_lossy().into(),
            nodename: info.nodename().to_string_lossy().into(),
            release: info.release().to_string_lossy().into(),
            version: info.version().to_string_lossy().into(),
            machine: info.machine().to_string_lossy().into(),
            osname: info.osname().to_string_lossy().into(),
        }
    }
}

/// Write metadata to the specified output path in TOML format
pub fn write_metadata(output_path: &Path, model_path: &Path) -> Result<()> {
    let metadata = Metadata {
        run: RunMetadata::new(model_path),
        program: ProgramMetadata::default(),
        platform: PlatformMetadata::default(),
    };
    let file_path = output_path.join(METADATA_FILE_NAME);
    fs::write(&file_path, toml::to_string(&metadata)?)?;

    Ok(())
}
