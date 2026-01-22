//! File patches to be used in integration tests.
//!
//! This is used to test small variations on existing example models.
use crate::patch::FilePatch;
use anyhow::{Context, Result};
use std::{collections::BTreeMap, sync::LazyLock};

/// A map of file patches, keyed by name
type PatchMap = BTreeMap<&'static str, Vec<FilePatch>>;

/// The file patches, keyed by name
static PATCHES: LazyLock<PatchMap> = LazyLock::new(get_all_patches);

/// Get all patches
fn get_all_patches() -> PatchMap {
    [
        (
            // The simple example with gas boiler process made divisible
            "simple_divisible",
            vec![
                FilePatch::new("processes.csv")
                    .with_deletion("RGASBR,Gas boiler,all,RSHEAT,2020,2040,1.0,")
                    .with_addition("RGASBR,Gas boiler,all,RSHEAT,2020,2040,1.0,1000"),
            ],
        ),
        // The simple example with the objective type set to NPV
        (
            "simple_npv",
            vec![FilePatch::new("agent_objectives.csv").with_replace_value("lcox", "npv")],
        ),
    ]
    .into_iter()
    .collect()
}

/// Get the names for all the patches
pub fn get_patch_names() -> impl Iterator<Item = &'static str> {
    PATCHES.keys().copied()
}

/// Get patches for the named patched example
pub fn get_patches(name: &str) -> Result<&[FilePatch]> {
    Ok(PATCHES
        .get(name)
        .with_context(|| format!("Patched example '{name}' not found"))?)
}
