//! Patches to be used in integration tests.
//!
//! This is used to test small variations on existing example models.
use crate::patch::FilePatch;
use anyhow::{Context, Result};
use std::{collections::BTreeMap, sync::LazyLock};
/// Holds patch information for a patched example model
pub struct PatchInfo {
    /// The base example to patch
    pub base_example: &'static str,
    /// File patches to apply to the base example
    pub file_patches: Vec<FilePatch>,
    /// An optional TOML patch to apply to the base example
    pub toml_patch: Option<&'static str>,
}

impl PatchInfo {
    /// Create a new `PatchInfo` with the specified base example, file patches, and optional TOML patch
    pub fn new(
        base_example: &'static str,
        file_patches: Vec<FilePatch>,
        toml_patch: Option<&'static str>,
    ) -> Self {
        Self {
            base_example,
            file_patches,
            toml_patch,
        }
    }
    /// Create a new `PatchInfo` with the specified base example, file patches, and TOML patch
    pub fn new_with_toml_patch(
        base_example: &'static str,
        file_patches: Vec<FilePatch>,
        toml_patch: &'static str,
    ) -> Self {
        Self {
            base_example,
            file_patches,
            toml_patch: Some(toml_patch),
        }
    }
}
/// Map of patches keyed by name, with the file patches and an optional TOML patch
type PatchMap = BTreeMap<&'static str, PatchInfo>;

/// The patches, keyed by name
static PATCHES: LazyLock<PatchMap> = LazyLock::new(get_all_patches);

/// Get all patches
#[allow(clippy::too_many_lines)]
fn get_all_patches() -> PatchMap {
    [
        // The simple example with gas boiler process made divisible
        (
            "simple_divisible",
            PatchInfo::new(
                "simple",
                vec![
                    FilePatch::new("processes.csv")
                        .with_deletion("RGASBR,Gas boiler,all,RSHEAT,2020,2040,1.0,false")
                        .with_addition("RGASBR,Gas boiler,all,RSHEAT,2020,2040,1.0,true"),
                ],
                None,
            ),
        ),
        // The simple example with objective type set to NPV for one agent
        (
            "simple_npv",
            PatchInfo::new(
                "simple",
                vec![
                    FilePatch::new("agent_objectives.csv")
                        .with_deletion("A0_RES,all,lcox,,")
                        .with_addition("A0_RES,all,npv,,"),
                ],
                None,
            ),
        ),
        (
            // The circularity example with Agent A0_ELC's objective type set to NPV
            "circularity_npv",
            PatchInfo::new(
                "circularity",
                vec![
                    FilePatch::new("agent_objectives.csv")
                        .with_deletion("A0_ELC,all,lcox,,")
                        .with_addition("A0_ELC,all,npv,,"),
                ],
                None,
            ),
        ),
        (
            // The simple example with electricity priced using marginal costs
            "simple_marginal",
            PatchInfo::new(
                "simple",
                vec![FilePatch::new("commodities.csv").with_replacement(&[
                    "id,description,type,time_slice_level,pricing_strategy,units",
                    "GASPRD,Gas produced,sed,season,,PJ",
                    "GASNAT,Natural gas,sed,season,,PJ",
                    "ELCTRI,Electricity,sed,daynight,marginal,PJ",
                    "RSHEAT,Residential heating,svd,daynight,,PJ",
                    "CO2EMT,CO2 emitted,oth,annual,,ktCO2",
                ])],
                None,
            ),
        ),
        (
            // The simple example with gas commodities priced using full costs
            "simple_full",
            PatchInfo::new(
                "simple",
                vec![FilePatch::new("commodities.csv").with_replacement(&[
                    "id,description,type,time_slice_level,pricing_strategy,units",
                    "GASPRD,Gas produced,sed,season,full,PJ",
                    "GASNAT,Natural gas,sed,season,full,PJ",
                    "ELCTRI,Electricity,sed,daynight,,PJ",
                    "RSHEAT,Residential heating,svd,daynight,,PJ",
                    "CO2EMT,CO2 emitted,oth,annual,,ktCO2",
                ])],
                None,
            ),
        ),
        (
            // The simple example with electricity priced using average marginal costs
            "simple_marginal_average",
            PatchInfo::new(
                "simple",
                vec![FilePatch::new("commodities.csv").with_replacement(&[
                    "id,description,type,time_slice_level,pricing_strategy,units",
                    "GASPRD,Gas produced,sed,season,,PJ",
                    "GASNAT,Natural gas,sed,season,,PJ",
                    "ELCTRI,Electricity,sed,daynight,marginal_average,PJ",
                    "RSHEAT,Residential heating,svd,daynight,,PJ",
                    "CO2EMT,CO2 emitted,oth,annual,,ktCO2",
                ])],
                None,
            ),
        ),
        (
            // The simple example with electricity priced using shadow prices
            "simple_shadow",
            PatchInfo::new(
                "simple",
                vec![FilePatch::new("commodities.csv").with_replacement(&[
                    "id,description,type,time_slice_level,pricing_strategy,units",
                    "GASPRD,Gas produced,sed,season,,PJ",
                    "GASNAT,Natural gas,sed,season,,PJ",
                    "ELCTRI,Electricity,sed,daynight,shadow,PJ",
                    "RSHEAT,Residential heating,svd,daynight,,PJ",
                    "CO2EMT,CO2 emitted,oth,annual,,ktCO2",
                ])],
                None,
            ),
        ),
        // The simple example with the ironing-out loop turned on
        (
            "simple_ironing_out",
            PatchInfo::new("simple", vec![], Some("max_ironing_out_iterations = 10")),
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
pub fn get_patches(name: &str) -> Result<&'static PatchInfo> {
    PATCHES
        .get(name)
        .with_context(|| format!("Patched example '{name}' not found"))
}
