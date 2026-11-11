//! Shared helpers for building and solving HiGHS optimisation models.
use crate::asset::AssetRef;
use anyhow::{Context, Result, anyhow};
use highs::HighsModelStatus;
use std::collections::HashMap;
use std::error::Error;

/// Defines the possible errors that can occur when running the solver
#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum ModelError {
    /// An optimal solution could not be found
    #[display("Could not find optimal result: {_0:?}")]
    NonOptimal(HighsModelStatus),
    /// Another error occurred
    #[display("{_0}")]
    Other(anyhow::Error),
}

impl ModelError {
    /// Convert this error into an [`anyhow::Error`]
    pub fn into_anyhow(self) -> anyhow::Error {
        match self {
            ModelError::NonOptimal(status) => anyhow!("Could not find optimal result: {status:?}"),
            ModelError::Other(error) => error,
        }
    }
}

impl Error for ModelError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ModelError::NonOptimal(_) => None,
            ModelError::Other(error) => Some(error.as_ref()),
        }
    }
}

/// Apply the specified HiGHS options from a [`toml::Table`]
pub fn apply_highs_options_from_toml(
    model: &mut highs::Model,
    options: &toml::Table,
) -> Result<()> {
    // Attempt to set an option, returning an error if it fails
    macro_rules! try_set_opt {
        ($option:expr, $value:expr) => {{
            model
                .try_set_option($option.as_str(), $value)
                .map_err(|_| anyhow!("Invalid option name or value"))?;

            Ok(())
        }};
    }

    // Iterate through options, applying each in turn to the HiGHS model
    for (option, value) in options {
        match value {
            toml::Value::String(value) => try_set_opt!(option, value.as_str()),
            toml::Value::Integer(value) => match i32::try_from(*value) {
                Ok(value) => try_set_opt!(option, value),
                Err(_) => Err(anyhow!("Value out of range")),
            },
            toml::Value::Float(value) => try_set_opt!(option, *value),
            toml::Value::Boolean(value) => try_set_opt!(option, *value),
            _ => Err(anyhow!("HiGHS options cannot have this type")),
        }
        .with_context(|| format!("Failed to set option \"{option}\" to value \"{value}\""))?;
    }

    Ok(())
}

/// Try to solve the model, returning an error if the model is incoherent or result is non-optimal
pub fn solve_optimal(model: highs::Model) -> Result<highs::SolvedModel, ModelError> {
    let solved = model
        .try_solve()
        .map_err(|status| anyhow!("Incoherent model: {status:?}"))?;

    match solved.status() {
        HighsModelStatus::Optimal => Ok(solved),
        status => Err(status.into()),
    }
}

/// Groups assets that have equivalent dispatch properties.
///
/// Assets are first bucketed by `dispatch_equivalence_hash()` to avoid unnecessary pairwise
/// comparisons. Within each hash bucket, assets are compared using `is_dispatch_equivalent()`,
/// which is the authoritative check for equivalence. This also handles hash collisions correctly.
///
/// The caller must ensure that `assets` contains only assets eligible for equal-utilisation
/// constraints.
pub fn group_dispatch_equivalent_assets<'a, I>(assets: I) -> Vec<Vec<&'a AssetRef>>
where
    I: Iterator<Item = &'a AssetRef>,
{
    // Group assets by comparing each one with the first asset in each group. The hash index
    // avoids comparing an asset with groups that cannot contain an equivalent asset, while the
    // exact comparison preserves correctness in the event of hash collisions.
    let mut asset_groups: Vec<Vec<&AssetRef>> = Vec::new();
    let mut group_indices: HashMap<u64, Vec<usize>> = HashMap::new();
    for asset in assets {
        let hash = asset.dispatch_equivalence_hash();

        // Only groups with the same hash can possibly match.
        let candidate_groups = group_indices.get(&hash);

        // Find a group whose representative is actually equivalent.
        let matching_group = candidate_groups
            .into_iter()
            .flatten()
            .find(|&&group_index| asset_groups[group_index][0].is_dispatch_equivalent(asset));

        if let Some(group_index) = matching_group {
            asset_groups[*group_index].push(asset);
        } else {
            let group_index = asset_groups.len();
            asset_groups.push(vec![asset]);
            group_indices.entry(hash).or_default().push(group_index);
        }
    }

    asset_groups
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::{Asset, AssetCapacity};
    use crate::fixture::{asset, process};
    use crate::process::Process;
    use crate::units::{Capacity, MoneyPerActivity};
    use rstest::rstest;
    use std::sync::Arc;

    #[test]
    fn groups_no_assets() {
        assert_eq!(
            group_dispatch_equivalent_assets(std::iter::empty()),
            [] as [std::vec::Vec<&AssetRef>; 0]
        );
    }

    #[rstest]
    fn groups_equivalent_assets(asset: Asset) {
        let mut equivalent = asset.clone();
        equivalent.set_capacity(AssetCapacity::single(Capacity(3.0)));
        let assets = [AssetRef::from(asset), AssetRef::from(equivalent)];

        let groups = group_dispatch_equivalent_assets(assets.iter());

        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].len(), 2);
    }

    #[rstest]
    fn groups_non_equivalent_assets_are_separate(asset: Asset, mut process: Process) {
        Arc::make_mut(process.parameters.get_mut(&("GBR".into(), 2015)).unwrap())
            .variable_operating_cost = MoneyPerActivity(1.0);
        let different = Asset::new_ready(
            "agent1".into(),
            Arc::new(process),
            "GBR".into(),
            AssetCapacity::single(Capacity(2.0)),
            2015,
        )
        .unwrap();
        let assets = [AssetRef::from(asset), AssetRef::from(different)];

        let groups = group_dispatch_equivalent_assets(assets.iter());

        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].len(), 1);
        assert_eq!(groups[1].len(), 1);
    }
}
