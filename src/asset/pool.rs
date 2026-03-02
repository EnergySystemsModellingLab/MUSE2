//! Defines a data structure for representing the current active pool of assets.
use super::{AssetID, AssetRef, AssetState};
use itertools::Itertools;
use log::warn;
use std::cmp::min;
use std::slice;

/// The active pool of [`super::Asset`]s
#[derive(Default)]
pub struct AssetPool {
    /// The pool of active assets, sorted by ID
    assets: Vec<AssetRef>,
    /// Next available asset ID number
    next_id: u32,
    /// Next available group ID number
    next_group_id: u32,
}

impl AssetPool {
    /// Create a new empty [`AssetPool`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the active pool as a slice of [`AssetRef`]s
    pub fn as_slice(&self) -> &[AssetRef] {
        &self.assets
    }

    /// Commission new assets for the specified milestone year from the input data
    pub fn commission_new(&mut self, year: u32, user_assets: &mut Vec<AssetRef>) {
        let to_commission = user_assets.extract_if(.., |asset| asset.commission_year <= year);

        for asset in to_commission {
            // Ignore assets that have already been decommissioned
            if asset.max_decommission_year() <= year {
                warn!(
                    "Asset '{}' with commission year {} and lifetime {} was decommissioned before \
                    the start of the simulation",
                    asset.process_id(),
                    asset.commission_year,
                    asset.process_parameter.lifetime
                );
                continue;
            }

            self.commission(asset, "user input");
        }
    }

    /// Commission the specified asset or, if divisible, its children
    fn commission(&mut self, asset: AssetRef, reason: &str) {
        asset.into_for_each_child(&mut self.next_group_id, |parent, mut child| {
            child
                .make_mut()
                .commission(AssetID(self.next_id), parent.cloned(), reason);
            self.next_id += 1;
            self.assets.push(child);
        });
    }

    /// Decommission old assets for the specified milestone year
    pub fn decommission_old<E: Extend<AssetRef>>(&mut self, year: u32, decommissioned: &mut E) {
        let to_decommission = self
            .assets
            .extract_if(.., move |asset| asset.max_decommission_year() <= year)
            .map(move |mut asset| {
                asset.make_mut().decommission(year, "end of life");
                asset
            });
        decommissioned.extend(to_decommission);
    }

    /// Decommission mothballed assets if mothballed long enough
    pub fn decommission_mothballed<E: Extend<AssetRef>>(
        &mut self,
        year: u32,
        mothball_years: u32,
        decommissioned: &mut E,
    ) {
        let to_decommission = self
            .assets
            .extract_if(.., move |asset| {
                asset
                    .get_mothballed_year()
                    .is_some_and(|myear| myear <= year - min(mothball_years, year))
            })
            .map(move |mut asset| {
                let decommissioned = asset.get_mothballed_year().unwrap() + mothball_years;
                asset.make_mut().decommission(
                    decommissioned,
                    &format!(
                        "The asset has not been used for the set mothball years ({mothball_years} \
                        years)."
                    ),
                );
                asset
            });
        decommissioned.extend(to_decommission);
    }

    /// Mothball the specified assets if they are no longer in the active pool and put them back again.
    ///
    /// # Arguments
    ///
    /// * `assets` - Assets to possibly mothball
    /// * `year` - Mothball year
    ///
    /// # Panics
    ///
    /// Panics if any of the provided assets was never commissioned.
    pub fn mothball_unretained<I>(&mut self, assets: I, year: u32)
    where
        I: IntoIterator<Item = AssetRef>,
    {
        for mut asset in assets {
            if match asset.state {
                AssetState::Commissioned { .. } => !self.assets.contains(&asset),
                _ => panic!("Cannot mothball asset that has not been commissioned"),
            } {
                // If not already set, we set the current year as the mothball year,
                // i.e. the first one the asset was not used.
                if asset.get_mothballed_year().is_none() {
                    asset.make_mut().mothball(year);
                }

                // And we put it back to the pool, so they can be chosen the next milestone year
                // if not decommissioned earlier.
                self.assets.push(asset);
            }
        }
        self.assets.sort();
    }

    /// Get an asset with the specified ID.
    ///
    /// # Returns
    ///
    /// An [`AssetRef`] if found, else `None`. The asset may not be found if it has already been
    /// decommissioned.
    pub fn get(&self, id: AssetID) -> Option<&AssetRef> {
        // Assets are sorted by ID
        let idx = self
            .assets
            .binary_search_by(|asset| match &asset.state {
                AssetState::Commissioned { id: asset_id, .. } => asset_id.cmp(&id),
                _ => panic!("Active pool should only contain commissioned assets"),
            })
            .ok()?;

        Some(&self.assets[idx])
    }

    /// Iterate over active assets
    #[allow(clippy::iter_without_into_iter)]
    pub fn iter(&self) -> slice::Iter<'_, AssetRef> {
        self.assets.iter()
    }

    /// Return current active pool and clear
    pub fn take(&mut self) -> Vec<AssetRef> {
        std::mem::take(&mut self.assets)
    }

    /// Extend the active pool with Commissioned or Selected assets
    pub fn extend<I>(&mut self, assets: I)
    where
        I: IntoIterator<Item = AssetRef>,
    {
        // Check all assets are either Commissioned or Selected, and, if the latter,
        // then commission them
        for mut asset in assets {
            match &asset.state {
                AssetState::Commissioned { .. } => {
                    asset.make_mut().unmothball();
                    self.assets.push(asset);
                }
                AssetState::Selected { .. } => {
                    self.commission(asset, "selected");
                }
                _ => panic!(
                    "Cannot extend asset pool with asset in state {}. Only assets in \
                Commissioned or Selected states are allowed.",
                    asset.state
                ),
            }
        }

        // New assets may not have been sorted, but we need them sorted by ID
        self.assets.sort();

        // Sanity check: all assets should be unique
        debug_assert_eq!(self.assets.iter().unique().count(), self.assets.len());
    }
}

#[cfg(test)]
mod tests {
    use super::super::Asset;
    use super::*;
    use crate::fixture::{asset, asset_divisible, process, process_parameter_map};
    use crate::process::{Process, ProcessParameter};
    use crate::units::{
        Capacity, Dimensionless, MoneyPerActivity, MoneyPerCapacity, MoneyPerCapacityPerYear,
    };
    use itertools::{Itertools, assert_equal};
    use rstest::{fixture, rstest};
    use std::iter;
    use std::rc::Rc;

    #[fixture]
    fn user_assets(mut process: Process) -> Vec<AssetRef> {
        // Update process parameters (lifetime = 20 years)
        let process_param = ProcessParameter {
            capital_cost: MoneyPerCapacity(5.0),
            fixed_operating_cost: MoneyPerCapacityPerYear(2.0),
            variable_operating_cost: MoneyPerActivity(1.0),
            lifetime: 20,
            discount_rate: Dimensionless(0.9),
        };
        let process_parameter_map = process_parameter_map(process.regions.clone(), process_param);
        process.parameters = process_parameter_map;

        let rc_process = Rc::new(process);
        [2020, 2010]
            .map(|year| {
                Asset::new_future(
                    "agent1".into(),
                    Rc::clone(&rc_process),
                    "GBR".into(),
                    Capacity(1.0),
                    year,
                )
                .unwrap()
                .into()
            })
            .into_iter()
            .collect_vec()
    }

    #[rstest]
    fn asset_pool_new() {
        assert!(AssetPool::new().assets.is_empty());
    }

    #[rstest]
    fn asset_pool_commission_new1(mut user_assets: Vec<AssetRef>) {
        // Asset to be commissioned in this year
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2010, &mut user_assets);
        assert_equal(asset_pool.iter(), iter::once(&asset_pool.assets[0]));
    }

    #[rstest]
    fn asset_pool_commission_new2(mut user_assets: Vec<AssetRef>) {
        // Commission year has passed
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2011, &mut user_assets);
        assert_equal(asset_pool.iter(), iter::once(&asset_pool.assets[0]));
    }

    #[rstest]
    fn asset_pool_commission_new3(mut user_assets: Vec<AssetRef>) {
        // Nothing to commission for this year
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2000, &mut user_assets);
        assert!(asset_pool.iter().next().is_none()); // no active assets
    }

    /// Number of expected children for divisible asset
    #[allow(clippy::cast_possible_truncation)]
    #[allow(clippy::cast_sign_loss)]
    fn expected_children_for_divisible(asset: &Asset) -> usize {
        (asset.total_capacity() / asset.process.unit_size.expect("Asset is not divisible"))
            .value()
            .ceil() as usize
    }

    #[rstest]
    fn asset_pool_commission_new_divisible(asset_divisible: Asset) {
        let commission_year = asset_divisible.commission_year;
        let expected_children = expected_children_for_divisible(&asset_divisible);
        let mut asset_pool = AssetPool::new();
        let mut user_assets = vec![asset_divisible.into()];
        assert!(asset_pool.assets.is_empty());
        asset_pool.commission_new(commission_year, &mut user_assets);
        assert!(user_assets.is_empty());
        assert!(!asset_pool.assets.is_empty());
        assert_eq!(asset_pool.assets.len(), expected_children);
        assert_eq!(asset_pool.next_group_id, 1);
    }

    #[rstest]
    fn asset_pool_commission_already_decommissioned(asset: Asset) {
        let year = asset.max_decommission_year();
        let mut asset_pool = AssetPool::new();
        assert!(asset_pool.assets.is_empty());
        asset_pool.commission_new(year, &mut vec![asset.into()]);
        assert!(asset_pool.assets.is_empty());
    }

    #[rstest]
    fn asset_pool_decommission_old(mut user_assets: Vec<AssetRef>) {
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert!(user_assets.is_empty());
        assert_eq!(asset_pool.assets.len(), 2);
        let mut decommissioned = Vec::new();

        // should decommission first asset (lifetime == 5)
        asset_pool.decommission_old(2030, &mut decommissioned);
        assert_eq!(asset_pool.assets.len(), 1);
        assert_eq!(asset_pool.assets[0].commission_year, 2020);
        assert_eq!(decommissioned.len(), 1);
        assert_eq!(decommissioned[0].commission_year, 2010);
        assert_eq!(decommissioned[0].decommission_year(), Some(2030));

        // nothing to decommission
        decommissioned.clear();
        asset_pool.decommission_old(2032, &mut decommissioned);
        assert_eq!(asset_pool.assets.len(), 1);
        assert_eq!(asset_pool.assets[0].commission_year, 2020);

        // should decommission second asset
        decommissioned.clear();
        asset_pool.decommission_old(2040, &mut decommissioned);
        assert!(asset_pool.assets.is_empty());
        assert_eq!(decommissioned.len(), 1);
        assert_eq!(decommissioned[0].commission_year, 2020);
        assert_eq!(decommissioned[0].decommission_year(), Some(2040));
    }

    #[rstest]
    fn asset_pool_get(mut user_assets: Vec<AssetRef>) {
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.get(AssetID(0)), Some(&asset_pool.assets[0]));
        assert_eq!(asset_pool.get(AssetID(1)), Some(&asset_pool.assets[1]));
    }

    #[rstest]
    fn asset_pool_extend_empty(mut user_assets: Vec<AssetRef>) {
        // Start with commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let original_count = asset_pool.assets.len();

        // Extend with empty iterator
        asset_pool.extend(Vec::<AssetRef>::new());

        assert_eq!(asset_pool.assets.len(), original_count);
    }

    #[rstest]
    fn asset_pool_extend_existing_assets(mut user_assets: Vec<AssetRef>) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.assets.len(), 2);
        let existing_assets = asset_pool.take();

        // Extend with the same assets (should maintain their IDs)
        asset_pool.extend(existing_assets.clone());

        assert_eq!(asset_pool.assets.len(), 2);
        assert_eq!(asset_pool.assets[0].id(), Some(AssetID(0)));
        assert_eq!(asset_pool.assets[1].id(), Some(AssetID(1)));
    }

    #[rstest]
    fn asset_pool_extend_new_assets(mut user_assets: Vec<AssetRef>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let original_count = asset_pool.assets.len();

        // Create new non-commissioned assets
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_selected(
                "agent2".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.5),
                2015,
            )
            .unwrap()
            .into(),
            Asset::new_selected(
                "agent3".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(2.5),
                2020,
            )
            .unwrap()
            .into(),
        ];

        asset_pool.extend(new_assets);

        assert_eq!(asset_pool.assets.len(), original_count + 2);
        // New assets should get IDs 2 and 3
        assert_eq!(asset_pool.assets[original_count].id(), Some(AssetID(2)));
        assert_eq!(asset_pool.assets[original_count + 1].id(), Some(AssetID(3)));
        assert_eq!(
            asset_pool.assets[original_count].agent_id(),
            Some(&"agent2".into())
        );
        assert_eq!(
            asset_pool.assets[original_count + 1].agent_id(),
            Some(&"agent3".into())
        );
    }

    #[rstest]
    fn asset_pool_extend_new_divisible_assets(
        mut user_assets: Vec<AssetRef>,
        mut process: Process,
    ) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let original_count = asset_pool.assets.len();

        // Create new non-commissioned assets
        process.unit_size = Some(Capacity(4.0));
        let process_rc = Rc::new(process);
        let new_assets: Vec<AssetRef> = vec![
            Asset::new_selected(
                "agent2".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(11.0),
                2015,
            )
            .unwrap()
            .into(),
        ];
        let expected_children = expected_children_for_divisible(&new_assets[0]);
        asset_pool.extend(new_assets);
        assert_eq!(asset_pool.assets.len(), original_count + expected_children);
    }

    #[rstest]
    fn asset_pool_extend_mixed_assets(mut user_assets: Vec<AssetRef>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);

        // Create a new non-commissioned asset
        let new_asset = Asset::new_selected(
            "agent_new".into(),
            process.into(),
            "GBR".into(),
            Capacity(3.0),
            2015,
        )
        .unwrap()
        .into();

        // Extend with just the new asset (not mixing with existing to avoid duplicates)
        asset_pool.extend(vec![new_asset]);

        assert_eq!(asset_pool.assets.len(), 3);
        // Check that we have the original assets plus the new one
        assert!(asset_pool.assets.iter().any(|a| a.id() == Some(AssetID(0))));
        assert!(asset_pool.assets.iter().any(|a| a.id() == Some(AssetID(1))));
        assert!(asset_pool.assets.iter().any(|a| a.id() == Some(AssetID(2))));
        // Check that the new asset has the correct agent
        assert!(
            asset_pool
                .assets
                .iter()
                .any(|a| a.agent_id() == Some(&"agent_new".into()))
        );
    }

    #[rstest]
    fn asset_pool_extend_maintains_sort_order(mut user_assets: Vec<AssetRef>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);

        // Create new assets that would be out of order if added at the end
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_selected(
                "agent_high_id".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2010,
            )
            .unwrap()
            .into(),
            Asset::new_selected(
                "agent_low_id".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2015,
            )
            .unwrap()
            .into(),
        ];

        asset_pool.extend(new_assets);

        // Check that assets are sorted by ID
        let ids: Vec<u32> = asset_pool.iter().map(|a| a.id().unwrap().0).collect();
        assert_equal(ids, 0..4);
    }

    #[rstest]
    fn asset_pool_extend_no_duplicates_expected(mut user_assets: Vec<AssetRef>) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let original_count = asset_pool.assets.len();

        // The extend method expects unique assets - adding duplicates would violate
        // the debug assertion, so this test verifies the normal case
        asset_pool.extend(Vec::new());

        assert_eq!(asset_pool.assets.len(), original_count);
        // Verify all assets are still unique (this is what the debug_assert checks)
        assert_eq!(
            asset_pool.assets.iter().unique().count(),
            asset_pool.assets.len()
        );
    }

    #[rstest]
    fn asset_pool_extend_increments_next_id(mut user_assets: Vec<AssetRef>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.next_id, 2); // Should be 2 after commissioning 2 assets

        // Create new non-commissioned assets
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_selected(
                "agent1".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2015,
            )
            .unwrap()
            .into(),
            Asset::new_selected(
                "agent2".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2020,
            )
            .unwrap()
            .into(),
        ];

        asset_pool.extend(new_assets);

        // next_id should have incremented for each new asset
        assert_eq!(asset_pool.next_id, 4);
        assert_eq!(asset_pool.assets[2].id(), Some(AssetID(2)));
        assert_eq!(asset_pool.assets[3].id(), Some(AssetID(3)));
    }

    #[rstest]
    fn asset_pool_mothball_unretained(mut user_assets: Vec<AssetRef>) {
        // Commission some assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.assets.len(), 2);

        // Remove one asset from the active pool (simulating it being removed elsewhere)
        let removed_asset = asset_pool.assets.remove(0);
        assert_eq!(asset_pool.assets.len(), 1);

        // Try to mothball both the removed asset (not in active) and an active asset
        let assets_to_check = vec![removed_asset.clone(), asset_pool.assets[0].clone()];
        asset_pool.mothball_unretained(assets_to_check, 2025);

        // Only the removed asset should be mothballed (since it's not in active pool)
        assert_eq!(asset_pool.assets.len(), 2); // And should be back into the pool
        assert_eq!(asset_pool.assets[0].get_mothballed_year(), Some(2025));
    }

    #[rstest]
    fn asset_pool_decommission_unused(mut user_assets: Vec<AssetRef>) {
        // Commission some assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.assets.len(), 2);

        // Make an asset unused for a few years
        let mothball_years: u32 = 10;
        asset_pool.assets[0]
            .make_mut()
            .mothball(2025 - mothball_years);

        assert_eq!(
            asset_pool.assets[0].get_mothballed_year(),
            Some(2025 - mothball_years)
        );

        // Decommission unused assets
        let mut decommissioned = Vec::new();
        asset_pool.decommission_mothballed(2025, mothball_years, &mut decommissioned);

        // Only the removed asset should be decommissioned (since it's not in active pool)
        assert_eq!(asset_pool.assets.len(), 1); // Active pool unchanged
        assert_eq!(decommissioned.len(), 1);
        assert_eq!(decommissioned[0].decommission_year(), Some(2025));
    }

    #[rstest]
    fn asset_pool_decommission_if_not_active_none_active(mut user_assets: Vec<AssetRef>) {
        // Commission some assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let all_assets = asset_pool.assets.clone();

        // Clear the active pool (simulating all assets being removed)
        asset_pool.assets.clear();

        // Try to mothball the assets that are no longer active
        asset_pool.mothball_unretained(all_assets.clone(), 2025);

        // All assets should be mothballed
        assert_eq!(asset_pool.assets.len(), 2);
        assert_eq!(asset_pool.assets[0].id(), all_assets[0].id());
        assert_eq!(asset_pool.assets[0].get_mothballed_year(), Some(2025));
        assert_eq!(asset_pool.assets[1].id(), all_assets[1].id());
        assert_eq!(asset_pool.assets[1].get_mothballed_year(), Some(2025));
    }

    #[rstest]
    #[should_panic(expected = "Cannot mothball asset that has not been commissioned")]
    fn asset_pool_decommission_if_not_active_non_commissioned_asset(process: Process) {
        // Create a non-commissioned asset
        let non_commissioned_asset = Asset::new_future(
            "agent_new".into(),
            process.into(),
            "GBR".into(),
            Capacity(1.0),
            2015,
        )
        .unwrap()
        .into();

        // This should panic because the asset was never commissioned
        let mut asset_pool = AssetPool::new();
        asset_pool.mothball_unretained(vec![non_commissioned_asset], 2025);
    }
}
