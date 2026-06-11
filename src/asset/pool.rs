//! Defines a data structure for representing the current active pool of assets.
use super::{AssetID, AssetRef, AssetState, UserAsset};
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

    /// Commission new assets for the specified milestone year from the input data.
    ///
    /// Returns the newly commissioned assets (children, for divisible assets).
    pub fn commission_new(&mut self, year: u32, user_assets: &mut Vec<UserAsset>) -> &[AssetRef] {
        let start = self.assets.len();
        let to_commission = user_assets.extract_if(.., |asset| asset.commission_year <= year);

        for asset in to_commission {
            // Ignore assets that have already been decommissioned
            if asset.max_decommission_year() <= year {
                warn!(
                    "User asset '{}' with commission year {} with maximum decommission year {} \
                    was decommissioned before start of the simulation",
                    asset.process_id(),
                    asset.commission_year,
                    asset.max_decommission_year
                );
                continue;
            }

            self.commission(asset.into());
        }

        &self.assets[start..]
    }

    /// Commission the specified asset or, if divisible, its children
    fn commission(&mut self, asset: AssetRef) {
        asset.into_for_each_child(&mut self.next_group_id, |parent, mut child| {
            child
                .make_mut()
                .commission(AssetID(self.next_id), parent.cloned());
            self.next_id += 1;
            self.assets.push(child);
        });
    }

    /// Decommission old assets for the specified milestone year
    pub fn decommission_old(&mut self, year: u32) {
        self.assets
            .extract_if(.., |asset| asset.max_decommission_year() <= year)
            .for_each(|mut asset| {
                asset.make_mut().decommission("end of life");
            });
    }

    /// Decommission mothballed assets if mothballed long enough
    pub fn decommission_mothballed(&mut self, year: u32, mothball_years: u32) {
        self.assets
            .extract_if(.., |asset| {
                asset
                    .get_mothballed_year()
                    .is_some_and(|myear| myear <= year - min(mothball_years, year))
            })
            .for_each(|mut asset| {
                asset.make_mut().decommission(&format!(
                    "The asset has not been used for the set mothball years ({mothball_years} \
                        years)."
                ));
            });
    }

    /// Mothball the specified assets if they are no longer in the active pool and put them back
    /// again.
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
            let in_pool = match asset.state {
                AssetState::Commissioned { .. } => !self.assets.contains(&asset),
                _ => panic!("Cannot mothball asset that has not been commissioned"),
            };

            if in_pool {
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

    /// Extend the active pool with Commissioned or Ready assets.
    ///
    /// Returns the newly commissioned assets (those that were in `Ready` state on entry).
    pub fn extend<I>(&mut self, assets: I) -> &[AssetRef]
    where
        I: IntoIterator<Item = AssetRef>,
    {
        let first_new_id = self.next_id;

        // Check all assets are either Commissioned or Ready, and, if the latter,
        // then commission them
        for mut asset in assets {
            match &asset.state {
                AssetState::Commissioned { .. } => {
                    asset.make_mut().unmothball();
                    self.assets.push(asset);
                }
                AssetState::Ready { .. } => {
                    self.commission(asset);
                }
                _ => panic!(
                    "Cannot extend asset pool with asset in state {}. Only assets in \
                    Commissioned or Ready states are allowed.",
                    asset.state
                ),
            }
        }

        // New assets may not have been sorted, but we need them sorted by ID
        self.assets.sort();

        // Sanity check: all assets should be unique
        debug_assert_eq!(self.assets.iter().unique().count(), self.assets.len());

        // Newly commissioned assets have IDs >= first_new_id. Since assets are sorted by ID,
        // they are at the tail of the slice.
        let new_start = self.assets.partition_point(|a| match &a.state {
            AssetState::Commissioned { id, .. } => id.0 < first_new_id,
            _ => panic!("Active pool should only contain commissioned assets"),
        });
        &self.assets[new_start..]
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
    fn user_assets(mut process: Process) -> Vec<UserAsset> {
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
                UserAsset::new(
                    "agent1".into(),
                    Rc::clone(&rc_process),
                    "GBR".into(),
                    Capacity(1.0),
                    year,
                    None,
                )
                .unwrap()
            })
            .into_iter()
            .collect_vec()
    }

    #[rstest]
    fn asset_pool_new() {
        assert!(AssetPool::new().assets.is_empty());
    }

    #[rstest]
    fn asset_pool_commission_new1(mut user_assets: Vec<UserAsset>) {
        // Asset to be commissioned in this year
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2010, &mut user_assets);
        assert_equal(asset_pool.iter(), iter::once(&asset_pool.assets[0]));
    }

    #[rstest]
    fn asset_pool_commission_new2(mut user_assets: Vec<UserAsset>) {
        // Commission year has passed
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2011, &mut user_assets);
        assert_equal(asset_pool.iter(), iter::once(&asset_pool.assets[0]));
    }

    #[rstest]
    fn asset_pool_commission_new3(mut user_assets: Vec<UserAsset>) {
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
    #[allow(clippy::cast_possible_truncation)]
    fn asset_pool_commission_new_returns_only_assets_from_call(asset_divisible: Asset) {
        let year = asset_divisible.commission_year();
        let expected_children = expected_children_for_divisible(&asset_divisible);
        let mut asset_pool = AssetPool::new();

        // First call commissions one divisible asset and returns all of its children
        let mut user_assets = vec![asset_divisible.clone().into()];
        let first_batch = asset_pool.commission_new(year, &mut user_assets).to_vec();
        assert_eq!(first_batch.len(), expected_children);
        assert!(first_batch.iter().all(|asset| asset.parent().is_some()));

        // IDs should form a contiguous sequence starting from 0
        let n = expected_children as u32;
        assert_equal(first_batch.iter().map(|a| a.id().unwrap().0), 0..n);

        // Second call should return only assets commissioned in this second invocation
        let mut later_assets = vec![asset_divisible.into()];
        let second_batch = asset_pool
            .commission_new(year + 1, &mut later_assets)
            .to_vec();
        assert_eq!(asset_pool.assets.len(), expected_children * 2);
        assert!(
            second_batch
                .iter()
                .all(|asset| !first_batch.iter().any(|old| old == asset))
        );

        // IDs of the second batch continue directly on from the first
        assert_equal(second_batch.iter().map(|a| a.id().unwrap().0), n..n * 2);
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
    fn asset_pool_decommission_old(mut user_assets: Vec<UserAsset>) {
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert!(user_assets.is_empty());
        assert_eq!(asset_pool.assets.len(), 2);

        // should decommission first asset (lifetime == 5)
        asset_pool.decommission_old(2030);
        assert_eq!(asset_pool.assets.len(), 1);
        assert_eq!(asset_pool.assets[0].commission_year, 2020);

        // nothing to decommission
        asset_pool.decommission_old(2032);
        assert_eq!(asset_pool.assets.len(), 1);
        assert_eq!(asset_pool.assets[0].commission_year, 2020);

        // should decommission second asset
        asset_pool.decommission_old(2040);
        assert!(asset_pool.assets.is_empty());
    }

    #[rstest]
    fn asset_pool_get(mut user_assets: Vec<UserAsset>) {
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.get(AssetID(0)), Some(&asset_pool.assets[0]));
        assert_eq!(asset_pool.get(AssetID(1)), Some(&asset_pool.assets[1]));
    }

    #[rstest]
    fn asset_pool_extend_empty(mut user_assets: Vec<UserAsset>) {
        // Start with commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let original_count = asset_pool.assets.len();

        // Extend with empty iterator
        asset_pool.extend(Vec::<AssetRef>::new());

        assert_eq!(asset_pool.assets.len(), original_count);
    }

    #[rstest]
    fn asset_pool_extend_existing_assets(mut user_assets: Vec<UserAsset>) {
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
    fn asset_pool_extend_new_assets(mut user_assets: Vec<UserAsset>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        let original_count = asset_pool.assets.len();

        // Create new non-commissioned assets
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_ready(
                "agent2".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.5),
                2015,
            )
            .unwrap()
            .into(),
            Asset::new_ready(
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
        mut user_assets: Vec<UserAsset>,
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
            Asset::new_ready(
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
    #[allow(clippy::cast_possible_truncation)]
    fn asset_pool_extend_returns_only_newly_commissioned_assets(
        mut user_assets: Vec<UserAsset>,
        mut process: Process,
    ) {
        let mut asset_pool = AssetPool::new();

        // Seed pool with already commissioned assets and take them as the existing set
        let initial_assets = asset_pool.commission_new(2020, &mut user_assets);
        let initial_count = initial_assets.len();
        let existing_assets = asset_pool.take();

        // Add one ready divisible asset so extend() commissions multiple new children
        process.unit_size = Some(Capacity(4.0));
        let process_rc = Rc::new(process);
        let ready_divisible: AssetRef = Asset::new_ready(
            "agent_selected".into(),
            Rc::clone(&process_rc),
            "GBR".into(),
            Capacity(11.0),
            2020,
        )
        .unwrap()
        .into();
        let expected_new = expected_children_for_divisible(&ready_divisible);

        // Extend with a mix of existing commissioned assets and one ready divisible asset
        let returned = asset_pool
            .extend(
                existing_assets
                    .iter()
                    .cloned()
                    .chain(iter::once(ready_divisible)),
            )
            .to_vec();

        // Returned assets should be exactly the newly commissioned children
        assert_eq!(returned.len(), expected_new);
        assert_eq!(asset_pool.assets.len(), initial_count + expected_new);
        assert!(returned.iter().all(|asset| asset.parent().is_some()));

        // IDs form a contiguous range immediately after the pre-existing assets
        let start = initial_count as u32;
        let end = (initial_count + expected_new) as u32;
        assert_equal(returned.iter().map(|a| a.id().unwrap().0), start..end);
    }

    #[rstest]
    fn asset_pool_extend_mixed_assets(mut user_assets: Vec<UserAsset>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);

        // Create a new non-commissioned asset
        let new_asset = Asset::new_ready(
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
    fn asset_pool_extend_maintains_sort_order(mut user_assets: Vec<UserAsset>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);

        // Create new assets that would be out of order if added at the end
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_ready(
                "agent_high_id".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2010,
            )
            .unwrap()
            .into(),
            Asset::new_ready(
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
    fn asset_pool_extend_no_duplicates_expected(mut user_assets: Vec<UserAsset>) {
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
    fn asset_pool_extend_increments_next_id(mut user_assets: Vec<UserAsset>, process: Process) {
        // Start with some commissioned assets
        let mut asset_pool = AssetPool::new();
        asset_pool.commission_new(2020, &mut user_assets);
        assert_eq!(asset_pool.next_id, 2); // Should be 2 after commissioning 2 assets

        // Create new non-commissioned assets
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_ready(
                "agent1".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2015,
            )
            .unwrap()
            .into(),
            Asset::new_ready(
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
    fn asset_pool_mothball_unretained(mut user_assets: Vec<UserAsset>) {
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
    fn asset_pool_decommission_unused(mut user_assets: Vec<UserAsset>) {
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
        asset_pool.decommission_mothballed(2025, mothball_years);

        // Only the removed asset should be decommissioned (since it's not in active pool)
        assert_eq!(asset_pool.assets.len(), 1); // Active pool unchanged
    }

    #[rstest]
    fn asset_pool_decommission_if_not_active_none_active(mut user_assets: Vec<UserAsset>) {
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
        let non_commissioned_asset = Asset::new_ready(
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
