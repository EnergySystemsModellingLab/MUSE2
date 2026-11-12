//! Types representing the activities of assets and the commodity flows derived from them.
use crate::asset::{AssetPool, AssetRef};
use crate::commodity::CommodityID;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Flow};
use indexmap::IndexMap;
use std::collections::HashMap;

/// A map of asset activities in each time slice
pub type ActivityMap = IndexMap<(AssetRef, TimeSliceID), Activity>;

/// A map of commodity flows for assets in each time slice
pub type FlowMap = IndexMap<(AssetRef, CommodityID, TimeSliceID), Flow>;

/// Create a map of commodity flows from asset activities.
///
/// The flow of each commodity is the asset's activity multiplied by its flow coefficient.
pub fn create_flow_map<'a, I>(activity: I) -> FlowMap
where
    I: IntoIterator<Item = (&'a AssetRef, &'a TimeSliceID, Activity)>,
{
    let mut flows = FlowMap::new();
    for (asset, time_slice, activity) in activity {
        for flow in asset.iter_flows() {
            let flow_key = (asset.clone(), flow.commodity.id.clone(), time_slice.clone());
            flows.insert(flow_key, activity * flow.coeff);
        }
    }

    flows
}

/// The assets selected by agents, along with the activities determined during asset selection
#[derive(Clone, Default)]
pub struct Selection {
    /// The selected assets (including retained commissioned assets)
    pub assets: Vec<AssetRef>,
    /// The activity of each selected asset in each time slice
    pub activity: ActivityMap,
}

impl Selection {
    /// Create a [`Selection`] from the results of an asset selection optimisation.
    ///
    /// `selected` pairs each asset as it appears in `activity` with the asset that was ultimately
    /// selected. These may compare unequal (e.g. candidates are assigned an agent on selection), so
    /// activities are re-keyed to the selected assets. Activities of assets which were not selected
    /// are discarded.
    pub fn from_optimisation(selected: Vec<(AssetRef, AssetRef)>, activity: &ActivityMap) -> Self {
        let activity = {
            let selected_by_original: HashMap<_, _> =
                selected.iter().map(|(old, new)| (old, new)).collect();
            activity
                .iter()
                .filter_map(|((asset, time_slice), activity)| {
                    let asset = selected_by_original.get(asset)?;
                    Some((((*asset).clone(), time_slice.clone()), *activity))
                })
                .collect::<ActivityMap>()
        };
        debug_assert!(
            selected
                .iter()
                .all(|(_, new)| activity.keys().any(|(asset, _)| asset == new)),
            "All selected assets must have activities"
        );
        let assets = selected.into_iter().map(|(_, new)| new).collect();

        Self { assets, activity }
    }

    /// Whether no assets were selected
    pub fn is_empty(&self) -> bool {
        self.assets.is_empty()
    }

    /// Add the assets and activities from another [`Selection`]
    pub fn extend(&mut self, other: Selection) {
        debug_assert!(
            other
                .assets
                .iter()
                .all(|asset| !self.assets.contains(asset)),
            "Assets cannot be selected more than once"
        );
        self.assets.extend(other.assets);
        self.activity.extend(other.activity);
    }

    /// Retain only the assets (and their activities) for which `predicate` returns true
    pub fn retain<F>(&mut self, predicate: F)
    where
        F: Fn(&AssetRef) -> bool,
    {
        self.assets.retain(&predicate);
        self.activity.retain(|(asset, _), _| predicate(asset));
    }

    /// Add the selected assets to the pool, re-keying activities to newly commissioned assets.
    ///
    /// Returns the newly commissioned assets. Retained assets keep their selected capacities.
    pub fn commission<'a>(&mut self, pool: &'a mut AssetPool) -> &'a [AssetRef] {
        let new_assets = pool.extend(self.assets.iter().cloned());
        let mut commissioned = new_assets.iter();
        let selected = self
            .assets
            .iter()
            .map(|asset| {
                let selected = if asset.id().is_none() {
                    commissioned.next().expect("Missing commissioned asset")
                } else {
                    asset
                };
                (asset.clone(), selected.clone())
            })
            .collect();
        *self = Self::from_optimisation(selected, &self.activity);
        new_assets
    }

    /// Create a map of commodity flows from the activities of the selected assets
    pub fn flow_map(&self) -> FlowMap {
        create_flow_map(
            self.activity
                .iter()
                .map(|((asset, time_slice), activity)| (asset, time_slice, *activity)),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asset::Asset;
    use crate::commodity::Commodity;
    use crate::fixture::{agent_id, process, process_flows_map, svd_commodity, time_slice};
    use crate::process::{FlowType, Process, ProcessFlow};
    use crate::units::{Capacity, FlowPerActivity, MoneyPerFlow};
    use indexmap::indexmap;
    use rstest::rstest;
    use std::sync::Arc;

    fn candidate(process: &Process, commission_year: u32) -> AssetRef {
        Asset::new_candidate(
            Arc::new(process.clone()),
            "GBR".into(),
            Capacity(1.0),
            commission_year,
        )
        .unwrap()
        .into()
    }

    #[rstest]
    fn selection_from_optimisation_rekeys_activity(process: Process, time_slice: TimeSliceID) {
        let selected_candidate = candidate(&process, 2015);
        let unselected_candidate = candidate(&process, 2016);
        let mut selected_asset = selected_candidate.clone();
        selected_asset
            .make_mut()
            .select_candidate_for_investment(agent_id());
        assert_ne!(selected_candidate, selected_asset);

        let activity = indexmap! {
            (selected_candidate.clone(), time_slice.clone()) => Activity(1.0),
            (unselected_candidate, time_slice.clone()) => Activity(2.0),
        };
        let selection = Selection::from_optimisation(
            vec![(selected_candidate, selected_asset.clone())],
            &activity,
        );

        assert_eq!(selection.assets, [selected_asset.clone()]);
        assert_eq!(
            selection.activity,
            indexmap! { (selected_asset, time_slice) => Activity(1.0) }
        );
    }

    #[rstest]
    fn selection_commission_rekeys_activity_to_commissioned_assets(
        process: Process,
        time_slice: TimeSliceID,
    ) {
        let mut pool = AssetPool::new();

        // A retained asset, taken from the pool as at the start of a milestone year
        let mut retained = candidate(&process, 2015);
        retained
            .make_mut()
            .select_candidate_for_investment(agent_id());
        let retained = pool.extend([retained])[0].clone();
        pool.take();
        assert!(retained.id().is_some());
        let retained_id = retained.id();

        let mut ready = candidate(&process, 2020);
        ready.make_mut().select_candidate_for_investment(agent_id());
        let mut selection = Selection {
            assets: vec![ready.clone(), retained.clone()],
            activity: indexmap! {
                (ready, time_slice.clone()) => Activity(1.0),
                (retained.clone(), time_slice.clone()) => Activity(2.0),
            },
        };

        let new_assets = selection.commission(&mut pool).to_vec();

        assert_eq!(new_assets.len(), 1);
        assert!(new_assets[0].id().is_some());
        assert_eq!(selection.assets, [new_assets[0].clone(), retained.clone()]);
        assert_eq!(selection.assets[1].id(), retained_id);
        assert_eq!(
            selection.activity,
            indexmap! {
                (new_assets[0].clone(), time_slice.clone()) => Activity(1.0),
                (retained, time_slice) => Activity(2.0),
            }
        );
    }

    #[rstest]
    fn selection_retain_filters_assets_and_activity(process: Process, time_slice: TimeSliceID) {
        let asset1 = candidate(&process, 2015);
        let asset2 = candidate(&process, 2016);
        let mut selection = Selection {
            assets: vec![asset1.clone(), asset2.clone()],
            activity: indexmap! {
                (asset1.clone(), time_slice.clone()) => Activity(1.0),
                (asset2.clone(), time_slice.clone()) => Activity(2.0),
            },
        };

        selection.retain(|asset| *asset == asset2);

        assert_eq!(selection.assets, std::slice::from_ref(&asset2));
        assert_eq!(
            selection.activity,
            indexmap! { (asset2, time_slice) => Activity(2.0) }
        );
    }

    #[rstest]
    fn selection_flow_map_multiplies_activity_by_coeff(
        mut process: Process,
        svd_commodity: Commodity,
        time_slice: TimeSliceID,
    ) {
        let commodity = Arc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Arc::clone(&commodity),
            coeff: FlowPerActivity(2.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        process.flows = process_flows_map(
            process.regions.clone(),
            Arc::new(indexmap! { commodity.id.clone() => process_flow }),
        );
        let asset = candidate(&process, 2015);
        let selection = Selection {
            assets: vec![asset.clone()],
            activity: indexmap! { (asset.clone(), time_slice.clone()) => Activity(3.0) },
        };

        assert_eq!(
            selection.flow_map(),
            indexmap! { (asset, commodity.id.clone(), time_slice) => Flow(6.0) }
        );
    }
}
