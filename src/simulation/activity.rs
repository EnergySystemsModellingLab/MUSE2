//! Types representing the activities of assets and the commodity flows derived from them.
use crate::asset::AssetRef;
use crate::commodity::CommodityID;
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Flow};
use indexmap::IndexMap;

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
