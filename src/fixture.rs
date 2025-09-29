//! Fixtures for tests

use crate::agent::{
    Agent, AgentCommodityPortionsMap, AgentCostLimitsMap, AgentID, AgentMap, AgentObjectiveMap,
    AgentSearchSpaceMap, DecisionRule,
};
use crate::asset::{Asset, AssetPool};
use crate::commodity::{Commodity, CommodityID, CommodityLevyMap, CommodityType, DemandMap};
use crate::process::{
    Process, ProcessActivityLimitsMap, ProcessFlowsMap, ProcessMap, ProcessParameter,
    ProcessParameterMap,
};
use crate::region::RegionID;
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceLevel};
use crate::units::{
    ActivityPerCapacity, Capacity, Dimensionless, MoneyPerActivity, MoneyPerCapacity,
    MoneyPerCapacityPerYear, Year,
};
use indexmap::IndexSet;
use indexmap::indexmap;
use itertools::Itertools;
use rstest::fixture;
use std::collections::HashMap;
use std::iter;
use std::rc::Rc;

/// Assert that an error with the given message occurs
macro_rules! assert_error {
    ($result:expr, $msg:expr) => {
        assert_eq!(
            $result.unwrap_err().chain().next().unwrap().to_string(),
            $msg
        );
    };
}
pub(crate) use assert_error;

#[fixture]
pub fn region_id() -> RegionID {
    "GBR".into()
}

#[fixture]
pub fn commodity_ids() -> IndexSet<CommodityID> {
    IndexSet::from(["commodity1".into()])
}

#[fixture]
pub fn region_ids() -> IndexSet<RegionID> {
    ["GBR".into(), "USA".into()].into_iter().collect()
}

#[fixture]
pub fn agent_id() -> AgentID {
    "agent1".into()
}

#[fixture]
pub fn commodity_id() -> CommodityID {
    "commodity1".into()
}

#[fixture]
pub fn svd_commodity() -> Commodity {
    Commodity {
        id: "commodity1".into(),
        description: "".into(),
        kind: CommodityType::ServiceDemand,
        time_slice_level: TimeSliceLevel::DayNight,
        levies: CommodityLevyMap::new(),
        demand: DemandMap::new(),
    }
}

#[fixture]
pub fn sed_commodity() -> Commodity {
    Commodity {
        id: "sed_commodity".into(),
        description: "Test SED commodity".into(),
        kind: CommodityType::SupplyEqualsDemand,
        time_slice_level: TimeSliceLevel::DayNight,
        levies: CommodityLevyMap::new(),
        demand: DemandMap::new(),
    }
}

#[fixture]
pub fn other_commodity() -> Commodity {
    Commodity {
        id: "other_commodity".into(),
        description: "Test other commodity".into(),
        kind: CommodityType::Other,
        time_slice_level: TimeSliceLevel::DayNight,
        levies: CommodityLevyMap::new(),
        demand: DemandMap::new(),
    }
}

pub fn get_svd_map(commodity: &Commodity) -> HashMap<CommodityID, &Commodity> {
    iter::once((commodity.id.clone(), commodity)).collect()
}

#[fixture]
pub fn asset(process: Process) -> Asset {
    let region_id: RegionID = "GBR".into();
    let agent_id = "agent1".into();
    let commission_year = 2015;
    Asset::new_future(
        agent_id,
        process.into(),
        region_id,
        Capacity(2.0),
        commission_year,
    )
    .unwrap()
}

#[fixture]
pub fn assets(asset: Asset) -> AssetPool {
    let year = asset.commission_year();
    let mut assets = AssetPool::new(iter::once(asset).collect());
    assets.update_for_year(year);
    assets
}

#[fixture]
pub fn process_parameter_map(region_ids: IndexSet<RegionID>) -> ProcessParameterMap {
    let parameter = Rc::new(ProcessParameter {
        capital_cost: MoneyPerCapacity(0.0),
        fixed_operating_cost: MoneyPerCapacityPerYear(0.0),
        variable_operating_cost: MoneyPerActivity(0.0),
        lifetime: 1,
        discount_rate: Dimensionless(1.0),
    });

    region_ids
        .into_iter()
        .cartesian_product(2010..=2020)
        .map(|(region_id, year)| ((region_id, year), parameter.clone()))
        .collect()
}

#[fixture]
pub fn process(
    region_ids: IndexSet<RegionID>,
    process_parameter_map: ProcessParameterMap,
) -> Process {
    Process {
        id: "process1".into(),
        description: "Description".into(),
        years: (2010..=2020).collect(),
        activity_limits: ProcessActivityLimitsMap::new(),
        flows: ProcessFlowsMap::new(),
        parameters: process_parameter_map,
        regions: region_ids,
        primary_output: None,
        capacity_to_activity: ActivityPerCapacity(1.0),
    }
}

#[fixture]
pub fn processes(process: Process) -> ProcessMap {
    indexmap! { process.id.clone() => process.into()}
}

#[fixture]
pub fn agents() -> AgentMap {
    iter::once((
        "agent1".into(),
        Agent {
            id: "agent1".into(),
            description: "".into(),
            commodity_portions: AgentCommodityPortionsMap::new(),
            search_space: AgentSearchSpaceMap::new(),
            decision_rule: DecisionRule::Single,
            cost_limits: AgentCostLimitsMap::new(),
            regions: IndexSet::new(),
            objectives: AgentObjectiveMap::new(),
        },
    ))
    .collect()
}

#[fixture]
pub fn time_slice() -> TimeSliceID {
    TimeSliceID {
        season: "winter".into(),
        time_of_day: "day".into(),
    }
}

#[fixture]
pub fn time_slice_info() -> TimeSliceInfo {
    TimeSliceInfo {
        times_of_day: iter::once("day".into()).collect(),
        seasons: iter::once(("winter".into(), Year(1.0))).collect(),
        time_slices: [(
            TimeSliceID {
                season: "winter".into(),
                time_of_day: "day".into(),
            },
            Year(1.0),
        )]
        .into_iter()
        .collect(),
    }
}

#[fixture]
pub fn time_slice_info2() -> TimeSliceInfo {
    TimeSliceInfo {
        times_of_day: ["day".into(), "night".into()].into_iter().collect(),
        seasons: iter::once(("winter".into(), Year(1.0))).collect(),
        time_slices: [
            (
                TimeSliceID {
                    season: "winter".into(),
                    time_of_day: "day".into(),
                },
                Year(0.5),
            ),
            (
                TimeSliceID {
                    season: "winter".into(),
                    time_of_day: "night".into(),
                },
                Year(0.5),
            ),
        ]
        .into_iter()
        .collect(),
    }
}
