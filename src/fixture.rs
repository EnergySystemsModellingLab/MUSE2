//! Fixtures for tests

use crate::agent::{
    Agent, AgentCommodityPortionsMap, AgentID, AgentMap, AgentObjectiveMap, AgentSearchSpaceMap,
    DecisionRule,
};
use crate::asset::{Asset, AssetCapacity, AssetPool, AssetRef};
use crate::commodity::{
    Commodity, CommodityID, CommodityLevyMap, CommodityType, DemandMap, PricingStrategy,
};
use crate::patch::{FilePatch, ModelPatch};
use crate::process::{
    ActivityLimits, Process, ProcessActivityLimitsMap, ProcessFlow, ProcessFlowsMap,
    ProcessInvestmentConstraintsMap, ProcessMap, ProcessParameter, ProcessParameterMap,
};
use crate::region::RegionID;
use crate::simulation::investment::appraisal::LCOXMetric;
use crate::simulation::investment::appraisal::{
    AppraisalOutput, MetricTrait, coefficients::ObjectiveCoefficients,
};
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceLevel};
use crate::units::{
    Activity, ActivityPerCapacity, Capacity, Dimensionless, Flow, MoneyPerActivity,
    MoneyPerCapacity, MoneyPerCapacityPerYear, MoneyPerFlow, Year,
};
use anyhow::Result;
use indexmap::indexmap;
use indexmap::{IndexMap, IndexSet};
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

/// Build a patched copy of `examples/simple` to a temporary directory and return the `TempDir`.
///
/// If the patched model cannot be built, for whatever reason, this function will panic.
pub(crate) fn build_patched_simple_tempdir(file_patches: Vec<FilePatch>) -> tempfile::TempDir {
    ModelPatch::from_example("simple")
        .with_file_patches(file_patches)
        .build_to_tempdir()
        .unwrap()
}

/// Check whether the simple example passes or fails validation after applying file patches
macro_rules! patch_and_validate_simple {
    ($file_patches:expr) => {{
        (|| -> Result<()> {
            let tmp = crate::fixture::build_patched_simple_tempdir($file_patches);
            crate::input::load_model(tmp.path())?;
            Ok(())
        })()
    }};
}
pub(crate) use patch_and_validate_simple;

/// Check whether validation succeeds for simple example with patches
macro_rules! assert_validate_ok_simple {
    ($file_patches:expr) => {
        assert!(crate::fixture::patch_and_validate_simple!($file_patches).is_ok())
    };
}
pub(crate) use assert_validate_ok_simple;

/// Check whether validation fails with specific message
macro_rules! assert_validate_fails_with_simple {
    ($file_patches:expr, $expected_msg:expr) => {
        let result = crate::fixture::patch_and_validate_simple!($file_patches);
        let msg = result
            .unwrap_err()
            .chain()
            .skip(1)
            .next()
            .unwrap()
            .to_string();

        assert_eq!(msg, $expected_msg);
    };
}
pub(crate) use assert_validate_fails_with_simple;

/// Check whether the simple example runs successfully after applying file patches
macro_rules! patch_and_run_simple {
    ($file_patches:expr) => {{
        (|| -> Result<()> {
            let tmp = crate::fixture::build_patched_simple_tempdir($file_patches);
            let (model, assets) = crate::input::load_model(tmp.path())?;
            let output_path = tmp.path().join("output");
            std::fs::create_dir_all(&output_path)?;

            crate::simulation::run(&model, assets, &output_path, false)?;
            Ok(())
        })()
    }};
}
pub(crate) use patch_and_run_simple;

/// Check whether the simple example runs successfully after applying file patches
macro_rules! assert_patched_runs_ok_simple {
    ($file_patches:expr) => {
        assert!(crate::fixture::patch_and_run_simple!($file_patches).is_ok())
    };
}
pub(crate) use assert_patched_runs_ok_simple;

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
        description: String::new(),
        kind: CommodityType::ServiceDemand,
        time_slice_level: TimeSliceLevel::DayNight,
        pricing_strategy: PricingStrategy::Shadow,
        levies_prod: CommodityLevyMap::new(),
        levies_cons: CommodityLevyMap::new(),
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
        pricing_strategy: PricingStrategy::Shadow,
        levies_prod: CommodityLevyMap::new(),
        levies_cons: CommodityLevyMap::new(),
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
        pricing_strategy: PricingStrategy::Shadow,
        levies_prod: CommodityLevyMap::new(),
        levies_cons: CommodityLevyMap::new(),
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
pub fn process_parameter() -> ProcessParameter {
    ProcessParameter {
        capital_cost: MoneyPerCapacity(0.0),
        fixed_operating_cost: MoneyPerCapacityPerYear(0.0),
        variable_operating_cost: MoneyPerActivity(0.0),
        lifetime: 5,
        discount_rate: Dimensionless(1.0),
    }
}

#[fixture]
/// Create a `ProcessParameterMap` with the specified parameters for each region and year
pub fn process_parameter_map(
    region_ids: IndexSet<RegionID>,
    process_parameter: ProcessParameter,
) -> ProcessParameterMap {
    let parameter = Rc::new(process_parameter);
    region_ids
        .into_iter()
        .cartesian_product(2010..=2020)
        .map(|(region_id, year)| ((region_id, year), parameter.clone()))
        .collect()
}

#[fixture]
/// Create a `ProcessAvailabilities` with full availability for all time slices
pub fn process_activity_limits(time_slice_info: TimeSliceInfo) -> ActivityLimits {
    ActivityLimits::new_with_full_availability(&time_slice_info)
}

#[fixture]
/// Create a `ProcessActivityLimitsMap` with full availability for each region and year
pub fn process_activity_limits_map(
    region_ids: IndexSet<RegionID>,
    process_activity_limits: ActivityLimits,
) -> ProcessActivityLimitsMap {
    region_ids
        .into_iter()
        .cartesian_product(2010..=2020)
        .map(|(region_id, year)| ((region_id, year), Rc::new(process_activity_limits.clone())))
        .collect()
}

#[fixture]
/// Create an empty set of `ProcessInvestmentConstraints` for a given region/year
/// Returns a `HashMap` keyed by (`RegionID`, year) with empty Rc<ProcessInvestmentConstraint>
pub fn process_investment_constraints() -> ProcessInvestmentConstraintsMap {
    HashMap::new()
}

#[fixture]
/// Create an empty set of `ProcessFlows` for a given region/year
pub fn process_flows() -> Rc<IndexMap<CommodityID, ProcessFlow>> {
    Rc::new(IndexMap::new())
}

#[fixture]
/// Create a `ProcessFlowsMap` with the provided flows for each region/year
pub fn process_flows_map(
    region_ids: IndexSet<RegionID>,
    process_flows: Rc<IndexMap<CommodityID, ProcessFlow>>,
) -> ProcessFlowsMap {
    region_ids
        .into_iter()
        .cartesian_product(2010..=2020)
        .map(|(region_id, year)| ((region_id, year), process_flows.clone()))
        .collect()
}

#[fixture]
/// Create a Process with the given components
pub fn process(
    region_ids: IndexSet<RegionID>,
    process_parameter_map: ProcessParameterMap,
    process_activity_limits_map: ProcessActivityLimitsMap,
    process_flows_map: ProcessFlowsMap,
    process_investment_constraints: ProcessInvestmentConstraintsMap,
) -> Process {
    Process {
        id: "process1".into(),
        description: "Description".into(),
        years: 2010..=2020,
        activity_limits: process_activity_limits_map,
        flows: process_flows_map,
        parameters: process_parameter_map,
        regions: region_ids,
        primary_output: None,
        capacity_to_activity: ActivityPerCapacity(1.0),
        investment_constraints: process_investment_constraints,
        unit_size: None,
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
            description: String::new(),
            commodity_portions: AgentCommodityPortionsMap::new(),
            search_space: AgentSearchSpaceMap::new(),
            decision_rule: DecisionRule::Single,
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

#[fixture]
pub fn appraisal_output(asset: Asset, time_slice: TimeSliceID) -> AppraisalOutput {
    let activity_coefficients = indexmap! { time_slice.clone() => MoneyPerActivity(0.5) };
    let activity = indexmap! { time_slice.clone() => Activity(10.0) };
    let demand = indexmap! { time_slice.clone() => Flow(100.0) };
    let unmet_demand = indexmap! { time_slice.clone() => Flow(5.0) };
    AppraisalOutput {
        asset: AssetRef::from(asset),
        capacity: AssetCapacity::Continuous(Capacity(42.0)),
        coefficients: ObjectiveCoefficients {
            capacity_coefficient: MoneyPerCapacity(2.14),
            activity_coefficients,
            unmet_demand_coefficient: MoneyPerFlow(10000.0),
        },
        activity,
        demand,
        unmet_demand,
        metric: Box::new(LCOXMetric::new(MoneyPerActivity(4.14))),
    }
}

/// Creates appraisal outputs from assets with corresponding metrics. If no assets provided,
/// creates default candidate assets based on the number of metrics.
///
/// # Panics
/// Panics if `assets` and `metrics` have different lengths
#[fixture]
pub fn appraisal_outputs(
    #[default(vec![])] assets: Vec<Asset>,
    #[default(vec![])] metrics: Vec<Box<dyn MetricTrait>>,
    process: Process,
    region_id: RegionID,
) -> Vec<AppraisalOutput> {
    // If no assets provided, create default candidate assets based on number of metrics
    let assets = if assets.is_empty() {
        let process_rc = Rc::new(process);
        (0..metrics.len())
            .map(|i| {
                Asset::new_candidate(
                    // Use the same process with different reference count
                    Rc::clone(&process_rc),
                    region_id.clone(),
                    Capacity(10.0),
                    u32::try_from(2010 + i).unwrap(), // Different commission years
                )
                .unwrap()
            })
            .collect()
    } else {
        assets
    };

    assert_eq!(
        assets.len(),
        metrics.len(),
        "assets and metrics must have the same length"
    );

    assets
        .into_iter()
        .zip(metrics)
        .map(|(asset, metric)| AppraisalOutput {
            asset: AssetRef::from(asset),
            capacity: AssetCapacity::Continuous(Capacity(10.0)),
            coefficients: ObjectiveCoefficients {
                capacity_coefficient: MoneyPerCapacity(0.0),
                activity_coefficients: IndexMap::new(),
                unmet_demand_coefficient: MoneyPerFlow(0.0),
            },
            activity: IndexMap::new(),
            demand: IndexMap::new(),
            unmet_demand: IndexMap::new(),
            metric,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn patch_and_validate_simple_ok() {
        let patches = Vec::new();
        patch_and_validate_simple!(patches).unwrap();
    }

    #[test]
    fn patch_and_run_simple_ok() {
        let patches = Vec::new();
        patch_and_run_simple!(patches).unwrap();
    }

    #[test]
    fn patch_and_validate_simple_fail() {
        let patch = FilePatch::new("commodities.csv")
            .with_deletion("RSHEAT,Residential heating,svd,daynight");
        assert!(patch_and_validate_simple!(vec![patch]).is_err());
    }

    #[test]
    fn patch_and_run_simple_fail() {
        let patch = FilePatch::new("commodities.csv")
            .with_deletion("RSHEAT,Residential heating,svd,daynight");
        assert!(patch_and_run_simple!(vec![patch]).is_err());
    }
}
