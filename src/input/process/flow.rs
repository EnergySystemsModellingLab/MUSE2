//! Code for reading process flows file
use super::super::{input_err_msg, read_csv};
use crate::commodity::{CommodityID, CommodityMap};
use crate::process::{FlowType, ProcessFlow, ProcessFlowsMap, ProcessID, ProcessMap};
use crate::region::{RegionID, parse_region_str};
use crate::units::{FlowPerActivity, MoneyPerFlow};
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use indexmap::IndexMap;
use itertools::iproduct;
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

const PROCESS_FLOWS_FILE_NAME: &str = "process_flows.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct ProcessFlowRaw {
    process_id: String,
    commodity_id: String,
    commission_years: String,
    regions: String,
    coeff: FlowPerActivity,
    #[serde(default)]
    #[serde(rename = "type")]
    kind: FlowType,
    cost: Option<MoneyPerFlow>,
}

impl ProcessFlowRaw {
    fn validate(&self) -> Result<()> {
        // Check that flow is not infinity, nan, 0 etc.
        ensure!(
            self.coeff.is_normal(),
            "Invalid value for coeff ({})",
            self.coeff
        );

        // **TODO**: https://github.com/EnergySystemsModellingLab/MUSE2/issues/300
        ensure!(
            self.kind == FlowType::Fixed,
            "Commodity flexible assets are not currently supported"
        );

        // Check that flow cost is non-negative
        if let Some(cost) = self.cost {
            ensure!(
                (cost.value() >= 0.0),
                "Invalid value for flow cost ({cost}). Must be >=0."
            );
        }

        Ok(())
    }
}

/// Read process flows from a CSV file
pub fn read_process_flows(
    model_dir: &Path,
    processes: &mut ProcessMap,
    commodities: &CommodityMap,
) -> Result<HashMap<ProcessID, ProcessFlowsMap>> {
    let file_path = model_dir.join(PROCESS_FLOWS_FILE_NAME);
    let process_flow_csv = read_csv(&file_path)?;
    read_process_flows_from_iter(process_flow_csv, processes, commodities)
        .with_context(|| input_err_msg(&file_path))
}

/// Read '`ProcessFlowRaw`' records from an iterator and convert them into '`ProcessFlow`' records.
fn read_process_flows_from_iter<I>(
    iter: I,
    processes: &mut ProcessMap,
    commodities: &CommodityMap,
) -> Result<HashMap<ProcessID, ProcessFlowsMap>>
where
    I: Iterator<Item = ProcessFlowRaw>,
{
    let mut flows_map: HashMap<ProcessID, ProcessFlowsMap> = HashMap::new();
    for record in iter {
        record.validate()?;

        // Get process
        let (id, process) = processes
            .get_key_value(record.process_id.as_str())
            .with_context(|| format!("Process {} not found", record.process_id))?;

        // Get regions
        let process_regions = &process.regions;
        let record_regions =
            parse_region_str(&record.regions, process_regions).with_context(|| {
                format!("Invalid region for process {id}. Valid regions are {process_regions:?}")
            })?;

        // Get years
        let process_years = &process.years;
        let record_years =
            parse_year_str(&record.commission_years, process_years).with_context(|| {
                format!("Invalid year for process {id}. Valid years are {process_years:?}")
            })?;

        // Get commodity
        let commodity = commodities
            .get(record.commodity_id.as_str())
            .with_context(|| format!("{} is not a valid commodity ID", &record.commodity_id))?;

        // Create ProcessFlow object
        let process_flow = ProcessFlow {
            commodity: Rc::clone(commodity),
            coeff: record.coeff,
            kind: FlowType::Fixed,
            cost: record.cost.unwrap_or(MoneyPerFlow(0.0)),
        };

        // Insert flow into the map
        let region_year_map = flows_map.entry(id.clone()).or_default();
        for (year, region_id) in iproduct!(record_years, record_regions.iter()) {
            let flows_map = region_year_map
                .entry((region_id.clone(), year))
                .or_default();
            let existing = Rc::get_mut(flows_map)
                .unwrap() // safe: there will only be one copy
                .insert(commodity.id.clone(), process_flow.clone())
                .is_some();
            ensure!(
                !existing,
                "Duplicate process flow entry for region {}, year {} and commodity {}",
                region_id,
                year,
                commodity.id
            );
        }
    }

    validate_flows_and_update_primary_output(processes, &flows_map)?;
    validate_secondary_io_flows(processes, &flows_map)?;

    Ok(flows_map)
}

fn validate_flows_and_update_primary_output(
    processes: &mut ProcessMap,
    flows_map: &HashMap<ProcessID, ProcessFlowsMap>,
) -> Result<()> {
    for (process_id, process) in processes.iter_mut() {
        let map = flows_map
            .get(process_id)
            .with_context(|| format!("Missing flows map for process {process_id}"))?;

        ensure!(
            map.len() == process.years.len() * process.regions.len(),
            "Flows map for process {process_id} does not cover all regions and years"
        );

        let mut iter = iproduct!(process.years.iter(), process.regions.iter());

        let primary_output = if let Some(primary_output) = &process.primary_output {
            Some(primary_output.clone())
        } else {
            let (year, region_id) = iter.next().unwrap();
            infer_primary_output(&map[&(region_id.clone(), *year)]).with_context(|| {
                format!("Could not infer primary_output for process {process_id}")
            })?
        };

        for (&year, region_id) in iter {
            let flows = &map[&(region_id.clone(), year)];

            // Check that the process has flows for this region/year
            check_flows_primary_output(flows, primary_output.as_ref()).with_context(|| {
                format!(
                    "Invalid primary output configuration for process {process_id} \
                    (region: {region_id}, year: {year})"
                )
            })?;
        }

        // Update primary output if needed
        if process.primary_output != primary_output {
            // Safe: There should only be one ref to process
            Rc::get_mut(process).unwrap().primary_output = primary_output;
        }
    }

    Ok(())
}

/// Infer the primary output.
///
/// This is only possible if there is only one output flow for the process.
fn infer_primary_output(map: &IndexMap<CommodityID, ProcessFlow>) -> Result<Option<CommodityID>> {
    let mut iter = map
        .iter()
        .filter_map(|(commodity_id, flow)| flow.is_output().then_some(commodity_id));

    let Some(first_output) = iter.next() else {
        // If there are only input flows, then the primary output should be None
        return Ok(None);
    };

    ensure!(
        iter.next().is_none(),
        "Need to specify primary_output explicitly if there are multiple output flows"
    );

    Ok(Some(first_output.clone()))
}

/// Check the flows are correct for the specified primary output (or lack thereof)
fn check_flows_primary_output(
    flows_map: &IndexMap<CommodityID, ProcessFlow>,
    primary_output: Option<&CommodityID>,
) -> Result<()> {
    if let Some(primary_output) = primary_output {
        let flow = flows_map.get(primary_output).with_context(|| {
            format!("Primary output commodity '{primary_output}' isn't a process flow")
        })?;

        ensure!(
            flow.is_output(),
            "Primary output commodity '{primary_output}' isn't an output flow",
        );
    } else {
        ensure!(
            flows_map.values().all(ProcessFlow::is_input),
            "First year is only inputs, but subsequent years have outputs, although no primary \
            output is specified"
        );
    }

    Ok(())
}

/// Checks that non-primary io are defined for all years (within a region) and that
/// they are only inputs or only outputs in all years.
fn validate_secondary_io_flows(
    processes: &mut ProcessMap,
    flows_map: &HashMap<ProcessID, ProcessFlowsMap>,
) -> Result<()> {
    for (process_id, process) in processes.iter() {
        // Get the flows for this process - there should be no error, as was checked already
        let map = flows_map
            .get(process_id)
            .with_context(|| format!("Missing flows map for process {process_id}"))?;

        // Get the non-primary io flows for all years, if any, arranged by (commodity, region)
        let iter = iproduct!(process.years.iter(), process.regions.iter());
        let mut flows: HashMap<(CommodityID, RegionID), Vec<bool>> = HashMap::new();
        for (&year, region_id) in iter {
            let flow = map[&(region_id.clone(), year)]
                .iter()
                .filter_map(|(commodity_id, flow)| {
                    (Some(commodity_id) != process.primary_output.as_ref())
                        .then_some(((commodity_id.clone(), region_id.clone()), flow.is_input()))
                });

            for (key, value) in flow {
                flows.entry(key).or_insert_with(Vec::new).push(value);
            }
        }

        // Finally we check that the flows for a given commodity and region are defined for all
        // years and that they are all inputs or all outputs
        for ((commodity_id, region_id), value) in flows.iter() {
            ensure!(
                value.len() == process.years.len(),
                "Flow of commodity {commodity_id} in region {region_id} for process {process_id} \
                does not cover all years"
            );
            ensure!(
                value.iter().all(|&x| x == value[0]),
                "Flow of commodity {commodity_id} in region {region_id} for process {process_id} \
                behaves as input or output in different years."
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{assert_error, process, sed_commodity, svd_commodity};
    use crate::process::{FlowType, Process, ProcessFlow, ProcessMap};
    use crate::units::{FlowPerActivity, MoneyPerFlow};
    use indexmap::IndexMap;
    use itertools::Itertools;
    use map_macro::hash_map;
    use rstest::rstest;
    use std::iter;
    use std::rc::Rc;

    fn flow(commodity: Rc<Commodity>, coeff: f64) -> ProcessFlow {
        ProcessFlow {
            commodity,
            coeff: FlowPerActivity(coeff),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        }
    }

    fn build_maps<I>(
        process: Process,
        flows: I,
    ) -> (ProcessMap, HashMap<ProcessID, ProcessFlowsMap>)
    where
        I: Clone + Iterator<Item = (CommodityID, ProcessFlow)>,
    {
        let map: Rc<IndexMap<_, _>> = Rc::new(flows.clone().collect());
        let flows_inner = iproduct!(&process.regions, &process.years)
            .map(|(region_id, year)| ((region_id.clone(), *year), map.clone()))
            .collect();
        let flows = hash_map! {process.id.clone() => flows_inner};
        let processes = iter::once((process.id.clone(), process.into())).collect();

        (processes, flows)
    }

    #[rstest]
    fn single_output_infer_primary(#[from(svd_commodity)] commodity: Commodity, process: Process) {
        let commodity = Rc::new(commodity);
        let (mut processes, flows_map) = build_maps(
            process,
            std::iter::once((commodity.id.clone(), flow(commodity.clone(), 1.0))),
        );
        assert!(validate_flows_and_update_primary_output(&mut processes, &flows_map).is_ok());
        assert_eq!(
            processes.values().exactly_one().unwrap().primary_output,
            Some(commodity.id.clone())
        );
    }

    #[rstest]
    fn multiple_outputs_error(
        #[from(svd_commodity)] commodity1: Commodity,
        #[from(sed_commodity)] commodity2: Commodity,
        process: Process,
    ) {
        let commodity1 = Rc::new(commodity1);
        let commodity2 = Rc::new(commodity2);
        let (mut processes, flows_map) = build_maps(
            process,
            [
                (commodity1.id.clone(), flow(commodity1.clone(), 1.0)),
                (commodity2.id.clone(), flow(commodity2.clone(), 2.0)),
            ]
            .into_iter(),
        );
        let res = validate_flows_and_update_primary_output(&mut processes, &flows_map);
        assert_error!(res, "Could not infer primary_output for process process1");
    }

    #[rstest]
    fn explicit_primary_output(
        #[from(svd_commodity)] commodity1: Commodity,
        #[from(sed_commodity)] commodity2: Commodity,
        process: Process,
    ) {
        let commodity1 = Rc::new(commodity1);
        let commodity2 = Rc::new(commodity2);
        let mut process = process;
        process.primary_output = Some(commodity2.id.clone());
        let (mut processes, flows_map) = build_maps(
            process,
            [
                (commodity1.id.clone(), flow(commodity1.clone(), 1.0)),
                (commodity2.id.clone(), flow(commodity2.clone(), 2.0)),
            ]
            .into_iter(),
        );
        assert!(validate_flows_and_update_primary_output(&mut processes, &flows_map).is_ok());
        assert_eq!(
            processes.values().exactly_one().unwrap().primary_output,
            Some(commodity2.id.clone())
        );
    }

    #[rstest]
    fn all_inputs_no_primary(
        #[from(svd_commodity)] commodity1: Commodity,
        #[from(sed_commodity)] commodity2: Commodity,
        process: Process,
    ) {
        let commodity1 = Rc::new(commodity1);
        let commodity2 = Rc::new(commodity2);
        let (mut processes, flows_map) = build_maps(
            process,
            [
                (commodity1.id.clone(), flow(commodity1.clone(), -1.0)),
                (commodity2.id.clone(), flow(commodity2.clone(), -2.0)),
            ]
            .into_iter(),
        );
        assert!(validate_flows_and_update_primary_output(&mut processes, &flows_map).is_ok());
        assert_eq!(
            processes.values().exactly_one().unwrap().primary_output,
            None
        );
    }
}
