//! Code for reading commodity-related data from CSV files.
use super::{input_err_msg, read_csv};
use crate::ISSUES_URL;
use crate::commodity::{
    BalanceType, Commodity, CommodityID, CommodityLevyMap, CommodityMap, CommodityType, DemandMap,
    PricingStrategy,
};
use crate::model::{ALLOW_BROKEN_OPTION_NAME, broken_model_options_allowed};
use crate::region::RegionID;
use crate::time_slice::{TimeSliceInfo, TimeSliceLevel};
use anyhow::{Context, Ok, Result, ensure};
use indexmap::{IndexMap, IndexSet};
use log::warn;
use serde::Deserialize;
use std::path::Path;

mod levy;
use levy::read_commodity_levies;
mod demand;
use demand::read_demand;
mod demand_slicing;

const COMMODITY_FILE_NAME: &str = "commodities.csv";

#[derive(PartialEq, Debug, Deserialize)]
struct CommodityRaw {
    pub id: CommodityID,
    pub description: String,
    #[serde(rename = "type")] // NB: we can't name a field type as it's a reserved keyword
    pub kind: CommodityType,
    pub time_slice_level: TimeSliceLevel,
    pub pricing_strategy: Option<PricingStrategy>,
    pub units: String,
}

/// Read commodity data from the specified model directory.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `region_ids` - All possible region IDs
/// * `time_slice_info` - Information about time slices
/// * `milestone_years` - All milestone years
///
/// # Returns
///
/// An `IndexMap` mapping `CommodityID` to `Commodity`, or an error.
pub fn read_commodities(
    model_dir: &Path,
    region_ids: &IndexSet<RegionID>,
    time_slice_info: &TimeSliceInfo,
    milestone_years: &[u32],
) -> Result<CommodityMap> {
    // Read commodities table
    let commodities = read_commodities_file(model_dir)?;
    let commodity_ids = commodities.keys().cloned().collect();

    // Read costs table
    let mut costs = read_commodity_levies(
        model_dir,
        &commodity_ids,
        region_ids,
        time_slice_info,
        milestone_years,
    )?;

    // Read demand table
    let mut demand = read_demand(
        model_dir,
        &commodities,
        region_ids,
        time_slice_info,
        milestone_years,
    )?;

    // Populate maps for each Commodity
    Ok(commodities
        .into_iter()
        .map(|(id, mut commodity)| {
            if let Some(mut costs) = costs.remove(&id) {
                if let Some(levies) = costs.remove(&BalanceType::Consumption) {
                    commodity.levies_cons = levies;
                }
                if let Some(levies) = costs.remove(&BalanceType::Production) {
                    commodity.levies_prod = levies;
                }
            }
            if let Some(demand) = demand.remove(&id) {
                commodity.demand = demand;
            }

            (id, commodity.into())
        })
        .collect())
}

fn read_commodities_file(model_dir: &Path) -> Result<IndexMap<CommodityID, Commodity>> {
    let file_path = model_dir.join(COMMODITY_FILE_NAME);
    let commodities_csv = read_csv(&file_path)?;
    read_commodities_file_from_iter(commodities_csv).with_context(|| input_err_msg(&file_path))
}

fn read_commodities_file_from_iter<I>(iter: I) -> Result<IndexMap<CommodityID, Commodity>>
where
    I: Iterator<Item = CommodityRaw>,
{
    let mut commodities = IndexMap::new();
    for commodity_raw in iter {
        let pricing_strategy = match commodity_raw.pricing_strategy {
            Some(strategy) => strategy,
            None => default_pricing_strategy(&commodity_raw.kind),
        };

        let commodity = Commodity {
            id: commodity_raw.id.clone(),
            description: commodity_raw.description,
            kind: commodity_raw.kind,
            time_slice_level: commodity_raw.time_slice_level,
            pricing_strategy,
            levies_prod: CommodityLevyMap::default(),
            levies_cons: CommodityLevyMap::default(),
            demand: DemandMap::default(),
            units: commodity_raw.units,
        };

        validate_commodity(&commodity)?;

        ensure!(
            commodities.insert(commodity_raw.id, commodity).is_none(),
            "Duplicate commodity ID"
        );
    }

    Ok(commodities)
}

/// Get the default pricing strategy for a given commodity kind.
fn default_pricing_strategy(commodity_kind: &CommodityType) -> PricingStrategy {
    match commodity_kind {
        CommodityType::Other => PricingStrategy::Unpriced,
        CommodityType::SupplyEqualsDemand | CommodityType::ServiceDemand => PricingStrategy::Shadow,
    }
}

fn validate_commodity(commodity: &Commodity) -> Result<()> {
    // Check that the pricing strategy is appropriate for the commodity type
    match commodity.kind {
        CommodityType::Other => {
            ensure!(
                commodity.pricing_strategy == PricingStrategy::Unpriced,
                "Commodity {} of type Other must be unpriced. \
                    Update its pricing strategy to 'unpriced' or 'default'.",
                commodity.id
            );
        }
        CommodityType::SupplyEqualsDemand | CommodityType::ServiceDemand => {
            ensure!(
                commodity.pricing_strategy != PricingStrategy::Unpriced,
                "Commodity {} of type {:?} cannot be unpriced. \
                    Update its pricing strategy to a valid option.",
                commodity.id,
                commodity.kind
            );
        }
    }

    // Gatekeep alternative pricing options
    if !matches!(
        commodity.pricing_strategy,
        PricingStrategy::Shadow | PricingStrategy::Unpriced
    ) {
        ensure!(
            broken_model_options_allowed(),
            "Price strategies other than 'shadow' and 'unpriced' are currently experimental. \
            To run anyway, set the {ALLOW_BROKEN_OPTION_NAME} option to true."
        );
    }
    if commodity.pricing_strategy == PricingStrategy::ScarcityAdjusted {
        warn!(
            "The pricing strategy for {} is set to 'scarcity'. Commodity prices may be \
            incorrect if assets have more than one output commodity. See: {ISSUES_URL}/677",
            commodity.id
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use crate::time_slice::TimeSliceLevel;

    fn make_commodity(kind: CommodityType, pricing_strategy: PricingStrategy) -> Commodity {
        Commodity {
            id: "ELC".into(),
            description: "test".into(),
            kind,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy,
            levies_prod: CommodityLevyMap::default(),
            levies_cons: CommodityLevyMap::default(),
            demand: DemandMap::default(),
            units: "kWh".into(),
        }
    }

    #[test]
    fn validate_commodity_works() {
        let commodity = make_commodity(CommodityType::SupplyEqualsDemand, PricingStrategy::Shadow);
        validate_commodity(&commodity).unwrap();
    }

    #[test]
    fn validate_commodity_other_priced() {
        let commodity = make_commodity(CommodityType::Other, PricingStrategy::MarginalCost);
        assert_error!(
            validate_commodity(&commodity),
            "Commodity ELC of type Other must be unpriced. Update its pricing strategy to 'unpriced' or 'default'."
        );
    }

    #[test]
    fn validate_commodity_sed_unpriced() {
        let commodity =
            make_commodity(CommodityType::SupplyEqualsDemand, PricingStrategy::Unpriced);
        assert_error!(
            validate_commodity(&commodity),
            "Commodity ELC of type SupplyEqualsDemand cannot be unpriced. Update its pricing strategy to a valid option."
        );
    }
}
