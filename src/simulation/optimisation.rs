//! Code for performing dispatch optimisation.
//!
//! This is used to calculate commodity flows and prices.
use crate::asset::{Asset, AssetRef};
use crate::commodity::CommodityID;
use crate::input::format_items_with_cap;
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::CommodityPrices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Flow, Money, MoneyPerActivity, MoneyPerFlow, UnitType};
use anyhow::{Result, bail, ensure};
use highs::{HighsModelStatus, HighsStatus, RowProblem as Problem, Sense};
use indexmap::{IndexMap, IndexSet};
use itertools::{chain, iproduct};
use log::debug;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::ops::Range;

mod constraints;
use constraints::{ConstraintKeys, add_asset_constraints};

/// A map of commodity flows calculated during the optimisation
pub type FlowMap = IndexMap<(AssetRef, CommodityID, TimeSliceID), Flow>;

/// A decision variable in the optimisation
///
/// Note that this type does **not** include the value of the variable; it just refers to a
/// particular column of the problem.
type Variable = highs::Col;

/// The map of variables related to assets
type AssetVariableMap = IndexMap<(AssetRef, TimeSliceID), Variable>;

/// Variables representing unmet demand for a given commodity
type UnmetDemandVariableMap = IndexMap<(CommodityID, RegionID, TimeSliceID), Variable>;

/// A map for easy lookup of variables in the problem.
///
/// The entries are ordered (see [`IndexMap`]).
///
/// We use this data structure for two things:
///
/// 1. In order define constraints for the optimisation
/// 2. To keep track of the combination of parameters that each variable corresponds to, for when we
///    are reading the results of the optimisation.
pub struct VariableMap {
    asset_vars: AssetVariableMap,
    existing_asset_var_idx: Range<usize>,
    unmet_demand_vars: UnmetDemandVariableMap,
    unmet_demand_var_idx: Range<usize>,
}

impl VariableMap {
    /// Create a new [`VariableMap`] and add variables to the problem
    ///
    /// # Arguments
    ///
    /// * `problem` - The optimisation problem
    /// * `model` - The model
    /// * `input_prices` - Optional explicit prices for input commodities
    /// * `existing_assets` - The asset pool
    /// * `candidate_assets` - Candidate assets for inclusion in active pool
    /// * `year` - Current milestone year
    fn new_with_asset_vars(
        problem: &mut Problem,
        model: &Model,
        input_prices: Option<&CommodityPrices>,
        existing_assets: &[AssetRef],
        candidate_assets: &[AssetRef],
        year: u32,
    ) -> Self {
        let mut asset_vars = AssetVariableMap::new();
        let existing_asset_var_idx = add_asset_variables(
            problem,
            &mut asset_vars,
            &model.time_slice_info,
            input_prices,
            existing_assets,
            year,
        );
        add_asset_variables(
            problem,
            &mut asset_vars,
            &model.time_slice_info,
            input_prices,
            candidate_assets,
            year,
        );

        Self {
            asset_vars,
            existing_asset_var_idx,
            unmet_demand_vars: UnmetDemandVariableMap::default(),
            unmet_demand_var_idx: Range::default(),
        }
    }

    /// Add unmet demand variables to the map and the problem
    ///
    /// # Arguments
    ///
    /// * `problem` - The optimisation problem
    /// * `model` - The model
    /// * `commodities` - The subset of commodities the problem is being run for
    fn add_unmet_demand_variables(
        &mut self,
        problem: &mut Problem,
        model: &Model,
        commodities: &[CommodityID],
    ) {
        assert!(!commodities.is_empty());

        // This line **must** come before we add more variables
        let start = problem.num_cols();

        // Add variables
        let voll = model.parameters.value_of_lost_load;
        self.unmet_demand_vars.extend(
            iproduct!(
                commodities.iter(),
                model.iter_regions(),
                model.time_slice_info.iter_ids()
            )
            .map(|(commodity_id, region_id, time_slice)| {
                let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
                let var = problem.add_column(voll.value(), 0.0..);

                (key, var)
            }),
        );

        self.unmet_demand_var_idx = start..problem.num_cols();
    }

    /// Get the asset [`Variable`] corresponding to the given parameters.
    fn get_asset_var(&self, asset: &AssetRef, time_slice: &TimeSliceID) -> Variable {
        let key = (asset.clone(), time_slice.clone());

        *self
            .asset_vars
            .get(&key)
            .expect("No asset variable found for given params")
    }

    /// Get the unmet demand [`Variable`] corresponding to the given parameters.
    fn get_unmet_demand_var(
        &self,
        commodity_id: &CommodityID,
        region_id: &RegionID,
        time_slice: &TimeSliceID,
    ) -> Variable {
        let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());

        *self
            .unmet_demand_vars
            .get(&key)
            .expect("No unmet demand variable for given params")
    }

    /// Iterate over the asset variables
    fn iter_asset_vars(&self) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Variable)> {
        self.asset_vars
            .iter()
            .map(|((asset, time_slice), var)| (asset, time_slice, *var))
    }

    /// Iterate over the keys for asset variables
    fn asset_var_keys(&self) -> indexmap::map::Keys<'_, (AssetRef, TimeSliceID), Variable> {
        self.asset_vars.keys()
    }
}

/// The solution to the dispatch optimisation problem
#[allow(clippy::struct_field_names)]
pub struct Solution<'a> {
    solution: highs::Solution,
    variables: VariableMap,
    time_slice_info: &'a TimeSliceInfo,
    constraint_keys: ConstraintKeys,
    /// The objective value for the solution
    pub objective_value: Money,
}

impl Solution<'_> {
    /// Create a map of commodity flows for each asset's coeffs at every time slice.
    ///
    /// Note that this only includes commodity flows which relate to assets, so not every commodity
    /// in the simulation will necessarily be represented.
    pub fn create_flow_map(&self) -> FlowMap {
        // The decision variables represent assets' activity levels, not commodity flows. We
        // multiply this value by the flow coeffs to get commodity flows.
        let mut flows = FlowMap::new();
        for (asset, time_slice, activity) in self.iter_activity_for_existing() {
            for flow in asset.iter_flows() {
                let flow_key = (asset.clone(), flow.commodity.id.clone(), time_slice.clone());
                let flow_value = activity * flow.coeff;
                flows.insert(flow_key, flow_value);
            }
        }

        flows
    }

    /// Activity for each existing asset
    pub fn iter_activity(&self) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        self.variables
            .asset_var_keys()
            .zip(self.solution.columns())
            .map(|((asset, time_slice), activity)| (asset, time_slice, Activity(*activity)))
    }

    /// Activity for each existing asset
    fn iter_activity_for_existing(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        self.zip_var_keys_with_output(
            &self.variables.existing_asset_var_idx,
            self.solution.columns(),
        )
    }

    /// Iterate over unmet demand
    pub fn iter_unmet_demand(
        &self,
    ) -> impl Iterator<Item = (&CommodityID, &RegionID, &TimeSliceID, Flow)> {
        self.variables
            .unmet_demand_vars
            .keys()
            .zip(self.solution.columns()[self.variables.unmet_demand_var_idx.clone()].iter())
            .map(|((commodity_id, region_id, time_slice), flow)| {
                (commodity_id, region_id, time_slice, Flow(*flow))
            })
    }

    /// Keys and dual values for commodity balance constraints.
    pub fn iter_commodity_balance_duals(
        &self,
    ) -> impl Iterator<Item = (&CommodityID, &RegionID, &TimeSliceID, MoneyPerFlow)> {
        // Each commodity balance constraint applies to a particular time slice
        // selection (depending on time slice level). Where this covers multiple time slices,
        // we return the same dual for each individual time slice.
        self.constraint_keys
            .commodity_balance_keys
            .zip_duals(self.solution.dual_rows())
            .flat_map(|((commodity_id, region_id, ts_selection), price)| {
                ts_selection
                    .iter(self.time_slice_info)
                    .map(move |(ts, _)| (commodity_id, region_id, ts, price))
            })
    }

    /// Keys and dual values for activity constraints.
    pub fn iter_activity_duals(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, MoneyPerActivity)> {
        self.constraint_keys
            .activity_keys
            .zip_duals(self.solution.dual_rows())
            .map(|((asset, time_slice), dual)| (asset, time_slice, dual))
    }

    /// Keys and values for column duals.
    pub fn iter_column_duals(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, MoneyPerActivity)> {
        self.variables
            .asset_var_keys()
            .zip(self.solution.dual_columns())
            .map(|((asset, time_slice), dual)| (asset, time_slice, MoneyPerActivity(*dual)))
    }

    /// Zip a subset of keys in the variable map with a subset of the given output variable.
    ///
    /// # Arguments
    ///
    /// * `variable_idx` - The subset of variables to look at
    /// * `output` - The output variable of interest
    fn zip_var_keys_with_output<'a, T: UnitType>(
        &'a self,
        variable_idx: &Range<usize>,
        output: &'a [f64],
    ) -> impl Iterator<Item = (&'a AssetRef, &'a TimeSliceID, T)> + use<'a, T> {
        let keys = self.variables.asset_var_keys().skip(variable_idx.start);
        assert!(keys.len() >= variable_idx.len());

        keys.zip(output[variable_idx.clone()].iter())
            .map(|((asset, time_slice), value)| (asset, time_slice, T::new(*value)))
    }
}

/// Defines the possible errors that can occur when running the solver
#[derive(Debug, Clone)]
pub enum ModelError {
    /// The model definition is incoherent.
    ///
    /// Users should not be able to trigger this error.
    Incoherent(HighsStatus),
    /// The model is infeasible, probably because demand could not be met by supplied assets
    Infeasible,
    /// An optimal solution could not be found for some other reason
    NonOptimal(HighsModelStatus),
}

impl fmt::Display for ModelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModelError::Incoherent(status) => write!(f, "Incoherent model: {status:?}"),
            ModelError::Infeasible => {
                write!(
                    f,
                    "The solver has indicated that the problem is infeasible. It may be because \
                    the assets in this year cannot meet the required demand."
                )
            }
            ModelError::NonOptimal(status) => {
                write!(f, "Could not find optimal result: {status:?}")
            }
        }
    }
}

impl Error for ModelError {}

/// Try to solve the model, returning an error if the model is incoherent or result is non-optimal
pub fn solve_optimal(model: highs::Model) -> Result<highs::SolvedModel, ModelError> {
    let solved = model.try_solve().map_err(ModelError::Incoherent)?;

    match solved.status() {
        HighsModelStatus::Optimal => Ok(solved),
        HighsModelStatus::Infeasible => Err(ModelError::Infeasible),
        status => Err(ModelError::NonOptimal(status)),
    }
}

/// Sanity check for input prices.
///
/// Input prices should only be provided for commodities for which there will be no commodity
/// balance constraint.
fn check_input_prices(input_prices: &CommodityPrices, commodities: &[CommodityID]) {
    let commodities_set: HashSet<_> = commodities.iter().collect();
    let has_prices_for_commodity_subset = input_prices
        .keys()
        .any(|(commodity_id, _, _)| commodities_set.contains(commodity_id));
    assert!(
        !has_prices_for_commodity_subset,
        "Input prices were included for commodities that are being modelled, which is not allowed."
    );
}

/// Provides the interface for running the dispatch optimisation.
///
/// The caller can allow the dispatch run to return without error when demand is not met by calling
/// the `with_unmet_demand_allowed` method.
///
/// For a detailed description, please see the [dispatch optimisation formulation][1].
///
/// [1]: https://energysystemsmodellinglab.github.io/MUSE2/model/dispatch_optimisation.html
pub struct DispatchRun<'model, 'run> {
    model: &'model Model,
    existing_assets: &'run [AssetRef],
    candidate_assets: &'run [AssetRef],
    commodities: &'run [CommodityID],
    input_prices: Option<&'run CommodityPrices>,
    year: u32,
}

impl<'model, 'run> DispatchRun<'model, 'run> {
    /// Create a new [`DispatchRun`] for the specified model and assets for a given year
    pub fn new(model: &'model Model, assets: &'run [AssetRef], year: u32) -> Self {
        Self {
            model,
            existing_assets: assets,
            candidate_assets: &[],
            commodities: &[],
            input_prices: None,
            year,
        }
    }

    /// Include the specified candidate assets in the dispatch run
    pub fn with_candidates(self, candidate_assets: &'run [AssetRef]) -> Self {
        Self {
            candidate_assets,
            ..self
        }
    }

    /// Only apply commodity balance constraints to the specified subset of commodities
    pub fn with_commodity_subset(self, commodities: &'run [CommodityID]) -> Self {
        assert!(!commodities.is_empty());

        Self {
            commodities,
            ..self
        }
    }

    /// Explicitly provide prices for certain input commodities
    pub fn with_input_prices(self, input_prices: &'run CommodityPrices) -> Self {
        Self {
            input_prices: Some(input_prices),
            ..self
        }
    }

    /// Perform the dispatch optimisation.
    ///
    /// # Arguments
    ///
    /// * `run_description` - Which dispatch run for the current year this is
    /// * `writer` - For saving output data
    ///
    /// # Returns
    ///
    /// A solution containing new commodity flows for assets and prices for (some) commodities or an
    /// error.
    pub fn run(self, run_description: &str, writer: &mut DataWriter) -> Result<Solution<'model>> {
        let solution = self.run_no_save()?;
        writer.write_dispatch_debug_info(self.year, run_description, &solution)?;
        Ok(solution)
    }

    /// Run dispatch without saving the results.
    ///
    /// This is an internal function as callers always want to save results.
    fn run_no_save(&self) -> Result<Solution<'model>> {
        // If the user provided no commodities, we all use of them
        let all_commodities: Vec<_>;
        let commodities = if self.commodities.is_empty() {
            all_commodities = self.model.commodities.keys().cloned().collect();
            &all_commodities
        } else {
            self.commodities
        };
        if let Some(input_prices) = self.input_prices {
            check_input_prices(input_prices, commodities);
        }

        // Try running dispatch. If it fails because the model is infeasible, it is likely that this
        // is due to unmet demand, in this case, we rerun dispatch including extra variables to
        // track the unmet demand so we can report the offending regions/commodities to users
        match self.run_without_unmet_demand(commodities) {
            Ok(solution) => Ok(solution),
            Err(ModelError::Infeasible) => {
                let pairs = self
                    .get_regions_and_commodities_with_unmet_demand(commodities)
                    .expect("Failed to run dispatch to calculate unmet demand");

                ensure!(
                    !pairs.is_empty(),
                    "Model is infeasible, but there was no unmet demand"
                );

                bail!(
                    "Demand was not met for the following region and commodity pairs: {}",
                    format_items_with_cap(pairs)
                )
            }
            Err(err) => Err(err)?,
        }
    }

    /// Run dispatch without unmet demand variables
    fn run_without_unmet_demand(
        &self,
        commodities: &[CommodityID],
    ) -> Result<Solution<'model>, ModelError> {
        self.run_internal(commodities, /*allow_unmet_demand=*/ false)
    }

    /// Run dispatch to diagnose which regions and commodities have unmet demand
    fn get_regions_and_commodities_with_unmet_demand(
        &self,
        commodities: &[CommodityID],
    ) -> Result<IndexSet<(RegionID, CommodityID)>> {
        let solution = self.run_internal(commodities, /*allow_unmet_demand=*/ true)?;
        Ok(solution
            .iter_unmet_demand()
            .filter(|(_, _, _, flow)| *flow > Flow(0.0))
            .map(|(commodity_id, region_id, _, _)| (region_id.clone(), commodity_id.clone()))
            .collect())
    }

    /// Run dispatch for specified commodities, optionally including unmet demand variables
    fn run_internal(
        &self,
        commodities: &[CommodityID],
        allow_unmet_demand: bool,
    ) -> Result<Solution<'model>, ModelError> {
        // Set up problem
        let mut problem = Problem::default();
        let mut variables = VariableMap::new_with_asset_vars(
            &mut problem,
            self.model,
            self.input_prices,
            self.existing_assets,
            self.candidate_assets,
            self.year,
        );

        // If unmet demand is enabled for this dispatch run (and is allowed by the model param) then
        // we add variables representing unmet demand
        if allow_unmet_demand {
            variables.add_unmet_demand_variables(&mut problem, self.model, commodities);
        }

        // Add constraints
        let all_assets = chain(self.existing_assets.iter(), self.candidate_assets.iter());
        let constraint_keys = add_asset_constraints(
            &mut problem,
            &variables,
            self.model,
            &all_assets,
            commodities,
            self.year,
        );

        // Solve model
        let solution = solve_optimal(problem.optimise(Sense::Minimise))?;

        let objective_value = Money(solution.objective_value());
        debug!("Objective value: {objective_value}");

        Ok(Solution {
            solution: solution.get_solution(),
            variables,
            time_slice_info: &self.model.time_slice_info,
            constraint_keys,
            objective_value,
        })
    }
}

/// Add variables to the optimisation problem.
///
/// # Arguments
///
/// * `problem` - The optimisation problem
/// * `variables` - The map of asset variables
/// * `time_slice_info` - Information about assets
/// * `input_prices` - Optional explicit prices for input commodities
/// * `assets` - Assets to include
/// * `year` - Current milestone year
fn add_asset_variables(
    problem: &mut Problem,
    variables: &mut AssetVariableMap,
    time_slice_info: &TimeSliceInfo,
    input_prices: Option<&CommodityPrices>,
    assets: &[AssetRef],
    year: u32,
) -> Range<usize> {
    // This line **must** come before we add more variables
    let start = problem.num_cols();

    for (asset, time_slice) in iproduct!(assets.iter(), time_slice_info.iter_ids()) {
        let coeff = calculate_cost_coefficient(asset, year, time_slice, input_prices);
        let var = problem.add_column(coeff.value(), 0.0..);
        let key = (asset.clone(), time_slice.clone());
        let existing = variables.insert(key, var).is_some();
        assert!(!existing, "Duplicate entry for var");
    }

    start..problem.num_cols()
}

/// Calculate the cost coefficient for a decision variable.
///
/// Normally, the cost coefficient is the same as the asset's operating costs for the given year and
/// time slice. If `input_prices` is provided then those prices are added to the flow costs for the
/// relevant commodities, if they are input flows for the asset.
///
/// # Arguments
///
/// * `asset` - The asset to calculate the coefficient for
/// * `year` - The current milestone year
/// * `time_slice` - The time slice to which this coefficient applies
/// * `input_prices` - Optional map of prices to include for input commodities
///
/// # Returns
///
/// The cost coefficient to be used for the relevant decision variable.
fn calculate_cost_coefficient(
    asset: &Asset,
    year: u32,
    time_slice: &TimeSliceID,
    input_prices: Option<&CommodityPrices>,
) -> MoneyPerActivity {
    let opex = asset.get_operating_cost(year, time_slice);
    let input_cost = input_prices
        .map(|prices| asset.get_input_cost_from_prices(prices, time_slice))
        .unwrap_or_default();
    opex + input_cost
}
