//! Code for performing dispatch optimisation.
//!
//! This is used to calculate commodity flows and prices.
use crate::asset::{Asset, AssetRef, AssetState};
use crate::commodity::CommodityID;
use crate::input::format_items_with_cap;
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::simulation::CommodityPrices;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Capacity, Flow, Money, MoneyPerActivity, MoneyPerFlow};
use anyhow::{Result, bail};
use highs::{HighsModelStatus, HighsStatus, RowProblem as Problem, Sense};
use indexmap::{IndexMap, IndexSet};
use itertools::{chain, iproduct};
use log::warn;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::ops::Range;

mod constraints;
use constraints::{ConstraintKeys, add_model_constraints};

/// A map of commodity flows calculated during the optimisation
pub type FlowMap = IndexMap<(AssetRef, CommodityID, TimeSliceID), Flow>;

/// A decision variable in the optimisation
///
/// Note that this type does **not** include the value of the variable; it just refers to a
/// particular column of the problem.
type Variable = highs::Col;

/// The map of activity variables for assets
type ActivityVariableMap = IndexMap<(AssetRef, TimeSliceID), Variable>;

/// A map of capacity variables for assets
type CapacityVariableMap = IndexMap<AssetRef, Variable>;

/// Variables representing unmet demand for a given market
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
    activity_vars: ActivityVariableMap,
    existing_asset_var_idx: Range<usize>,
    capacity_vars: CapacityVariableMap,
    capacity_var_idx: Range<usize>,
    unmet_demand_vars: UnmetDemandVariableMap,
    unmet_demand_var_idx: Range<usize>,
}

impl VariableMap {
    /// Create a new [`VariableMap`] and add activity variables to the problem
    ///
    /// # Arguments
    ///
    /// * `problem` - The optimisation problem
    /// * `model` - The model
    /// * `input_prices` - Optional explicit prices for input commodities
    /// * `existing_assets` - The asset pool
    /// * `candidate_assets` - Candidate assets for inclusion in active pool
    /// * `year` - Current milestone year
    fn new_with_activity_vars(
        problem: &mut Problem,
        model: &Model,
        input_prices: Option<&CommodityPrices>,
        existing_assets: &[AssetRef],
        candidate_assets: &[AssetRef],
        year: u32,
    ) -> Self {
        let mut activity_vars = ActivityVariableMap::new();
        let existing_asset_var_idx = add_activity_variables(
            problem,
            &mut activity_vars,
            &model.time_slice_info,
            input_prices,
            existing_assets,
            year,
        );
        add_activity_variables(
            problem,
            &mut activity_vars,
            &model.time_slice_info,
            input_prices,
            candidate_assets,
            year,
        );

        Self {
            activity_vars,
            existing_asset_var_idx,
            capacity_vars: CapacityVariableMap::new(),
            capacity_var_idx: Range::default(),
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
    /// * `markets_to_allow_unmet_demand` - The subset of markets to assign unmet demand variables to
    fn add_unmet_demand_variables(
        &mut self,
        problem: &mut Problem,
        model: &Model,
        markets_to_allow_unmet_demand: &[(CommodityID, RegionID)],
    ) {
        assert!(!markets_to_allow_unmet_demand.is_empty());

        // This line **must** come before we add more variables
        let start = problem.num_cols();

        // Add variables
        let voll = model.parameters.value_of_lost_load;
        self.unmet_demand_vars.extend(
            iproduct!(
                markets_to_allow_unmet_demand.iter(),
                model.time_slice_info.iter_ids()
            )
            .map(|((commodity_id, region_id), time_slice)| {
                let key = (commodity_id.clone(), region_id.clone(), time_slice.clone());
                let var = problem.add_column(voll.value(), 0.0..);
                (key, var)
            }),
        );

        self.unmet_demand_var_idx = start..problem.num_cols();
    }

    /// Get the activity [`Variable`] corresponding to the given parameters.
    fn get_activity_var(&self, asset: &AssetRef, time_slice: &TimeSliceID) -> Variable {
        let key = (asset.clone(), time_slice.clone());

        *self
            .activity_vars
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
        *self
            .unmet_demand_vars
            .get(&(commodity_id.clone(), region_id.clone(), time_slice.clone()))
            .expect("No unmet demand variable for given params")
    }

    /// Iterate over the activity variables
    fn iter_activity_vars(&self) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Variable)> {
        self.activity_vars
            .iter()
            .map(|((asset, time_slice), var)| (asset, time_slice, *var))
    }

    /// Iterate over the keys for activity variables
    fn activity_var_keys(&self) -> indexmap::map::Keys<'_, (AssetRef, TimeSliceID), Variable> {
        self.activity_vars.keys()
    }

    /// Iterate over capacity variables
    fn iter_capacity_vars(&self) -> impl Iterator<Item = (&AssetRef, Variable)> {
        self.capacity_vars.iter().map(|(asset, var)| (asset, *var))
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
            .activity_var_keys()
            .zip(self.solution.columns())
            .map(|((asset, time_slice), activity)| (asset, time_slice, Activity(*activity)))
    }

    /// Activity for each existing asset
    fn iter_activity_for_existing(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        let cols = &self.solution.columns()[self.variables.existing_asset_var_idx.clone()];
        self.variables
            .activity_var_keys()
            .skip(self.variables.existing_asset_var_idx.start)
            .zip(cols.iter())
            .map(|((asset, time_slice), &value)| (asset, time_slice, Activity(value)))
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

    /// Iterate over capacity values
    pub fn iter_capacity(&self) -> impl Iterator<Item = (&AssetRef, Capacity)> {
        self.variables
            .capacity_vars
            .keys()
            .zip(self.solution.columns()[self.variables.capacity_var_idx.clone()].iter())
            .map(|(asset, capacity)| (asset, Capacity(*capacity)))
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
    ///
    /// Note: if there are any flexible capacity assets, these will have two duals with identical
    /// keys, and there will be no way to distinguish between them in the resulting iterator.
    /// Recommended for now only to use this function when there are no flexible capacity assets.
    pub fn iter_activity_duals(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, MoneyPerActivity)> {
        if self.iter_capacity().next().is_some() {
            warn!(
                "Warning: iter_activity_duals called on solution of model with flexible capacity assets.
                The resulting duals may be ambiguous."
            );
        }
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
            .activity_var_keys()
            .zip(self.solution.dual_columns())
            .map(|((asset, time_slice), dual)| (asset, time_slice, MoneyPerActivity(*dual)))
    }

    /// Get the markets for which unmet demand is positive in any time slice.
    pub fn get_markets_with_unmet_demand(&self) -> IndexSet<(CommodityID, RegionID)> {
        self.iter_unmet_demand()
            .filter(|(_, _, _, flow)| *flow > Flow(0.0))
            .map(|(commodity_id, region_id, _, _)| (commodity_id.clone(), region_id.clone()))
            .collect()
    }
}

/// Defines the possible errors that can occur when running the solver
#[derive(Debug, Clone)]
pub enum ModelError {
    /// The model definition is incoherent.
    ///
    /// Users should not be able to trigger this error.
    Incoherent(HighsStatus),
    /// An optimal solution could not be found
    NonOptimal(HighsModelStatus),
}

impl fmt::Display for ModelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModelError::Incoherent(status) => write!(f, "Incoherent model: {status:?}"),
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
        status => Err(ModelError::NonOptimal(status)),
    }
}

/// Select prices for commodities not being balanced
fn select_input_prices(
    input_prices: &CommodityPrices,
    markets_to_balance: &[(CommodityID, RegionID)],
) -> CommodityPrices {
    let commodity_regions_set: HashSet<(&CommodityID, &RegionID)> =
        markets_to_balance.iter().map(|m| (&m.0, &m.1)).collect();
    input_prices
        .iter()
        .filter(|(commodity_id, region_id, _, _)| {
            !commodity_regions_set.contains(&(commodity_id, region_id))
        })
        .collect()
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
    flexible_capacity_assets: &'run [AssetRef],
    candidate_assets: &'run [AssetRef],
    markets_to_balance: &'run [(CommodityID, RegionID)],
    markets_to_allow_unmet_demand: &'run [(CommodityID, RegionID)],
    input_prices: Option<&'run CommodityPrices>,
    year: u32,
    capacity_margin: f64,
}

impl<'model, 'run> DispatchRun<'model, 'run> {
    /// Create a new [`DispatchRun`] for the specified model and assets for a given year
    pub fn new(model: &'model Model, assets: &'run [AssetRef], year: u32) -> Self {
        Self {
            model,
            existing_assets: assets,
            flexible_capacity_assets: &[],
            candidate_assets: &[],
            markets_to_balance: &[],
            markets_to_allow_unmet_demand: &[],
            input_prices: None,
            year,
            capacity_margin: 0.0,
        }
    }

    /// Include the specified flexible capacity assets in the dispatch run
    pub fn with_flexible_capacity_assets(
        self,
        flexible_capacity_assets: &'run [AssetRef],
        capacity_margin: f64,
    ) -> Self {
        Self {
            flexible_capacity_assets,
            capacity_margin,
            ..self
        }
    }

    /// Include the specified candidate assets in the dispatch run
    pub fn with_candidates(self, candidate_assets: &'run [AssetRef]) -> Self {
        Self {
            candidate_assets,
            ..self
        }
    }

    /// Only apply commodity balance constraints to the specified subset of markets
    pub fn with_market_balance_subset(self, markets: &'run [(CommodityID, RegionID)]) -> Self {
        assert!(!markets.is_empty());

        Self {
            markets_to_balance: markets,
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

    /// Allow unmet demand variables for the specified subset of markets
    pub fn with_unmet_demand_vars(self, markets: &'run [(CommodityID, RegionID)]) -> Self {
        Self {
            markets_to_allow_unmet_demand: markets,
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
        // If the user provided no markets to balance, we all use of them
        let all_markets: Vec<_>;
        let markets_to_balance = if self.markets_to_balance.is_empty() {
            all_markets = self.model.iter_markets().collect();
            &all_markets
        } else {
            self.markets_to_balance
        };

        // Select prices for commodities not being balanced
        let input_prices_owned = self
            .input_prices
            .map(|prices| select_input_prices(prices, markets_to_balance));
        let input_prices = input_prices_owned.as_ref();

        // Try running dispatch. If it fails because the model is infeasible, it is likely that this
        // is due to unmet demand, in this case, we rerun dispatch including with unmet demand
        // variables for all markets so we can report the offending markets to users
        match self.run_internal(
            markets_to_balance,
            self.markets_to_allow_unmet_demand,
            input_prices,
        ) {
            Ok(solution) => {
                writer.write_dispatch_debug_info(self.year, run_description, &solution)?;
                Ok(solution)
            }
            Err(ModelError::NonOptimal(HighsModelStatus::Infeasible)) => {
                let solution =
                    self.run_internal(markets_to_balance, markets_to_balance, input_prices)?;
                writer.write_dispatch_debug_info(self.year, run_description, &solution)?;
                let markets_with_unmet_demand: IndexSet<String> = solution
                    .iter_unmet_demand()
                    .filter(|(_, _, _, flow)| *flow > Flow(0.0))
                    .map(|(commodity_id, region_id, _, _)| format!("{commodity_id}|{region_id}"))
                    .collect();
                bail!(
                    "The solver has indicated that the problem is infeasible, probably because \
                    the supplied assets could not meet the required demand. Demand was not met \
                    for the following markets: {}",
                    format_items_with_cap(markets_with_unmet_demand)
                )
            }
            Err(err) => Err(err)?,
        }
    }

    /// Run dispatch to balance the specified markets, allowing unmet demand for a subset of these
    fn run_internal(
        &self,
        markets_to_balance: &[(CommodityID, RegionID)],
        markets_to_allow_unmet_demand: &[(CommodityID, RegionID)],
        input_prices: Option<&CommodityPrices>,
    ) -> Result<Solution<'model>, ModelError> {
        // Set up problem
        let mut problem = Problem::default();
        let mut variables = VariableMap::new_with_activity_vars(
            &mut problem,
            self.model,
            input_prices,
            self.existing_assets,
            self.candidate_assets,
            self.year,
        );

        // Check flexible capacity assets is a subset of existing assets
        for asset in self.flexible_capacity_assets {
            assert!(
                self.existing_assets.contains(asset),
                "Flexible capacity assets must be a subset of existing assets. Offending asset: {asset:?}"
            );
        }

        // Add capacity variables for flexible capacity assets
        if !self.flexible_capacity_assets.is_empty() {
            variables.capacity_var_idx = add_capacity_variables(
                &mut problem,
                &mut variables.capacity_vars,
                self.flexible_capacity_assets,
                self.capacity_margin,
            );
        }

        // Check that markets_to_allow_unmet_demand is a subset of markets_to_balance
        for market in markets_to_allow_unmet_demand {
            assert!(
                markets_to_balance.contains(market),
                "markets_to_allow_unmet_demand must be a subset of markets_to_balance. \
                 Offending market: {market:?}"
            );
        }

        // Add variables representing unmet demand
        if !markets_to_allow_unmet_demand.is_empty() {
            variables.add_unmet_demand_variables(
                &mut problem,
                self.model,
                markets_to_allow_unmet_demand,
            );
        }

        // Add constraints
        let all_assets = chain(self.existing_assets.iter(), self.candidate_assets.iter());
        let constraint_keys = add_model_constraints(
            &mut problem,
            &variables,
            self.model,
            &all_assets,
            markets_to_balance,
            markets_to_allow_unmet_demand,
            self.year,
        );

        // Solve model
        let solution = solve_optimal(problem.optimise(Sense::Minimise))?;

        Ok(Solution {
            solution: solution.get_solution(),
            variables,
            time_slice_info: &self.model.time_slice_info,
            constraint_keys,
            objective_value: Money(solution.objective_value()),
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
fn add_activity_variables(
    problem: &mut Problem,
    variables: &mut ActivityVariableMap,
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

fn add_capacity_variables(
    problem: &mut Problem,
    variables: &mut CapacityVariableMap,
    assets: &[AssetRef],
    capacity_margin: f64,
) -> Range<usize> {
    // This line **must** come before we add more variables
    let start = problem.num_cols();

    for asset in assets {
        // Can only have flexible capacity for Candidate or Selected assets
        if !matches!(
            asset.state(),
            AssetState::Candidate | AssetState::Selected { .. }
        ) {
            panic!(
                "Flexible capacity can only be assigned to Candidate or Selected assets. Offending asset: {asset:?}"
            );
        }

        let current_capacity = asset.capacity().value();
        let bounds =
            (1.0 - capacity_margin) * current_capacity..=(1.0 + capacity_margin) * current_capacity;
        let var = problem.add_column(0.0, bounds);
        let existing = variables.insert(asset.clone(), var).is_some();
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
