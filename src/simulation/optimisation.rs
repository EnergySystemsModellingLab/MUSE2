//! Code for performing dispatch optimisation.
//!
//! This is used to calculate commodity flows and prices.
use crate::asset::{Asset, AssetCapacity, AssetRef, AssetState};
use crate::commodity::CommodityID;
use crate::finance::annual_capital_cost;
use crate::input::format_items_with_cap;
use crate::model::Model;
use crate::output::DataWriter;
use crate::process::ProcessID;
use crate::region::RegionID;
use crate::simulation::PriceMap;
use crate::time_slice::{TimeSliceID, TimeSliceInfo, TimeSliceLevel, TimeSliceSelection};
use crate::units::{
    Activity, Capacity, Dimensionless, Flow, Money, MoneyPerActivity, MoneyPerCapacity,
    MoneyPerFlow, Year,
};
use anyhow::{Context, Result, anyhow, bail, ensure};
use highs::{HighsModelStatus, RowProblem as Problem, Sense};
use indexmap::{IndexMap, IndexSet};
use itertools::{chain, iproduct};
use std::collections::HashMap;
use std::error::Error;
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
    candidate_asset_var_idx: Range<usize>,
    capacity_vars: CapacityVariableMap,
    capacity_var_idx: Range<usize>,
    unmet_demand_vars: UnmetDemandVariableMap,
    unmet_demand_var_idx: Range<usize>,
    /// Cost coefficients for every variable, captured at construction for reuse in the second solve
    cost_terms: Vec<(Variable, f64)>,
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
        input_prices: Option<&PriceMap>,
        existing_assets: &[AssetRef],
        candidate_assets: &[AssetRef],
        year: u32,
    ) -> Self {
        let mut activity_vars = ActivityVariableMap::new();
        let mut cost_terms = Vec::new();
        let existing_asset_var_idx = add_activity_variables(
            problem,
            &mut activity_vars,
            &mut cost_terms,
            &model.time_slice_info,
            input_prices,
            existing_assets,
            year,
        );
        let candidate_asset_var_idx = add_activity_variables(
            problem,
            &mut activity_vars,
            &mut cost_terms,
            &model.time_slice_info,
            input_prices,
            candidate_assets,
            year,
        );

        Self {
            activity_vars,
            existing_asset_var_idx,
            candidate_asset_var_idx,
            capacity_vars: CapacityVariableMap::new(),
            capacity_var_idx: Range::default(),
            unmet_demand_vars: UnmetDemandVariableMap::default(),
            unmet_demand_var_idx: Range::default(),
            cost_terms,
        }
    }

    /// Add unmet demand variables to the map and the problem
    ///
    /// # Arguments
    ///
    /// * `problem` - The optimisation problem
    /// * `model` - The model
    /// * `markets_to_allow_unmet_demand` - The subset of markets to add unmet demand variables for
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
                self.cost_terms.push((var, voll.value()));
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
    /// Create a map of commodity flows for each asset's coeffs at every time slice
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

    /// Activity for all assets (existing and candidate, if present)
    pub fn iter_activity(&self) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        self.variables
            .activity_var_keys()
            .zip(self.solution.columns())
            .map(|((asset, time_slice), activity)| (asset, time_slice, Activity(*activity)))
    }

    /// Activity for each existing asset
    pub fn iter_activity_for_existing(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        let cols = &self.solution.columns()[self.variables.existing_asset_var_idx.clone()];
        self.variables
            .activity_var_keys()
            .skip(self.variables.existing_asset_var_idx.start)
            .zip(cols.iter())
            .map(|((asset, time_slice), &value)| (asset, time_slice, Activity(value)))
    }

    /// Activity for each candidate asset
    pub fn iter_activity_for_candidates(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        let cols = &self.solution.columns()[self.variables.candidate_asset_var_idx.clone()];
        self.variables
            .activity_var_keys()
            .skip(self.variables.candidate_asset_var_idx.start)
            .zip(cols.iter())
            .map(|((asset, time_slice), &value)| (asset, time_slice, Activity(value)))
    }

    /// Iterate over the keys for activity for each candidate asset
    pub fn iter_activity_keys_for_candidates(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID)> {
        self.iter_activity_for_candidates()
            .map(|(asset, time_slice, _activity)| (asset, time_slice))
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
    ///
    /// Will return `AssetCapacity::Continuous` or `AssetCapacity::Discrete` depending on whether
    /// the asset has a defined unit size.
    pub fn iter_capacity(&self) -> impl Iterator<Item = (&AssetRef, AssetCapacity)> {
        self.variables
            .capacity_vars
            .keys()
            .zip(self.solution.columns()[self.variables.capacity_var_idx.clone()].iter())
            .map(|(asset, capacity_var)| {
                // If the asset has a defined unit size, the capacity variable represents number of
                // units, otherwise it represents absolute capacity
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let asset_capacity = if let Some(unit_size) = asset.unit_size() {
                    AssetCapacity::Discrete(capacity_var.round() as u32, unit_size)
                } else {
                    AssetCapacity::Continuous(Capacity(*capacity_var))
                };
                (asset, asset_capacity)
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
    ///
    /// Note: if there are any flexible capacity assets, these will have two duals with identical
    /// keys, and there will be no way to distinguish between them in the resulting iterator.
    /// Recommended for now only to use this function when there are no flexible capacity assets.
    ///
    /// Also note: this excludes seasonal and annual constraints. Recommended for now not to use
    /// this for models that include seasonal or annual availability constraints.
    pub fn iter_activity_duals(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, MoneyPerActivity)> {
        self.constraint_keys
            .activity_keys
            .zip_duals(self.solution.dual_rows())
            .filter(|&((_asset, ts_selection), _dual)| {
                matches!(ts_selection, TimeSliceSelection::Single(_))
            })
            .map(|((asset, ts_selection), dual)| {
                // `unwrap` is safe here because we just matched Single(_)
                let (time_slice, _) = ts_selection.iter(self.time_slice_info).next().unwrap();
                (asset, time_slice, dual)
            })
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
}

/// Defines the possible errors that can occur when running the solver
#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum ModelError {
    /// An optimal solution could not be found
    #[display("Could not find optimal result: {_0:?}")]
    NonOptimal(HighsModelStatus),
    /// Another error occurred
    #[display("{_0}")]
    Other(anyhow::Error),
}

impl ModelError {
    /// Convert this error into an [`anyhow::Error`]
    pub fn into_anyhow(self) -> anyhow::Error {
        match self {
            ModelError::NonOptimal(status) => anyhow!("Could not find optimal result: {status:?}"),
            ModelError::Other(error) => error,
        }
    }
}

impl Error for ModelError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ModelError::NonOptimal(_) => None,
            ModelError::Other(error) => Some(error.as_ref()),
        }
    }
}

/// Apply the specified HiGHS options from a [`toml::Table`]
pub fn apply_highs_options_from_toml(
    model: &mut highs::Model,
    options: &toml::Table,
) -> Result<()> {
    // Attempt to set an option, returning an error if it fails
    macro_rules! try_set_opt {
        ($option:expr, $value:expr) => {{
            model
                .try_set_option($option.as_str(), $value)
                .map_err(|_| anyhow!("Invalid option name or value"))?;

            Ok(())
        }};
    }

    // Iterate through options, applying each in turn to the HiGHS model
    for (option, value) in options {
        match value {
            toml::Value::String(value) => try_set_opt!(option, value.as_str()),
            toml::Value::Integer(value) => match i32::try_from(*value) {
                Ok(value) => try_set_opt!(option, value),
                Err(_) => Err(anyhow!("Value out of range")),
            },
            toml::Value::Float(value) => try_set_opt!(option, *value),
            toml::Value::Boolean(value) => try_set_opt!(option, *value),
            _ => Err(anyhow!("HiGHS options cannot have this type")),
        }
        .with_context(|| format!("Failed to set option \"{option}\" to value \"{value}\""))?;
    }

    Ok(())
}

/// Try to solve the model, returning an error if the model is incoherent or result is non-optimal
pub fn solve_optimal(model: highs::Model) -> Result<highs::SolvedModel, ModelError> {
    let solved = model
        .try_solve()
        .map_err(|status| anyhow!("Incoherent model: {status:?}"))?;

    match solved.status() {
        HighsModelStatus::Optimal => Ok(solved),
        status => Err(status.into()),
    }
}

/// Filter prices data to only include prices for markets not being balanced
///
/// Markets being balanced (i.e. with commodity balance constraints) will have prices calculated
/// internally by the solver, so we need to remove them to prevent double-counting.
fn filter_input_prices(
    input_prices: &PriceMap,
    markets_to_balance: &[(CommodityID, RegionID)],
) -> PriceMap {
    input_prices
        .iter()
        .filter(|(commodity_id, region_id, _, _)| {
            !markets_to_balance
                .iter()
                .any(|(c, r)| c == *commodity_id && r == *region_id)
        })
        .collect()
}

/// Provides the interface for running the dispatch optimisation.
///
/// The run will attempt to meet unmet demand: if the solver reports infeasibility
/// the implementation will rerun including unmet-demand variables to identify offending
/// markets and provide a clearer error message.
///
/// For a detailed description, please see the [dispatch optimisation formulation][1].
///
#[doc = concat!("[1]: ", crate::docs_url!("/model/dispatch_optimisation.html"))]
#[must_use = "Must call run() method on DispatchRun struct"]
pub struct DispatchRun<'model, 'run> {
    model: &'model Model,
    existing_assets: &'run [AssetRef],
    flexible_capacity_assets: &'run [AssetRef],
    capacity_limits: Option<&'run HashMap<AssetRef, AssetCapacity>>,
    candidate_assets: &'run [AssetRef],
    markets_to_balance: &'run [(CommodityID, RegionID)],
    input_prices: Option<&'run PriceMap>,
    year: u32,
    capacity_margin: Dimensionless,
}

impl<'model, 'run> DispatchRun<'model, 'run> {
    /// Create a new [`DispatchRun`] for the specified model and assets for a given year
    pub fn new(model: &'model Model, assets: &'run [AssetRef], year: u32) -> Self {
        Self {
            model,
            existing_assets: assets,
            flexible_capacity_assets: &[],
            capacity_limits: None,
            candidate_assets: &[],
            markets_to_balance: &[],
            input_prices: None,
            year,
            capacity_margin: Dimensionless(0.0),
        }
    }

    /// Include the specified flexible capacity assets in the dispatch run
    pub fn with_flexible_capacity_assets(
        self,
        flexible_capacity_assets: &'run [AssetRef],
        capacity_limits: Option<&'run HashMap<AssetRef, AssetCapacity>>,
        capacity_margin: Dimensionless,
    ) -> Self {
        Self {
            flexible_capacity_assets,
            capacity_limits,
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
    pub fn with_market_balance_subset(
        self,
        markets_to_balance: &'run [(CommodityID, RegionID)],
    ) -> Self {
        assert!(!markets_to_balance.is_empty());

        Self {
            markets_to_balance,
            ..self
        }
    }

    /// Explicitly provide prices for certain input commodities
    pub fn with_input_prices(self, input_prices: &'run PriceMap) -> Self {
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
    pub fn run(&self, run_description: &str, writer: &mut DataWriter) -> Result<Solution<'model>> {
        // If the user provided no markets to balance, we use all of them
        let all_markets: Vec<_>;
        let markets_to_balance = if self.markets_to_balance.is_empty() {
            all_markets = self.model.iter_markets().collect();
            &all_markets
        } else {
            self.markets_to_balance
        };

        // Select prices for markets not being balanced
        let input_prices_owned = self
            .input_prices
            .map(|prices| filter_input_prices(prices, markets_to_balance));
        let input_prices = input_prices_owned.as_ref();

        // Try running dispatch. If it fails because the model is infeasible, it is likely that this
        // is due to unmet demand, in this case, we rerun dispatch including extra variables to
        // track the unmet demand so we can report the offending markets to users
        match self.run_without_unmet_demand_variables(markets_to_balance, input_prices) {
            Ok(solution) => {
                // Normal successful run: write debug info and return
                writer.write_dispatch_debug_info(self.year, run_description, &solution)?;
                Ok(solution)
            }
            Err(ModelError::NonOptimal(HighsModelStatus::Infeasible)) => {
                // Re-run including unmet demand variables so we can record detailed unmet-demand
                // debug output before returning an error to the caller.
                let solution = self
                    .run_internal(
                        markets_to_balance,
                        /*allow_unmet_demand=*/ true,
                        input_prices,
                    )
                    .expect("Failed to run dispatch to calculate unmet demand");

                // Write debug CSVs to help diagnosis
                writer.write_dispatch_debug_info(self.year, run_description, &solution)?;

                // Collect markets with unmet demand from the solution
                let markets: IndexSet<_> = solution
                    .iter_unmet_demand()
                    .filter(|(_, _, _, flow)| *flow > Flow(0.0))
                    .map(|(commodity_id, region_id, _, _)| {
                        (commodity_id.clone(), region_id.clone())
                    })
                    .collect();

                ensure!(
                    !markets.is_empty(),
                    "Model is infeasible, but there was no unmet demand"
                );

                bail!(
                    "The solver has indicated that the problem is infeasible, probably because \
                    the supplied assets could not meet the required demand. Demand was not met \
                    for the following markets: {}",
                    format_items_with_cap(markets)
                );
            }
            Err(err) => Err(err.into_anyhow()),
        }
    }

    /// Run dispatch without unmet demand variables
    fn run_without_unmet_demand_variables(
        &self,
        markets_to_balance: &[(CommodityID, RegionID)],
        input_prices: Option<&PriceMap>,
    ) -> Result<Solution<'model>, ModelError> {
        self.run_internal(
            markets_to_balance,
            /*allow_unmet_demand=*/ false,
            input_prices,
        )
    }

    /// Run dispatch to balance the specified markets, optionally including unmet demand variables
    #[allow(clippy::too_many_lines)]
    fn run_internal(
        &self,
        markets_to_balance: &[(CommodityID, RegionID)],
        allow_unmet_demand: bool,
        input_prices: Option<&PriceMap>,
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

        // If unmet demand is enabled for this dispatch run (and is allowed by the model param) then
        // we add variables representing unmet demand for all markets being balanced
        if allow_unmet_demand {
            variables.add_unmet_demand_variables(&mut problem, self.model, markets_to_balance);
        }

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
                self.capacity_limits,
                self.capacity_margin,
            );
            let capacity_costs: Vec<_> = variables
                .iter_capacity_vars()
                .map(|(asset, var)| (var, calculate_capacity_coefficient(asset).value()))
                .collect();
            variables.cost_terms.extend(capacity_costs);
        }

        // Add constraints
        let all_assets = chain(
            self.existing_assets.iter(),
            self.candidate_assets.iter(),
        );
        let constraint_keys = add_model_constraints(
            &mut problem,
            &variables,
            self.model,
            &all_assets,
            markets_to_balance,
            self.year,
            self.candidate_assets,
        );

        // Take pre-computed cost terms (stored during problem construction, not recomputed here)
        let cost_terms = std::mem::take(&mut variables.cost_terms);

        // First solve: minimise cost
        let mut highs_model = problem.optimise(Sense::Minimise);
        apply_highs_options_from_toml(
            &mut highs_model,
            &self.model.parameters.highs.dispatch_options,
        )
        .context("Failed to apply custom HiGHS options to dispatch optimisation")?;
        let solved1 = solve_optimal(highs_model)?;

        // Second lexicographic solve: minimise L1 utilisation spread subject to cost <= Z*
        let z_star;
        let solved2 = if self.model.parameters.dispatch_activity_equalisation {
            z_star = solved1.objective_value();
            let tolerance = self
                .model
                .parameters
                .dispatch_activity_equalisation_tolerance;

            let mut highs_model2 = highs::Model::from(solved1);

            // Zero all linear objective coefficients so only the spreading objective remains
            for &var in variables.activity_vars.values() {
                highs_model2.change_column_cost(var, 0.0);
            }
            for &var in variables.unmet_demand_vars.values() {
                highs_model2.change_column_cost(var, 0.0);
            }
            for (_, var) in variables.iter_capacity_vars() {
                highs_model2.change_column_cost(var, 0.0);
            }

            // Constrain total cost to be no worse than z_star * (1 + tolerance)
            highs_model2.add_row(..=(z_star * (1.0 + tolerance)), cost_terms);

            // Add L1 spreading variables and constraints
            add_activity_equalisation_to_model(
                &mut highs_model2,
                &variables,
                self.existing_assets.iter(),
                self.model,
            );

            apply_highs_options_from_toml(
                &mut highs_model2,
                &self.model.parameters.highs.dispatch_options,
            )
            .context("Failed to apply custom HiGHS options to dispatch equalisation solve")?;
            solve_optimal(highs_model2)?
        } else {
            z_star = solved1.objective_value();
            solved1
        };

        let solution = Solution {
            solution: solved2.get_solution(),
            variables,
            time_slice_info: &self.model.time_slice_info,
            constraint_keys,
            // Always report the primary cost objective, not the spreading objective value
            objective_value: Money(z_star),
        };
        Ok(solution)
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
    cost_terms: &mut Vec<(Variable, f64)>,
    time_slice_info: &TimeSliceInfo,
    input_prices: Option<&PriceMap>,
    assets: &[AssetRef],
    year: u32,
) -> Range<usize> {
    // This line **must** come before we add more variables
    let start = problem.num_cols();

    for (asset, time_slice) in iproduct!(assets.iter(), time_slice_info.iter_ids()) {
        let coeff = calculate_activity_coefficient(asset, year, time_slice, input_prices);
        let var = problem.add_column(coeff.value(), 0.0..);
        cost_terms.push((var, coeff.value()));
        let key = (asset.clone(), time_slice.clone());
        let existing = variables.insert(key, var).is_some();
        assert!(!existing, "Duplicate entry for var");
    }

    start..problem.num_cols()
}

/// Add pairwise L1 equalisation constraints for a group of `(activity_variable, inv_cap)` terms.
///
/// For every unordered pair `(i, j)` in `terms`, introduces a non-negative auxiliary variable
/// `d` with objective coefficient 1 and the constraints `d >= u_i - u_j` and `d >= u_j - u_i`,
/// where `u_k = act_k * inv_cap_k`. Minimising the sum of all `d` minimises total pairwise L1
/// spread of utilisation within the group. Does nothing if `terms` has fewer than two entries.
fn add_pairwise_equalisation(model: &mut highs::Model, terms: &[(Variable, f64)]) {
    if terms.len() < 2 {
        return;
    }

    for i in 0..terms.len() {
        for j in (i + 1)..terms.len() {
            let (act_a, inv_cap_a) = terms[i];
            let (act_b, inv_cap_b) = terms[j];
            let d = model.add_col(1.0, 0.0.., []);
            model.add_row(0.0.., [(d, 1.0), (act_a, -inv_cap_a), (act_b, inv_cap_b)]);
            model.add_row(0.0.., [(d, 1.0), (act_a, inv_cap_a), (act_b, -inv_cap_b)]);
        }
    }
}

/// Returns the balance level of the asset's primary output commodity, defaulting to `DayNight`.
fn activity_balance_level(asset: &AssetRef, muse_model: &Model) -> TimeSliceLevel {
    asset
        .primary_output_commodity()
        .and_then(|id| muse_model.commodities.get(id))
        .map_or(TimeSliceLevel::DayNight, |c| c.time_slice_level)
}

/// Add pairwise L1 utilisation-spreading variables and constraints to a [`highs::Model`].
///
/// Applies two independent sets of equalisation groups, both contributing to the same objective:
///
/// 1. **Asset groups** keyed by `(process, time_slice)`: equalises utilisation across assets
///    sharing the same process and time slice (`u = activity / capacity`).
/// 2. **Time-slice groups** keyed by the balance level of the asset's primary output commodity:
///    - `Annual`: one group per asset covering all time slices in the year.
///    - `Season`: one group per `(asset, season)`.
///    - `DayNight`: excluded, as these assets are balanced independently in every time slice.
///
/// Candidate assets must be excluded by the caller before passing the iterator.
fn add_activity_equalisation_to_model<'a, I>(
    model: &mut highs::Model,
    variables: &VariableMap,
    assets: I,
    muse_model: &Model,
) where
    I: Iterator<Item = &'a AssetRef>,
{
    let mut groups: IndexMap<(ProcessID, TimeSliceID), Vec<(Variable, f64)>> = IndexMap::new();
    let mut ts_groups: IndexMap<(AssetRef, TimeSliceSelection), Vec<(Variable, f64)>> =
        IndexMap::new();

    for asset in assets {
        // Use initial capacity as proxy; see comment in calculate_activity_coefficient for why
        // flexible-capacity assets are an approximation here.
        let cap = asset.total_capacity().value();
        if cap <= 1e-9 {
            continue;
        }
        let inv_cap = 1.0 / cap;

        // Add to process/time-slice group
        for time_slice in muse_model.time_slice_info.iter_ids() {
            let act = variables.get_activity_var(asset, time_slice);
            groups
                .entry((asset.process_id().clone(), time_slice.clone()))
                .or_default()
                .push((act, inv_cap));
        }

        // Create asset/time-slice-selection groups, using the balance level of the asset's primary
        // output commodity to determine the selection
        let balance_level = activity_balance_level(asset, muse_model);
        if balance_level == TimeSliceLevel::DayNight {
            // DayNight assets are balanced independently in every time slice, so there's no need
            // to add a spreading objective for them
            continue;
        }
        for selection in muse_model
            .time_slice_info
            .iter_selections_at_level(balance_level)
        {
            for (time_slice, _) in selection.iter(&muse_model.time_slice_info) {
                let act = variables.get_activity_var(asset, time_slice);
                let fraction = muse_model.time_slice_info.time_slices[time_slice].value();
                ts_groups
                    .entry((asset.clone(), selection.clone()))
                    .or_default()
                    .push((act, 1.0 / (cap * fraction)));
            }
        }
    }

    for ((_process, _time_slice), terms) in groups {
        add_pairwise_equalisation(model, &terms);
    }
    for ((_asset, _selection), terms) in ts_groups {
        add_pairwise_equalisation(model, &terms);
    }
}

fn add_capacity_variables(
    problem: &mut Problem,
    variables: &mut CapacityVariableMap,
    assets: &[AssetRef],
    capacity_limits: Option<&HashMap<AssetRef, AssetCapacity>>,
    capacity_margin: Dimensionless,
) -> Range<usize> {
    let capacity_margin = capacity_margin.value();

    // This line **must** come before we add more variables
    let start = problem.num_cols();

    for asset in assets {
        // Can only have flexible capacity for `Ready` assets
        assert!(
            matches!(asset.state(), AssetState::Ready { .. }),
            "Flexible capacity can only be assigned to `Ready` type assets. Offending asset: {asset:?}"
        );

        let current_capacity = asset.capacity();
        let coeff = calculate_capacity_coefficient(asset);

        // Retrieve capacity limit if provided
        let capacity_limit = capacity_limits.and_then(|limits| limits.get(asset));

        // Sanity check: make sure capacity_limit is compatible with current_capacity
        if let Some(limit) = capacity_limit {
            assert!(
                matches!(
                    (current_capacity, limit),
                    (AssetCapacity::Continuous(_), AssetCapacity::Continuous(_))
                        | (AssetCapacity::Discrete(_, _), AssetCapacity::Discrete(_, _))
                ),
                "Incompatible capacity types for asset capacity limit"
            );
        }

        // Add a capacity variable for each asset
        // Bounds are calculated based on current capacity with wiggle-room defined by
        // `capacity_margin`, and limited by `capacity_limit` if provided.
        let var = match current_capacity {
            AssetCapacity::Continuous(cap) => {
                // Continuous capacity: capacity variable represents total capacity
                let lower = ((1.0 - capacity_margin) * cap.value()).max(0.0);
                let mut upper = (1.0 + capacity_margin) * cap.value();
                if let Some(limit) = capacity_limit {
                    upper = upper.min(limit.total_capacity().value());
                }
                problem.add_column(coeff.value(), lower..=upper)
            }
            AssetCapacity::Discrete(units, unit_size) => {
                // Discrete capacity: capacity variable represents number of units
                let lower = ((1.0 - capacity_margin) * units as f64).max(0.0);
                let mut upper = (1.0 + capacity_margin) * units as f64;
                if let Some(limit) = capacity_limit {
                    upper = upper.min(limit.n_units().unwrap() as f64);
                }
                problem.add_integer_column((coeff * unit_size).value(), lower..=upper)
            }
        };

        let existing = variables.insert(asset.clone(), var).is_some();
        assert!(!existing, "Duplicate entry for var");
    }

    start..problem.num_cols()
}

/// Calculate the cost coefficient for an activity variable.
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
fn calculate_activity_coefficient(
    asset: &Asset,
    year: u32,
    time_slice: &TimeSliceID,
    input_prices: Option<&PriceMap>,
) -> MoneyPerActivity {
    let opex = asset.get_operating_cost(year, time_slice);
    if let Some(prices) = input_prices {
        opex + asset.get_input_cost_from_prices(prices, time_slice)
    } else {
        opex
    }
}

/// Calculate the cost coefficient for a capacity variable (for flexible capacity assets only).
///
/// This includes both the annual fixed operating cost and the annual capital cost.
fn calculate_capacity_coefficient(asset: &AssetRef) -> MoneyPerCapacity {
    let param = asset.process_parameter();
    let annual_fixed_operating_cost = param.fixed_operating_cost * Year(1.0);
    annual_fixed_operating_cost
        + annual_capital_cost(param.capital_cost, param.lifetime, param.discount_rate)
}

#[cfg(test)]
mod tests {
    use crate::patch::{FilePatch, ModelPatch};
    use indexmap::IndexMap;
    use serde::Deserialize;
    use std::fs;

    #[derive(Debug, Deserialize)]
    struct DispatchRow {
        milestone_year: u32,
        run_description: String,
        process_id: String,
        time_slice: String,
        activity: Option<f64>,
    }

    // Verifies that two identical assets receive equal dispatch under pairwise L1 equalisation.
    #[test]
    fn two_identical_assets_dispatch_evenly() {
        let tmp = ModelPatch::from_example("simple")
            .with_file_patches([
                FilePatch::new("assets.csv").with_addition("GASCGT,GBR,A0_ELC,2.430,2020")
            ])
            .build_to_tempdir()
            .unwrap();

        let output_path = tmp.path().join("output");
        fs::create_dir_all(&output_path).unwrap();

        let model = crate::input::load_model(tmp.path()).unwrap();
        crate::simulation::run(&model, &output_path, /*debug_model=*/ true).unwrap();

        // Read per-asset activity for GASCGT, grouped by time slice
        let mut activities: IndexMap<String, Vec<f64>> = IndexMap::new();
        let mut reader =
            csv::Reader::from_path(output_path.join("debug_dispatch_assets.csv")).unwrap();
        for result in reader.deserialize::<DispatchRow>() {
            let row = result.unwrap();
            if row.milestone_year == 2020
                && row.run_description == "final without candidates"
                && row.process_id == "GASCGT"
                && let Some(act) = row.activity
            {
                activities.entry(row.time_slice).or_default().push(act);
            }
        }

        // Every time slice where at least one GASCGT is active should have equal activities
        for (time_slice, acts) in &activities {
            assert_eq!(
                acts.len(),
                2,
                "Expected 2 GASCGT entries for time slice {time_slice}"
            );
            let [a, b] = acts.as_slice() else {
                unreachable!()
            };
            if *a > 0.0 || *b > 0.0 {
                let diff = (a - b).abs() / a.max(*b);
                assert!(
                    diff < 1e-6,
                    "GASCGT activities not equal in time slice {time_slice}: {a} vs {b}"
                );
            }
        }
    }

    // Verifies that a single GASDRV asset dispatches evenly across time slices within each season.
    // Gas is balanced at season level, so within a season all time slices are unconstrained and
    // should have equal utilisation rate (activity / timeslice_fraction).
    // Expected to fail until timeslice equalisation is implemented.
    #[test]
    fn single_asset_dispatches_evenly_across_timeslices() {
        #[derive(Debug, serde::Deserialize)]
        struct TimeSliceRow {
            season: String,
            time_of_day: String,
            fraction: f64,
        }

        let tmp = ModelPatch::from_example("simple")
            .build_to_tempdir()
            .unwrap();

        // Load time slice fractions so we can compute utilisation rate
        let mut fractions: std::collections::HashMap<String, f64> =
            std::collections::HashMap::new();
        let mut ts_reader = csv::Reader::from_path(tmp.path().join("time_slices.csv")).unwrap();
        for result in ts_reader.deserialize::<TimeSliceRow>() {
            let row = result.unwrap();
            fractions.insert(format!("{}.{}", row.season, row.time_of_day), row.fraction);
        }

        let output_path = tmp.path().join("output");
        fs::create_dir_all(&output_path).unwrap();

        let model = crate::input::load_model(tmp.path()).unwrap();
        crate::simulation::run(&model, &output_path, /*debug_model=*/ true).unwrap();

        // Read GASDRV activity, grouped by season, storing (fraction, activity) pairs
        let mut season_rates: IndexMap<String, Vec<f64>> = IndexMap::new();
        let mut reader =
            csv::Reader::from_path(output_path.join("debug_dispatch_assets.csv")).unwrap();
        for result in reader.deserialize::<DispatchRow>() {
            let row = result.unwrap();
            if row.milestone_year == 2020
                && row.run_description == "final without candidates"
                && row.process_id == "GASDRV"
                && let Some(act) = row.activity
            {
                let fraction = fractions[&row.time_slice];
                let season = row
                    .time_slice
                    .split('.')
                    .next()
                    .unwrap_or(&row.time_slice)
                    .to_owned();
                // Utilisation rate: activity per unit time
                season_rates.entry(season).or_default().push(act / fraction);
            }
        }

        assert!(
            !season_rates.is_empty(),
            "No GASDRV activity found in output"
        );

        // Within each season, every time slice should have equal utilisation rate
        for (season, rates) in &season_rates {
            let max = rates.iter().copied().fold(0.0_f64, f64::max);
            if max <= 0.0 {
                continue;
            }
            for &rate in rates {
                let diff = (rate - max).abs() / max;
                assert!(
                    diff < 1e-6,
                    "GASDRV utilisation rate not equal across time slices in season {season}: {rates:?}"
                );
            }
        }
    }
}
