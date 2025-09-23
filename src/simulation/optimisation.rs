//! Code for performing dispatch optimisation.
//!
//! This is used to calculate commodity flows and prices.
use crate::asset::{Asset, AssetRef};
use crate::commodity::CommodityID;
use crate::model::Model;
use crate::output::DataWriter;
use crate::region::RegionID;
use crate::time_slice::{TimeSliceID, TimeSliceInfo};
use crate::units::{Activity, Flow, Money, MoneyPerActivity, MoneyPerFlow, UnitType};
use anyhow::{Result, anyhow, ensure};
use highs::{HighsModelStatus, RowProblem as Problem, Sense};
use indexmap::IndexMap;
use itertools::{chain, iproduct};
use log::debug;
use std::collections::{HashMap, HashSet};
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

/// A map for easy lookup of variables in the problem.
///
/// The entries are ordered (see [`IndexMap`]).
///
/// We use this data structure for two things:
///
/// 1. In order define constraints for the optimisation
/// 2. To keep track of the combination of parameters that each variable corresponds to, for when we
///    are reading the results of the optimisation.
#[derive(Default)]
pub struct VariableMap(IndexMap<(AssetRef, TimeSliceID), Variable>);

impl VariableMap {
    /// Get the [`Variable`] corresponding to the given parameters.
    fn get(&self, asset: &AssetRef, time_slice: &TimeSliceID) -> Variable {
        let key = (asset.clone(), time_slice.clone());

        *self
            .0
            .get(&key)
            .expect("No variable found for given params")
    }

    /// Iterate over the variable map
    fn iter(&self) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Variable)> {
        self.0
            .iter()
            .map(|((asset, time_slice), var)| (asset, time_slice, *var))
    }
}

/// The solution to the dispatch optimisation problem
#[allow(clippy::struct_field_names)]
pub struct Solution<'a> {
    solution: highs::Solution,
    variables: VariableMap,
    active_asset_var_idx: Range<usize>,
    candidate_asset_var_idx: Range<usize>,
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
        for (asset, time_slice, activity) in self.iter_activity_for_active() {
            for flow in asset.iter_flows() {
                let flow_key = (asset.clone(), flow.commodity.id.clone(), time_slice.clone());
                let flow_value = activity * flow.coeff;
                flows.insert(flow_key, flow_value);
            }
        }

        flows
    }

    /// Activity for each active asset
    pub fn iter_activity(&self) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        self.variables
            .0
            .keys()
            .zip(self.solution.columns())
            .map(|((asset, time_slice), activity)| (asset, time_slice, Activity(*activity)))
    }

    /// Activity for each active asset
    fn iter_activity_for_active(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, Activity)> {
        self.zip_var_keys_with_output(&self.active_asset_var_idx, self.solution.columns())
    }

    /// Reduced costs for candidate assets
    pub fn iter_reduced_costs_for_candidates(
        &self,
    ) -> impl Iterator<Item = (&AssetRef, &TimeSliceID, MoneyPerActivity)> {
        self.zip_var_keys_with_output(&self.candidate_asset_var_idx, self.solution.dual_columns())
    }

    /// Keys and dual values for commodity balance constraints.
    pub fn iter_commodity_balance_duals(
        &self,
    ) -> impl Iterator<Item = (&CommodityID, &RegionID, &TimeSliceID, MoneyPerFlow)> {
        // Each commodity balance constraint applies to a particular time slice
        // selection (depending on time slice level). Where this covers multiple timeslices,
        // we return the same dual for each individual timeslice.
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
        let keys = self.variables.0.keys().skip(variable_idx.start);
        assert!(keys.len() >= variable_idx.len());

        keys.zip(output[variable_idx.clone()].iter())
            .map(|((asset, time_slice), value)| (asset, time_slice, T::new(*value)))
    }
}

/// Try to solve the model, returning an error if the model is incoherent or result is non-optimal
pub fn solve_optimal(model: highs::Model) -> Result<highs::SolvedModel> {
    let solved = model
        .try_solve()
        .map_err(|err| anyhow!("Incoherent model: {err:?}"))?;

    let status = solved.status();
    ensure!(
        status == HighsModelStatus::Optimal,
        "Could not find optimal result for model: {status:?}"
    );

    Ok(solved)
}

/// Sanity check for input prices.
///
/// Input prices should only be provided for commodities for which there will be no commodity
/// balance constraint.
fn check_input_prices(
    input_prices: &HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>,
    commodities: &[CommodityID],
) {
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
/// For a detailed description, please see the [dispatch optimisation formulation][1].
///
/// [1]: https://energysystemsmodellinglab.github.io/MUSE_2.0/model/dispatch_optimisation.html
pub struct DispatchRun<'model, 'run> {
    model: &'model Model,
    existing_assets: &'run [AssetRef],
    candidate_assets: &'run [AssetRef],
    commodities: &'run [CommodityID],
    input_prices: Option<&'run HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>>,
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
    pub fn with_input_prices(
        self,
        input_prices: &'run HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>,
    ) -> Self {
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
        // Set up problem
        let mut problem = Problem::default();
        let mut variables = VariableMap::default();
        let active_asset_var_idx = add_variables(
            &mut problem,
            &mut variables,
            &self.model.time_slice_info,
            self.input_prices,
            self.existing_assets,
            self.year,
        );
        let candidate_asset_var_idx = add_variables(
            &mut problem,
            &mut variables,
            &self.model.time_slice_info,
            self.input_prices,
            self.candidate_assets,
            self.year,
        );

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
            active_asset_var_idx,
            candidate_asset_var_idx,
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
/// * `variables` - The variable map
/// * `time_slice_info` - Information about assets
/// * `input_prices` - Optional explicit prices for input commodities
/// * `assets` - Assets to include
/// * `year` - Current milestone year
fn add_variables(
    problem: &mut Problem,
    variables: &mut VariableMap,
    time_slice_info: &TimeSliceInfo,
    input_prices: Option<&HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>>,
    assets: &[AssetRef],
    year: u32,
) -> Range<usize> {
    // This line **must** come before we add more variables
    let start = problem.num_cols();

    for (asset, time_slice) in iproduct!(assets.iter(), time_slice_info.iter_ids()) {
        let coeff = calculate_cost_coefficient(asset, year, time_slice, input_prices);
        let var = problem.add_column(coeff.value(), 0.0..);
        let key = (asset.clone(), time_slice.clone());
        let existing = variables.0.insert(key, var).is_some();
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
    input_prices: Option<&HashMap<(CommodityID, RegionID, TimeSliceID), MoneyPerFlow>>,
) -> MoneyPerActivity {
    let opex = asset.get_operating_cost(year, time_slice);
    let input_cost = input_prices
        .map(|prices| asset.get_input_cost_from_prices(prices, time_slice))
        .unwrap_or_default();
    opex + input_cost
}
