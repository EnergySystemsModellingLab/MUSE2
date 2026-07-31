//! Assets are instances of a process which are owned and invested in by agents.
use crate::agent::AgentID;
use crate::commodity::{CommodityID, CommodityType};
use crate::finance::annual_capital_cost;
use crate::process::{
    ActivityLimits, FlowDirection, Process, ProcessFlow, ProcessID, ProcessParameter,
};
use crate::region::RegionID;
use crate::simulation::PriceMap;
use crate::time_slice::{TimeSliceID, TimeSliceSelection};
use crate::units::{
    Activity, ActivityPerCapacity, Capacity, Dimensionless, FlowPerActivity, MoneyPerActivity,
    MoneyPerCapacity, MoneyPerFlow, Year,
};
use anyhow::{Context, Result, ensure};
use indexmap::IndexMap;
use itertools::Itertools;
use log::debug;
use map_macro::vec_deque;
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
use std::ops::RangeInclusive;
use std::rc::Rc;

mod capacity;
pub use capacity::AssetCapacity;
mod pool;
pub use pool::AssetPool;

/// A unique identifier for an asset
#[derive(
    Clone,
    Copy,
    Debug,
    derive_more::Display,
    Eq,
    Hash,
    Ord,
    PartialEq,
    PartialOrd,
    Deserialize,
    Serialize,
)]
pub struct AssetID(u32);

/// Indicates the year and number of units mothballed
#[derive(PartialEq, Debug, Clone)]
pub struct MothballEvent {
    year: u32,
    num_units: u32,
}

/// The state of an asset
///
/// New assets are created as either `Ready` or `Candidate` assets. `Ready` assets from the input
/// data have a fixed capacity and capital costs already accounted for, whereas `Candidate` assets'
/// capital costs are not yet accounted for, and their capacity is determined by the investment
/// algorithm.
///
/// `Ready` assets can be converted to `Commissioned` assets by calling the `commission` method (or
/// via pool operations that commission ready assets).
#[derive(Clone, Debug, PartialEq, strum::Display)]
pub enum AssetState {
    /// The asset has been commissioned
    Commissioned {
        /// The ID of the asset
        id: AssetID,
        /// The ID of the agent that owns the asset
        agent_id: AgentID,
        /// Years in which all of some of the asset was mothballed.
        ///
        /// Invariants: This **must** be sorted by year with older years first and the total number
        /// of units must not exceed the total for this asset.
        mothball_events: VecDeque<MothballEvent>,
    },
    /// The asset is ready for investment, but not yet confirmed
    Ready {
        /// The ID of the agent that would own the asset
        agent_id: AgentID,
        /// The reason why this asset is due to be commissioned
        commission_reason: &'static str,
    },
    /// The asset is a candidate for investment but has not yet been selected by an agent
    Candidate,
}

/// An asset controlled by an agent.
#[derive(Clone)]
pub struct Asset {
    /// The status of the asset
    state: AssetState,
    /// The [`Process`] that this asset corresponds to
    process: Rc<Process>,
    /// Activity limits for this asset
    activity_limits: Rc<ActivityLimits>,
    /// The commodity flows for this asset
    flows: Rc<IndexMap<CommodityID, ProcessFlow>>,
    /// The [`ProcessParameter`] corresponding to the asset's region and commission year
    process_parameter: Rc<ProcessParameter>,
    /// The region in which the asset is located
    region_id: RegionID,
    /// Capacity of asset (for candidates this is a hypothetical capacity which may be altered)
    capacity: Cell<AssetCapacity>,
    /// The year the asset was/will be commissioned
    commission_year: u32,
    /// The maximum year that the asset could be decommissioned
    max_decommission_year: u32,
}

impl Asset {
    /// Create a new candidate asset
    pub fn new_candidate(
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
    ) -> Result<Self> {
        let unit_size = process.unit_size;
        Self::new_with_state(
            AssetState::Candidate,
            process,
            region_id,
            AssetCapacity::from_capacity(capacity, unit_size),
            commission_year,
            None,
        )
    }

    /// Create a new candidate for use in dispatch runs
    ///
    /// These candidates will have a single continuous capacity specified by the model parameter
    /// `candidate_asset_capacity`, regardless of whether the underlying process is divisible or
    /// not.
    pub fn new_candidate_for_dispatch(
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
    ) -> Result<Self> {
        Self::new_with_state(
            AssetState::Candidate,
            process,
            region_id,
            AssetCapacity::Continuous(capacity),
            commission_year,
            None,
        )
    }

    /// Create a new candidate asset from a commissioned asset
    pub fn new_candidate_from_commissioned(asset: &Asset) -> Self {
        assert!(asset.is_commissioned(), "Asset must be commissioned");

        Self {
            state: AssetState::Candidate,
            ..asset.clone()
        }
    }

    /// Create a new ready asset
    ///
    /// This is only used for testing. In the real program, Ready assets can only be created from
    /// Candidate assets by calling `select_candidate_for_investment`.
    #[cfg(test)]
    pub fn new_ready(
        agent_id: AgentID,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
    ) -> Result<Self> {
        let unit_size = process.unit_size;
        Self::new_with_state(
            AssetState::Ready {
                agent_id,
                commission_reason: "selected",
            },
            process,
            region_id,
            AssetCapacity::from_capacity(capacity, unit_size),
            commission_year,
            None,
        )
    }

    /// Create a new commissioned asset
    ///
    /// This is only used for testing. WARNING: These assets always have an ID of zero, so can
    /// create hash collisions. Use with care.
    #[cfg(test)]
    pub fn new_commissioned(
        agent_id: AgentID,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
    ) -> Result<Self> {
        let unit_size = process.unit_size;
        Self::new_with_state(
            AssetState::Commissioned {
                id: AssetID(0),
                agent_id,
                mothball_events: vec_deque![],
            },
            process,
            region_id,
            AssetCapacity::from_capacity(capacity, unit_size),
            commission_year,
            None,
        )
    }

    /// Private helper to create an asset with the given state
    fn new_with_state(
        state: AssetState,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: AssetCapacity,
        commission_year: u32,
        max_decommission_year: Option<u32>,
    ) -> Result<Self> {
        check_region_year_valid_for_process(&process, &region_id, commission_year)?;
        ensure!(
            capacity.total_capacity() >= Capacity(0.0),
            "Capacity must be non-negative"
        );

        // There should be activity limits, commodity flows and process parameters for all
        // **milestone** years, but it is possible to have assets that are commissioned before the
        // simulation start from assets.csv. We check for the presence of the params lazily to
        // prevent users having to supply them for all the possible valid years before the time
        // horizon.
        let key = (region_id.clone(), commission_year);
        let activity_limits = process
            .activity_limits
            .get(&key)
            .with_context(|| {
                format!(
                    "No process availabilities supplied for process {} in region {} in year {}. \
                    You should update process_availabilities.csv.",
                    process.id, region_id, commission_year
                )
            })?
            .clone();
        let flows = process
            .flows
            .get(&key)
            .with_context(|| {
                format!(
                    "No commodity flows supplied for process {} in region {} in year {}. \
                    You should update process_flows.csv.",
                    process.id, region_id, commission_year
                )
            })?
            .clone();
        let process_parameter = process
            .parameters
            .get(&key)
            .with_context(|| {
                format!(
                    "No process parameters supplied for process {} in region {} in year {}. \
                    You should update process_parameters.csv.",
                    process.id, region_id, commission_year
                )
            })?
            .clone();

        let max_decommission_year =
            max_decommission_year.unwrap_or(commission_year + process_parameter.lifetime);
        ensure!(
            max_decommission_year > commission_year,
            "Max decommission year must be greater than commission year"
        );

        Ok(Self {
            state,
            process,
            activity_limits,
            flows,
            process_parameter,
            region_id,
            capacity: Cell::new(capacity),
            commission_year,
            max_decommission_year,
        })
    }

    /// Get the state of this asset
    pub fn state(&self) -> &AssetState {
        &self.state
    }

    /// The process parameter for this asset
    pub fn process_parameter(&self) -> &ProcessParameter {
        &self.process_parameter
    }

    /// The last year in which this asset should be decommissioned
    pub fn max_decommission_year(&self) -> u32 {
        self.max_decommission_year
    }

    /// Get the activity limits per unit of capacity for this asset in a particular time slice
    pub fn get_activity_per_capacity_limits(
        &self,
        time_slice: &TimeSliceID,
    ) -> RangeInclusive<ActivityPerCapacity> {
        let limits = &self.activity_limits.get_limit_for_time_slice(time_slice);
        let cap2act = self.process.capacity_to_activity;
        (cap2act * *limits.start())..=(cap2act * *limits.end())
    }

    /// Get the activity limits for this asset for a given time slice selection
    pub fn get_activity_limits_for_selection(
        &self,
        time_slice_selection: &TimeSliceSelection,
    ) -> RangeInclusive<Activity> {
        let activity_per_capacity_limits = self.activity_limits.get_limit(time_slice_selection);
        let cap2act = self.process.capacity_to_activity;
        let max_activity = self.total_capacity() * cap2act;
        let lb = max_activity * *activity_per_capacity_limits.start();
        let ub = max_activity * *activity_per_capacity_limits.end();
        lb..=ub
    }

    /// Get the activity limits per unit of capacity for this asset for a given time slice selection
    pub fn get_activity_per_capacity_limits_for_selection(
        &self,
        time_slice_selection: &TimeSliceSelection,
    ) -> RangeInclusive<ActivityPerCapacity> {
        let limits = self.activity_limits.get_limit(time_slice_selection);
        let cap2act = self.process.capacity_to_activity;
        (cap2act * *limits.start())..=(cap2act * *limits.end())
    }

    /// Iterate over activity limits for this asset
    pub fn iter_activity_limits(
        &self,
    ) -> impl Iterator<Item = (TimeSliceSelection, RangeInclusive<Activity>)> + '_ {
        let max_act = self.max_activity();
        self.activity_limits
            .iter_limits()
            .map(move |(ts_sel, limit)| {
                (
                    ts_sel,
                    (max_act * *limit.start())..=(max_act * *limit.end()),
                )
            })
    }

    /// Iterate over activity per capacity limits for this asset
    pub fn iter_activity_per_capacity_limits(
        &self,
    ) -> impl Iterator<Item = (TimeSliceSelection, RangeInclusive<ActivityPerCapacity>)> + '_ {
        let cap2act = self.process.capacity_to_activity;
        self.activity_limits
            .iter_limits()
            .map(move |(ts_sel, limit)| {
                (
                    ts_sel,
                    (cap2act * *limit.start())..=(cap2act * *limit.end()),
                )
            })
    }

    /// Gets the total SED/SVD output per unit of activity for this asset
    ///
    /// Note: Since we are summing coefficients from different commodities, this ONLY makes sense
    /// if these commodities have the same units (e.g., all in PJ). Users are currently not made to
    /// give units for commodities, so we cannot possibly enforce this. Something to potentially
    /// address in future.
    pub fn get_total_output_per_activity(&self) -> FlowPerActivity {
        self.iter_output_flows().map(|flow| flow.coeff).sum()
    }

    /// Get the operating cost for this asset in a given year and time slice
    pub fn get_operating_cost(&self, year: u32, time_slice: &TimeSliceID) -> MoneyPerActivity {
        // The cost for all commodity flows (including levies/incentives)
        let flows_cost = self
            .iter_flows()
            .map(|flow| flow.get_total_cost_per_activity(&self.region_id, year, time_slice))
            .sum();

        self.process_parameter.variable_operating_cost + flows_cost
    }

    /// Get the total revenue from all flows for this asset.
    ///
    /// If a price is missing, it is assumed to be zero.
    pub fn get_revenue_from_flows(
        &self,
        prices: &PriceMap,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        self.get_revenue_from_flows_with_filter(prices, time_slice, |_| true)
    }

    /// Get the total revenue from all flows excluding the primary output.
    ///
    /// If a price is missing, it is assumed to be zero.
    pub fn get_revenue_from_flows_excluding_primary(
        &self,
        prices: &PriceMap,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        let excluded_commodity = self.primary_output().map(|flow| &flow.commodity.id);

        self.get_revenue_from_flows_with_filter(prices, time_slice, |flow| {
            excluded_commodity.is_none_or(|commodity_id| commodity_id != &flow.commodity.id)
        })
    }

    /// Get the total cost of purchasing input commodities per unit of activity for this asset.
    ///
    /// If a price is missing, there is assumed to be no cost.
    pub fn get_input_cost_from_prices(
        &self,
        prices: &PriceMap,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        // Revenues of input flows are negative costs, so we negate the result
        -self.get_revenue_from_flows_with_filter(prices, time_slice, |x| {
            x.direction() == FlowDirection::Input
        })
    }

    /// Get the total revenue from a subset of flows.
    ///
    /// Takes a function as an argument to filter the flows. If a price is missing, it is assumed to
    /// be zero.
    fn get_revenue_from_flows_with_filter<F>(
        &self,
        prices: &PriceMap,
        time_slice: &TimeSliceID,
        mut filter_for_flows: F,
    ) -> MoneyPerActivity
    where
        F: FnMut(&ProcessFlow) -> bool,
    {
        self.iter_flows()
            .filter(|flow| filter_for_flows(flow))
            .map(|flow| {
                flow.coeff
                    * prices
                        .get(&flow.commodity.id, &self.region_id, time_slice)
                        .unwrap_or(MoneyPerFlow(0.0))
            })
            .sum()
    }

    /// Get the generic activity cost per unit of activity for this asset.
    ///
    /// These are all activity-related costs that are not associated with specific SED/SVD outputs.
    /// Includes levies, flow costs, costs of inputs and variable operating costs
    fn get_generic_activity_cost(
        &self,
        prices: &PriceMap,
        year: u32,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        // The cost of purchasing input commodities
        let cost_of_inputs = self.get_input_cost_from_prices(prices, time_slice);

        // Flow costs/levies for all flows except SED/SVD outputs
        let excludes_sed_svd_output = |flow: &&ProcessFlow| {
            !(flow.direction() == FlowDirection::Output
                && matches!(
                    flow.commodity.kind,
                    CommodityType::SupplyEqualsDemand | CommodityType::ServiceDemand
                ))
        };
        let flow_costs = self
            .iter_flows()
            .filter(excludes_sed_svd_output)
            .map(|flow| flow.get_total_cost_per_activity(&self.region_id, year, time_slice))
            .sum();

        cost_of_inputs + flow_costs + self.process_parameter.variable_operating_cost
    }

    /// Iterate over marginal costs for a filtered set of SED/SVD output commodities for this asset
    ///
    /// For each SED/SVD output commodity, the marginal cost is calculated as the sum of:
    /// - Generic activity costs (variable operating costs, cost of purchasing inputs, plus all
    ///   levies and flow costs not associated with specific SED/SVD outputs), which are
    ///   shared equally over all SED/SVD outputs
    /// - Production levies and flow costs for the specific SED/SVD output commodity
    pub fn iter_marginal_costs_with_filter<'a>(
        &'a self,
        prices: &'a PriceMap,
        year: u32,
        time_slice: &'a TimeSliceID,
        filter: impl Fn(&CommodityID) -> bool + 'a,
    ) -> Box<dyn Iterator<Item = (CommodityID, MoneyPerFlow)> + 'a> {
        // Iterator over SED/SVD output flows matching the filter
        let mut output_flows_iter = self
            .iter_output_flows()
            .filter(move |flow| filter(&flow.commodity.id))
            .peekable();

        // If there are no output flows after filtering, return an empty iterator
        if output_flows_iter.peek().is_none() {
            return Box::new(std::iter::empty::<(CommodityID, MoneyPerFlow)>());
        }

        // Calculate generic activity costs.
        // This is all activity costs not associated with specific SED/SVD outputs, which will get
        // shared equally over all SED/SVD outputs. Includes levies, flow costs, costs of inputs and
        // variable operating costs
        let generic_activity_cost = self.get_generic_activity_cost(prices, year, time_slice);

        // Share generic activity costs equally over all SED/SVD outputs
        // We sum the output coefficients of all SED/SVD commodities to get total output, then
        // divide costs by this total output to get the generic cost per unit of output.
        // Note: only works if all SED/SVD outputs have the same units - not currently checked!
        let total_output_per_activity = self.get_total_output_per_activity();
        assert!(total_output_per_activity > FlowPerActivity::EPSILON); // input checks should guarantee this
        let generic_cost_per_flow = generic_activity_cost / total_output_per_activity;

        // Iterate over SED/SVD output flows
        Box::new(output_flows_iter.map(move |flow| {
            // Get the costs for this specific commodity flow
            let commodity_specific_costs_per_flow =
                flow.get_total_cost_per_flow(&self.region_id, year, time_slice);

            // Add these to the generic costs to get total cost for this commodity
            let marginal_cost = generic_cost_per_flow + commodity_specific_costs_per_flow;
            (flow.commodity.id.clone(), marginal_cost)
        }))
    }

    /// Iterate over marginal costs for all SED/SVD output commodities for this asset
    ///
    /// See `iter_marginal_costs_with_filter` for details.
    pub fn iter_marginal_costs<'a>(
        &'a self,
        prices: &'a PriceMap,
        year: u32,
        time_slice: &'a TimeSliceID,
    ) -> Box<dyn Iterator<Item = (CommodityID, MoneyPerFlow)> + 'a> {
        self.iter_marginal_costs_with_filter(prices, year, time_slice, move |_| true)
    }

    /// Get the annual capital cost per unit of capacity for this asset
    pub fn get_annual_capital_cost_per_capacity(&self) -> MoneyPerCapacity {
        let capital_cost = self.process_parameter.capital_cost;
        let lifetime = self.process_parameter.lifetime;
        let discount_rate = self.process_parameter.discount_rate;
        annual_capital_cost(capital_cost, lifetime, discount_rate)
    }

    /// Get the annual fixed costs (AFC) per unit of activity for this asset
    ///
    /// Total capital costs and fixed opex are shared equally over the year in accordance with the
    /// annual activity.
    pub fn get_annual_fixed_costs_per_activity(
        &self,
        annual_activity: Activity,
    ) -> MoneyPerActivity {
        let annual_capital_cost_per_capacity = self.get_annual_capital_cost_per_capacity();
        let annual_fixed_opex = self.process_parameter.fixed_operating_cost * Year(1.0);
        let total_annual_fixed_costs =
            (annual_capital_cost_per_capacity + annual_fixed_opex) * self.total_capacity();
        assert!(
            annual_activity > Activity::EPSILON,
            "Cannot calculate annual fixed costs per activity for an asset with zero annual activity"
        );
        total_annual_fixed_costs / annual_activity
    }

    /// Get the annual fixed costs (AFC) per unit of output flow for this asset
    ///
    /// Total capital costs and fixed opex are shared equally across all output flows in accordance
    /// with the annual activity and total output per unit of activity.
    pub fn get_annual_fixed_costs_per_flow(&self, annual_activity: Activity) -> MoneyPerFlow {
        let annual_fixed_costs_per_activity =
            self.get_annual_fixed_costs_per_activity(annual_activity);
        let total_output_per_activity = self.get_total_output_per_activity();
        assert!(total_output_per_activity > FlowPerActivity::EPSILON); // input checks should guarantee this
        annual_fixed_costs_per_activity / total_output_per_activity
    }

    /// Maximum activity for this asset
    pub fn max_activity(&self) -> Activity {
        self.total_capacity() * self.process.capacity_to_activity
    }

    /// Get a specific process flow
    pub fn get_flow(&self, commodity_id: &CommodityID) -> Option<&ProcessFlow> {
        self.flows.get(commodity_id)
    }

    /// Iterate over the asset's flows
    pub fn iter_flows(&self) -> impl Iterator<Item = &ProcessFlow> {
        self.flows.values()
    }

    /// Iterate over the asset's output SED/SVD flows
    pub fn iter_output_flows(&self) -> impl Iterator<Item = &ProcessFlow> {
        self.flows.values().filter(|flow| {
            flow.direction() == FlowDirection::Output
                && matches!(
                    flow.commodity.kind,
                    CommodityType::SupplyEqualsDemand | CommodityType::ServiceDemand
                )
        })
    }

    /// Get the primary output flow (if any) for this asset
    pub fn primary_output(&self) -> Option<&ProcessFlow> {
        self.process
            .primary_output
            .as_ref()
            .map(|commodity_id| &self.flows[commodity_id])
    }

    /// Get the primary output commodity (if any) for this asset
    pub fn primary_output_commodity(&self) -> Option<&CommodityID> {
        self.process.primary_output.as_ref()
    }

    /// Whether this asset has been commissioned
    pub fn is_commissioned(&self) -> bool {
        matches!(&self.state, AssetState::Commissioned { .. })
    }

    /// Whether this asset is a candidate
    pub fn is_candidate(&self) -> bool {
        matches!(&self.state, AssetState::Candidate)
    }

    /// Get the commission year for this asset
    pub fn commission_year(&self) -> u32 {
        self.commission_year
    }

    /// Get the region ID for this asset
    pub fn region_id(&self) -> &RegionID {
        &self.region_id
    }

    /// Get the process for this asset
    pub fn process(&self) -> &Process {
        &self.process
    }

    /// Get the process ID for this asset
    pub fn process_id(&self) -> &ProcessID {
        &self.process.id
    }

    /// Get the ID for this asset
    pub fn id(&self) -> Option<AssetID> {
        match &self.state {
            AssetState::Commissioned { id, .. } => Some(*id),
            _ => None,
        }
    }

    /// Whether this asset is divisible
    pub fn is_divisible(&self) -> bool {
        matches!(self.capacity.get(), AssetCapacity::Discrete { .. })
    }

    /// Get the agent ID for this asset, if any
    pub fn agent_id(&self) -> Option<&AgentID> {
        match &self.state {
            AssetState::Commissioned { agent_id, .. } | AssetState::Ready { agent_id, .. } => {
                Some(agent_id)
            }
            AssetState::Candidate => None,
        }
    }

    /// Get the capacity for this asset
    pub fn capacity(&self) -> AssetCapacity {
        self.capacity.get()
    }

    /// Get the total capacity for this asset
    pub fn total_capacity(&self) -> Capacity {
        self.capacity().total_capacity()
    }

    /// Set the capacity of this asset.
    ///
    /// Note that this should be done with care!
    pub fn set_capacity(&mut self, capacity: AssetCapacity) {
        assert!(
            capacity.total_capacity() >= Capacity(0.0),
            "Capacity must be >= 0"
        );
        self.capacity().assert_same_type(capacity);
        assert!(
            self.get_num_mothballed_units() <= capacity.n_units().unwrap_or(1),
            "Cannot set capacity to a smaller number of units than are currently mothballed"
        );

        // As `capacity` is a `Cell`, we don't actually need a `mut` ref to `self`, but allowing for
        // changing the capacity of immutable refs would be potentially dangerous
        self.capacity.set(capacity);
    }

    /// Increase the capacity for this asset
    pub fn increase_capacity(&mut self, capacity: AssetCapacity) {
        assert!(
            capacity.total_capacity() > Capacity(0.0),
            "Capacity increase must be positive"
        );

        // As `capacity` is a `Cell`, we don't actually need a `mut` ref to `self`, but allowing for
        // changing the capacity of immutable refs would be potentially dangerous
        self.capacity.update(|c| c + capacity);
    }

    /// Commission the asset.
    ///
    /// Only assets with an [`AssetState`] of `Ready` can be commissioned. If the asset's state is
    /// something else, this function will panic.
    ///
    /// # Arguments
    ///
    /// * `id` - The ID to give the newly commissioned asset
    fn commission(&mut self, id: AssetID) {
        let (agent_id, reason) = match &self.state {
            AssetState::Ready {
                agent_id,
                commission_reason,
            } => (agent_id, commission_reason),
            state => panic!("Assets with state {state} cannot be commissioned"),
        };
        debug!(
            "Commissioning '{}' asset (ID: {}, capacity: {}) for agent '{}' (reason: {})",
            self.process_id(),
            id,
            self.total_capacity(),
            agent_id,
            reason
        );
        self.state = AssetState::Commissioned {
            id,
            agent_id: agent_id.clone(),
            mothball_events: vec_deque![],
        };
    }

    /// Select a Candidate asset for investment, converting it to a Ready state
    pub fn select_candidate_for_investment(&mut self, agent_id: AgentID) {
        assert!(
            self.is_candidate(),
            "select_candidate_for_investment can only be called on Candidate assets"
        );
        check_capacity_valid_for_asset(self.total_capacity()).unwrap();
        self.state = AssetState::Ready {
            agent_id,
            commission_reason: "selected",
        };
    }

    /// Get the mothball events for this asset, if commissioned
    fn get_mothball_events(&self) -> Option<&VecDeque<MothballEvent>> {
        match &self.state {
            AssetState::Commissioned {
                mothball_events, ..
            } => Some(mothball_events),
            _ => None,
        }
    }

    /// Get the mothball events (mutably) for this asset, if commissioned
    fn get_mothball_events_mut(&mut self) -> Option<&mut VecDeque<MothballEvent>> {
        match &mut self.state {
            AssetState::Commissioned {
                mothball_events, ..
            } => Some(mothball_events),
            _ => None,
        }
    }

    /// Whether this asset has any units mothballed
    pub fn has_any_mothballed_units(&self) -> bool {
        self.get_mothball_events()
            .is_some_and(|events| !events.is_empty())
    }

    /// Get the number of units which are mothballed.
    ///
    /// For non-commissioned assets, this always returns zero.
    pub fn get_num_mothballed_units(&self) -> u32 {
        let Some(events) = self.get_mothball_events() else {
            return 0;
        };

        events.iter().map(|event| event.num_units).sum()
    }

    /// Get the remaining number of units that are not mothballed.
    ///
    /// For non-commissioned assets, this always returns the total number of units.
    pub fn get_num_nonmothballed_units(&self) -> u32 {
        self.num_units() - self.get_num_mothballed_units()
    }

    /// The number of units this asset represents
    ///
    /// If divisible, returns the total number of units, otherwise returns one.
    pub fn num_units(&self) -> u32 {
        self.capacity().n_units().unwrap_or(1)
    }

    /// Get the unit size for this asset's capacity (if any)
    pub fn unit_size(&self) -> Option<Capacity> {
        match self.capacity() {
            AssetCapacity::Discrete(_, size) => Some(size),
            AssetCapacity::Continuous(_) => None,
        }
    }

    /// For non-commissioned assets, get the maximum capacity permitted to be installed based on the
    /// investment constraints for the asset's process.
    ///
    /// The limit is taken from the process's investment constraints for the asset's region and
    /// commission year, and the portion of the commodity demand being considered.
    ///
    /// For divisible assets, the returned capacity will be rounded down to the nearest multiple of
    /// the asset's unit size.
    pub fn max_installable_capacity(
        &self,
        commodity_portion: Dimensionless,
    ) -> Option<AssetCapacity> {
        assert!(
            !self.is_commissioned(),
            "max_installable_capacity can only be called on uncommissioned assets"
        );
        assert!(
            commodity_portion >= Dimensionless(0.0) && commodity_portion <= Dimensionless(1.0),
            "commodity_portion must be between 0 and 1 inclusive"
        );

        self.process
            .investment_constraints
            .get(&(self.region_id.clone(), self.commission_year))
            .and_then(|c| c.get_addition_limit().map(|l| l * commodity_portion))
            .map(|limit| AssetCapacity::from_capacity_floor(limit, self.unit_size()))
    }
}

#[allow(clippy::missing_fields_in_debug)]
impl std::fmt::Debug for Asset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Asset")
            .field("state", &self.state)
            .field("process_id", &self.process_id())
            .field("region_id", &self.region_id)
            .field("capacity", &self.total_capacity())
            .field("commission_year", &self.commission_year)
            .finish()
    }
}

/// Whether the process operates in the specified region and year
pub fn check_region_year_valid_for_process(
    process: &Process,
    region_id: &RegionID,
    year: u32,
) -> Result<()> {
    ensure!(
        process.regions.contains(region_id),
        "Process {} does not operate in region {}",
        process.id,
        region_id
    );
    ensure!(
        process.active_for_year(year),
        "Process {} does not operate in the year {}",
        process.id,
        year
    );
    Ok(())
}

/// An asset defined by the user in the assets input file
#[derive(Clone, Debug, PartialEq, derive_more::Deref, derive_more::Into)]
pub struct UserAsset(#[deref(forward)] AssetRef);

impl UserAsset {
    /// Create a new [`UserAsset`]
    pub fn new(
        agent_id: AgentID,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
        max_decommission_year: Option<u32>,
    ) -> Result<Self> {
        check_capacity_valid_for_asset(capacity)?;
        let unit_size = process.unit_size;
        let asset = Asset::new_with_state(
            AssetState::Ready {
                agent_id,
                commission_reason: "user input",
            },
            process,
            region_id,
            AssetCapacity::from_capacity(capacity, unit_size),
            commission_year,
            max_decommission_year,
        )?;

        Ok(Self(asset.into()))
    }
}

#[cfg(test)]
impl From<Asset> for UserAsset {
    fn from(asset: Asset) -> Self {
        assert!(
            matches!(asset.state, AssetState::Ready { .. }),
            "User assets must be in Ready state"
        );
        Self(asset.into())
    }
}

/// Whether the specified value is a valid capacity for an asset
pub fn check_capacity_valid_for_asset(capacity: Capacity) -> Result<()> {
    ensure!(
        capacity.is_finite() && capacity > Capacity(0.0),
        "Capacity must be a finite, positive number"
    );
    Ok(())
}

/// Log that the specified number of units has been decommissioned for the given asset
fn log_decommissioning(asset: &Asset, num_units: u32, reason: &str) {
    let (id, agent_id) = match &asset.state {
        AssetState::Commissioned { id, agent_id, .. } => (*id, agent_id.clone()),
        _ => panic!("Cannot decommission an asset that hasn't been commissioned"),
    };
    debug!(
        "Decommissioning {}/{} units of '{}' asset (ID: {}) for agent '{}' (reason: {})",
        num_units,
        asset.num_units(),
        asset.process_id(),
        id,
        agent_id,
        reason
    );
}

/// A wrapper containing a reference-counted [`Asset`].
///
/// [`AssetRef`] implements equality, ordering, and hashing using an [`AssetID`], if available, but
/// otherwise using a combination of other fields which should be unique at all the relevant points
/// in the simulation.
#[derive(Clone, Debug, derive_more::Deref, derive_more::From, derive_more::Into)]
pub struct AssetRef(#[deref(forward)] Rc<Asset>);

impl AssetRef {
    /// Make a mutable reference to the underlying [`Asset`]
    pub fn make_mut(&mut self) -> &mut Asset {
        Rc::make_mut(&mut self.0)
    }

    /// Get a representation of this [`AssetRef`] that can be used for comparisons
    fn get_asset_cmp(&self) -> AssetCmp<'_> {
        if let Some(id) = self.id() {
            AssetCmp::WithID(id)
        } else {
            AssetCmp::WithoutID((
                self.process_id(),
                self.region_id(),
                self.commission_year,
                self.agent_id(),
            ))
        }
    }

    /// Get an [`AssetRef`] representing a subset of this asset's units.
    ///
    /// For non-divisible assets, `new_num_units` must be one. If some of the asset's units are
    /// mothballed, these are discarded before non-mothballed units. For example, if an asset has
    /// seven units of which four are mothballed and we are reducing the number of units to four,
    /// the new asset will have one mothballed unit.
    ///
    /// # Panics
    ///
    /// Panics if `new_num_units` is zero or exceeds the total capacity of this asset.
    pub fn with_subset_of_units(self, new_num_units: u32) -> Self {
        if new_num_units == self.num_units() {
            return self;
        }

        assert!(new_num_units > 0, "Cannot make an asset with zero units");

        let (max_num_units, unit_size) = match self.capacity() {
            AssetCapacity::Discrete(max_num_units, unit_size) => (max_num_units, unit_size),
            AssetCapacity::Continuous(_) => {
                panic!("Non-divisible assets can only have one unit");
            }
        };

        assert!(
            new_num_units <= max_num_units,
            "Cannot make an asset with more units than original"
        );

        // Make a new Asset with fewer units. If there are more mothballed units than the new asset
        // will have, we reduce this number to avoid there being more mothballed units than the new
        // asset has, which would be a logic error. We discard mothballed before non-mothballed
        // units.
        let new_num_mothballed = new_num_units.saturating_sub(self.get_num_nonmothballed_units());
        let mut asset = self.with_mothballed_units(new_num_mothballed, None);
        asset
            .make_mut()
            .set_capacity(AssetCapacity::Discrete(new_num_units, unit_size));
        asset
    }

    /// Decommission this asset
    fn decommission(self, reason: &str) {
        log_decommissioning(&self, self.num_units(), reason);
    }

    /// Decommission any units that were mothballed at least `mothball_years` ago.
    ///
    /// If the asset still has some units remaining, it is returned, else None.
    fn with_decommission_mothballed(self, year: u32, mothball_years: u32) -> Option<Self> {
        let events = self
            .get_mothball_events()
            .expect("Can only decommission mothballed units in commissioned assets");

        // Mothball events are ordered oldest-first, so this sums the units mothballed longest ago
        let units_to_remove: u32 = events
            .iter()
            .take_while(|event| event.year <= year.saturating_sub(mothball_years))
            .map(|event| event.num_units)
            .sum();
        if units_to_remove == 0 {
            // Nothing to do. Return self unmodified.
            return Some(self);
        }

        let reason = format!(
            "The asset has not been used for the set mothball years ({mothball_years} years)."
        );
        let new_num_units = self.num_units() - units_to_remove;
        if new_num_units == 0 {
            self.decommission(&reason);
            return None;
        }

        // `with_subset_of_units` discards the oldest mothballed units first, which are exactly the
        // ones being decommissioned here.
        log_decommissioning(&self, units_to_remove, &reason);
        Some(self.with_subset_of_units(new_num_units))
    }

    /// Return a new [`AssetRef`] with the specified number of units mothballed.
    ///
    /// If `num_units` equals the number of already mothballed units, the original asset is
    /// returned. If additional units may be mothballed, a value must be provided for `year`.
    ///
    /// # Panics
    ///
    /// Panics if attempting to mothball more units than the asset represents or if attempting to
    /// change the number of mothballed units for a non-commissioned asset.
    pub fn with_mothballed_units(mut self, num_units: u32, year: Option<u32>) -> Self {
        if num_units == 0 {
            // Small optimisation
            return self.with_no_mothballed_units();
        }

        let num_already_mothballed = self.get_num_mothballed_units();
        if num_units == num_already_mothballed {
            // Nothing to do. Return self unmodified.
            return self;
        }

        assert!(
            num_units <= self.num_units(),
            "Cannot mothball more units than asset represents"
        );

        // Now that we know we have to modify self, call make_mut().
        let events = self.make_mut().get_mothball_events_mut().expect(
            "Cannot change number of mothballed units for an asset that hasn't been commissioned",
        );
        if num_units < num_already_mothballed {
            // Remove mothballing events until only required num of mothballed units remains,
            // starting with oldest
            let mut remaining = num_already_mothballed - num_units;
            while remaining > 0
                && let Some(event) = events.front_mut()
            {
                let to_remove = event.num_units.min(remaining);
                event.num_units -= to_remove;
                remaining -= to_remove;
                if event.num_units == 0 {
                    events.pop_front();
                }
            }
        } else {
            let year =
                year.expect("Cannot increase number of mothballed units without supplying year");

            if let Some(event) = events.back() {
                // Need to check this as adding events in the past breaks the expected invariant for
                // `mothball_events`
                assert!(
                    event.year <= year,
                    "Attempting to mothball units in a year in the past"
                );
            }

            // Mothball extra units in specified year
            events.push_back(MothballEvent {
                year,
                num_units: num_units - num_already_mothballed,
            });
        }

        self
    }

    /// Returns a new [`AssetRef`] with no mothballed units.
    ///
    /// If the asset has no mothballed units, the original asset is returned.
    pub fn with_no_mothballed_units(mut self) -> Self {
        if self.has_any_mothballed_units() {
            // Only commissioned assets can have mothballed units, so this is safe
            self.make_mut().get_mothball_events_mut().unwrap().clear();
        }

        self
    }
}

impl From<Asset> for AssetRef {
    fn from(value: Asset) -> Self {
        Self::from(Rc::new(value))
    }
}

impl Eq for AssetRef {}

impl PartialEq for AssetRef {
    fn eq(&self, other: &Self) -> bool {
        self.get_asset_cmp() == other.get_asset_cmp()
    }
}

impl Ord for AssetRef {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_asset_cmp().cmp(&other.get_asset_cmp())
    }
}

impl PartialOrd for AssetRef {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for AssetRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_asset_cmp().hash(state);
    }
}

/// A data structure representing the fields of an [`Asset`] that should be used for comparisons.
///
/// For assets that have an ID (i.e. have been commissioned at some point), we can compare based on
/// the ID. Otherwise, we fall back on comparing other properties. The combination of these
/// properties should be unique within the simulation (e.g. for assets being input into dispatch).
#[derive(PartialEq, PartialOrd, Eq, Ord, Hash)]
enum AssetCmp<'a> {
    WithID(AssetID),
    WithoutID((&'a ProcessID, &'a RegionID, u32, Option<&'a AgentID>)),
}

/// Additional methods for iterating over assets
pub trait AssetIterator<'a>: Iterator<Item = &'a AssetRef> + Sized
where
    Self: 'a,
{
    /// Filter assets by the agent that owns them
    fn filter_agent(self, agent_id: &'a AgentID) -> impl Iterator<Item = &'a AssetRef> + 'a {
        self.filter(move |asset| asset.agent_id() == Some(agent_id))
    }

    /// Iterate over assets that have the given commodity as a primary output
    fn filter_primary_producers_of(
        self,
        commodity_id: &'a CommodityID,
    ) -> impl Iterator<Item = &'a AssetRef> + 'a {
        self.filter(move |asset| {
            asset
                .primary_output()
                .is_some_and(|flow| &flow.commodity.id == commodity_id)
        })
    }

    /// Filter the assets by region
    fn filter_region(self, region_id: &'a RegionID) -> impl Iterator<Item = &'a AssetRef> + 'a {
        self.filter(move |asset| asset.region_id == *region_id)
    }

    /// Iterate over process flows affecting the given commodity
    fn flows_for_commodity(
        self,
        commodity_id: &'a CommodityID,
    ) -> impl Iterator<Item = (&'a AssetRef, &'a ProcessFlow)> + 'a {
        self.filter_map(|asset| Some((asset, asset.get_flow(commodity_id)?)))
    }

    /// Get the parent for each asset, if it has one, or itself.
    ///
    /// Child assets are converted to their parents and non-divisible assets are returned as is. Each
    /// parent asset is returned only once.
    ///
    /// If only a subset of a parent's children are present in this iterator, a new parent asset
    /// representing a portion of the total capacity will be created. This will have the same hash
    /// as the original parent.
    fn into_parent_or_self(self) -> Vec<AssetRef> {
        // **HACK**: Put commissioned divisible assets at end to maintain ordering of output files
        let mut out = self.cloned().collect_vec();
        out.sort_by(|a, b| {
            (a.is_commissioned() && a.is_divisible())
                .cmp(&(b.is_commissioned() && b.is_divisible()))
        });

        out
    }
}

impl<'a, I> AssetIterator<'a> for I where I: Iterator<Item = &'a AssetRef> + Sized + 'a {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{
        agent_id, assert_error, assert_patched_runs_ok_simple, assert_validate_fails_with_simple,
        asset, asset_divisible, process, process_activity_limits_map, process_flows_map, region_id,
        svd_commodity, time_slice, time_slice_info,
    };
    use crate::patch::FilePatch;
    use crate::process::{FlowType, Process, ProcessFlow};
    use crate::region::RegionID;
    use crate::time_slice::{TimeSliceID, TimeSliceInfo};
    use crate::units::{
        ActivityPerCapacity, Capacity, Dimensionless, FlowPerActivity, MoneyPerActivity,
        MoneyPerFlow,
    };
    use float_cmp::assert_approx_eq;
    use indexmap::indexmap;
    use itertools::assert_equal;
    use rstest::{fixture, rstest};
    use std::rc::Rc;

    /// A commissioned divisible asset with three units.
    #[fixture]
    fn commissioned_divisible(mut asset_divisible: Asset) -> AssetRef {
        asset_divisible.commission(AssetID(0));
        assert_eq!(asset_divisible.num_units(), 3);
        AssetRef::from(asset_divisible)
    }

    #[rstest]
    fn get_input_cost_from_prices_works(
        region_id: RegionID,
        svd_commodity: Commodity,
        mut process: Process,
        time_slice: TimeSliceID,
    ) {
        // Update the process flows using the existing commodity fixture
        let commodity_rc = Rc::new(svd_commodity);
        let process_flow = ProcessFlow {
            commodity: Rc::clone(&commodity_rc),
            coeff: FlowPerActivity(-2.0), // Input
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let process_flows = indexmap! { commodity_rc.id.clone() => process_flow.clone() };
        let process_flows_map = process_flows_map(process.regions.clone(), Rc::new(process_flows));
        process.flows = process_flows_map;

        // Create asset
        let asset =
            Asset::new_candidate(Rc::new(process), region_id.clone(), Capacity(1.0), 2020).unwrap();

        // Set input prices
        let mut input_prices = PriceMap::default();
        input_prices.insert(&commodity_rc.id, &region_id, &time_slice, MoneyPerFlow(3.0));

        // Call function
        let cost = asset.get_input_cost_from_prices(&input_prices, &time_slice);
        // Should be -coeff * price = -(-2.0) * 3.0 = 6.0
        assert_approx_eq!(MoneyPerActivity, cost, MoneyPerActivity(6.0));
    }

    #[fixture]
    fn process_with_activity_limits(
        mut process: Process,
        time_slice_info: TimeSliceInfo,
        time_slice: TimeSliceID,
    ) -> Process {
        // Add activity limits to the process
        let mut activity_limits = ActivityLimits::new_with_full_availability(&time_slice_info);
        activity_limits.add_time_slice_limit(time_slice, Dimensionless(0.1)..=Dimensionless(0.5));
        process.activity_limits =
            process_activity_limits_map(process.regions.clone(), activity_limits);

        // Update cap2act
        process.capacity_to_activity = ActivityPerCapacity(2.0);
        process
    }

    #[fixture]
    fn asset_with_activity_limits(process_with_activity_limits: Process) -> Asset {
        Asset::new_ready(
            "agent1".into(),
            Rc::new(process_with_activity_limits),
            "GBR".into(),
            Capacity(2.0),
            2010,
        )
        .unwrap()
    }

    #[rstest]
    fn asset_get_activity_per_capacity_limits(
        asset_with_activity_limits: Asset,
        time_slice: TimeSliceID,
    ) {
        // With cap2act of 2, and activity limits of 0.1..=0.5, should get 0.2..=1.0
        assert_eq!(
            asset_with_activity_limits.get_activity_per_capacity_limits(&time_slice),
            ActivityPerCapacity(0.2)..=ActivityPerCapacity(1.0)
        );
    }

    #[rstest]
    #[case(Capacity(0.01))]
    #[case(Capacity(0.5))]
    #[case(Capacity(1.0))]
    #[case(Capacity(100.0))]
    fn user_asset_new_valid(
        agent_id: AgentID,
        process: Process,
        region_id: RegionID,
        #[case] capacity: Capacity,
    ) {
        let asset =
            UserAsset::new(agent_id, process.into(), region_id, capacity, 2015, None).unwrap();
        assert!(asset.id().is_none());
    }

    #[rstest]
    #[case(Capacity(0.0))]
    #[case(Capacity(-0.01))]
    #[case(Capacity(-1.0))]
    #[case(Capacity(f64::NAN))]
    #[case(Capacity(f64::INFINITY))]
    #[case(Capacity(f64::NEG_INFINITY))]
    fn user_asset_new_invalid_capacity(
        agent_id: AgentID,
        process: Process,
        region_id: RegionID,
        #[case] capacity: Capacity,
    ) {
        assert_error!(
            UserAsset::new(agent_id, process.into(), region_id, capacity, 2015, None),
            "Capacity must be a finite, positive number"
        );
    }

    #[rstest]
    fn user_asset_new_invalid_commission_year(
        agent_id: AgentID,
        process: Process,
        region_id: RegionID,
    ) {
        assert_error!(
            UserAsset::new(
                agent_id,
                process.into(),
                region_id,
                Capacity(1.0),
                2007,
                None
            ),
            "Process process1 does not operate in the year 2007"
        );
    }

    #[rstest]
    fn user_asset_new_invalid_region(agent_id: AgentID, process: Process) {
        let region_id = RegionID("FRA".into());
        assert_error!(
            UserAsset::new(
                agent_id,
                process.into(),
                region_id,
                Capacity(1.0),
                2015,
                None
            ),
            "Process process1 does not operate in region FRA"
        );
    }

    #[rstest]
    #[case::subset(2, false)]
    #[case::all_all_units(3, true)]
    fn with_subset_of_units(
        asset_divisible: Asset,
        #[case] num_units: u32,
        #[case] expect_same_asset: bool,
    ) {
        let asset = AssetRef::from(asset_divisible);
        let asset_subset = asset.clone().with_subset_of_units(num_units);

        assert_eq!(
            asset_subset.capacity(),
            AssetCapacity::Discrete(num_units, Capacity(4.0))
        );
        assert_eq!(asset_subset.capacity().n_units(), Some(num_units));
        assert_eq!(asset_subset.id(), asset.id());
        assert_eq!(asset_subset.agent_id(), asset.agent_id());
        assert_eq!(Rc::ptr_eq(&asset_subset.0, &asset.0), expect_same_asset);
        assert_eq!(asset.capacity(), AssetCapacity::Discrete(3, Capacity(4.0)));
    }

    #[rstest]
    fn with_subset_of_units_non_divisible_asset(asset: Asset) {
        let asset = AssetRef::from(asset);
        assert!(Rc::ptr_eq(
            &asset.0,
            &asset.clone().with_subset_of_units(1).0
        ));
    }

    #[rstest]
    #[should_panic(expected = "Non-divisible assets can only have one unit")]
    fn with_subset_of_units_panics_for_non_divisible_asset(asset: Asset) {
        AssetRef::from(asset).with_subset_of_units(2);
    }

    #[rstest]
    #[should_panic(expected = "Cannot make an asset with zero units")]
    fn with_subset_of_units_panics_for_zero_units(commissioned_divisible: AssetRef) {
        commissioned_divisible.with_subset_of_units(0);
    }

    #[rstest]
    #[should_panic(expected = "Cannot make an asset with more units than original")]
    fn with_subset_of_units_panics_for_too_many_units(commissioned_divisible: AssetRef) {
        commissioned_divisible.with_subset_of_units(4);
    }

    #[rstest]
    fn asset_commission(process: Process) {
        // Test successful commissioning of Ready asset
        let mut asset = Asset::new_ready(
            "agent1".into(),
            process.into(),
            "GBR".into(),
            Capacity(1.0),
            2020,
        )
        .unwrap();
        asset.commission(AssetID(2));
        assert!(asset.is_commissioned());
        assert_eq!(asset.id(), Some(AssetID(2)));
    }

    #[rstest]
    #[should_panic(expected = "Assets with state Candidate cannot be commissioned")]
    fn commission_wrong_states(process: Process) {
        let mut asset =
            Asset::new_candidate(process.into(), "GBR".into(), Capacity(1.0), 2020).unwrap();
        asset.commission(AssetID(1));
    }

    #[test]
    fn commission_year_before_time_horizon() {
        let processes_patch = FilePatch::new("processes.csv")
            .with_deletion("GASDRV,Dry gas extraction,all,GASPRD,2020,2040,1.0,")
            .with_addition("GASDRV,Dry gas extraction,all,GASPRD,1980,2040,1.0,");

        // Check we can run model with asset commissioned before time horizon (simple starts in
        // 2020)
        let patches = vec![
            processes_patch.clone(),
            FilePatch::new("assets.csv").with_addition("GASDRV,GBR,A0_GEX,4002.26,1980"),
        ];
        assert_patched_runs_ok_simple!(patches);

        // This should fail if it is not one of the years supported by the process, though
        let patches = vec![
            processes_patch,
            FilePatch::new("assets.csv").with_addition("GASDRV,GBR,A0_GEX,4002.26,1970"),
        ];
        assert_validate_fails_with_simple!(
            patches,
            "Agent A0_GEX has asset with commission year 1970, not within process GASDRV commission years: 1980..=2040"
        );
    }

    #[test]
    fn commission_year_after_time_horizon() {
        let processes_patch = FilePatch::new("processes.csv")
            .with_deletion("GASDRV,Dry gas extraction,all,GASPRD,2020,2040,1.0,")
            .with_addition("GASDRV,Dry gas extraction,all,GASPRD,2020,2050,1.0,");

        // Check we can run model with asset commissioned after time horizon (simple ends in 2040)
        let patches = vec![
            processes_patch.clone(),
            FilePatch::new("assets.csv").with_addition("GASDRV,GBR,A0_GEX,4002.26,2050"),
        ];
        assert_patched_runs_ok_simple!(patches);

        // This should fail if it is not one of the years supported by the process, though
        let patches = vec![
            processes_patch,
            FilePatch::new("assets.csv").with_addition("GASDRV,GBR,A0_GEX,4002.26,2060"),
        ];
        assert_validate_fails_with_simple!(
            patches,
            "Agent A0_GEX has asset with commission year 2060, not within process GASDRV commission years: 2020..=2050"
        );
    }

    #[rstest]
    fn max_installable_capacity(mut process: Process, region_id: RegionID) {
        // Set an addition limit of 3 for (region, year 2015)
        process.investment_constraints.insert(
            (region_id.clone(), 2015),
            Rc::new(crate::process::ProcessInvestmentConstraint {
                addition_limit: Some(Capacity(3.0)),
            }),
        );
        let process_rc = Rc::new(process);

        // Create a candidate asset with commission year 2015
        let asset =
            Asset::new_candidate(process_rc.clone(), region_id.clone(), Capacity(1.0), 2015)
                .unwrap();

        // commodity_portion = 0.5 -> limit = 3 * 0.5 = 1.5
        let result = asset.max_installable_capacity(Dimensionless(0.5));
        assert_eq!(result, Some(AssetCapacity::Continuous(Capacity(1.5))));
    }

    #[rstest]
    #[case::none(0)]
    #[case::some(2)]
    #[case::all(3)]
    fn mothball_unit_counts(commissioned_divisible: AssetRef, #[case] num_mothballed: u32) {
        assert_eq!(commissioned_divisible.num_units(), 3);
        let asset = commissioned_divisible.with_mothballed_units(num_mothballed, Some(2020));
        assert_eq!(asset.get_num_mothballed_units(), num_mothballed);
        assert_eq!(asset.get_num_nonmothballed_units(), 3 - num_mothballed);
        assert_eq!(asset.has_any_mothballed_units(), num_mothballed > 0);
    }

    #[rstest]
    fn mothball_counts_non_commissioned(asset: Asset, process: Process) {
        // Non-commissioned assets never have mothballed units, regardless of state
        let ready = AssetRef::from(asset);
        let candidate = AssetRef::from(
            Asset::new_candidate(process.into(), "GBR".into(), Capacity(1.0), 2020).unwrap(),
        );
        for asset in [ready, candidate] {
            assert!(!asset.has_any_mothballed_units());
            assert_eq!(asset.get_num_mothballed_units(), 0);
            assert_eq!(asset.get_num_nonmothballed_units(), asset.num_units());
        }
    }

    #[rstest]
    fn with_mothballed_units_accumulates_events(commissioned_divisible: AssetRef) {
        // Mothball one unit in 2020
        let asset = commissioned_divisible.with_mothballed_units(1, Some(2020));
        assert_equal(
            asset.get_mothball_events().unwrap().iter(),
            &[MothballEvent {
                year: 2020,
                num_units: 1,
            }],
        );

        // Mothball a second unit in 2022: events are retained in chronological order
        let asset = asset.with_mothballed_units(2, Some(2022));
        assert_equal(
            asset.get_mothball_events().unwrap().iter(),
            &[
                MothballEvent {
                    year: 2020,
                    num_units: 1,
                },
                MothballEvent {
                    year: 2022,
                    num_units: 1,
                },
            ],
        );
    }

    #[rstest]
    fn with_mothballed_units_decrease_removes_oldest_first(commissioned_divisible: AssetRef) {
        // Mothball 1 unit in 2020, then 2 more (3 total) in 2022
        let asset = commissioned_divisible
            .with_mothballed_units(1, Some(2020))
            .with_mothballed_units(3, Some(2022));
        assert_equal(
            asset.get_mothball_events().unwrap().iter(),
            &[
                MothballEvent {
                    year: 2020,
                    num_units: 1,
                },
                MothballEvent {
                    year: 2022,
                    num_units: 2,
                },
            ],
        );

        // Reduce to a single mothballed unit: the oldest event is fully removed and the newer
        // event is partially reduced, leaving exactly one mothballed unit
        let asset = asset.with_mothballed_units(1, None);
        assert_eq!(asset.get_num_mothballed_units(), 1);
        assert_equal(
            asset.get_mothball_events().unwrap().iter(),
            &[MothballEvent {
                year: 2022,
                num_units: 1,
            }],
        );
    }

    #[rstest]
    fn with_mothballed_units_noop_returns_same_rc(commissioned_divisible: AssetRef) {
        let asset = commissioned_divisible.with_mothballed_units(2, Some(2020));
        // Requesting the same number of mothballed units is a no-op (the year is ignored)
        let same = asset.clone().with_mothballed_units(2, Some(2099));
        assert!(Rc::ptr_eq(&asset.0, &same.0));
    }

    #[rstest]
    fn with_mothballed_units_zero_unmothballs(commissioned_divisible: AssetRef) {
        let asset = commissioned_divisible.with_mothballed_units(2, Some(2020));
        assert!(asset.has_any_mothballed_units());

        let asset = asset.with_mothballed_units(0, None);
        assert!(!asset.has_any_mothballed_units());
        assert_eq!(asset.get_num_mothballed_units(), 0);
    }

    #[rstest]
    #[should_panic(expected = "Cannot mothball more units than asset represents")]
    fn with_mothballed_units_panics_for_too_many_units(commissioned_divisible: AssetRef) {
        commissioned_divisible.with_mothballed_units(4, Some(2020));
    }

    #[rstest]
    #[should_panic(
        expected = "Cannot change number of mothballed units for an asset that hasn't been commissioned"
    )]
    fn with_mothballed_units_panics_for_non_commissioned(asset: Asset) {
        AssetRef::from(asset).with_mothballed_units(1, Some(2020));
    }

    #[rstest]
    #[should_panic(expected = "Cannot increase number of mothballed units without supplying year")]
    fn with_mothballed_units_panics_when_increasing_without_year(commissioned_divisible: AssetRef) {
        commissioned_divisible.with_mothballed_units(1, None);
    }

    #[rstest]
    #[should_panic(expected = "Attempting to mothball units in a year in the past")]
    fn with_mothballed_units_panics_when_mothballing_in_the_past(commissioned_divisible: AssetRef) {
        // Mothball a unit in 2020, then attempt to mothball another in an earlier year, which would
        // break the chronological ordering invariant of the mothball events
        commissioned_divisible
            .with_mothballed_units(1, Some(2020))
            .with_mothballed_units(2, Some(2019));
    }

    #[rstest]
    fn with_no_mothballed_units_clears_events(commissioned_divisible: AssetRef) {
        let asset = commissioned_divisible.with_mothballed_units(2, Some(2020));
        let asset = asset.with_no_mothballed_units();
        assert!(!asset.has_any_mothballed_units());
        assert_eq!(asset.get_num_mothballed_units(), 0);
    }

    #[rstest]
    fn with_no_mothballed_units_noop_returns_same_rc(commissioned_divisible: AssetRef) {
        // `asset_divisble` has no mothballed units, so the original Rc is returned unchanged
        let asset = commissioned_divisible;
        let same = asset.clone().with_no_mothballed_units();
        assert!(Rc::ptr_eq(&asset.0, &same.0));
    }

    #[rstest]
    fn with_subset_of_units_caps_mothballed(commissioned_divisible: AssetRef) {
        // Mothball all 3 units
        let asset = commissioned_divisible.with_mothballed_units(3, Some(2020));
        assert_eq!(asset.get_num_mothballed_units(), 3);

        // Taking a subset of 2 units caps the mothballed count at the new number of units
        let subset = asset.with_subset_of_units(2);
        assert_eq!(subset.num_units(), 2);
        assert_eq!(subset.get_num_mothballed_units(), 2);
    }

    #[rstest]
    fn with_decommission_mothballed_nothing_old_enough(commissioned_divisible: AssetRef) {
        let asset = commissioned_divisible.with_mothballed_units(1, Some(2020));
        // Threshold is 2005, so the 2020 event is not old enough: the asset is returned unchanged
        let result = asset
            .clone()
            .with_decommission_mothballed(2025, 20)
            .unwrap();
        assert!(Rc::ptr_eq(&asset.0, &result.0));
    }

    #[rstest]
    fn with_decommission_mothballed_partial(commissioned_divisible: AssetRef) {
        // Mothball 1 unit in 2010 and 1 unit in 2020 (leaving 1 unit active)
        let asset = commissioned_divisible
            .with_mothballed_units(1, Some(2010))
            .with_mothballed_units(2, Some(2020));

        // With a threshold of 2015, only the 2010 event is old enough to decommission
        let result = asset.with_decommission_mothballed(2025, 10).unwrap();
        assert_eq!(result.num_units(), 2);
        assert_eq!(result.get_num_mothballed_units(), 1);
        assert_equal(
            result.get_mothball_events().unwrap().iter(),
            &[MothballEvent {
                year: 2020,
                num_units: 1,
            }],
        );
    }

    #[rstest]
    fn with_decommission_mothballed_all(commissioned_divisible: AssetRef) {
        // All units mothballed long enough ago: the whole asset is decommissioned
        let asset = commissioned_divisible.with_mothballed_units(3, Some(2010));
        assert!(asset.with_decommission_mothballed(2025, 10).is_none());
    }
}
