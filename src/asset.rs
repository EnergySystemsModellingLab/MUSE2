//! Assets are instances of a process which are owned and invested in by agents.
use crate::agent::AgentID;
use crate::commodity::CommodityID;
use crate::process::{
    ActivityLimits, FlowDirection, Process, ProcessFlow, ProcessID, ProcessParameter,
};
use crate::region::RegionID;
use crate::simulation::CommodityPrices;
use crate::time_slice::{TimeSliceID, TimeSliceSelection};
use crate::units::{Activity, ActivityPerCapacity, Capacity, MoneyPerActivity};
use anyhow::{Context, Result, ensure};
use indexmap::IndexMap;
use itertools::{Itertools, chain};
use log::{debug, warn};
use serde::{Deserialize, Serialize};
use std::cmp::min;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, RangeInclusive};
use std::rc::Rc;
use std::slice;

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

/// The state of an asset
///
/// New assets are created as either `Future` or `Candidate` assets. `Future` assets (which are
/// specified in the input data) have a fixed capacity and capital costs already accounted for,
/// whereas `Candidate` assets capital costs are not yet accounted for, and their capacity is
/// determined by the investment algorithm.
///
/// `Future` and `Candidate` assets can be converted to `Commissioned` assets by calling
/// `commission_future` or `commission_candidate` respectively.
///
/// `Commissioned` assets can be decommissioned by calling `decommission`.
#[derive(Clone, Debug, PartialEq, strum::Display)]
pub enum AssetState {
    /// The asset has been commissioned
    Commissioned {
        /// The ID of the asset
        id: AssetID,
        /// The ID of the agent that owns the asset
        agent_id: AgentID,
        /// Year in which the asset was mothballed. None, if it is not mothballed
        mothballed_year: Option<u32>,
    },
    /// The asset has been decommissioned
    Decommissioned {
        /// The ID of the asset
        id: AssetID,
        /// The ID of the agent that owned the asset
        agent_id: AgentID,
        /// The year the asset was decommissioned
        decommission_year: u32,
    },
    /// The asset is planned for commissioning in the future
    Future {
        /// The ID of the agent that will own the asset
        agent_id: AgentID,
    },
    /// The asset has been selected for investment, but not yet confirmed
    Selected {
        /// The ID of the agent that would own the asset
        agent_id: AgentID,
    },
    /// The asset is a candidate for investment but has not yet been selected by an agent
    Candidate,
    /// The asset has been divided and only its children matter, now
    Divided {
        /// The ID of the agent that owns the asset
        agent_id: AgentID,
        /// IDs of the children assets
        children: Vec<AssetID>,
    },
}

/// An asset controlled by an agent.
#[derive(Clone, PartialEq)]
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
    capacity: Capacity,
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
        Self::new_with_state(
            AssetState::Candidate,
            process,
            region_id,
            capacity,
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

    /// Create a new future asset
    pub fn new_future_with_max_decommission(
        agent_id: AgentID,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
        max_decommission_year: Option<u32>,
    ) -> Result<Self> {
        check_capacity_valid_for_asset(capacity)?;
        Self::new_with_state(
            AssetState::Future { agent_id },
            process,
            region_id,
            capacity,
            commission_year,
            max_decommission_year,
        )
    }

    /// Create a new future asset
    pub fn new_future(
        agent_id: AgentID,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
    ) -> Result<Self> {
        Self::new_future_with_max_decommission(
            agent_id,
            process,
            region_id,
            capacity,
            commission_year,
            None,
        )
    }

    /// Create a new selected asset
    ///
    /// This is only used for testing. In the real program, Selected assets can only be created from
    /// Candidate assets by calling `select_candidate_for_investment`.
    #[cfg(test)]
    fn new_selected(
        agent_id: AgentID,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
    ) -> Result<Self> {
        Self::new_with_state(
            AssetState::Selected { agent_id },
            process,
            region_id,
            capacity,
            commission_year,
            None,
        )
    }

    /// Private helper to create an asset with the given state
    fn new_with_state(
        state: AssetState,
        process: Rc<Process>,
        region_id: RegionID,
        capacity: Capacity,
        commission_year: u32,
        max_decommission_year: Option<u32>,
    ) -> Result<Self> {
        check_region_year_valid_for_process(&process, &region_id, commission_year)?;
        ensure!(capacity >= Capacity(0.0), "Capacity must be non-negative");

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
                    &process.id, region_id, commission_year
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
                    &process.id, region_id, commission_year
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
                    &process.id, region_id, commission_year
                )
            })?
            .clone();

        let max_decommission_year =
            max_decommission_year.unwrap_or(commission_year + process_parameter.lifetime);
        ensure!(
            max_decommission_year >= commission_year,
            "Max decommission year must be after/same as commission year"
        );

        Ok(Self {
            state,
            process,
            activity_limits,
            flows,
            process_parameter,
            region_id,
            capacity,
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

    /// Get the operating cost for this asset in a given year and time slice
    pub fn get_operating_cost(&self, year: u32, time_slice: &TimeSliceID) -> MoneyPerActivity {
        // The cost for all commodity flows (including levies/incentives)
        let flows_cost: MoneyPerActivity = self
            .iter_flows()
            .map(|flow| flow.get_total_cost(&self.region_id, year, time_slice))
            .sum();

        self.process_parameter.variable_operating_cost + flows_cost
    }

    /// Get the total revenue from all flows for this asset.
    ///
    /// If a price is missing, it is assumed to be zero.
    pub fn get_revenue_from_flows(
        &self,
        prices: &CommodityPrices,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        self.get_revenue_from_flows_with_filter(prices, time_slice, |_| true)
    }

    /// Get the total revenue from all flows excluding the primary output.
    ///
    /// If a price is missing, it is assumed to be zero.
    pub fn get_revenue_from_flows_excluding_primary(
        &self,
        prices: &CommodityPrices,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        let excluded_commodity = self.primary_output().map(|flow| &flow.commodity.id);

        self.get_revenue_from_flows_with_filter(prices, time_slice, |flow| {
            excluded_commodity.is_none_or(|commodity_id| commodity_id != &flow.commodity.id)
        })
    }

    /// Get the cost of input flows using the commodity prices in `input_prices`.
    ///
    /// If a price is missing, there is assumed to be no cost.
    pub fn get_input_cost_from_prices(
        &self,
        input_prices: &CommodityPrices,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        -self.get_revenue_from_flows_with_filter(input_prices, time_slice, |x| {
            x.direction() == FlowDirection::Input
        })
    }

    /// Get the total revenue from a subset of flows.
    ///
    /// Takes a function as an argument to filter the flows. If a price is missing, it is assumed to
    /// be zero.
    fn get_revenue_from_flows_with_filter<F>(
        &self,
        prices: &CommodityPrices,
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
                        .get(&flow.commodity.id, self.region_id(), time_slice)
                        .unwrap_or_default()
            })
            .sum()
    }

    /// Maximum activity for this asset
    pub fn max_activity(&self) -> Activity {
        self.capacity * self.process.capacity_to_activity
    }

    /// Get a specific process flow
    pub fn get_flow(&self, commodity_id: &CommodityID) -> Option<&ProcessFlow> {
        self.flows.get(commodity_id)
    }

    /// Iterate over the asset's flows
    pub fn iter_flows(&self) -> impl Iterator<Item = &ProcessFlow> {
        self.flows.values()
    }

    /// Get the primary output flow (if any) for this asset
    pub fn primary_output(&self) -> Option<&ProcessFlow> {
        self.process
            .primary_output
            .as_ref()
            .map(|commodity_id| &self.flows[commodity_id])
    }

    /// Whether this asset has been commissioned
    pub fn is_commissioned(&self) -> bool {
        matches!(&self.state, AssetState::Commissioned { .. })
    }

    /// Get the commission year for this asset
    pub fn commission_year(&self) -> u32 {
        self.commission_year
    }

    /// Get the decommission year for this asset
    pub fn decommission_year(&self) -> Option<u32> {
        match &self.state {
            AssetState::Decommissioned {
                decommission_year, ..
            } => Some(*decommission_year),
            _ => None,
        }
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
            AssetState::Commissioned { id, .. } | AssetState::Decommissioned { id, .. } => {
                Some(*id)
            }
            _ => None,
        }
    }

    /// Get the agent ID for this asset
    pub fn agent_id(&self) -> Option<&AgentID> {
        match &self.state {
            AssetState::Commissioned { agent_id, .. }
            | AssetState::Decommissioned { agent_id, .. }
            | AssetState::Future { agent_id }
            | AssetState::Selected { agent_id }
            | AssetState::Divided { agent_id, .. } => Some(agent_id),
            AssetState::Candidate => None,
        }
    }

    /// Get the capacity for this asset
    pub fn capacity(&self) -> Capacity {
        self.capacity
    }

    /// Set the capacity for this asset (only for Candidate or Selected assets)
    pub fn set_capacity(&mut self, capacity: Capacity) {
        assert!(
            matches!(
                self.state,
                AssetState::Candidate | AssetState::Selected { .. }
            ),
            "set_capacity can only be called on Candidate or Selected assets"
        );
        assert!(capacity >= Capacity(0.0), "Capacity must be >= 0");
        self.capacity = capacity;
    }

    /// Increase the capacity for this asset (only for Candidate assets)
    pub fn increase_capacity(&mut self, capacity: Capacity) {
        assert!(
            self.state == AssetState::Candidate,
            "increase_capacity can only be called on Candidate assets"
        );
        assert!(capacity >= Capacity(0.0), "Added capacity must be >= 0");
        self.capacity += capacity;
    }

    /// Decommission this asset
    fn decommission(&mut self, decommission_year: u32, reason: &str) {
        let (id, agent_id) = match &self.state {
            AssetState::Commissioned { id, agent_id, .. } => (*id, agent_id.clone()),
            _ => panic!("Cannot decommission an asset that hasn't been commissioned"),
        };
        debug!(
            "Decommissioning '{}' asset (ID: {}) for agent '{}' (reason: {})",
            self.process_id(),
            id,
            agent_id,
            reason
        );

        self.state = AssetState::Decommissioned {
            id,
            agent_id,
            decommission_year: decommission_year.min(self.max_decommission_year()),
        };
    }

    /// Commission the asset.
    ///
    /// Only assets with an [`AssetState`] of `Future` or `Selected` can be commissioned. If the
    /// asset's state is something else, this function will panic.
    ///
    /// # Arguments
    ///
    /// * `id` - The ID to give the newly commissioned asset
    /// * `reason` - The reason for commissioning (included in log)
    fn commission(&mut self, id: AssetID, reason: &str) {
        let agent_id = match &self.state {
            AssetState::Future { agent_id } | AssetState::Selected { agent_id } => agent_id,
            state => panic!("Assets with state {state} cannot be commissioned"),
        };
        debug!(
            "Commissioning '{}' asset (ID: {}, capacity: {}) for agent '{}' (reason: {})",
            self.process_id(),
            id,
            self.capacity(),
            agent_id,
            reason
        );
        self.state = AssetState::Commissioned {
            id,
            agent_id: agent_id.clone(),
            mothballed_year: None,
        };
    }

    /// Select a Candidate asset for investment, converting it to a Selected state
    pub fn select_candidate_for_investment(&mut self, agent_id: AgentID) {
        assert!(
            self.state == AssetState::Candidate,
            "select_candidate_for_investment can only be called on Candidate assets"
        );
        check_capacity_valid_for_asset(self.capacity).unwrap();
        self.state = AssetState::Selected { agent_id };
    }

    /// Set the year this asset was mothballed
    pub fn mothball(&mut self, year: u32) {
        let (id, agent_id) = match &self.state {
            AssetState::Commissioned { id, agent_id, .. } => (*id, agent_id.clone()),
            _ => panic!("Cannot mothballed an asset that hasn't been commissioned"),
        };
        self.state = AssetState::Commissioned {
            id,
            agent_id: agent_id.clone(),
            mothballed_year: Some(year),
        };
    }

    /// Remove the mothballed year - presumably because the asset has been used
    pub fn unmothball(&mut self) {
        let (id, agent_id) = match &self.state {
            AssetState::Commissioned { id, agent_id, .. } => (*id, agent_id.clone()),
            _ => panic!("Cannot mothballed an asset that hasn't been commissioned"),
        };
        self.state = AssetState::Commissioned {
            id,
            agent_id: agent_id.clone(),
            mothballed_year: None,
        };
    }

    /// Get the mothballed year for the asset
    pub fn get_mothballed_year(&self) -> Option<u32> {
        let AssetState::Commissioned {
            mothballed_year, ..
        } = &self.state
        else {
            panic!("Cannot mothballed an asset that hasn't been commissioned")
        };
        *mothballed_year
    }
}

#[allow(clippy::missing_fields_in_debug)]
impl std::fmt::Debug for Asset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Asset")
            .field("state", &self.state)
            .field("process_id", &self.process_id())
            .field("region_id", &self.region_id)
            .field("capacity", &self.capacity)
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

/// Whether the specified value is a valid capacity for an asset
pub fn check_capacity_valid_for_asset(capacity: Capacity) -> Result<()> {
    ensure!(
        capacity.is_finite() && capacity > Capacity(0.0),
        "Capacity must be a finite, positive number"
    );
    Ok(())
}

/// A wrapper around [`Asset`] for storing references in maps.
///
/// If the asset has been commissioned, then comparison and hashing is done based on the asset ID,
/// otherwise a combination of other parameters is used.
///
/// [`Ord`] is implemented for [`AssetRef`], but it will panic for non-commissioned assets.
#[derive(Clone, Debug)]
pub struct AssetRef(Rc<Asset>);

impl AssetRef {
    /// Make a mutable reference to the underlying [`Asset`]
    pub fn make_mut(&mut self) -> &mut Asset {
        Rc::make_mut(&mut self.0)
    }
}

impl From<Rc<Asset>> for AssetRef {
    fn from(value: Rc<Asset>) -> Self {
        Self(value)
    }
}

impl From<Asset> for AssetRef {
    fn from(value: Asset) -> Self {
        Self::from(Rc::new(value))
    }
}

impl From<AssetRef> for Rc<Asset> {
    fn from(value: AssetRef) -> Self {
        value.0
    }
}

impl Deref for AssetRef {
    type Target = Asset;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for AssetRef {
    fn eq(&self, other: &Self) -> bool {
        // For assets to be considered equal, they must have the same process, region, commission
        // year and state
        Rc::ptr_eq(&self.0.process, &other.0.process)
            && self.0.region_id == other.0.region_id
            && self.0.commission_year == other.0.commission_year
            && self.0.state == other.0.state
    }
}

impl Eq for AssetRef {}

impl Hash for AssetRef {
    /// Hash an asset according to its state:
    /// - Commissioned assets are hashed based on their ID alone
    /// - Selected assets are hashed based on `process_id`, `region_id`, `commission_year` and `agent_id`
    /// - Candidate assets are hashed based on `process_id`, `region_id` and `commission_year`
    /// - Future and Decommissioned assets cannot currently be hashed
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0.state {
            AssetState::Commissioned { id, .. } => {
                // Hashed based on their ID alone, since this is sufficient to uniquely identify the
                // asset
                id.hash(state);
            }
            AssetState::Candidate | AssetState::Selected { .. } => {
                // Hashed based on process_id, region_id, commission_year and (for Selected assets)
                // agent_id
                self.0.process.id.hash(state);
                self.0.region_id.hash(state);
                self.0.commission_year.hash(state);
                self.0.agent_id().hash(state);
            }
            AssetState::Future { .. }
            | AssetState::Decommissioned { .. }
            | AssetState::Divided { .. } => {
                // We shouldn't currently need to hash Future, Decommissioned or Divided assets
                unimplemented!("Cannot hash Future or Decommissioned assets");
            }
        }
    }
}

impl PartialOrd for AssetRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AssetRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id().unwrap().cmp(&other.id().unwrap())
    }
}

/// A pool of [`Asset`]s
pub struct AssetPool {
    /// The pool of active assets, sorted by ID
    active: Vec<AssetRef>,
    /// Assets that have not yet been commissioned, sorted by commission year
    future: Vec<Asset>,
    /// Assets that have been decommissioned
    decommissioned: Vec<AssetRef>,
    /// Next available asset ID number
    next_id: u32,
}

impl AssetPool {
    /// Create a new [`AssetPool`]
    pub fn new(mut assets: Vec<Asset>) -> Self {
        // Sort in order of commission year
        assets.sort_by(|a, b| a.commission_year.cmp(&b.commission_year));

        Self {
            active: Vec::new(),
            future: assets,
            decommissioned: Vec::new(),
            next_id: 0,
        }
    }

    /// Get the active pool as a slice of [`AssetRef`]s
    pub fn as_slice(&self) -> &[AssetRef] {
        &self.active
    }

    /// Decommission assets whose lifetime has passed,
    /// and commission new assets
    pub fn update_for_year(&mut self, year: u32) {
        self.decommission_old(year);
        self.commission_new(year);
    }

    /// Commission new assets for the specified milestone year from the input data
    fn commission_new(&mut self, year: u32) {
        // Count the number of assets to move
        let count = self
            .future
            .iter()
            .take_while(|asset| asset.commission_year <= year)
            .count();

        // Move assets from future to active
        for mut asset in self.future.drain(0..count) {
            // Ignore assets that have already been decommissioned
            if asset.max_decommission_year() <= year {
                warn!(
                    "Asset '{}' with commission year {} and lifetime {} was decommissioned before \
                    the start of the simulation",
                    asset.process_id(),
                    asset.commission_year,
                    asset.process_parameter().lifetime
                );
                continue;
            }

            asset.commission(AssetID(self.next_id), "user input");
            self.next_id += 1;
            self.active.push(asset.into());
        }
    }

    /// Decommission old assets for the specified milestone year
    fn decommission_old(&mut self, year: u32) {
        // Remove assets which are due for decommissioning
        let to_decommission = self
            .active
            .extract_if(.., |asset| asset.max_decommission_year() <= year);

        for mut asset in to_decommission {
            // Set `decommission_year` and move to `self.decommissioned`
            asset.make_mut().decommission(year, "end of life");
            self.decommissioned.push(asset);
        }
    }

    /// Decomission mothballed assets if mothballed long enough
    pub fn decommission_mothballed(&mut self, year: u32, mothball_years: u32) {
        // Remove assets which are due for decommissioning
        let to_decommission = self.active.extract_if(.., |asset| {
            asset.get_mothballed_year().is_some()
                && asset.get_mothballed_year() <= Some(year - min(mothball_years, year))
        });

        for mut asset in to_decommission {
            // Set `decommission_year` and move to `self.decommissioned`
            let decommissioned = asset.get_mothballed_year().unwrap() + mothball_years;
            asset.make_mut().decommission(
                decommissioned,
                &format!("The asset has not been used for the set mothball years ({mothball_years} years)."),
            );
            self.decommissioned.push(asset);
        }
    }

    /// Mothball the specified assets if they are no longer in the active pool and put them back again.
    ///
    /// # Arguments
    ///
    /// * `assets` - Assets to possibly mothball
    /// * `year` - Mothball year
    ///
    /// # Panics
    ///
    /// Panics if any of the provided assets was never commissioned.
    pub fn mothball_unretained<I>(&mut self, assets: I, year: u32)
    where
        I: IntoIterator<Item = AssetRef>,
    {
        for mut asset in assets {
            if match asset.state {
                AssetState::Commissioned { .. } => !self.active.contains(&asset),
                _ => panic!("Cannot mothball asset that has not been commissioned"),
            } {
                // If not already set, we set the current year as the mothball year,
                // i.e. the first one the asset was not used.
                if asset.get_mothballed_year().is_none() {
                    asset.make_mut().mothball(year);
                }

                // And we put it back to the pool, so they can be chosen the next milestone year
                // if not decommissioned earlier.
                self.active.push(asset);
            }
        }
        self.active.sort();
    }

    /// Get an asset with the specified ID.
    ///
    /// # Returns
    ///
    /// An [`AssetRef`] if found, else `None`. The asset may not be found if it has already been
    /// decommissioned.
    pub fn get(&self, id: AssetID) -> Option<&AssetRef> {
        // The assets in `active` are in order of ID
        let idx = self
            .active
            .binary_search_by(|asset| match &asset.state {
                AssetState::Commissioned { id: asset_id, .. } => asset_id.cmp(&id),
                _ => panic!("Active pool should only contain commissioned assets"),
            })
            .ok()?;

        Some(&self.active[idx])
    }

    /// Iterate over active assets
    pub fn iter_active(&self) -> slice::Iter<'_, AssetRef> {
        self.active.iter()
    }

    /// Iterate over decommissioned assets
    pub fn iter_decommissioned(&self) -> slice::Iter<'_, AssetRef> {
        self.decommissioned.iter()
    }

    /// Iterate over all commissioned and decommissioned assets.
    ///
    /// NB: Not-yet-commissioned assets are not included.
    pub fn iter_all(&self) -> impl Iterator<Item = &AssetRef> {
        chain(self.iter_active(), self.iter_decommissioned())
    }

    /// Return current active pool and clear
    pub fn take(&mut self) -> Vec<AssetRef> {
        std::mem::take(&mut self.active)
    }

    /// Extend the active pool with Commissioned or Selected assets
    pub fn extend<I>(&mut self, assets: I)
    where
        I: IntoIterator<Item = AssetRef>,
    {
        // Check all assets are either Commissioned or Selected, and, if the latter,
        // then commission them
        let assets = assets.into_iter().map(|mut asset| match &asset.state {
            AssetState::Commissioned { .. } => {
                asset.make_mut().unmothball();
                asset
            }
            AssetState::Selected { .. } => {
                asset
                    .make_mut()
                    .commission(AssetID(self.next_id), "selected");
                self.next_id += 1;
                asset
            }
            _ => panic!(
                "Cannot extend asset pool with asset in state {}. Only assets in \
                Commissioned or Selected states are allowed.",
                asset.state
            ),
        });

        // New assets may not have been sorted, but active needs to be sorted by ID
        self.active.extend(assets);
        self.active.sort();

        // Sanity check: all assets should be unique
        debug_assert_eq!(self.active.iter().unique().count(), self.active.len());
    }
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
}

impl<'a, I> AssetIterator<'a> for I where I: Iterator<Item = &'a AssetRef> + Sized + 'a {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use crate::fixture::{
        assert_error, asset, process, process_activity_limits_map, process_flows_map,
        process_parameter_map, region_id, svd_commodity, time_slice, time_slice_info,
    };
    use crate::process::{FlowType, Process, ProcessFlow, ProcessParameter};
    use crate::region::RegionID;
    use crate::time_slice::{TimeSliceID, TimeSliceInfo};
    use crate::units::{
        ActivityPerCapacity, Capacity, Dimensionless, FlowPerActivity, MoneyPerActivity,
        MoneyPerCapacity, MoneyPerCapacityPerYear, MoneyPerFlow,
    };
    use indexmap::indexmap;
    use itertools::{Itertools, assert_equal};
    use rstest::{fixture, rstest};
    use std::iter;
    use std::rc::Rc;

    #[rstest]
    fn test_get_input_cost_from_prices(
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
        let mut input_prices = CommodityPrices::default();
        input_prices.insert(&commodity_rc.id, &region_id, &time_slice, MoneyPerFlow(3.0));

        // Call function
        let cost = asset.get_input_cost_from_prices(&input_prices, &time_slice);
        // Should be -coeff * price = -(-2.0) * 3.0 = 6.0
        assert_eq!(cost.0, 6.0);
    }

    #[rstest]
    #[case(Capacity(0.01))]
    #[case(Capacity(0.5))]
    #[case(Capacity(1.0))]
    #[case(Capacity(100.0))]
    fn test_asset_new_valid(process: Process, #[case] capacity: Capacity) {
        let agent_id = AgentID("agent1".into());
        let region_id = RegionID("GBR".into());
        let asset = Asset::new_future(agent_id, process.into(), region_id, capacity, 2015).unwrap();
        assert!(asset.id().is_none());
    }

    #[rstest]
    #[case(Capacity(0.0))]
    #[case(Capacity(-0.01))]
    #[case(Capacity(-1.0))]
    #[case(Capacity(f64::NAN))]
    #[case(Capacity(f64::INFINITY))]
    #[case(Capacity(f64::NEG_INFINITY))]
    fn test_asset_new_invalid_capacity(process: Process, #[case] capacity: Capacity) {
        let agent_id = AgentID("agent1".into());
        let region_id = RegionID("GBR".into());
        assert_error!(
            Asset::new_future(agent_id, process.into(), region_id, capacity, 2015),
            "Capacity must be a finite, positive number"
        );
    }

    #[rstest]
    fn test_asset_new_invalid_commission_year(process: Process) {
        let agent_id = AgentID("agent1".into());
        let region_id = RegionID("GBR".into());
        assert_error!(
            Asset::new_future(agent_id, process.into(), region_id, Capacity(1.0), 2007),
            "Process process1 does not operate in the year 2007"
        );
    }

    #[rstest]
    fn test_asset_new_invalid_region(process: Process) {
        let agent_id = AgentID("agent1".into());
        let region_id = RegionID("FRA".into());
        assert_error!(
            Asset::new_future(agent_id, process.into(), region_id, Capacity(1.0), 2015),
            "Process process1 does not operate in region FRA"
        );
    }

    #[fixture]
    fn asset_pool(mut process: Process) -> AssetPool {
        // Update process parameters (lifetime = 20 years)
        let process_param = ProcessParameter {
            capital_cost: MoneyPerCapacity(5.0),
            fixed_operating_cost: MoneyPerCapacityPerYear(2.0),
            variable_operating_cost: MoneyPerActivity(1.0),
            lifetime: 20,
            discount_rate: Dimensionless(0.9),
        };
        let process_parameter_map = process_parameter_map(process.regions.clone(), process_param);
        process.parameters = process_parameter_map;

        let rc_process = Rc::new(process);
        let future = [2020, 2010]
            .map(|year| {
                Asset::new_future(
                    "agent1".into(),
                    Rc::clone(&rc_process),
                    "GBR".into(),
                    Capacity(1.0),
                    year,
                )
                .unwrap()
            })
            .into_iter()
            .collect_vec();

        AssetPool::new(future)
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
        Asset::new_future(
            "agent1".into(),
            Rc::new(process_with_activity_limits),
            "GBR".into(),
            Capacity(2.0),
            2010,
        )
        .unwrap()
    }

    #[rstest]
    fn test_asset_get_activity_per_capacity_limits(
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
    fn test_asset_pool_new(asset_pool: AssetPool) {
        // Should be in order of commission year
        assert!(asset_pool.active.is_empty());
        assert!(asset_pool.future.len() == 2);
        assert!(asset_pool.future[0].commission_year == 2010);
        assert!(asset_pool.future[1].commission_year == 2020);
    }

    #[rstest]
    fn test_asset_pool_commission_new1(mut asset_pool: AssetPool) {
        // Asset to be commissioned in this year
        asset_pool.commission_new(2010);
        assert_equal(asset_pool.iter_active(), iter::once(&asset_pool.active[0]));
    }

    #[rstest]
    fn test_asset_pool_commission_new2(mut asset_pool: AssetPool) {
        // Commission year has passed
        asset_pool.commission_new(2011);
        assert_equal(asset_pool.iter_active(), iter::once(&asset_pool.active[0]));
    }

    #[rstest]
    fn test_asset_pool_commission_new3(mut asset_pool: AssetPool) {
        // Nothing to commission for this year
        asset_pool.commission_new(2000);
        assert!(asset_pool.iter_active().next().is_none()); // no active assets
    }

    #[rstest]
    fn test_asset_pool_commission_already_decommissioned(asset: Asset) {
        let year = asset.max_decommission_year();
        let mut asset_pool = AssetPool::new(vec![asset]);
        assert!(asset_pool.active.is_empty());
        asset_pool.update_for_year(year);
        assert!(asset_pool.active.is_empty());
    }

    #[rstest]
    fn test_asset_pool_decommission_old(mut asset_pool: AssetPool) {
        asset_pool.commission_new(2020);
        assert!(asset_pool.future.is_empty());
        assert_eq!(asset_pool.active.len(), 2);
        asset_pool.decommission_old(2030); // should decommission first asset (lifetime == 5)
        assert_eq!(asset_pool.active.len(), 1);
        assert_eq!(asset_pool.active[0].commission_year, 2020);
        assert_eq!(asset_pool.decommissioned.len(), 1);
        assert_eq!(asset_pool.decommissioned[0].commission_year, 2010);
        assert_eq!(asset_pool.decommissioned[0].decommission_year(), Some(2030));
        asset_pool.decommission_old(2032); // nothing to decommission
        assert_eq!(asset_pool.active.len(), 1);
        assert_eq!(asset_pool.active[0].commission_year, 2020);
        assert_eq!(asset_pool.decommissioned.len(), 1);
        assert_eq!(asset_pool.decommissioned[0].commission_year, 2010);
        assert_eq!(asset_pool.decommissioned[0].decommission_year(), Some(2030));
        asset_pool.decommission_old(2040); // should decommission second asset
        assert!(asset_pool.active.is_empty());
        assert_eq!(asset_pool.decommissioned.len(), 2);
        assert_eq!(asset_pool.decommissioned[0].commission_year, 2010);
        assert_eq!(asset_pool.decommissioned[0].decommission_year(), Some(2030));
        assert_eq!(asset_pool.decommissioned[1].commission_year, 2020);
        assert_eq!(asset_pool.decommissioned[1].decommission_year(), Some(2040));
    }

    #[rstest]
    fn test_asset_pool_get(mut asset_pool: AssetPool) {
        asset_pool.commission_new(2020);
        assert_eq!(asset_pool.get(AssetID(0)), Some(&asset_pool.active[0]));
        assert_eq!(asset_pool.get(AssetID(1)), Some(&asset_pool.active[1]));
    }

    #[rstest]
    fn test_asset_pool_extend_empty(mut asset_pool: AssetPool) {
        // Start with commissioned assets
        asset_pool.commission_new(2020);
        let original_count = asset_pool.active.len();

        // Extend with empty iterator
        asset_pool.extend(Vec::<AssetRef>::new());

        assert_eq!(asset_pool.active.len(), original_count);
    }

    #[rstest]
    fn test_asset_pool_extend_existing_assets(mut asset_pool: AssetPool) {
        // Start with some commissioned assets
        asset_pool.commission_new(2020);
        assert_eq!(asset_pool.active.len(), 2);
        let existing_assets = asset_pool.take();

        // Extend with the same assets (should maintain their IDs)
        asset_pool.extend(existing_assets.clone());

        assert_eq!(asset_pool.active.len(), 2);
        assert_eq!(asset_pool.active[0].id(), Some(AssetID(0)));
        assert_eq!(asset_pool.active[1].id(), Some(AssetID(1)));
    }

    #[rstest]
    fn test_asset_pool_extend_new_assets(mut asset_pool: AssetPool, process: Process) {
        // Start with some commissioned assets
        asset_pool.commission_new(2020);
        let original_count = asset_pool.active.len();

        // Create new non-commissioned assets
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_selected(
                "agent2".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.5),
                2015,
            )
            .unwrap()
            .into(),
            Asset::new_selected(
                "agent3".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(2.5),
                2020,
            )
            .unwrap()
            .into(),
        ];

        asset_pool.extend(new_assets);

        assert_eq!(asset_pool.active.len(), original_count + 2);
        // New assets should get IDs 2 and 3
        assert_eq!(asset_pool.active[original_count].id(), Some(AssetID(2)));
        assert_eq!(asset_pool.active[original_count + 1].id(), Some(AssetID(3)));
        assert_eq!(
            asset_pool.active[original_count].agent_id(),
            Some(&"agent2".into())
        );
        assert_eq!(
            asset_pool.active[original_count + 1].agent_id(),
            Some(&"agent3".into())
        );
    }

    #[rstest]
    fn test_asset_pool_extend_mixed_assets(mut asset_pool: AssetPool, process: Process) {
        // Start with some commissioned assets
        asset_pool.commission_new(2020);

        // Create a new non-commissioned asset
        let new_asset = Asset::new_selected(
            "agent_new".into(),
            process.into(),
            "GBR".into(),
            Capacity(3.0),
            2015,
        )
        .unwrap()
        .into();

        // Extend with just the new asset (not mixing with existing to avoid duplicates)
        asset_pool.extend(vec![new_asset]);

        assert_eq!(asset_pool.active.len(), 3);
        // Check that we have the original assets plus the new one
        assert!(asset_pool.active.iter().any(|a| a.id() == Some(AssetID(0))));
        assert!(asset_pool.active.iter().any(|a| a.id() == Some(AssetID(1))));
        assert!(asset_pool.active.iter().any(|a| a.id() == Some(AssetID(2))));
        // Check that the new asset has the correct agent
        assert!(
            asset_pool
                .active
                .iter()
                .any(|a| a.agent_id() == Some(&"agent_new".into()))
        );
    }

    #[rstest]
    fn test_asset_pool_extend_maintains_sort_order(mut asset_pool: AssetPool, process: Process) {
        // Start with some commissioned assets
        asset_pool.commission_new(2020);

        // Create new assets that would be out of order if added at the end
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_selected(
                "agent_high_id".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2010,
            )
            .unwrap()
            .into(),
            Asset::new_selected(
                "agent_low_id".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2015,
            )
            .unwrap()
            .into(),
        ];

        asset_pool.extend(new_assets);

        // Check that assets are sorted by ID
        let ids: Vec<u32> = asset_pool
            .iter_active()
            .map(|a| a.id().unwrap().0)
            .collect();
        assert_equal(ids, 0..4);
    }

    #[rstest]
    fn test_asset_pool_extend_no_duplicates_expected(mut asset_pool: AssetPool) {
        // Start with some commissioned assets
        asset_pool.commission_new(2020);
        let original_count = asset_pool.active.len();

        // The extend method expects unique assets - adding duplicates would violate
        // the debug assertion, so this test verifies the normal case
        asset_pool.extend(Vec::new());

        assert_eq!(asset_pool.active.len(), original_count);
        // Verify all assets are still unique (this is what the debug_assert checks)
        assert_eq!(
            asset_pool.active.iter().unique().count(),
            asset_pool.active.len()
        );
    }

    #[rstest]
    fn test_asset_pool_extend_increments_next_id(mut asset_pool: AssetPool, process: Process) {
        // Start with some commissioned assets
        asset_pool.commission_new(2020);
        assert_eq!(asset_pool.next_id, 2); // Should be 2 after commissioning 2 assets

        // Create new non-commissioned assets
        let process_rc = Rc::new(process);
        let new_assets = vec![
            Asset::new_selected(
                "agent1".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2015,
            )
            .unwrap()
            .into(),
            Asset::new_selected(
                "agent2".into(),
                Rc::clone(&process_rc),
                "GBR".into(),
                Capacity(1.0),
                2020,
            )
            .unwrap()
            .into(),
        ];

        asset_pool.extend(new_assets);

        // next_id should have incremented for each new asset
        assert_eq!(asset_pool.next_id, 4);
        assert_eq!(asset_pool.active[2].id(), Some(AssetID(2)));
        assert_eq!(asset_pool.active[3].id(), Some(AssetID(3)));
    }

    #[rstest]
    fn test_asset_pool_mothball_unretained(mut asset_pool: AssetPool) {
        // Commission some assets
        asset_pool.commission_new(2020);
        assert_eq!(asset_pool.active.len(), 2);

        // Remove one asset from the active pool (simulating it being removed elsewhere)
        let removed_asset = asset_pool.active.remove(0);
        assert_eq!(asset_pool.active.len(), 1);

        // Try to mothball both the removed asset (not in active) and an active asset
        let assets_to_check = vec![removed_asset.clone(), asset_pool.active[0].clone()];
        asset_pool.mothball_unretained(assets_to_check, 2025);

        // Only the removed asset should be mothballed (since it's not in active pool)
        assert_eq!(asset_pool.active.len(), 2); // And should be back into the pool
        assert_eq!(asset_pool.active[0].get_mothballed_year(), Some(2025));
    }

    #[rstest]
    fn test_asset_pool_decommission_unused(mut asset_pool: AssetPool) {
        // Commission some assets
        asset_pool.commission_new(2020);
        assert_eq!(asset_pool.active.len(), 2);
        assert_eq!(asset_pool.decommissioned.len(), 0);

        // Make an asset unused for a few years
        let mothball_years: u32 = 10;
        asset_pool.active[0]
            .make_mut()
            .mothball(2025 - mothball_years);

        assert_eq!(
            asset_pool.active[0].get_mothballed_year(),
            Some(2025 - mothball_years)
        );

        // Decomission unused assets
        asset_pool.decommission_mothballed(2025, mothball_years);

        // Only the removed asset should be decommissioned (since it's not in active pool)
        assert_eq!(asset_pool.active.len(), 1); // Active pool unchanged
        assert_eq!(asset_pool.decommissioned.len(), 1);
        assert_eq!(asset_pool.decommissioned[0].decommission_year(), Some(2025));
    }

    #[rstest]
    fn test_asset_pool_decommission_if_not_active_none_active(mut asset_pool: AssetPool) {
        // Commission some assets
        asset_pool.commission_new(2020);
        let all_assets = asset_pool.active.clone();

        // Clear the active pool (simulating all assets being removed)
        asset_pool.active.clear();

        // Try to mothball the assets that are no longer active
        asset_pool.mothball_unretained(all_assets.clone(), 2025);

        // All assets should be mothballed
        assert_eq!(asset_pool.active.len(), 2);
        assert_eq!(asset_pool.active[0].id(), all_assets[0].id());
        assert_eq!(asset_pool.active[0].get_mothballed_year(), Some(2025));
        assert_eq!(asset_pool.active[1].id(), all_assets[1].id());
        assert_eq!(asset_pool.active[1].get_mothballed_year(), Some(2025));
    }

    #[rstest]
    #[should_panic(expected = "Cannot mothball asset that has not been commissioned")]
    fn test_asset_pool_decommission_if_not_active_non_commissioned_asset(
        mut asset_pool: AssetPool,
        process: Process,
    ) {
        // Create a non-commissioned asset
        let non_commissioned_asset = Asset::new_future(
            "agent_new".into(),
            process.into(),
            "GBR".into(),
            Capacity(1.0),
            2015,
        )
        .unwrap()
        .into();

        // This should panic because the asset was never commissioned
        asset_pool.mothball_unretained(vec![non_commissioned_asset], 2025);
    }

    #[rstest]
    fn test_asset_commission(process: Process) {
        // Test successful commissioning of Future asset
        let process_rc = Rc::new(process);
        let mut asset1 = Asset::new_future(
            "agent1".into(),
            Rc::clone(&process_rc),
            "GBR".into(),
            Capacity(1.0),
            2020,
        )
        .unwrap();
        asset1.commission(AssetID(1), "");
        assert!(asset1.is_commissioned());
        assert_eq!(asset1.id(), Some(AssetID(1)));

        // Test successful commissioning of Selected asset
        let mut asset2 = Asset::new_selected(
            "agent1".into(),
            Rc::clone(&process_rc),
            "GBR".into(),
            Capacity(1.0),
            2020,
        )
        .unwrap();
        asset2.commission(AssetID(2), "");
        assert!(asset2.is_commissioned());
        assert_eq!(asset2.id(), Some(AssetID(2)));
    }

    #[rstest]
    #[case::commission_during_process_lifetime(2024, 2024)]
    #[case::decommission_after_process_lifetime_ends(2026, 2025)]
    fn test_asset_decommission(
        #[case] requested_decommission_year: u32,
        #[case] expected_decommission_year: u32,
        process: Process,
    ) {
        // Test successful commissioning of Future asset
        let process_rc = Rc::new(process);
        let mut asset = Asset::new_future(
            "agent1".into(),
            Rc::clone(&process_rc),
            "GBR".into(),
            Capacity(1.0),
            2020,
        )
        .unwrap();
        asset.commission(AssetID(1), "");
        assert!(asset.is_commissioned());
        assert_eq!(asset.id(), Some(AssetID(1)));

        // Test successful decommissioning
        asset.decommission(requested_decommission_year, "");
        assert!(!asset.is_commissioned());
        assert_eq!(asset.decommission_year(), Some(expected_decommission_year));
    }

    #[rstest]
    #[case::decommission_after_predefined_max_year(2026, 2025, Some(2025))]
    #[case::decommission_before_predefined_max_year(2024, 2024, Some(2025))]
    #[case::decommission_during_process_lifetime_end_no_max_year(2024, 2024, None)]
    #[case::decommission_after_process_lifetime_end_no_max_year(2026, 2025, None)]
    fn test_asset_decommission_with_max_decommission_year_predefined(
        #[case] requested_decommission_year: u32,
        #[case] expected_decommission_year: u32,
        #[case] max_decommission_year: Option<u32>,
        process: Process,
    ) {
        // Test successful commissioning of Future asset
        let process_rc = Rc::new(process);
        let mut asset = Asset::new_future_with_max_decommission(
            "agent1".into(),
            Rc::clone(&process_rc),
            "GBR".into(),
            Capacity(1.0),
            2020,
            max_decommission_year,
        )
        .unwrap();
        asset.commission(AssetID(1), "");
        assert!(asset.is_commissioned());
        assert_eq!(asset.id(), Some(AssetID(1)));

        // Test successful decommissioning
        asset.decommission(requested_decommission_year, "");
        assert!(!asset.is_commissioned());
        assert_eq!(asset.decommission_year(), Some(expected_decommission_year));
    }

    #[rstest]
    #[should_panic(expected = "Assets with state Candidate cannot be commissioned")]
    fn test_commission_wrong_states(process: Process) {
        let mut asset =
            Asset::new_candidate(process.into(), "GBR".into(), Capacity(1.0), 2020).unwrap();
        asset.commission(AssetID(1), "");
    }

    #[rstest]
    #[should_panic(expected = "Cannot decommission an asset that hasn't been commissioned")]
    fn test_decommission_wrong_state(process: Process) {
        let mut asset =
            Asset::new_candidate(process.into(), "GBR".into(), Capacity(1.0), 2020).unwrap();
        asset.decommission(2025, "");
    }
}
