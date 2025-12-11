//! Processes are used for converting between different commodities. The data structures in this
//! module are used to represent these conversions along with the associated costs.
use crate::commodity::{Commodity, CommodityID};
use crate::id::define_id_type;
use crate::region::RegionID;
use crate::time_slice::{Season, TimeSliceID, TimeSliceInfo, TimeSliceLevel, TimeSliceSelection};
use crate::units::{
    ActivityPerCapacity, Dimensionless, FlowPerActivity, MoneyPerActivity, MoneyPerCapacity,
    MoneyPerCapacityPerYear, MoneyPerFlow,
};
use anyhow::{Result, ensure};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::rc::Rc;

define_id_type! {ProcessID}

/// A map of [`Process`]es, keyed by process ID
pub type ProcessMap = IndexMap<ProcessID, Rc<Process>>;

/// A map indicating activity limits for a [`Process`] throughout the year.
pub type ProcessActivityLimitsMap = HashMap<(RegionID, u32), Rc<ActivityLimits>>;

/// A map of [`ProcessParameter`]s, keyed by region and year
pub type ProcessParameterMap = HashMap<(RegionID, u32), Rc<ProcessParameter>>;

/// A map of process flows, keyed by region and year.
///
/// The value is actually a map itself, keyed by commodity ID.
pub type ProcessFlowsMap = HashMap<(RegionID, u32), Rc<IndexMap<CommodityID, ProcessFlow>>>;

/// Map of process investment constraints, keyed by region and year
pub type ProcessInvestmentConstraintsMap =
    HashMap<(RegionID, u32), Rc<ProcessInvestmentConstraint>>;

/// Represents a process within the simulation
#[derive(PartialEq, Debug)]
pub struct Process {
    /// A unique identifier for the process (e.g. GASDRV)
    pub id: ProcessID,
    /// A human-readable description for the process (e.g. dry gas extraction)
    pub description: String,
    /// The years in which this process is available for investment.
    pub years: RangeInclusive<u32>,
    /// Limits on activity for each time slice (as a fraction of maximum)
    pub activity_limits: ProcessActivityLimitsMap,
    /// Maximum annual commodity flows for this process
    pub flows: ProcessFlowsMap,
    /// Additional parameters for this process
    pub parameters: ProcessParameterMap,
    /// The regions in which this process can operate
    pub regions: IndexSet<RegionID>,
    /// The primary output for this process, if any
    pub primary_output: Option<CommodityID>,
    /// Factor for calculating the maximum consumption/production over a year.
    ///
    /// Used for converting one unit of capacity to maximum energy of asset per year. For example,
    /// if capacity is measured in GW and energy is measured in PJ, the `capacity_to_activity` for the
    /// process is 31.536 because 1 GW of capacity can produce 31.536 PJ energy output in a year.
    pub capacity_to_activity: ActivityPerCapacity,
    /// Investment constraints for this process
    pub investment_constraints: ProcessInvestmentConstraintsMap,
}

impl Process {
    /// Whether the process can be commissioned in a given year
    pub fn active_for_year(&self, year: u32) -> bool {
        self.years.contains(&year)
    }
}

/// Defines the activity limits for a process in a given region and year
///
/// Activity limits represent the minimum and maximum fraction of the potential annual activity that
/// can be undertaken in each time slice, season, or the year as a whole. The limits stored and
/// returned by this struct are dimensionless; to convert to actual activity limits for an asset,
/// multiply by the capacity to activity factor of the process and the installed capacity of the
/// asset. In other words, the limits stored and returned by this struct are the absolute limits on
/// activity per `1/capacity_to_activity` units of capacity.
///
/// All time slices must have an entry in `self.time_slice_limits`. If no specific availability limit
/// is provided for a time slice, this will just represent the length of the time slice as a
/// fraction of the year. Seasonal and annual limits may be stored if provided by the user, and only
/// if they provide an extra level of constraint on top of the time slice limits.
///
/// Limits can be retrieved in three ways:
/// - Retrieve the limit for a specific time slice using `get_limit_for_time_slice()`.
/// - Retrieve a limit for a specific time slice selection (time slice, season, or annual)
///   using `get_limit()`.
/// - Retrieve all limits as an iterator using `iter_limits()`. Note: individual
///   limits within this iterator cannot be relied upon, but the totality of these limits can be
///   used to construct a set of constraints that ensures that all limits are respected.
#[derive(PartialEq, Debug, Clone)]
pub struct ActivityLimits {
    /// Optional annual limit
    annual_limit: Option<RangeInclusive<Dimensionless>>,
    /// Optional limits for each season
    seasonal_limits: IndexMap<Season, RangeInclusive<Dimensionless>>,
    /// Limits for each time slice (mandatory for all time slices)
    time_slice_limits: IndexMap<TimeSliceID, RangeInclusive<Dimensionless>>,
}

impl ActivityLimits {
    /// Create a new `ActivityLimits` with full availability for all time slices
    pub fn new_with_full_availability(time_slice_info: &TimeSliceInfo) -> Self {
        // Initialize time slice limits to full availability
        let mut ts_limits = IndexMap::new();
        for (ts_id, ts_length) in time_slice_info.iter() {
            ts_limits.insert(
                ts_id.clone(),
                Dimensionless(0.0)..=Dimensionless(ts_length.value()),
            );
        }

        ActivityLimits {
            annual_limit: None,
            seasonal_limits: IndexMap::new(),
            time_slice_limits: ts_limits,
        }
    }

    /// Create a new `ActivityLimits` from a map of limits for time slice selections
    ///
    /// The limits provided here may be for individual time slices, seasons, or the entire year.
    /// Provided limits must reflect the fraction of potential annual activity available in the
    /// given time slice selection. In other words, these are the absolute limits on activity per
    /// `1/capacity_to_activity` units of capacity.
    ///
    /// It is not mandatory to provide any limits; if no limits are provided, full availability
    /// will be assumed for all time slices (limited only by time slice lengths). However, if
    /// limits are provided for any individual time slices, they must be provided for ALL time
    /// slices. Similarly, if limits are provided for any seasons, they must be provided for ALL
    /// seasons.
    ///
    /// No calculations are done here to account for time slice lengths; this must be handled by the
    /// user when providing the limits. For example, a limit of 0..=0.1 for a time slice indicates
    /// that 10% of the potential annual activity can be undertaken in that time slice. If the time
    /// slice is 10% of the year, then this provides no additional constraint.
    ///
    /// Checks are done to ensure that provided limits are compatible with each other. For example,
    /// a limit of "..0.01" for "winter" would be incompatible with a limit of "0.2.." for
    /// "winter.day" (i.e. if activity must be >0.2 in "winter.night", then if cannot possibly be
    /// ≤0.01 in winter as a whole).
    pub fn new_from_limits(
        limits: &HashMap<TimeSliceSelection, RangeInclusive<Dimensionless>>,
        time_slice_info: &TimeSliceInfo,
    ) -> Result<Self> {
        let mut result = ActivityLimits::new_with_full_availability(time_slice_info);

        // Add time slice limits first
        let mut time_slices_added = IndexSet::new();
        for (ts_selection, limit) in limits {
            if let TimeSliceSelection::Single(ts_id) = ts_selection {
                result.add_time_slice_limit(ts_id.clone(), limit.clone());
                time_slices_added.insert(ts_id.clone());
            }
        }

        // Check that limits have been added for all or no time slices
        if !time_slices_added.is_empty() {
            let missing = time_slice_info
                .iter_ids()
                .filter(|ts_id| !time_slices_added.contains(*ts_id))
                .collect::<Vec<_>>();
            ensure!(
                missing.is_empty(),
                "Missing availability limits for time slices: [{}]. Please provide",
                missing.iter().join(", ")
            );
        }

        // Then add seasonal limits
        // Error will be raised if seasonal limits are incompatible with time slice limits
        let mut seasons_added = IndexSet::new();
        for (ts_selection, limit) in limits {
            if let TimeSliceSelection::Season(season) = ts_selection {
                result.add_seasonal_limit(season.clone(), limit.clone())?;
                seasons_added.insert(season.clone());
            }
        }

        // Check that limits have been added for all or no seasons
        if !seasons_added.is_empty() {
            let missing = time_slice_info
                .iter_seasons()
                .filter(|season| !seasons_added.contains(*season))
                .collect::<Vec<_>>();
            ensure!(
                missing.is_empty(),
                "Missing availability limits for seasons: [{}]. Please provide",
                missing.iter().join(", "),
            );
        }

        // Then add annual limit
        // Error will be raised if annual limit is incompatible with time slice/seasonal limits
        if let Some(limit) = limits.get(&TimeSliceSelection::Annual) {
            result.add_annual_limit(limit.clone())?;
        }

        Ok(result)
    }

    /// Add a limit for a specific time slice
    pub fn add_time_slice_limit(
        &mut self,
        ts_id: TimeSliceID,
        limit: RangeInclusive<Dimensionless>,
    ) {
        self.time_slice_limits.insert(ts_id, limit);
    }

    /// Add a limit for a specific season
    fn add_seasonal_limit(
        &mut self,
        season: Season,
        limit: RangeInclusive<Dimensionless>,
    ) -> Result<()> {
        // Get current limit for the season
        let current_limit = self.get_limit_for_season(&season);

        // Ensure that the new limit overlaps with the current limit
        // If not, it's impossible to satisfy both limits, so we must exit with an error
        ensure!(
            *limit.start() <= *current_limit.end() && *limit.end() >= *current_limit.start(),
            "Availability limit for season {season} clashes with time slice limits",
        );

        // Only insert the seasonal limit if it provides an extra level of constraint above the
        // existing time slice limits. This is to minimize the number of seasonal limits stored and
        // returned by `iter_limits()`, therefore preventing unnecessary constraints from being
        // added to the optimization model.
        if *limit.start() > *current_limit.start() || *limit.end() < *current_limit.end() {
            self.seasonal_limits.insert(season, limit);
        }

        Ok(())
    }

    /// Add an annual limit
    fn add_annual_limit(&mut self, limit: RangeInclusive<Dimensionless>) -> Result<()> {
        // Get current limit for the year
        let current_limit = self.get_limit_for_year(&TimeSliceInfo::default());

        // Ensure that the new limit overlaps with the current limit
        // If not, it's impossible to satisfy both limits, so we must exit with an error
        ensure!(
            *limit.start() <= *current_limit.end() && *limit.end() >= *current_limit.start(),
            "Annual availability limit clashes with time slice/seasonal limits",
        );

        // Only insert the annual limit if it provides an extra level of constraint above the
        // existing time slice/seasonal limits. This prevents unnecessary constraints from being
        // stored and added to the optimization model.
        if *limit.start() > *current_limit.start() || *limit.end() < *current_limit.end() {
            self.annual_limit = Some(limit);
        }

        Ok(())
    }

    /// Get the limit for a given time slice selection
    pub fn get_limit(
        &self,
        time_slice_selection: &TimeSliceSelection,
        time_slice_info: &TimeSliceInfo,
    ) -> RangeInclusive<Dimensionless> {
        match time_slice_selection {
            TimeSliceSelection::Single(ts_id) => self.get_limit_for_time_slice(ts_id),
            TimeSliceSelection::Season(season) => self.get_limit_for_season(season),
            TimeSliceSelection::Annual => self.get_limit_for_year(time_slice_info),
        }
    }

    /// Get the limit for a given time slice
    pub fn get_limit_for_time_slice(
        &self,
        time_slice: &TimeSliceID,
    ) -> RangeInclusive<Dimensionless> {
        // Get limit for this specific time slice
        let ts_limit = self.time_slice_limits[time_slice].clone();
        let lower = *ts_limit.start();
        let mut upper = *ts_limit.end();

        // If there's a seasonal/annual limit, we must cap the timeslice limit to ensure that it
        // doesn't exceed the upper bound of the season/year
        if let Some(seasonal_limit) = self.seasonal_limits.get(&time_slice.season) {
            upper = upper.min(*seasonal_limit.end());
        }
        if let Some(annual_limit) = &self.annual_limit {
            upper = upper.min(*annual_limit.end());
        }

        lower..=upper
    }

    /// Get the limit for a given season
    fn get_limit_for_season(&self, season: &Season) -> RangeInclusive<Dimensionless> {
        // Get sum of limits for all time slices in this season
        let mut lower = Dimensionless(0.0);
        let mut upper = Dimensionless(0.0);
        for (ts, limit) in &self.time_slice_limits {
            if &ts.season == season {
                lower += *limit.start();
                upper += *limit.end();
            }
        }

        // Bound this by the seasonal limit, if specified
        if let Some(seasonal_limit) = self.seasonal_limits.get(season) {
            lower = lower.max(*seasonal_limit.start());
            upper = upper.min(*seasonal_limit.end());
        }

        // If there's an annual limit, we must also cap the seasonal limit to ensure it doesn't
        // exceed the upper bound of the year
        if let Some(annual_limit) = &self.annual_limit {
            upper = upper.min(*annual_limit.end());
        }

        lower..=upper
    }

    /// Get the limit for the entire year
    fn get_limit_for_year(&self, time_slice_info: &TimeSliceInfo) -> RangeInclusive<Dimensionless> {
        // Get the sum of limits for all seasons
        let mut total_lower = Dimensionless(0.0);
        let mut total_upper = Dimensionless(0.0);
        for ts_selection in time_slice_info.iter_selections_at_level(TimeSliceLevel::Season) {
            let TimeSliceSelection::Season(season) = ts_selection else {
                panic!("Expected season selection")
            };
            let season_limit = self.get_limit_for_season(&season);
            total_lower += *season_limit.start();
            total_upper += *season_limit.end();
        }

        // Bound this by the annual limit, if specified
        if let Some(annual_limit) = &self.annual_limit {
            total_lower = total_lower.max(*annual_limit.start());
            total_upper = total_upper.min(*annual_limit.end());
        }

        total_lower..=total_upper
    }

    /// Iterate over all limits
    ///
    /// This first iterates over all individual timeslice limits, followed by seasonal limits (if
    /// any), and finally the annual limit (if any).
    pub fn iter_limits(
        &self,
    ) -> impl Iterator<Item = (TimeSliceSelection, &RangeInclusive<Dimensionless>)> {
        // Iterate over all time slice limits
        let time_slice_limits = self
            .time_slice_limits
            .iter()
            .map(|(ts_id, limit)| (TimeSliceSelection::Single(ts_id.clone()), limit));

        // Then seasonal limits, if any
        let seasonal_limits = self
            .seasonal_limits
            .iter()
            .map(|(season, limit)| (TimeSliceSelection::Season(season.clone()), limit));

        // Then annual limit, if any
        let annual_limits = self
            .annual_limit
            .as_ref()
            .map(|limit| (TimeSliceSelection::Annual, limit));

        // Chain all limits together
        time_slice_limits
            .chain(seasonal_limits)
            .chain(annual_limits)
    }
}

/// Represents a maximum annual commodity coeff for a given process
#[derive(PartialEq, Debug, Clone)]
pub struct ProcessFlow {
    /// The commodity produced or consumed by this flow
    pub commodity: Rc<Commodity>,
    /// Maximum annual commodity flow quantity relative to other commodity flows.
    ///
    /// Positive value indicates flow out and negative value indicates flow in.
    pub coeff: FlowPerActivity,
    /// Identifies if a flow is fixed or flexible.
    pub kind: FlowType,
    /// Cost per unit flow.
    ///
    /// For example, cost per unit of natural gas produced. The user can apply it to any specified
    /// flow.
    pub cost: MoneyPerFlow,
}

impl ProcessFlow {
    /// Get the cost per unit flow for a given region, year, and time slice.
    ///
    /// Includes flow costs and levies/incentives, if any.
    pub fn get_total_cost_per_flow(
        &self,
        region_id: &RegionID,
        year: u32,
        time_slice: &TimeSliceID,
    ) -> MoneyPerFlow {
        self.cost + self.get_levy(region_id, year, time_slice)
    }

    /// Get the cost for this flow per unit of activity for a given region, year, and time slice.
    ///
    /// This includes cost per unit flow and levies/incentives, if any.
    pub fn get_total_cost_per_activity(
        &self,
        region_id: &RegionID,
        year: u32,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        let cost_per_unit = self.get_total_cost_per_flow(region_id, year, time_slice);
        self.coeff.abs() * cost_per_unit
    }

    /// Get the levy/incentive for this process flow with the given parameters, if any
    fn get_levy(&self, region_id: &RegionID, year: u32, time_slice: &TimeSliceID) -> MoneyPerFlow {
        match self.direction() {
            FlowDirection::Input => *self
                .commodity
                .levies_cons
                .get(&(region_id.clone(), year, time_slice.clone()))
                .unwrap_or(&MoneyPerFlow(0.0)),
            FlowDirection::Output => *self
                .commodity
                .levies_prod
                .get(&(region_id.clone(), year, time_slice.clone()))
                .unwrap_or(&MoneyPerFlow(0.0)),
            FlowDirection::Zero => MoneyPerFlow(0.0),
        }
    }

    /// Direction of the flow
    pub fn direction(&self) -> FlowDirection {
        match self.coeff {
            x if x < FlowPerActivity(0.0) => FlowDirection::Input,
            x if x > FlowPerActivity(0.0) => FlowDirection::Output,
            _ => FlowDirection::Zero,
        }
    }
}

/// Type of commodity flow (see [`ProcessFlow`])
#[derive(PartialEq, Default, Debug, Clone, DeserializeLabeledStringEnum)]
pub enum FlowType {
    /// The input to output flow ratio is fixed
    #[default]
    #[string = "fixed"]
    Fixed,
    /// The flow ratio can vary, subject to overall flow of a specified group of commodities whose
    /// input/output ratio must be as per user input data
    #[string = "flexible"]
    Flexible,
}

/// Direction of the flow (see [`ProcessFlow`])
#[derive(PartialEq, Debug)]
pub enum FlowDirection {
    /// The flow is an input (i.e., coeff < 0)
    Input,
    /// The flow is an output (i.e., coeff > 0)
    Output,
    /// The flow is zero, neither input nor output (i.e., coeff == 0)
    Zero,
}

/// Additional parameters for a process
#[derive(PartialEq, Clone, Debug)]
pub struct ProcessParameter {
    /// Overnight capital cost per unit capacity
    pub capital_cost: MoneyPerCapacity,
    /// Annual operating cost per unit capacity
    pub fixed_operating_cost: MoneyPerCapacityPerYear,
    /// Annual variable operating cost per unit activity
    pub variable_operating_cost: MoneyPerActivity,
    /// Lifetime in years of an asset created from this process
    pub lifetime: u32,
    /// Process-specific discount rate
    pub discount_rate: Dimensionless,
}

/// A constraint imposed on investments in the process
#[derive(PartialEq, Debug, Clone)]
pub struct ProcessInvestmentConstraint {
    /// Addition constraint: Yearly limit an agent can invest
    /// in the process, shared according to the agent's
    /// proportion of the processes primary commodity demand
    pub addition_limit: Option<f64>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::{CommodityLevyMap, CommodityType, DemandMap, PricingStrategy};
    use crate::fixture::{assert_error, region_id, time_slice, time_slice_info2};
    use crate::time_slice::TimeSliceLevel;
    use crate::time_slice::TimeSliceSelection;
    use float_cmp::assert_approx_eq;
    use rstest::{fixture, rstest};
    use std::collections::HashMap;
    use std::rc::Rc;

    #[fixture]
    fn commodity_with_levy(region_id: RegionID, time_slice: TimeSliceID) -> Rc<Commodity> {
        let mut levies_prod = CommodityLevyMap::new();
        let mut levies_cons = CommodityLevyMap::new();

        // Add levy for the default region and time slice
        levies_prod.insert(
            (region_id.clone(), 2020, time_slice.clone()),
            MoneyPerFlow(10.0),
        );
        levies_cons.insert(
            (region_id.clone(), 2020, time_slice.clone()),
            MoneyPerFlow(-10.0),
        );
        // Add levy for a different region
        levies_prod.insert(("USA".into(), 2020, time_slice.clone()), MoneyPerFlow(5.0));
        levies_cons.insert(("USA".into(), 2020, time_slice.clone()), MoneyPerFlow(-5.0));
        // Add levy for a different year
        levies_prod.insert(
            (region_id.clone(), 2030, time_slice.clone()),
            MoneyPerFlow(7.0),
        );
        levies_cons.insert(
            (region_id.clone(), 2030, time_slice.clone()),
            MoneyPerFlow(-7.0),
        );
        // Add levy for a different time slice
        levies_prod.insert(
            (
                region_id.clone(),
                2020,
                TimeSliceID {
                    season: "summer".into(),
                    time_of_day: "day".into(),
                },
            ),
            MoneyPerFlow(3.0),
        );
        levies_cons.insert(
            (
                region_id.clone(),
                2020,
                TimeSliceID {
                    season: "summer".into(),
                    time_of_day: "day".into(),
                },
            ),
            MoneyPerFlow(-3.0),
        );

        Rc::new(Commodity {
            id: "test_commodity".into(),
            description: "Test commodity".into(),
            kind: CommodityType::ServiceDemand,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy: PricingStrategy::Shadow,
            levies_prod: levies_prod,
            levies_cons: levies_cons,
            demand: DemandMap::new(),
        })
    }

    #[fixture]
    fn commodity_with_consumption_levy(
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) -> Rc<Commodity> {
        let mut levies = CommodityLevyMap::new();
        levies.insert((region_id, 2020, time_slice), MoneyPerFlow(10.0));

        Rc::new(Commodity {
            id: "test_commodity".into(),
            description: "Test commodity".into(),
            kind: CommodityType::ServiceDemand,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy: PricingStrategy::Shadow,
            levies_prod: CommodityLevyMap::new(),
            levies_cons: levies,
            demand: DemandMap::new(),
        })
    }

    #[fixture]
    fn commodity_with_production_levy(
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) -> Rc<Commodity> {
        let mut levies = CommodityLevyMap::new();
        levies.insert((region_id, 2020, time_slice), MoneyPerFlow(10.0));

        Rc::new(Commodity {
            id: "test_commodity".into(),
            description: "Test commodity".into(),
            kind: CommodityType::ServiceDemand,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy: PricingStrategy::Shadow,
            levies_prod: levies,
            levies_cons: CommodityLevyMap::new(),
            demand: DemandMap::new(),
        })
    }

    #[fixture]
    fn commodity_with_incentive(region_id: RegionID, time_slice: TimeSliceID) -> Rc<Commodity> {
        let mut levies_prod = CommodityLevyMap::new();
        levies_prod.insert(
            (region_id.clone(), 2020, time_slice.clone()),
            MoneyPerFlow(-5.0),
        );
        let mut levies_cons = CommodityLevyMap::new();
        levies_cons.insert((region_id, 2020, time_slice), MoneyPerFlow(5.0));

        Rc::new(Commodity {
            id: "test_commodity".into(),
            description: "Test commodity".into(),
            kind: CommodityType::ServiceDemand,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy: PricingStrategy::Shadow,
            levies_prod: levies_prod,
            levies_cons: levies_cons,
            demand: DemandMap::new(),
        })
    }

    #[fixture]
    fn commodity_no_levies() -> Rc<Commodity> {
        Rc::new(Commodity {
            id: "test_commodity".into(),
            description: "Test commodity".into(),
            kind: CommodityType::ServiceDemand,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy: PricingStrategy::Shadow,
            levies_prod: CommodityLevyMap::new(),
            levies_cons: CommodityLevyMap::new(),
            demand: DemandMap::new(),
        })
    }

    #[fixture]
    fn flow_with_cost() -> ProcessFlow {
        ProcessFlow {
            commodity: Rc::new(Commodity {
                id: "test_commodity".into(),
                description: "Test commodity".into(),
                kind: CommodityType::ServiceDemand,
                time_slice_level: TimeSliceLevel::Annual,
                pricing_strategy: PricingStrategy::Shadow,
                levies_prod: CommodityLevyMap::new(),
                levies_cons: CommodityLevyMap::new(),
                demand: DemandMap::new(),
            }),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(5.0),
        }
    }

    #[fixture]
    fn flow_with_cost_and_levy(region_id: RegionID, time_slice: TimeSliceID) -> ProcessFlow {
        let mut levies = CommodityLevyMap::new();
        levies.insert((region_id, 2020, time_slice), MoneyPerFlow(10.0));

        ProcessFlow {
            commodity: Rc::new(Commodity {
                id: "test_commodity".into(),
                description: "Test commodity".into(),
                kind: CommodityType::ServiceDemand,
                time_slice_level: TimeSliceLevel::Annual,
                pricing_strategy: PricingStrategy::Shadow,
                levies_prod: levies,
                levies_cons: CommodityLevyMap::new(),
                demand: DemandMap::new(),
            }),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(5.0),
        }
    }

    #[fixture]
    fn flow_with_cost_and_incentive(region_id: RegionID, time_slice: TimeSliceID) -> ProcessFlow {
        let mut levies = CommodityLevyMap::new();
        levies.insert((region_id, 2020, time_slice), MoneyPerFlow(-3.0));

        ProcessFlow {
            commodity: Rc::new(Commodity {
                id: "test_commodity".into(),
                description: "Test commodity".into(),
                kind: CommodityType::ServiceDemand,
                time_slice_level: TimeSliceLevel::Annual,
                pricing_strategy: PricingStrategy::Shadow,
                levies_prod: levies,
                levies_cons: CommodityLevyMap::new(),
                demand: DemandMap::new(),
            }),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(5.0),
        }
    }

    #[rstest]
    fn test_get_levy_no_levies(
        commodity_no_levies: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_no_levies,
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(0.0)
        );
    }

    #[rstest]
    fn test_get_levy_with_levy(
        commodity_with_levy: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_levy,
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(10.0)
        );
    }

    #[rstest]
    fn test_get_levy_with_incentive(
        commodity_with_incentive: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_incentive,
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(-5.0)
        );
    }

    #[rstest]
    fn test_get_levy_different_region(commodity_with_levy: Rc<Commodity>, time_slice: TimeSliceID) {
        let flow = ProcessFlow {
            commodity: commodity_with_levy,
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&"USA".into(), 2020, &time_slice),
            MoneyPerFlow(5.0)
        );
    }

    #[rstest]
    fn test_get_levy_different_year(
        commodity_with_levy: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_levy,
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2030, &time_slice),
            MoneyPerFlow(7.0)
        );
    }

    #[rstest]
    fn test_get_levy_different_time_slice(commodity_with_levy: Rc<Commodity>, region_id: RegionID) {
        let flow = ProcessFlow {
            commodity: commodity_with_levy,
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        let different_time_slice = TimeSliceID {
            season: "summer".into(),
            time_of_day: "day".into(),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &different_time_slice),
            MoneyPerFlow(3.0)
        );
    }

    #[rstest]
    fn test_get_levy_consumption_positive_coeff(
        commodity_with_consumption_levy: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_consumption_levy,
            coeff: FlowPerActivity(1.0), // Positive coefficient means production
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(0.0)
        );
    }

    #[rstest]
    fn test_get_levy_consumption_negative_coeff(
        commodity_with_consumption_levy: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_consumption_levy,
            coeff: FlowPerActivity(-1.0), // Negative coefficient means consumption
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(10.0)
        );
    }

    #[rstest]
    fn test_get_levy_production_positive_coeff(
        commodity_with_production_levy: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_production_levy,
            coeff: FlowPerActivity(1.0), // Positive coefficient means production
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(10.0)
        );
    }

    #[rstest]
    fn test_get_levy_production_negative_coeff(
        commodity_with_production_levy: Rc<Commodity>,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        let flow = ProcessFlow {
            commodity: commodity_with_production_levy,
            coeff: FlowPerActivity(-1.0), // Negative coefficient means consumption
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert_eq!(
            flow.get_levy(&region_id, 2020, &time_slice),
            MoneyPerFlow(0.0)
        );
    }

    #[rstest]
    fn test_get_total_cost_base_cost(
        flow_with_cost: ProcessFlow,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        assert_eq!(
            flow_with_cost.get_total_cost_per_activity(&region_id, 2020, &time_slice),
            MoneyPerActivity(5.0)
        );
    }

    #[rstest]
    fn test_get_total_cost_with_levy(
        flow_with_cost_and_levy: ProcessFlow,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        assert_eq!(
            flow_with_cost_and_levy.get_total_cost_per_activity(&region_id, 2020, &time_slice),
            MoneyPerActivity(15.0)
        );
    }

    #[rstest]
    fn test_get_total_cost_with_incentive(
        flow_with_cost_and_incentive: ProcessFlow,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        assert_eq!(
            flow_with_cost_and_incentive.get_total_cost_per_activity(&region_id, 2020, &time_slice),
            MoneyPerActivity(2.0)
        );
    }

    #[rstest]
    fn test_get_total_cost_negative_coeff(
        mut flow_with_cost: ProcessFlow,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        flow_with_cost.coeff = FlowPerActivity(-2.0);
        assert_eq!(
            flow_with_cost.get_total_cost_per_activity(&region_id, 2020, &time_slice),
            MoneyPerActivity(10.0)
        );
    }

    #[rstest]
    fn test_get_total_cost_zero_coeff(
        mut flow_with_cost: ProcessFlow,
        region_id: RegionID,
        time_slice: TimeSliceID,
    ) {
        flow_with_cost.coeff = FlowPerActivity(0.0);
        assert_eq!(
            flow_with_cost.get_total_cost_per_activity(&region_id, 2020, &time_slice),
            MoneyPerActivity(0.0)
        );
    }

    #[test]
    fn test_is_input_and_is_output() {
        let commodity = Rc::new(Commodity {
            id: "test_commodity".into(),
            description: "Test commodity".into(),
            kind: CommodityType::ServiceDemand,
            time_slice_level: TimeSliceLevel::Annual,
            pricing_strategy: PricingStrategy::Shadow,
            levies_prod: CommodityLevyMap::new(),
            levies_cons: CommodityLevyMap::new(),
            demand: DemandMap::new(),
        });

        let flow_in = ProcessFlow {
            commodity: Rc::clone(&commodity),
            coeff: FlowPerActivity(-1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let flow_out = ProcessFlow {
            commodity: Rc::clone(&commodity),
            coeff: FlowPerActivity(1.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };
        let flow_zero = ProcessFlow {
            commodity: Rc::clone(&commodity),
            coeff: FlowPerActivity(0.0),
            kind: FlowType::Fixed,
            cost: MoneyPerFlow(0.0),
        };

        assert!(flow_in.direction() == FlowDirection::Input);
        assert!(flow_out.direction() == FlowDirection::Output);
        assert!(flow_zero.direction() == FlowDirection::Zero);
    }

    #[rstest]
    fn test_new_with_full_availability(time_slice_info2: TimeSliceInfo) {
        let limits = ActivityLimits::new_with_full_availability(&time_slice_info2);

        // Each timeslice from the info should be present in the limits
        for (ts_id, ts_len) in time_slice_info2.iter() {
            let l = limits.get_limit_for_time_slice(&ts_id);
            // Lower bound should be zero and upper bound equal to timeslice length
            assert_eq!(*l.start(), Dimensionless(0.0));
            assert_eq!(*l.end(), Dimensionless(ts_len.value()));
        }

        // Annual limit should be 0..1
        let annual_limit = limits.get_limit(&TimeSliceSelection::Annual, &time_slice_info2);
        assert_approx_eq!(Dimensionless, *annual_limit.start(), Dimensionless(0.0));
        assert_approx_eq!(Dimensionless, *annual_limit.end(), Dimensionless(1.0));
    }

    #[rstest]
    fn test_new_from_limits_with_seasonal_limit_applied(time_slice_info2: TimeSliceInfo) {
        let mut limits = HashMap::new();

        // Set a seasonal upper limit that is stricter than the sum of timeslices
        limits.insert(
            TimeSliceSelection::Season("winter".into()),
            Dimensionless(0.0)..=Dimensionless(0.01),
        );

        let result = ActivityLimits::new_from_limits(&limits, &time_slice_info2).unwrap();

        // Each timeslice upper bound should be capped by the seasonal upper bound (0.01)
        for (ts_id, _ts_len) in time_slice_info2.iter() {
            let ts_limit = result.get_limit_for_time_slice(&ts_id);
            assert_eq!(*ts_limit.end(), Dimensionless(0.01));
        }

        // The seasonal limit should reflect the given bound
        let season_limit = result.get_limit(
            &TimeSliceSelection::Season("winter".into()),
            &time_slice_info2,
        );
        assert_eq!(*season_limit.end(), Dimensionless(0.01));
    }

    #[rstest]
    fn test_new_from_limits_missing_timeslices_error(time_slice_info2: TimeSliceInfo) {
        let mut limits = HashMap::new();

        // Add a single timeslice limit but do not provide limits for all timeslices
        let first_ts = time_slice_info2.iter().next().unwrap().0.clone();
        limits.insert(
            TimeSliceSelection::Single(first_ts),
            Dimensionless(0.0)..=Dimensionless(0.1),
        );

        assert_error!(
            ActivityLimits::new_from_limits(&limits, &time_slice_info2),
            "Missing availability limits for time slices: [winter.night]. Please provide"
        );
    }

    #[rstest]
    fn test_new_from_limits_incompatible_limits(time_slice_info2: TimeSliceInfo) {
        let mut limits = HashMap::new();

        // Time slice limits capping activity to 0.1 in each ts
        for (ts_id, _ts_len) in time_slice_info2.iter() {
            limits.insert(
                TimeSliceSelection::Single(ts_id.clone()),
                Dimensionless(0.0)..=Dimensionless(0.1),
            );
        }

        // Seasonal limit that is incompatible (lower limit above the sum of time slice upper limits)
        limits.insert(
            TimeSliceSelection::Season("winter".into()),
            Dimensionless(0.99)..=Dimensionless(1.0),
        );

        assert_error!(
            ActivityLimits::new_from_limits(&limits, &time_slice_info2),
            "Availability limit for season winter clashes with time slice limits"
        );
    }
}
