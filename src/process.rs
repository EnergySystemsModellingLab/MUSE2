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
use indexmap::{IndexMap, IndexSet};
use serde_string_enum::DeserializeLabeledStringEnum;
use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::rc::Rc;

define_id_type! {ProcessID}

/// A map of [`Process`]es, keyed by process ID
pub type ProcessMap = IndexMap<ProcessID, Rc<Process>>;

/// A map indicating activity limits for a [`Process`] throughout the year.
pub type ProcessActivityLimitsMap = HashMap<(RegionID, u32), Rc<ProcessAvailabilities>>;

/// A map of [`ProcessParameter`]s, keyed by region and year
pub type ProcessParameterMap = HashMap<(RegionID, u32), Rc<ProcessParameter>>;

/// A map of process flows, keyed by region and year.
///
/// The value is actually a map itself, keyed by commodity ID.
pub type ProcessFlowsMap = HashMap<(RegionID, u32), Rc<IndexMap<CommodityID, ProcessFlow>>>;

/// Map of process investment constraints, keyed by region and year
pub type ProcessInvestmentConstraintsMap =
    HashMap<(RegionID, u32), Rc<Vec<ProcessInvestmentConstraint>>>;

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

// Defines the availability limits for a process in a given region and year
///
/// Values are calculated as availability multiplied by time slice/season length. The limits are
/// given as ranges, depending on the user-specified limit type and value for availability.
///
/// All time slices must have an entry in `time_slice_limits`. Seasonal and annual limits may be
/// present if provided by the user, and only if they provide an extra level of constraint on top of
/// the time slice limits.
#[derive(PartialEq, Debug, Clone, Default)]
pub struct ProcessAvailabilities {
    /// Optional annual limit
    pub annual_limit: Option<RangeInclusive<Dimensionless>>,
    /// Optional limits for each season
    pub seasonal_limits: IndexMap<Season, RangeInclusive<Dimensionless>>,
    /// Limits for each time slice (mandatory for all time slices)
    pub time_slice_limits: IndexMap<TimeSliceID, RangeInclusive<Dimensionless>>,
}

impl ProcessAvailabilities {
    /// Get the availability limit for a given time slice selection
    pub fn get_availability_limit(
        &self,
        time_slice_selection: &TimeSliceSelection,
        time_slice_info: &TimeSliceInfo,
    ) -> RangeInclusive<Dimensionless> {
        match time_slice_selection {
            TimeSliceSelection::Single(ts_id) => self.get_availability_limit_for_time_slice(ts_id),
            TimeSliceSelection::Season(season) => self.get_availability_limit_for_season(season),
            TimeSliceSelection::Annual => self.get_availability_limit_for_year(time_slice_info),
        }
    }

    /// Get the availability limit for a given time slice
    pub fn get_availability_limit_for_time_slice(
        &self,
        time_slice: &TimeSliceID,
    ) -> RangeInclusive<Dimensionless> {
        // Get limit for this specific time slice
        let ts_limit = self.time_slice_limits[time_slice].clone();
        let ts_lower = *ts_limit.start();
        let ts_upper = *ts_limit.end();

        // Time slice availability cannot exceed seasonal or annual limits, if specified
        let seasonal_upper = self
            .seasonal_limits
            .get(&time_slice.season)
            .map_or(Dimensionless(1.0), |limit| *limit.end());
        let annual_upper = self
            .annual_limit
            .as_ref()
            .map_or(Dimensionless(1.0), |limit| *limit.end());
        let max_limit = ts_upper.min(seasonal_upper).min(annual_upper);

        ts_lower..=max_limit
    }

    /// Get the availability limit for a given season
    fn get_availability_limit_for_season(&self, season: &Season) -> RangeInclusive<Dimensionless> {
        // Get sum of limits for all time slices in this season
        let mut total_lower = Dimensionless(0.0);
        let mut total_upper = Dimensionless(0.0);
        for (ts, limit) in &self.time_slice_limits {
            if &ts.season == season {
                total_lower += *limit.start();
                total_upper += *limit.end();
            }
        }

        // Also get seasonal limit, if specified
        if let Some(seasonal_limit) = self.seasonal_limits.get(season) {
            total_lower = total_lower.max(*seasonal_limit.start());
            total_upper = total_upper.min(*seasonal_limit.end());
        }

        // Seasonal availability cannot exceed the upper annual limit, if specified
        if let Some(annual_limit) = &self.annual_limit {
            total_upper = total_upper.min(*annual_limit.end());
        }

        total_lower..=total_upper
    }

    /// Get the availability limit for the entire year
    fn get_availability_limit_for_year(
        &self,
        time_slice_info: &TimeSliceInfo,
    ) -> RangeInclusive<Dimensionless> {
        // Sum limits for each season
        let mut total_lower = Dimensionless(0.0);
        let mut total_upper = Dimensionless(0.0);
        for ts_selection in time_slice_info.iter_selections_at_level(TimeSliceLevel::Season) {
            let TimeSliceSelection::Season(season) = ts_selection else {
                panic!("Expected season selection")
            };
            let season_limit = self.get_availability_limit_for_season(&season);
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

    /// Iterate over all time slice availability limits
    ///
    /// This first iterates over all individual timeslice limits, followed by seasonal limits (if
    /// any), and finally the annual limit (if any).
    pub fn iter_availability_limits(
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
    /// Get the cost for this flow with the given parameters.
    ///
    /// This includes cost per unit flow and levies/incentives, if any.
    pub fn get_total_cost(
        &self,
        region_id: &RegionID,
        year: u32,
        time_slice: &TimeSliceID,
    ) -> MoneyPerActivity {
        let cost_per_unit = self.cost + self.get_levy(region_id, year, time_slice);

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

/// Value specification for different possible types of investment constraints
#[derive(Debug, Clone, PartialEq)]
pub enum InvestmentConstraintValue {
    /// Addition constraint: Yearly limit an agent can invest
    /// in the process, shared according to the agent's
    /// proportion of the processes primary commodity demand
    Addition {
        /// constraint value to apply
        addition_limit: f64,
    },
    /// Growth constraint: Not implemented yet
    Growth {
        /// growth constraint seed value
        growth_constraint_seed: f64,
    },
    /// Limit constraint: Not implemented yet
    Limit {},
}

/// A constraint imposed on investments in the process
#[derive(PartialEq, Debug, Clone)]
pub struct ProcessInvestmentConstraint {
    /// The name of the investment constraint
    pub constraint_name: String,
    /// The parameters value required to impose the constraint
    pub constraint_value: InvestmentConstraintValue,
    /// The limit type for the constraint
    pub limit_type: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::{CommodityLevyMap, CommodityType, DemandMap};
    use crate::fixture::{region_id, time_slice};
    use crate::time_slice::TimeSliceLevel;
    use rstest::{fixture, rstest};
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
            flow_with_cost.get_total_cost(&region_id, 2020, &time_slice),
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
            flow_with_cost_and_levy.get_total_cost(&region_id, 2020, &time_slice),
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
            flow_with_cost_and_incentive.get_total_cost(&region_id, 2020, &time_slice),
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
            flow_with_cost.get_total_cost(&region_id, 2020, &time_slice),
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
            flow_with_cost.get_total_cost(&region_id, 2020, &time_slice),
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
}
