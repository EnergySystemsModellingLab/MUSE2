//! General functions related to finance.
use crate::time_slice::TimeSliceID;
use crate::units::{Activity, Capacity, Dimensionless, Money, MoneyPerActivity, MoneyPerCapacity};
use indexmap::IndexMap;

/// Calculates the capital recovery factor (CRF) for a given lifetime and discount rate.
///
/// The CRF is used to annualize capital costs over the lifetime of an asset.
pub fn capital_recovery_factor(lifetime: u32, discount_rate: Dimensionless) -> Dimensionless {
    if lifetime == 0 {
        return Dimensionless(0.0);
    }
    if discount_rate == Dimensionless(0.0) {
        return Dimensionless(1.0) / Dimensionless(lifetime as f64);
    }

    let factor = (Dimensionless(1.0) + discount_rate).powi(lifetime.try_into().unwrap());
    (discount_rate * factor) / (factor - Dimensionless(1.0))
}

/// Calculates the annual capital cost for a process per unit of capacity
pub fn annual_capital_cost(
    capital_cost: MoneyPerCapacity,
    lifetime: u32,
    discount_rate: Dimensionless,
) -> MoneyPerCapacity {
    let crf = capital_recovery_factor(lifetime, discount_rate);
    capital_cost * crf
}

/// Calculates the SNAS (Specific Net Annualised Surplus) based on capacity and activity.
///
/// It is just the negative of the LCOX, although, unlike LCOX, it should be called with
/// activity costs that INCLUDE the commodity of interest.
pub fn snas(
    capacity: Capacity,
    annual_fixed_cost: MoneyPerCapacity,
    activity: &IndexMap<TimeSliceID, Activity>,
    activity_costs: &IndexMap<TimeSliceID, MoneyPerActivity>,
) -> Option<MoneyPerActivity> {
    lcox(capacity, annual_fixed_cost, activity, activity_costs).map(|lcox| -lcox)
}

/// Calculates annual LCOX based on capacity and activity.
///
/// It should be called with activity costs that EXCLUDE the commodity of interest.
/// If the total activity is zero, then it returns `None`, otherwise `Some` LCOX value.
pub fn lcox(
    capacity: Capacity,
    annual_fixed_cost: MoneyPerCapacity,
    activity: &IndexMap<TimeSliceID, Activity>,
    activity_costs: &IndexMap<TimeSliceID, MoneyPerActivity>,
) -> Option<MoneyPerActivity> {
    // Calculate the annualised fixed costs
    let annualised_fixed_cost = annual_fixed_cost * capacity;

    // Calculate the total activity costs
    let mut total_activity_costs = Money(0.0);
    let mut total_activity = Activity(0.0);
    for (time_slice, activity) in activity {
        let activity_cost = activity_costs[time_slice];
        total_activity += *activity;
        total_activity_costs += activity_cost * *activity;
    }

    (total_activity > Activity(0.0))
        .then(|| (annualised_fixed_cost + total_activity_costs) / total_activity)
}

#[cfg(test)]
#[allow(clippy::unreadable_literal)]
mod tests {
    use super::*;
    use crate::time_slice::TimeSliceID;
    use float_cmp::assert_approx_eq;
    use rstest::rstest;

    #[rstest]
    #[case(0, 0.05, 0.0)] // Edge case: lifetime==0
    #[case(10, 0.0, 0.1)] // Other edge case: discount_rate==0
    #[case(10, 0.05, 0.1295045749654567)]
    #[case(5, 0.03, 0.2183545714005762)]
    fn capital_recovery_factor_works(
        #[case] lifetime: u32,
        #[case] discount_rate: f64,
        #[case] expected: f64,
    ) {
        let result = capital_recovery_factor(lifetime, Dimensionless(discount_rate));
        assert_approx_eq!(f64, result.0, expected, epsilon = 1e-10);
    }

    #[rstest]
    #[case(1000.0, 10, 0.05, 129.5045749654567)]
    #[case(500.0, 5, 0.03, 109.17728570028798)]
    #[case(1000.0, 0, 0.05, 0.0)] // Zero lifetime
    #[case(2000.0, 20, 0.0, 100.0)] // Zero discount rate
    fn annual_capital_cost_works(
        #[case] capital_cost: f64,
        #[case] lifetime: u32,
        #[case] discount_rate: f64,
        #[case] expected: f64,
    ) {
        let expected = MoneyPerCapacity(expected);
        let result = annual_capital_cost(
            MoneyPerCapacity(capital_cost),
            lifetime,
            Dimensionless(discount_rate),
        );
        assert_approx_eq!(MoneyPerCapacity, result, expected, epsilon = 1e-8);
    }

    #[rstest]
    #[case(
        100.0, 50.0,
        vec![("winter", "day", 10.0), ("summer", "night", 20.0)],
        vec![("winter", "day", 5.0), ("summer", "night", 3.0)],
        Some(170.33333333333334) // (100*50 + 10*5 + 20*3) / (10+20) = 5110/30
    )]
    #[case(
        50.0, 100.0,
        vec![("winter", "day", 25.0)],
        vec![("winter", "day", 0.0)],
        Some(200.0) // (50*100 + 25*0) / 25 = 5000/25
    )]
    #[case(
        50.0, 100.0,
        vec![("winter", "day", 0.0)],
        vec![("winter", "day", 0.0)],
        None // (50*0 + 25*0) / 0 = not feasible
    )]
    fn lcox_works(
        #[case] capacity: f64,
        #[case] annual_fixed_cost: f64,
        #[case] activity_data: Vec<(&str, &str, f64)>,
        #[case] cost_data: Vec<(&str, &str, f64)>,
        #[case] expected: Option<f64>,
    ) {
        let activity = activity_data
            .into_iter()
            .map(|(season, time_of_day, value)| {
                (
                    TimeSliceID {
                        season: season.into(),
                        time_of_day: time_of_day.into(),
                    },
                    Activity(value),
                )
            })
            .collect();

        let activity_costs = cost_data
            .into_iter()
            .map(|(season, time_of_day, value)| {
                (
                    TimeSliceID {
                        season: season.into(),
                        time_of_day: time_of_day.into(),
                    },
                    MoneyPerActivity(value),
                )
            })
            .collect();

        let result = lcox(
            Capacity(capacity),
            MoneyPerCapacity(annual_fixed_cost),
            &activity,
            &activity_costs,
        );

        let expected = expected.map(MoneyPerActivity);
        assert_approx_eq!(Option<MoneyPerActivity>, result, expected);
    }
}
