//! Represents the capacity of an asset
use crate::units::{Capacity, Dimensionless};
use std::cmp::Ordering;
use std::ops::{Add, Sub};

/// Capacity of an asset, which may be continuous or a discrete number of indivisible units
#[derive(Clone, PartialEq, Copy, Debug)]
pub enum AssetCapacity {
    /// Continuous capacity
    Continuous(Capacity),
    /// Discrete capacity represented by a number of indivisible units
    /// Stores: (number of units, unit size)
    Discrete(u32, Capacity),
}

impl Add for AssetCapacity {
    type Output = Self;

    // Add two AssetCapacity values together
    fn add(self, rhs: AssetCapacity) -> Self {
        match (self, rhs) {
            (AssetCapacity::Continuous(cap1), AssetCapacity::Continuous(cap2)) => {
                AssetCapacity::Continuous(cap1 + cap2)
            }
            (AssetCapacity::Discrete(units1, size1), AssetCapacity::Discrete(units2, size2)) => {
                Self::check_same_unit_size(size1, size2);
                AssetCapacity::Discrete(units1 + units2, size1)
            }
            _ => panic!("Cannot add different types of AssetCapacity ({self:?} and {rhs:?})"),
        }
    }
}

impl Sub for AssetCapacity {
    type Output = Self;

    // Subtract rhs from self, ensuring that the result is non-negative
    fn sub(self, rhs: AssetCapacity) -> Self {
        match (self, rhs) {
            (AssetCapacity::Continuous(cap1), AssetCapacity::Continuous(cap2)) => {
                AssetCapacity::Continuous((cap1 - cap2).max(Capacity(0.0)))
            }
            (AssetCapacity::Discrete(units1, size1), AssetCapacity::Discrete(units2, size2)) => {
                Self::check_same_unit_size(size1, size2);
                AssetCapacity::Discrete(units1 - units2.min(units1), size1)
            }
            _ => panic!("Cannot subtract different types of AssetCapacity ({self:?} and {rhs:?})"),
        }
    }
}

impl Eq for AssetCapacity {}

impl PartialOrd for AssetCapacity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AssetCapacity {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (AssetCapacity::Continuous(a), AssetCapacity::Continuous(b)) => a.total_cmp(b),
            (AssetCapacity::Discrete(units1, size1), AssetCapacity::Discrete(units2, size2)) => {
                Self::check_same_unit_size(*size1, *size2);
                units1.cmp(units2)
            }
            _ => panic!("Cannot compare different types of AssetCapacity ({self:?} and {other:?})"),
        }
    }
}

impl AssetCapacity {
    /// Validates that two discrete capacities have the same unit size.
    fn check_same_unit_size(size1: Capacity, size2: Capacity) {
        assert_eq!(
            size1, size2,
            "Can't perform operation on capacities with different unit sizes ({size1} and {size2})",
        );
    }

    /// Create an `AssetCapacity` from a total capacity and optional unit size
    ///
    /// If a unit size is provided, the capacity is represented as a discrete number of units,
    /// calculated as the ceiling of (capacity / `unit_size`). If no unit size is provided, the
    /// capacity is represented as continuous.
    pub fn from_capacity(capacity: Capacity, unit_size: Option<Capacity>) -> Self {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        match unit_size {
            Some(size) => {
                let num_units = (capacity / size).value().ceil() as u32;
                AssetCapacity::Discrete(num_units, size)
            }
            None => AssetCapacity::Continuous(capacity),
        }
    }

    /// Create an `AssetCapacity` from a total capacity and optional unit size
    ///
    /// If a unit size is provided, the capacity is represented as a discrete number of units,
    /// calculated as the floor of (capacity / `unit_size`). If no unit size is provided, the
    /// capacity is represented as continuous.
    pub fn from_capacity_floor(capacity: Capacity, unit_size: Option<Capacity>) -> Self {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        match unit_size {
            Some(size) => {
                let num_units = (capacity / size).value().floor() as u32;
                AssetCapacity::Discrete(num_units, size)
            }
            None => AssetCapacity::Continuous(capacity),
        }
    }

    /// Returns the total capacity represented by this `AssetCapacity`.
    pub fn total_capacity(&self) -> Capacity {
        match self {
            AssetCapacity::Continuous(cap) => *cap,
            AssetCapacity::Discrete(units, size) => *size * Dimensionless(*units as f64),
        }
    }

    /// Returns the number of units if this is a discrete capacity, or `None` if continuous.
    pub fn n_units(&self) -> Option<u32> {
        match self {
            AssetCapacity::Continuous(_) => None,
            AssetCapacity::Discrete(units, _) => Some(*units),
        }
    }

    /// Asserts that both capacities are the same type (both continuous or both discrete).
    pub fn assert_same_type(&self, other: AssetCapacity) {
        assert!(
            matches!(self, AssetCapacity::Continuous(_))
                == matches!(other, AssetCapacity::Continuous(_)),
            "Cannot change capacity type"
        );
    }

    /// Applies a limit factor to the capacity, scaling it accordingly.
    ///
    /// For discrete capacities, the number of units is scaled by the limit factor and rounded up to
    /// the nearest integer.
    pub fn apply_limit_factor(self, limit_factor: Dimensionless) -> Self {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        match self {
            AssetCapacity::Continuous(cap) => AssetCapacity::Continuous(cap * limit_factor),
            AssetCapacity::Discrete(units, size) => {
                let new_units = (units as f64 * limit_factor.value()).ceil() as u32;
                AssetCapacity::Discrete(new_units, size)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::units::{Capacity, Dimensionless};
    use rstest::rstest;

    #[rstest]
    #[case::exact_multiple(Capacity(12.0), Some(Capacity(4.0)), Some(3), Capacity(12.0))]
    #[case::rounded_up(Capacity(11.0), Some(Capacity(4.0)), Some(3), Capacity(12.0))]
    #[case::unit_size_greater_than_capacity(
        Capacity(3.0),
        Some(Capacity(4.0)),
        Some(1),
        Capacity(4.0)
    )]
    #[case::continuous(Capacity(5.5), None, None, Capacity(5.5))]
    fn from_capacity(
        #[case] capacity: Capacity,
        #[case] unit_size: Option<Capacity>,
        #[case] expected_n: Option<u32>,
        #[case] expected_total: Capacity,
    ) {
        let got = AssetCapacity::from_capacity(capacity, unit_size);
        assert_eq!(got.n_units(), expected_n);
        assert_eq!(got.total_capacity(), expected_total);
    }

    #[rstest]
    #[case::exact_multiple(Capacity(12.0), Some(Capacity(4.0)), Some(3), Capacity(12.0))]
    #[case::rounded_down(Capacity(11.0), Some(Capacity(4.0)), Some(2), Capacity(8.0))]
    #[case::unit_size_greater_than_capacity(
        Capacity(3.0),
        Some(Capacity(4.0)),
        Some(0),
        Capacity(0.0)
    )]
    #[case::continuous(Capacity(5.5), None, None, Capacity(5.5))]
    fn from_capacity_floor(
        #[case] capacity: Capacity,
        #[case] unit_size: Option<Capacity>,
        #[case] expected_n: Option<u32>,
        #[case] expected_total: Capacity,
    ) {
        let got = AssetCapacity::from_capacity_floor(capacity, unit_size);
        assert_eq!(got.n_units(), expected_n);
        assert_eq!(got.total_capacity(), expected_total);
    }

    #[rstest]
    #[case::round_up(3u32, Capacity(4.0), Dimensionless(0.5), 2u32)]
    #[case::exact(3u32, Capacity(4.0), Dimensionless(0.33), 1u32)]
    fn apply_limit_factor(
        #[case] start_units: u32,
        #[case] unit_size: Capacity,
        #[case] factor: Dimensionless,
        #[case] expected_units: u32,
    ) {
        let orig = AssetCapacity::Discrete(start_units, unit_size);
        let got = orig.apply_limit_factor(factor);
        assert_eq!(got, AssetCapacity::Discrete(expected_units, unit_size));
    }
}
