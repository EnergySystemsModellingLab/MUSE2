//! Represents the capacity of an asset
use crate::units::{Capacity, Dimensionless};
use std::cmp::Ordering;
use std::ops::{Add, Sub};

/// Capacity of an asset, which may be continuous or a discrete number of indivisible units
#[derive(Clone, PartialEq, Copy, Debug)]
pub struct AssetCapacity {
    /// Number of units
    num_units: u32,
    /// Size of each unit
    unit_size: Capacity,
}

impl AssetCapacity {
    /// Create a new `AssetCapacity` with the given number of units and unit size
    pub fn new(num_units: u32, unit_size: Capacity) -> Self {
        assert!(unit_size.is_finite(), "Unit size must be a finite number");
        assert!(num_units > 0, "Number of units must be a positive integer");
        AssetCapacity {
            num_units,
            unit_size,
        }
    }

    /// Return the smaller of `self` or `other`.
    ///
    /// # Panics
    ///
    /// Panics if the comparison is not meaningful. This happens if either `AssetCapacity` contains
    /// a NaN value, one is discrete and the other continuous or if both are discrete and the unit
    /// size differs.
    pub fn min(self, other: AssetCapacity) -> AssetCapacity {
        match self.partial_cmp(&other) {
            None => panic!("Comparing invalid AssetCapacity values ({self:?} and {other:?})"),
            Some(Ordering::Greater) => other,
            _ => self,
        }
    }

    /// Returns the number of units in this `AssetCapacity`.
    pub fn num_units(&self) -> u32 {
        self.num_units
    }

    /// Returns the unit size of this `AssetCapacity`.
    pub fn unit_size(&self) -> Capacity {
        self.unit_size
    }
}

impl Add for AssetCapacity {
    type Output = Self;

    // Add two AssetCapacity values together
    fn add(self, rhs: AssetCapacity) -> Self {
        let size1 = self.unit_size;
        let size2 = rhs.unit_size;
        Self::check_same_unit_size(size1, size2);
        AssetCapacity {
            num_units: self.num_units + rhs.num_units,
            unit_size: size1,
        }
    }
}

impl Sub for AssetCapacity {
    type Output = Self;

    // Subtract rhs from self, ensuring that the result is non-negative
    fn sub(self, rhs: AssetCapacity) -> Self {
        let size1 = self.unit_size;
        let size2 = rhs.unit_size;
        Self::check_same_unit_size(size1, size2);
        assert!(
            self.num_units >= rhs.num_units,
            "Cannot subtract a larger AssetCapacity ({rhs:?}) from a smaller one ({self:?})"
        );
        AssetCapacity {
            num_units: self.num_units - rhs.num_units,
            unit_size: size1,
        }
    }
}

impl PartialOrd for AssetCapacity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let size1 = self.unit_size;
        let size2 = other.unit_size;
        Self::check_same_unit_size(size1, size2);
        Some(self.num_units.cmp(&other.num_units))
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

    /// Returns the total capacity represented by this `AssetCapacity`.
    pub fn total_capacity(&self) -> Capacity {
        self.unit_size * Dimensionless(self.num_units as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::units::Capacity;
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
        assert_eq!(got.num_units(), expected_n);
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
        assert_eq!(got.num_units(), expected_n);
        assert_eq!(got.total_capacity(), expected_total);
    }

    #[rstest]
    #[case::less(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Continuous(Capacity(6.0)),
        Some(Ordering::Less)
    )]
    #[case::equal(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Continuous(Capacity(4.0)),
        Some(Ordering::Equal)
    )]
    #[case::greater(
        AssetCapacity::Continuous(Capacity(6.0)),
        AssetCapacity::Continuous(Capacity(4.0)),
        Some(Ordering::Greater)
    )]
    fn partial_cmp_continuous(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
        #[case] expected: Option<Ordering>,
    ) {
        assert_eq!(left.partial_cmp(&right), expected);
        assert_eq!(left == right, expected == Some(Ordering::Equal));
    }

    #[rstest]
    #[case::less(
        AssetCapacity::Discrete(2, Capacity(3.0)),
        AssetCapacity::Discrete(4, Capacity(3.0)),
        Some(Ordering::Less)
    )]
    #[case::equal(
        AssetCapacity::Discrete(4, Capacity(3.0)),
        AssetCapacity::Discrete(4, Capacity(3.0)),
        Some(Ordering::Equal)
    )]
    #[case::greater(
        AssetCapacity::Discrete(5, Capacity(3.0)),
        AssetCapacity::Discrete(4, Capacity(3.0)),
        Some(Ordering::Greater)
    )]
    fn partial_cmp_discrete_with_matching_unit_size(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
        #[case] expected: Option<Ordering>,
    ) {
        assert_eq!(left.partial_cmp(&right), expected);
        assert_eq!(left == right, expected == Some(Ordering::Equal));
    }

    #[rstest]
    #[case::mixed_types(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Discrete(4, Capacity(1.0))
    )]
    #[case::different_unit_sizes(
        AssetCapacity::Discrete(4, Capacity(1.0)),
        AssetCapacity::Discrete(4, Capacity(2.0))
    )]
    #[case::nan_continuous(
        AssetCapacity::Continuous(Capacity(f64::NAN)),
        AssetCapacity::Continuous(Capacity(4.0))
    )]
    fn partial_cmp_returns_none_for_invalid_comparisons(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
    ) {
        assert_eq!(left.partial_cmp(&right), None);
        assert_ne!(left, right);
    }

    #[rstest]
    #[case::continuous(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Continuous(Capacity(6.0)),
        AssetCapacity::Continuous(Capacity(4.0))
    )]
    #[case::discrete(
        AssetCapacity::Discrete(2, Capacity(3.0)),
        AssetCapacity::Discrete(4, Capacity(3.0)),
        AssetCapacity::Discrete(2, Capacity(3.0))
    )]
    fn min_returns_smaller_capacity(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
        #[case] expected: AssetCapacity,
    ) {
        assert_eq!(left.min(right), expected);
    }

    #[rstest]
    #[case::mixed_types(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Discrete(4, Capacity(1.0))
    )]
    #[case::different_unit_sizes(
        AssetCapacity::Discrete(4, Capacity(1.0)),
        AssetCapacity::Discrete(4, Capacity(2.0))
    )]
    #[should_panic(expected = "Comparing invalid AssetCapacity values")]
    fn min_panics_for_invalid_comparisons(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
    ) {
        let _ = left.min(right);
    }
}
