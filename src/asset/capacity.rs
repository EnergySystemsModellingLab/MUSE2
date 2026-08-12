//! Represents the capacity of an asset
use crate::units::{Capacity, Dimensionless};
use std::cmp::Ordering;
use std::ops::{Add, Sub};

/// Capacity of an asset, expressed in terms of a number of discrete units of a given size.
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
        assert!(
            unit_size.is_finite() && unit_size.0 >= 0.0,
            "Unit size must be a finite non-negative number"
        );
        assert!(num_units > 0, "Number of units must be a positive integer");
        AssetCapacity {
            num_units,
            unit_size,
        }
    }

    /// Create a new `AssetCapacity` with a single unit of the given size
    pub fn single(unit_size: Capacity) -> Self {
        Self::new(1, unit_size)
    }

    /// Return the smaller of `self` or `other`.
    ///
    /// # Panics
    ///
    /// Panics if the unit size differs.
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

    /// Validates that two capacities have the same unit size.
    fn check_same_unit_size(&self, other: AssetCapacity) {
        assert_eq!(
            self.unit_size, other.unit_size,
            "Can't perform operation on capacities with different unit sizes ({} and {})",
            self.unit_size, other.unit_size,
        );
    }

    /// Returns the total capacity represented by this `AssetCapacity`.
    pub fn total_capacity(&self) -> Capacity {
        self.unit_size * Dimensionless(self.num_units as f64)
    }
}

impl Add for AssetCapacity {
    type Output = Self;

    // Add two AssetCapacity values together
    fn add(self, rhs: AssetCapacity) -> Self {
        self.check_same_unit_size(rhs);
        AssetCapacity {
            num_units: self.num_units + rhs.num_units,
            unit_size: self.unit_size,
        }
    }
}

impl Sub for AssetCapacity {
    type Output = Self;

    // Subtract rhs from self, ensuring that the result is non-negative
    fn sub(self, rhs: AssetCapacity) -> Self {
        self.check_same_unit_size(rhs);
        assert!(
            self.num_units >= rhs.num_units,
            "Cannot subtract a larger AssetCapacity ({rhs:?}) from a smaller one ({self:?})"
        );
        AssetCapacity {
            num_units: self.num_units - rhs.num_units,
            unit_size: self.unit_size,
        }
    }
}

impl PartialOrd for AssetCapacity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let size1 = self.unit_size;
        let size2 = other.unit_size;
        (size1 == size2).then(|| self.num_units.cmp(&other.num_units))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::units::Capacity;
    use rstest::rstest;

    #[rstest]
    #[case(1, Capacity(1.0), Capacity(1.0))]
    #[case(2, Capacity(3.5), Capacity(7.0))]
    #[case(u32::MAX, Capacity(0.0), Capacity(0.0))]
    fn new_works(
        #[case] num_units: u32,
        #[case] unit_size: Capacity,
        #[case] expected_total_capacity: Capacity,
    ) {
        let capacity = AssetCapacity::new(num_units, unit_size);

        assert_eq!(capacity.num_units(), num_units);
        assert_eq!(capacity.unit_size(), unit_size);
        assert_eq!(capacity.total_capacity(), expected_total_capacity);
    }

    #[rstest]
    #[case(0, Capacity(1.0))]
    #[should_panic(expected = "Number of units must be a positive integer")]
    fn new_rejects_zero_units(#[case] num_units: u32, #[case] unit_size: Capacity) {
        let _ = AssetCapacity::new(num_units, unit_size);
    }

    #[rstest]
    #[case(Capacity(-1.0))]
    #[case(Capacity(f64::INFINITY))]
    #[case(Capacity(f64::NEG_INFINITY))]
    #[case(Capacity(f64::NAN))]
    #[should_panic(expected = "Unit size must be a finite non-negative number")]
    fn new_rejects_non_finite_unit_size(#[case] unit_size: Capacity) {
        let num_units = 1;
        let _ = AssetCapacity::new(num_units, unit_size);
    }

    #[rstest]
    #[case::less(
        AssetCapacity::new(2, Capacity(3.0)),
        AssetCapacity::new(4, Capacity(3.0)),
        Some(Ordering::Less)
    )]
    #[case::equal(
        AssetCapacity::new(4, Capacity(3.0)),
        AssetCapacity::new(4, Capacity(3.0)),
        Some(Ordering::Equal)
    )]
    #[case::greater(
        AssetCapacity::new(5, Capacity(3.0)),
        AssetCapacity::new(4, Capacity(3.0)),
        Some(Ordering::Greater)
    )]
    fn partial_cmp_with_matching_unit_size(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
        #[case] expected: Option<Ordering>,
    ) {
        assert_eq!(left.partial_cmp(&right), expected);
        assert_eq!(left == right, expected == Some(Ordering::Equal));
    }

    #[rstest]
    #[case::different_unit_sizes(
        AssetCapacity::new(4, Capacity(1.0)),
        AssetCapacity::new(4, Capacity(2.0))
    )]
    fn partial_cmp_returns_none_for_invalid_comparisons(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
    ) {
        assert_eq!(left.partial_cmp(&right), None);
        assert_ne!(left, right);
    }

    #[rstest]
    #[case::discrete(
        AssetCapacity::new(2, Capacity(3.0)),
        AssetCapacity::new(4, Capacity(3.0)),
        AssetCapacity::new(2, Capacity(3.0))
    )]
    fn min_returns_smaller_capacity(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
        #[case] expected: AssetCapacity,
    ) {
        assert_eq!(left.min(right), expected);
    }

    #[rstest]
    #[case::different_unit_sizes(
        AssetCapacity::new(4, Capacity(1.0)),
        AssetCapacity::new(4, Capacity(2.0))
    )]
    #[should_panic(expected = "Comparing invalid AssetCapacity values")]
    fn min_panics_for_invalid_comparisons(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
    ) {
        let _ = left.min(right);
    }

    #[test]
    fn subtracting_equal_capacities_returns_zero_units() {
        let capacity = AssetCapacity::new(2, Capacity(3.0));

        assert_eq!(
            capacity - capacity,
            AssetCapacity {
                num_units: 0,
                unit_size: Capacity(3.0),
            }
        );
    }
}
