//! Represents the capacity of an asset
use crate::units::{Capacity, Dimensionless};
use std::cmp::Ordering;
use std::ops::{Add, Sub};

/// Capacity of an asset, expressed in terms of a number of discrete tranches of a given size.
#[derive(Clone, PartialEq, Copy, Debug)]
pub struct AssetCapacity {
    /// Number of tranches
    num_tranches: u32,
    /// Size of each tranche
    tranche_size: Capacity,
}

impl AssetCapacity {
    /// Create a new `AssetCapacity` with the given number of tranches and tranche size
    pub fn new(num_tranches: u32, tranche_size: Capacity) -> Self {
        assert!(
            tranche_size.is_finite() && tranche_size >= Capacity(0.0),
            "Tranche size must be a finite non-negative number"
        );
        AssetCapacity {
            num_tranches,
            tranche_size,
        }
    }

    /// Create a new `AssetCapacity` with a single tranche of the given size
    pub fn single(tranche_size: Capacity) -> Self {
        Self::new(1, tranche_size)
    }

    /// Return the smaller of `self` or `other`.
    ///
    /// # Panics
    ///
    /// Panics if the tranche size differs.
    pub fn min(self, other: AssetCapacity) -> AssetCapacity {
        match self.partial_cmp(&other) {
            None => panic!("Comparing invalid AssetCapacity values ({self:?} and {other:?})"),
            Some(Ordering::Greater) => other,
            _ => self,
        }
    }

    /// Returns the number of tranches in this `AssetCapacity`.
    pub fn num_tranches(&self) -> u32 {
        self.num_tranches
    }

    /// Returns the tranche size of this `AssetCapacity`.
    pub fn tranche_size(&self) -> Capacity {
        self.tranche_size
    }

    /// Validates that two capacities have the same tranche size.
    fn check_same_tranche_size(&self, other: AssetCapacity) {
        assert_eq!(
            self.tranche_size, other.tranche_size,
            "Can't perform operation on capacities with different tranche sizes ({} and {})",
            self.tranche_size, other.tranche_size,
        );
    }

    /// Returns the total capacity represented by this `AssetCapacity`.
    pub fn total_capacity(&self) -> Capacity {
        self.tranche_size * Dimensionless(self.num_tranches as f64)
    }
}

impl Add for AssetCapacity {
    type Output = Self;

    // Add two AssetCapacity values together
    fn add(self, rhs: AssetCapacity) -> Self {
        self.check_same_tranche_size(rhs);
        AssetCapacity {
            num_tranches: self.num_tranches + rhs.num_tranches,
            tranche_size: self.tranche_size,
        }
    }
}

impl Sub for AssetCapacity {
    type Output = Self;

    // Subtract rhs from self, ensuring that the result is non-negative
    fn sub(self, rhs: AssetCapacity) -> Self {
        self.check_same_tranche_size(rhs);
        assert!(
            self.num_tranches >= rhs.num_tranches,
            "Cannot subtract a larger AssetCapacity ({rhs:?}) from a smaller one ({self:?})"
        );
        AssetCapacity {
            num_tranches: self.num_tranches - rhs.num_tranches,
            tranche_size: self.tranche_size,
        }
    }
}

impl PartialOrd for AssetCapacity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let size1 = self.tranche_size;
        let size2 = other.tranche_size;
        (size1 == size2).then(|| self.num_tranches.cmp(&other.num_tranches))
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
        #[case] num_tranches: u32,
        #[case] tranche_size: Capacity,
        #[case] expected_total_capacity: Capacity,
    ) {
        let capacity = AssetCapacity::new(num_tranches, tranche_size);

        assert_eq!(capacity.num_tranches(), num_tranches);
        assert_eq!(capacity.tranche_size(), tranche_size);
        assert_eq!(capacity.total_capacity(), expected_total_capacity);
    }

    #[rstest]
    #[case(Capacity(-1.0))]
    #[case(Capacity(f64::INFINITY))]
    #[case(Capacity(f64::NEG_INFINITY))]
    #[case(Capacity(f64::NAN))]
    #[should_panic(expected = "Tranche size must be a finite non-negative number")]
    fn new_rejects_non_finite_tranche_size(#[case] tranche_size: Capacity) {
        let num_tranches = 1;
        let _ = AssetCapacity::new(num_tranches, tranche_size);
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
    fn partial_cmp_with_matching_tranche_size(
        #[case] left: AssetCapacity,
        #[case] right: AssetCapacity,
        #[case] expected: Option<Ordering>,
    ) {
        assert_eq!(left.partial_cmp(&right), expected);
        assert_eq!(left == right, expected == Some(Ordering::Equal));
    }

    #[rstest]
    #[case::different_tranche_sizes(
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
    #[case::different_tranche_sizes(
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
    fn subtracting_equal_capacities_returns_zero_tranches() {
        let capacity = AssetCapacity::new(2, Capacity(3.0));

        assert_eq!(
            capacity - capacity,
            AssetCapacity {
                num_tranches: 0,
                tranche_size: Capacity(3.0),
            }
        );
    }
}
