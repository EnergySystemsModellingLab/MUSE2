//! Represents the capacity of an asset
use crate::units::{Capacity, Dimensionless};
use std::cmp::Ordering;
use std::ops::{Add, Sub};

/// Capacity of an asset, which may be continuous or a number of discrete tranches of a given size.
#[derive(Clone, PartialEq, Copy, Debug)]
pub enum AssetCapacity {
    /// Continuous capacity, which is treated as a single indivisible tranche
    Continuous(Capacity),
    /// Capacity expressed as a number of equally sized tranches
    Discrete {
        /// Number of tranches
        num_tranches: u32,
        /// Size of each tranche
        tranche_size: Capacity,
    },
}

impl AssetCapacity {
    /// Create a new continuous `AssetCapacity` with the given capacity
    pub fn continuous(capacity: Capacity) -> Self {
        assert!(
            capacity.is_finite() && capacity >= Capacity(0.0),
            "Capacity must be a finite non-negative number"
        );
        AssetCapacity::Continuous(capacity)
    }

    /// Create a new discrete `AssetCapacity` with the given number of tranches and tranche size
    pub fn new(num_tranches: u32, tranche_size: Capacity) -> Self {
        assert!(
            tranche_size.is_finite() && tranche_size >= Capacity(0.0),
            "Tranche size must be a finite non-negative number"
        );
        AssetCapacity::Discrete {
            num_tranches,
            tranche_size,
        }
    }

    /// Create a new discrete `AssetCapacity` with a single tranche of the given size
    pub fn single(tranche_size: Capacity) -> Self {
        Self::new(1, tranche_size)
    }

    /// Return the smaller of `self` or `other`.
    ///
    /// # Panics
    ///
    /// Panics if the comparison is not meaningful. This happens if either `AssetCapacity` contains
    /// a NaN value, one is discrete and the other continuous or if both are discrete and the
    /// tranche size differs.
    pub fn min(self, other: AssetCapacity) -> AssetCapacity {
        match self.partial_cmp(&other) {
            None => panic!("Comparing invalid AssetCapacity values ({self:?} and {other:?})"),
            Some(Ordering::Greater) => other,
            _ => self,
        }
    }

    /// Returns the number of tranches in this `AssetCapacity`.
    ///
    /// A continuous capacity is a single tranche.
    pub fn num_tranches(&self) -> u32 {
        match self {
            AssetCapacity::Continuous(_) => 1,
            AssetCapacity::Discrete { num_tranches, .. } => *num_tranches,
        }
    }

    /// Returns the tranche size of this `AssetCapacity`.
    ///
    /// For a continuous capacity, this is the full capacity.
    pub fn tranche_size(&self) -> Capacity {
        match self {
            AssetCapacity::Continuous(capacity) => *capacity,
            AssetCapacity::Discrete { tranche_size, .. } => *tranche_size,
        }
    }

    /// Returns a copy of this `AssetCapacity` with the given number of tranches.
    ///
    /// # Panics
    ///
    /// Panics if this capacity is continuous and `num_tranches` is not one.
    pub fn with_num_tranches(self, num_tranches: u32) -> Self {
        match self {
            AssetCapacity::Continuous(_) => {
                assert_eq!(
                    num_tranches, 1,
                    "Cannot change the number of tranches of a continuous capacity"
                );
                self
            }
            AssetCapacity::Discrete { tranche_size, .. } => Self::new(num_tranches, tranche_size),
        }
    }

    /// Validates that two discrete capacities have the same tranche size.
    fn check_same_tranche_size(size1: Capacity, size2: Capacity) {
        assert_eq!(
            size1, size2,
            "Can't perform operation on capacities with different tranche sizes ({size1} and {size2})",
        );
    }

    /// Returns the total capacity represented by this `AssetCapacity`.
    pub fn total_capacity(&self) -> Capacity {
        match self {
            AssetCapacity::Continuous(capacity) => *capacity,
            AssetCapacity::Discrete {
                num_tranches,
                tranche_size,
            } => *tranche_size * Dimensionless(*num_tranches as f64),
        }
    }
}

impl Add for AssetCapacity {
    type Output = Self;

    // Add two AssetCapacity values together
    fn add(self, rhs: AssetCapacity) -> Self {
        match (self, rhs) {
            (AssetCapacity::Continuous(cap1), AssetCapacity::Continuous(cap2)) => {
                AssetCapacity::Continuous(cap1 + cap2)
            }
            (
                AssetCapacity::Discrete {
                    num_tranches: n1,
                    tranche_size: size1,
                },
                AssetCapacity::Discrete {
                    num_tranches: n2,
                    tranche_size: size2,
                },
            ) => {
                Self::check_same_tranche_size(size1, size2);
                AssetCapacity::Discrete {
                    num_tranches: n1 + n2,
                    tranche_size: size1,
                }
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
            (
                AssetCapacity::Discrete {
                    num_tranches: n1,
                    tranche_size: size1,
                },
                AssetCapacity::Discrete {
                    num_tranches: n2,
                    tranche_size: size2,
                },
            ) => {
                Self::check_same_tranche_size(size1, size2);
                assert!(
                    n1 >= n2,
                    "Cannot subtract a larger AssetCapacity ({rhs:?}) from a smaller one ({self:?})"
                );
                AssetCapacity::Discrete {
                    num_tranches: n1 - n2,
                    tranche_size: size1,
                }
            }
            _ => panic!("Cannot subtract different types of AssetCapacity ({self:?} and {rhs:?})"),
        }
    }
}

impl PartialOrd for AssetCapacity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (AssetCapacity::Continuous(a), AssetCapacity::Continuous(b)) => a.partial_cmp(b),
            (
                AssetCapacity::Discrete {
                    num_tranches: n1,
                    tranche_size: size1,
                },
                AssetCapacity::Discrete {
                    num_tranches: n2,
                    tranche_size: size2,
                },
            ) => (size1 == size2).then(|| n1.cmp(n2)),
            _ => None,
        }
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
    #[case::continuous_less(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Continuous(Capacity(6.0)),
        Some(Ordering::Less)
    )]
    #[case::continuous_equal(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::Continuous(Capacity(4.0)),
        Some(Ordering::Equal)
    )]
    #[case::continuous_greater(
        AssetCapacity::Continuous(Capacity(6.0)),
        AssetCapacity::Continuous(Capacity(4.0)),
        Some(Ordering::Greater)
    )]
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
    fn partial_cmp_with_valid_comparisons(
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
        AssetCapacity::new(4, Capacity(1.0))
    )]
    #[case::different_tranche_sizes(
        AssetCapacity::new(4, Capacity(1.0)),
        AssetCapacity::new(4, Capacity(2.0))
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
    #[case::mixed_types(
        AssetCapacity::Continuous(Capacity(4.0)),
        AssetCapacity::new(4, Capacity(1.0))
    )]
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
            AssetCapacity::Discrete {
                num_tranches: 0,
                tranche_size: Capacity(3.0),
            }
        );
    }
}
