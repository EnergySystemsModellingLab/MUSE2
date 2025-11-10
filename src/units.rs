//! This module defines various unit types and their conversions.

use float_cmp::{ApproxEq, F64Margin};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};

/// A trait encompassing most of the functionality of unit types
pub trait UnitType:
    fmt::Debug
    + Copy
    + PartialEq
    + PartialOrd
    + Serialize
    + Add
    + Sub
    + Div
    + Mul<Dimensionless, Output = Self>
    + AddAssign
    + SubAssign
    + Sum
    + ApproxEq<Margin = F64Margin>
    + fmt::Display
    + Default
{
    /// Create from an f64 value
    fn new(value: f64) -> Self;
    /// Returns the underlying f64 value.
    fn value(&self) -> f64;
    /// Returns true if the value is a normal number.
    fn is_normal(&self) -> bool;
    /// Returns true if the value is finite.
    fn is_finite(&self) -> bool;
    /// Returns the absolute value of this unit.
    fn abs(&self) -> Self;
    /// Returns the max of two values
    fn max(&self, other: Self) -> Self;
    /// Returns the min of two values
    fn min(&self, other: Self) -> Self;
    /// Returns ordering between self and other
    fn total_cmp(&self, other: &Self) -> std::cmp::Ordering;
}

macro_rules! base_unit_struct {
    ($name:ident) => {
        /// A unit type representing a dimensionless value.
        #[derive(
            Debug,
            Clone,
            Copy,
            PartialEq,
            PartialOrd,
            Default,
            Serialize,
            derive_more::Add,
            derive_more::Sub,
        )]
        pub struct $name(pub f64);

        impl std::ops::Div<$name> for $name {
            type Output = Dimensionless;
            fn div(self, rhs: $name) -> Dimensionless {
                Dimensionless(self.0 / rhs.0)
            }
        }
        impl std::iter::Sum for $name {
            fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
                iter.fold($name(0.0), |a, b| $name(a.0 + b.0))
            }
        }
        impl AddAssign for $name {
            fn add_assign(&mut self, other: Self) {
                self.0 += other.0;
            }
        }
        impl SubAssign for $name {
            fn sub_assign(&mut self, other: Self) {
                self.0 -= other.0;
            }
        }
        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
        impl float_cmp::ApproxEq for $name {
            type Margin = float_cmp::F64Margin;
            fn approx_eq<T: Into<Self::Margin>>(self, other: Self, margin: T) -> bool {
                self.0.approx_eq(other.0, margin)
            }
        }
        impl std::ops::Neg for $name {
            type Output = $name;
            fn neg(self) -> $name {
                $name(-self.0)
            }
        }
        impl $name {
            /// Create from an f64 value
            pub fn new(value: f64) -> Self {
                $name(value)
            }
            /// Returns the underlying f64 value.
            pub fn value(&self) -> f64 {
                self.0
            }
            /// Returns true if the value is a normal number.
            pub fn is_normal(&self) -> bool {
                self.0.is_normal()
            }
            /// Returns true if the value is finite.
            pub fn is_finite(&self) -> bool {
                self.0.is_finite()
            }
            /// Returns the absolute value of this unit.
            pub fn abs(&self) -> Self {
                $name(self.0.abs())
            }
            /// Returns the max of two values
            pub fn max(&self, other: Self) -> Self {
                Self(self.0.max(other.0))
            }
            /// Returns the min of two values
            pub fn min(&self, other: Self) -> Self {
                Self(self.0.min(other.0))
            }
            /// Returns ordering between self and other
            pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.0.total_cmp(&other.0)
            }
        }
        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let value = f64::deserialize(deserializer)?;
                if !value.is_finite() {
                    Err(serde::de::Error::custom("Value cannot be NaN or infinite"))?;
                }

                Ok($name(value))
            }
        }
        impl UnitType for $name {
            /// Create from an f64 value
            fn new(value: f64) -> Self {
                Self::new(value)
            }
            /// Returns the underlying f64 value.
            fn value(&self) -> f64 {
                Self::value(&self)
            }
            /// Returns true if the value is a normal number.
            fn is_normal(&self) -> bool {
                Self::is_normal(&self)
            }
            /// Returns true if the value is finite.
            fn is_finite(&self) -> bool {
                Self::is_finite(&self)
            }
            /// Returns the absolute value of this unit.
            fn abs(&self) -> Self {
                Self::abs(&self)
            }
            /// Returns the max of two values
            fn max(&self, other: Self) -> Self {
                Self::max(&self, other)
            }
            /// Returns the min of two values
            fn min(&self, other: Self) -> Self {
                Self::min(&self, other)
            }
            /// Returns ordering between self and other
            fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
                Self::total_cmp(&self, other)
            }
        }
    };
}

// Define Dimensionless first
base_unit_struct!(Dimensionless);

// Add extra methods for Dimensionless
impl From<f64> for Dimensionless {
    fn from(val: f64) -> Self {
        Self(val)
    }
}
impl From<Dimensionless> for f64 {
    fn from(val: Dimensionless) -> Self {
        val.0
    }
}

impl Mul for Dimensionless {
    type Output = Dimensionless;

    fn mul(self, rhs: Self) -> Self::Output {
        Dimensionless(self.0 * rhs.0)
    }
}

impl Dimensionless {
    /// Raises this dimensionless number to the power of `rhs`.
    pub fn powi(self, rhs: i32) -> Self {
        Dimensionless(self.0.powi(rhs))
    }
}

// Define all other units with Dimensionless interactions
macro_rules! unit_struct {
    ($name:ident) => {
        base_unit_struct!($name);

        impl std::ops::Mul<Dimensionless> for $name {
            type Output = $name;
            fn mul(self, rhs: Dimensionless) -> $name {
                $name(self.0 * rhs.0)
            }
        }
        impl std::ops::Mul<$name> for Dimensionless {
            type Output = $name;
            fn mul(self, rhs: $name) -> $name {
                $name(self.0 * rhs.0)
            }
        }
        impl std::ops::Div<Dimensionless> for $name {
            type Output = $name;
            fn div(self, rhs: Dimensionless) -> $name {
                $name(self.0 / rhs.0)
            }
        }
    };
}

// Base quantities
unit_struct!(Money);
unit_struct!(Flow);
unit_struct!(Activity);
unit_struct!(Capacity);
unit_struct!(Year);

// Derived quantities
unit_struct!(PerYear);
unit_struct!(MoneyPerYear);
unit_struct!(MoneyPerFlow);
unit_struct!(MoneyPerCapacity);
unit_struct!(MoneyPerCapacityPerYear);
unit_struct!(MoneyPerActivity);
unit_struct!(ActivityPerYear);
unit_struct!(ActivityPerCapacity);
unit_struct!(ActivityPerCapacityPerYear);
unit_struct!(FlowPerActivity);
unit_struct!(FlowPerCapacity);

macro_rules! impl_div {
    ($Lhs:ident, $Rhs:ident, $Out:ident) => {
        impl std::ops::Div<$Rhs> for $Lhs {
            type Output = $Out;
            fn div(self, rhs: $Rhs) -> $Out {
                $Out(self.0 / rhs.0)
            }
        }
        impl std::ops::Div<$Out> for $Lhs {
            type Output = $Rhs;
            fn div(self, rhs: $Out) -> $Rhs {
                $Rhs(self.0 / rhs.0)
            }
        }
        impl std::ops::Mul<$Rhs> for $Out {
            type Output = $Lhs;
            fn mul(self, by: $Rhs) -> $Lhs {
                $Lhs(self.0 * by.0)
            }
        }
        impl std::ops::Mul<$Lhs> for $Out {
            type Output = $Rhs;
            fn mul(self, by: $Lhs) -> $Rhs {
                $Rhs(self.0 * by.0)
            }
        }
        impl std::ops::Mul<$Out> for $Rhs {
            type Output = $Lhs;
            fn mul(self, by: $Out) -> $Lhs {
                $Lhs(self.0 * by.0)
            }
        }
        impl std::ops::Mul<$Out> for $Lhs {
            type Output = $Rhs;
            fn mul(self, by: $Out) -> $Rhs {
                $Rhs(self.0 * by.0)
            }
        }
    };
}

// Division rules for derived quantities
impl_div!(Flow, Activity, FlowPerActivity);
impl_div!(Flow, Capacity, FlowPerCapacity);
impl_div!(Money, Year, MoneyPerYear);
impl_div!(Money, Flow, MoneyPerFlow);
impl_div!(Money, Capacity, MoneyPerCapacity);
impl_div!(Money, Activity, MoneyPerActivity);
impl_div!(Activity, Capacity, ActivityPerCapacity);
impl_div!(Activity, Year, ActivityPerYear);
impl_div!(ActivityPerCapacity, Year, ActivityPerCapacityPerYear);
impl_div!(MoneyPerYear, Capacity, MoneyPerCapacityPerYear);
impl_div!(MoneyPerActivity, FlowPerActivity, MoneyPerFlow);
impl_div!(MoneyPerCapacity, Year, MoneyPerCapacityPerYear);
impl_div!(FlowPerCapacity, ActivityPerCapacity, FlowPerActivity);

impl std::ops::Mul<PerYear> for Year {
    type Output = Dimensionless;

    fn mul(self, by: PerYear) -> Self::Output {
        Dimensionless(self.0 * by.0)
    }
}

impl std::ops::Mul<PerYear> for Activity {
    type Output = ActivityPerYear;

    fn mul(self, by: PerYear) -> Self::Output {
        ActivityPerYear(self.0 * by.0)
    }
}

impl std::ops::Mul<PerYear> for ActivityPerCapacity {
    type Output = ActivityPerCapacityPerYear;

    fn mul(self, by: PerYear) -> Self::Output {
        ActivityPerCapacityPerYear(self.0 * by.0)
    }
}

impl std::ops::Div<Year> for Dimensionless {
    type Output = PerYear;

    fn div(self, rhs: Year) -> Self::Output {
        PerYear(self.0 / rhs.0)
    }
}
