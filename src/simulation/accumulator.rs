//! Generic accumulator primitives for aggregation.
use crate::units::{Dimensionless, UnitType};

/// Generic accumulator contract.
pub trait Accumulator<Input> {
    /// The type yielded when the accumulator is solved.
    type Output;

    /// Add a single input sample.
    fn add(&mut self, input: Input);

    /// Finalise the accumulator and return a result if enough information was provided.
    fn finalise(&self) -> Option<Self::Output>;
}

/// Maximum value accumulator.
#[derive(Clone, Copy, Debug, Default)]
pub struct MaxAccumulator<T>
where
    T: UnitType,
{
    value: Option<T>,
}

impl<T> Accumulator<T> for MaxAccumulator<T>
where
    T: UnitType,
{
    type Output = T;

    fn add(&mut self, input: T) {
        self.value = Some(match self.value {
            Some(current) => current.max(input),
            None => input,
        });
    }

    fn finalise(&self) -> Option<Self::Output> {
        self.value
    }
}

/// Minimum value accumulator.
#[derive(Clone, Copy, Debug, Default)]
pub struct MinAccumulator<T>
where
    T: UnitType,
{
    value: Option<T>,
}

impl<T> Accumulator<T> for MinAccumulator<T>
where
    T: UnitType,
{
    type Output = T;

    fn add(&mut self, input: T) {
        self.value = Some(match self.value {
            Some(current) => current.min(input),
            None => input,
        });
    }

    fn finalise(&self) -> Option<Self::Output> {
        self.value
    }
}

/// Weighted average accumulator.
#[derive(Clone, Copy, Debug)]
pub struct WeightedAverageAccumulator<T>
where
    T: UnitType,
{
    numerator: T,
    denominator: Dimensionless,
}

impl<T> Default for WeightedAverageAccumulator<T>
where
    T: UnitType + std::ops::Div<Dimensionless, Output = T>,
{
    fn default() -> Self {
        Self {
            numerator: T::new(0.0),
            denominator: Dimensionless(0.0),
        }
    }
}

impl<T> WeightedAverageAccumulator<T>
where
    T: UnitType + std::ops::Div<Dimensionless, Output = T>,
{
    /// Add a weighted value.
    pub fn add(&mut self, value: T, weight: Dimensionless) {
        self.numerator += value * weight;
        self.denominator += weight;
    }

    /// Solve the weighted average.
    pub fn finalise(&self) -> Option<T> {
        (self.denominator > Dimensionless::EPSILON).then(|| self.numerator / self.denominator)
    }
}

impl<T> Accumulator<(T, Dimensionless)> for WeightedAverageAccumulator<T>
where
    T: UnitType + std::ops::Div<Dimensionless, Output = T>,
{
    type Output = T;

    fn add(&mut self, input: (T, Dimensionless)) {
        self.add(input.0, input.1);
    }

    fn finalise(&self) -> Option<Self::Output> {
        Self::finalise(self)
    }
}

/// Weighted average accumulator with a backup weighting path.
#[derive(Clone, Copy, Debug)]
pub struct WeightedAverageBackupAccumulator<T>
where
    T: UnitType,
{
    primary_numerator: T,
    primary_denominator: Dimensionless,
    backup_numerator: T,
    backup_denominator: Dimensionless,
}

impl<T> Default for WeightedAverageBackupAccumulator<T>
where
    T: UnitType + std::ops::Div<Dimensionless, Output = T>,
{
    fn default() -> Self {
        Self {
            primary_numerator: T::new(0.0),
            primary_denominator: Dimensionless(0.0),
            backup_numerator: T::new(0.0),
            backup_denominator: Dimensionless(0.0),
        }
    }
}

impl<T> WeightedAverageBackupAccumulator<T>
where
    T: UnitType + std::ops::Div<Dimensionless, Output = T>,
{
    /// Add a weighted value with a backup weight.
    pub fn add(&mut self, value: T, weight: Dimensionless, backup_weight: Dimensionless) {
        self.primary_numerator += value * weight;
        self.primary_denominator += weight;
        self.backup_numerator += value * backup_weight;
        self.backup_denominator += backup_weight;
    }

    /// Solve the weighted average, falling back to backup weights if needed.
    pub fn finalise(&self) -> Option<T> {
        if self.primary_denominator > Dimensionless::EPSILON {
            Some(self.primary_numerator / self.primary_denominator)
        } else if self.backup_denominator > Dimensionless::EPSILON {
            Some(self.backup_numerator / self.backup_denominator)
        } else {
            None
        }
    }
}

impl<T> Accumulator<(T, Dimensionless, Dimensionless)> for WeightedAverageBackupAccumulator<T>
where
    T: UnitType + std::ops::Div<Dimensionless, Output = T>,
{
    type Output = T;

    fn add(&mut self, input: (T, Dimensionless, Dimensionless)) {
        self.add(input.0, input.1, input.2);
    }

    fn finalise(&self) -> Option<Self::Output> {
        Self::finalise(self)
    }
}
