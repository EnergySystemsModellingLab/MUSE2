//! Defines the `ModelParameters` struct, which represents the contents of `model.toml`.
use crate::asset::check_capacity_valid_for_asset;
use crate::input::{
    deserialise_proportion_nonzero, input_err_msg, is_sorted_and_unique, read_toml,
};
use crate::units::{Capacity, Dimensionless, MoneyPerFlow};
use anyhow::{Context, Result, ensure};
use log::warn;
use serde::Deserialize;
use serde_string_enum::DeserializeLabeledStringEnum;
use std::path::Path;

const MODEL_PARAMETERS_FILE_NAME: &str = "model.toml";

macro_rules! define_unit_param_default {
    ($name:ident, $type: ty, $value: expr) => {
        fn $name() -> $type {
            <$type>::new($value)
        }
    };
}

macro_rules! define_param_default {
    ($name:ident, $type: ty, $value: expr) => {
        fn $name() -> $type {
            $value
        }
    };
}

define_unit_param_default!(default_candidate_asset_capacity, Capacity, 0.0001);
define_unit_param_default!(default_capacity_limit_factor, Dimensionless, 0.1);
define_unit_param_default!(default_value_of_lost_load, MoneyPerFlow, 1e9);
define_unit_param_default!(default_price_tolerance, Dimensionless, 1e-6);
define_param_default!(default_max_ironing_out_iterations, u32, 10);

/// Represents the contents of the entire model file.
#[derive(Debug, Deserialize, PartialEq)]
pub struct ModelParameters {
    /// Milestone years
    pub milestone_years: Vec<u32>,
    /// The (small) value of capacity given to candidate assets.
    ///
    /// Don't change unless you know what you're doing.
    #[serde(default = "default_candidate_asset_capacity")]
    pub candidate_asset_capacity: Capacity,
    /// Defines the strategy used for calculating commodity prices
    #[serde(default)]
    pub pricing_strategy: PricingStrategy,
    /// Affects the maximum capacity that can be given to a newly created asset.
    ///
    /// It is the proportion of maximum capacity that could be required across time slices.
    #[serde(default = "default_capacity_limit_factor")]
    #[serde(deserialize_with = "deserialise_proportion_nonzero")]
    pub capacity_limit_factor: Dimensionless,
    /// The cost applied to unmet demand.
    ///
    /// Currently this only applies to the LCOX appraisal.
    #[serde(default = "default_value_of_lost_load")]
    pub value_of_lost_load: MoneyPerFlow,
    /// The maximum number of iterations to run the "ironing out" step of agent investment for
    #[serde(default = "default_max_ironing_out_iterations")]
    pub max_ironing_out_iterations: u32,
    /// The relative tolerance for price convergence in the ironing out loop
    #[serde(default = "default_price_tolerance")]
    pub price_tolerance: Dimensionless,
}

/// The strategy used for calculating commodity prices
#[derive(DeserializeLabeledStringEnum, Debug, PartialEq, Default)]
pub enum PricingStrategy {
    /// Take commodity prices directly from the shadow prices
    #[default]
    #[string = "shadow_prices"]
    ShadowPrices,
    /// Adjust shadow prices for scarcity
    #[string = "scarcity_adjusted"]
    ScarcityAdjusted,
}

/// Check that the `milestone_years` parameter is valid
fn check_milestone_years(years: &[u32]) -> Result<()> {
    ensure!(!years.is_empty(), "`milestone_years` is empty");

    ensure!(
        is_sorted_and_unique(years),
        "`milestone_years` must be composed of unique values in order"
    );

    Ok(())
}

/// Check that the `value_of_lost_load` parameter is valid
fn check_value_of_lost_load(value: MoneyPerFlow) -> Result<()> {
    ensure!(
        value.is_finite() && value > MoneyPerFlow(0.0),
        "value_of_lost_load must be a finite number greater than zero"
    );

    Ok(())
}

/// Check that the `max_ironing_out_iterations` parameter is valid
fn check_max_ironing_out_iterations(value: u32) -> Result<()> {
    ensure!(value > 0, "max_ironing_out_iterations cannot be zero");

    Ok(())
}

/// Check the `price_tolerance` parameter is valid
fn check_price_tolerance(value: Dimensionless) -> Result<()> {
    ensure!(
        value.is_finite() && value >= Dimensionless(0.0),
        "price_tolerance must be a finite number greater than zero"
    );

    Ok(())
}

impl ModelParameters {
    /// Read a model file from the specified directory.
    ///
    /// # Arguments
    ///
    /// * `model_dir` - Folder containing model configuration files
    ///
    /// # Returns
    ///
    /// The model file contents as a [`ModelParameters`] struct or an error if the file is invalid
    pub fn from_path<P: AsRef<Path>>(model_dir: P) -> Result<ModelParameters> {
        let file_path = model_dir.as_ref().join(MODEL_PARAMETERS_FILE_NAME);
        let model_params: ModelParameters = read_toml(&file_path)?;

        model_params
            .validate()
            .with_context(|| input_err_msg(file_path))?;

        Ok(model_params)
    }

    /// Validate parameters after reading in file
    fn validate(&self) -> Result<()> {
        // milestone_years
        check_milestone_years(&self.milestone_years)?;

        // pricing_strategy
        if self.pricing_strategy == PricingStrategy::ScarcityAdjusted {
            warn!(
                "The pricing strategy is set to 'scarcity_adjusted'. Commodity prices may be \
                incorrect if assets have more than one output commodity. See: {}/issues/677",
                env!("CARGO_PKG_REPOSITORY")
            );
        }

        // capacity_limit_factor already validated with deserialise_proportion_nonzero

        // candidate_asset_capacity
        check_capacity_valid_for_asset(self.candidate_asset_capacity)
            .context("Invalid value for candidate_asset_capacity")?;

        // value_of_lost_load
        check_value_of_lost_load(self.value_of_lost_load)?;

        // max_ironing_out_iterations
        check_max_ironing_out_iterations(self.max_ironing_out_iterations)?;

        // price_tolerance
        check_price_tolerance(self.price_tolerance)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::fmt::Display;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    /// Helper function to assert validation result based on expected validity
    fn assert_validation_result<T, U: Display>(
        result: Result<T>,
        expected_valid: bool,
        value: U,
        expected_error_fragment: &str,
    ) {
        if expected_valid {
            assert!(
                result.is_ok(),
                "Expected value {} to be valid, but got error: {:?}",
                value,
                result.err()
            );
        } else {
            assert!(
                result.is_err(),
                "Expected value {value} to be invalid, but it was accepted",
            );
            let error_message = result.err().unwrap().to_string();
            assert!(
                error_message.contains(expected_error_fragment),
                "Error message should mention the validation constraint, got: {error_message}",
            );
        }
    }

    #[test]
    fn test_check_milestone_years() {
        // Valid
        assert!(check_milestone_years(&[1]).is_ok());
        assert!(check_milestone_years(&[1, 2]).is_ok());

        // Invalid
        assert!(check_milestone_years(&[]).is_err());
        assert!(check_milestone_years(&[1, 1]).is_err());
        assert!(check_milestone_years(&[2, 1]).is_err());
    }

    #[test]
    fn test_model_params_from_path() {
        let dir = tempdir().unwrap();
        {
            let mut file = File::create(dir.path().join(MODEL_PARAMETERS_FILE_NAME)).unwrap();
            writeln!(file, "milestone_years = [2020, 2100]").unwrap();
        }

        let model_params = ModelParameters::from_path(dir.path()).unwrap();
        assert_eq!(model_params.milestone_years, [2020, 2100]);
    }

    #[rstest]
    #[case(1.0, true)] // Valid positive value
    #[case(1e-10, true)] // Valid very small positive value
    #[case(1e9, true)] // Valid large value (default)
    #[case(f64::MAX, true)] // Valid maximum finite value
    #[case(0.0, false)] // Invalid: exactly zero
    #[case(-1.0, false)] // Invalid: negative value
    #[case(-1e-10, false)] // Invalid: very small negative value
    #[case(f64::INFINITY, false)] // Invalid: infinite value
    #[case(f64::NEG_INFINITY, false)] // Invalid: negative infinite value
    #[case(f64::NAN, false)] // Invalid: NaN value
    fn test_check_value_of_lost_load(#[case] value: f64, #[case] expected_valid: bool) {
        let money_per_flow = MoneyPerFlow::new(value);
        let result = check_value_of_lost_load(money_per_flow);

        assert_validation_result(
            result,
            expected_valid,
            value,
            "value_of_lost_load must be a finite number greater than zero",
        );
    }

    #[rstest]
    #[case(1, true)] // Valid minimum value
    #[case(10, true)] // Valid default value
    #[case(100, true)] // Valid large value
    #[case(u32::MAX, true)] // Valid maximum value
    #[case(0, false)] // Invalid: zero
    fn test_check_max_ironing_out_iterations(#[case] value: u32, #[case] expected_valid: bool) {
        let result = check_max_ironing_out_iterations(value);

        assert_validation_result(
            result,
            expected_valid,
            value,
            "max_ironing_out_iterations cannot be zero",
        );
    }

    #[rstest]
    #[case(0.0, true)] // Valid minimum value (exactly zero)
    #[case(1e-10, true)] // Valid very small positive value
    #[case(1e-6, true)] // Valid default value
    #[case(1.0, true)] // Valid larger value
    #[case(f64::MAX, true)] // Valid maximum finite value
    #[case(-1e-10, false)] // Invalid: negative value
    #[case(-1.0, false)] // Invalid: negative value
    #[case(f64::INFINITY, false)] // Invalid: infinite value
    #[case(f64::NEG_INFINITY, false)] // Invalid: negative infinite value
    #[case(f64::NAN, false)] // Invalid: NaN value
    fn test_check_price_tolerance(#[case] value: f64, #[case] expected_valid: bool) {
        let dimensionless = Dimensionless::new(value);
        let result = check_price_tolerance(dimensionless);

        assert_validation_result(
            result,
            expected_valid,
            value,
            "price_tolerance must be a finite number greater than zero",
        );
    }
}
