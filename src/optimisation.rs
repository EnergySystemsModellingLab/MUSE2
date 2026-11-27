//! Shared helpers for building and solving HiGHS optimisation models.
use anyhow::{Context, Result, anyhow};
use highs::HighsModelStatus;
use std::error::Error;

/// Defines the possible errors that can occur when running the solver
#[derive(Debug, derive_more::Display, derive_more::From)]
pub enum ModelError {
    /// An optimal solution could not be found
    #[display("Could not find optimal result: {_0:?}")]
    NonOptimal(HighsModelStatus),
    /// Another error occurred
    #[display("{_0}")]
    Other(anyhow::Error),
}

impl ModelError {
    /// Convert this error into an [`anyhow::Error`]
    pub fn into_anyhow(self) -> anyhow::Error {
        match self {
            ModelError::NonOptimal(status) => anyhow!("Could not find optimal result: {status:?}"),
            ModelError::Other(error) => error,
        }
    }
}

impl Error for ModelError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ModelError::NonOptimal(_) => None,
            ModelError::Other(error) => Some(error.as_ref()),
        }
    }
}

/// Apply the specified HiGHS options from a [`toml::Table`]
pub fn apply_highs_options_from_toml(
    model: &mut highs::Model,
    options: &toml::Table,
) -> Result<()> {
    // Attempt to set an option, returning an error if it fails
    macro_rules! try_set_opt {
        ($option:expr, $value:expr) => {{
            model
                .try_set_option($option.as_str(), $value)
                .map_err(|_| anyhow!("Invalid option name or value"))?;

            Ok(())
        }};
    }

    // Iterate through options, applying each in turn to the HiGHS model
    for (option, value) in options {
        match value {
            toml::Value::String(value) => try_set_opt!(option, value.as_str()),
            toml::Value::Integer(value) => match i32::try_from(*value) {
                Ok(value) => try_set_opt!(option, value),
                Err(_) => Err(anyhow!("Value out of range")),
            },
            toml::Value::Float(value) => try_set_opt!(option, *value),
            toml::Value::Boolean(value) => try_set_opt!(option, *value),
            _ => Err(anyhow!("HiGHS options cannot have this type")),
        }
        .with_context(|| format!("Failed to set option \"{option}\" to value \"{value}\""))?;
    }

    Ok(())
}

/// Try to solve the model, returning an error if the model is incoherent or result is non-optimal
pub fn solve_optimal(model: highs::Model) -> Result<highs::SolvedModel, ModelError> {
    let solved = model
        .try_solve()
        .map_err(|status| anyhow!("Incoherent model: {status:?}"))?;

    match solved.status() {
        HighsModelStatus::Optimal => Ok(solved),
        status => Err(status.into()),
    }
}
