//! Code for working with the dangerous model options flag.
use log::warn;
use std::sync::OnceLock;

/// Global flag indicating whether potentially dangerous model options have been enabled.
///
/// This is stored in a `OnceLock` and must be set exactly once during startup (see
/// [`set_dangerous_model_options_flag`]).
static DANGEROUS_OPTIONS_ENABLED: OnceLock<bool> = OnceLock::new();

/// Whether potentially dangerous model options were enabled by the loaded config.
///
/// # Panics
///
/// Panics if the global flag has not yet been set.
pub fn dangerous_model_options_enabled() -> bool {
    dangerous_model_options_enabled_production()
}

/// Set the global flag indicating whether potentially dangerous model options are enabled.
///
/// This function can be called more than once, provided the value of `enabled` is the same in both
/// cases; it cannot be changed during runtime. (This should not happen during normal program
/// execution, but may happen during tests etc.)
pub fn set_dangerous_model_options_flag(enabled: bool) {
    set_dangerous_model_options_flag_production(enabled);
}

/// Check whether dangerous model options are enabled (outside of tests)
fn dangerous_model_options_enabled_production() -> bool {
    *DANGEROUS_OPTIONS_ENABLED
        .get()
        .expect("Dangerous options flag not set")
}

/// Set the dangerous model options flag (outside of tests)
fn set_dangerous_model_options_flag_production(enabled: bool) {
    if DANGEROUS_OPTIONS_ENABLED.set(enabled).is_err() {
        assert_eq!(
            enabled,
            dangerous_model_options_enabled_production(),
            "Attempted to change DANGEROUS_OPTIONS_ENABLED during runtime"
        );

        warn!("set_dangerous_model_options_flag called more than once");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "Attempted to change DANGEROUS_OPTIONS_ENABLED during runtime")]
    fn check_dangerous_model_options_production() {
        // Other tests may already have set this flag, but they **must** set it to false. Ensure
        // it's set.
        assert!(!DANGEROUS_OPTIONS_ENABLED.get_or_init(|| false));

        // Setting to false again should be fine
        std::panic::catch_unwind(|| set_dangerous_model_options_flag_production(false))
            .expect("Unexpected panic");

        // Double check the value is set
        assert!(!dangerous_model_options_enabled_production());

        // Should panic when trying to **change** value
        set_dangerous_model_options_flag_production(true);
    }
}
