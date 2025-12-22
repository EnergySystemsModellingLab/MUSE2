//! Integration tests for the `validate` command.
use muse2::cli::handle_validate_command;
use muse2::log::is_logger_initialised;
use muse2::settings::Settings;
use std::path::PathBuf;

/// Get the path to the example model.
fn get_model_dir() -> PathBuf {
    PathBuf::from("examples/simple")
}

/// An integration test for the `validate` command.
///
/// We also check that the logger is initialised after it is run.
#[test]
fn handle_validate_command_works() {
    unsafe { std::env::set_var("MUSE2_LOG_LEVEL", "off") };

    assert!(!is_logger_initialised());

    handle_validate_command(&get_model_dir(), Some(Settings::default())).unwrap();

    assert!(is_logger_initialised());
}
