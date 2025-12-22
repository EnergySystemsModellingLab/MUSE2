//! A regression test for the "simple" example
mod regression;
use regression::run_regression_test_with_debug_files;

#[test]
fn test_regression_simple() {
    run_regression_test_with_debug_files("simple");
}
