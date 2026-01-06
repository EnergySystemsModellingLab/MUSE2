//! A regression test for the "simple" example
mod regression;
use regression::run_regression_test_with_debug_files;

#[test]
fn regression_simple() {
    run_regression_test_with_debug_files("simple");
}
