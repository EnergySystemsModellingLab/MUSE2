//! A regression test for the "simple" example
mod regression;
use regression::run_regression_test;

#[test]
fn test_regression_simple() {
    run_regression_test("simple")
}
