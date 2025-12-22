//! A regression test for the `missing_commodity` example
mod regression;
use regression::run_regression_test;

#[test]
fn test_regression_missing_commodity() {
    run_regression_test("missing_commodity");
}
