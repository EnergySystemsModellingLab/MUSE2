//! A regression test for the `circularity` example
mod regression;
use regression::run_regression_test;

#[test]
fn regression_circularity() {
    run_regression_test("circularity");
}
