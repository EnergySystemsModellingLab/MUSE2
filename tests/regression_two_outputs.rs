//! A regression test for the `two_outputs` example
mod regression;
use regression::run_regression_test;

#[test]
fn regression_two_outputs() {
    run_regression_test("two_outputs");
}
