//! A regression test for the `muse1_default` example
mod regression;
use regression::run_regression_test;

#[test]
fn regression_muse1_default() {
    run_regression_test("muse1_default");
}
