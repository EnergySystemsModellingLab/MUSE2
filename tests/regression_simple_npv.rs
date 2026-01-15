//! A regression test for a patched version of the "simple" example using NPV for one agent.
mod regression;
use regression::run_regression_test_with_patches;

#[test]
fn regression_simple_npv() {
    run_regression_test_with_patches("simple", "simple_npv");
}
