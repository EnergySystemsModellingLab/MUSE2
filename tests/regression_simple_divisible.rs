//! A regression test for a patched version of the "simple" example with divisible gas boilers.
mod regression;
use regression::run_regression_test_with_patches;

#[test]
fn regression_simple_divisible() {
    run_regression_test_with_patches("simple", "simple_divisible");
}
