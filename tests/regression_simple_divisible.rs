//! A regression test for a patched version of the "simple" example with divisible gas boilers.
mod regression;
use muse2::patch::FilePatch;
use regression::run_regression_test_with_patches;

#[test]
fn regression_simple() {
    let patch = FilePatch::new("processes.csv")
        .with_deletion("RGASBR,Gas boiler,all,RSHEAT,2020,2040,1.0,")
        .with_addition("RGASBR,Gas boiler,all,RSHEAT,2020,2040,1.0,1000");

    run_regression_test_with_patches("simple", vec![patch], "simple_divisible");
}
