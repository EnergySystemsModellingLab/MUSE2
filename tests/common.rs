use assert_cmd::cargo_bin_cmd;

pub fn assert_muse2_runs(args: &[&str]) {
    cargo_bin_cmd!("muse2")
        .env("MUSE2_USE_DEFAULT_SETTINGS", "1")
        .args(args)
        .assert()
        .success();
}

/// Define a regression test with extra command-line arguments
#[allow(unused_macros)]
macro_rules! define_regression_test_with_extra_args {
    ($example:ident, $extra_args:expr) => {
        #[test]
        fn $example() {
            run_regression_test(stringify!($example), $extra_args);
        }
    };
}
#[allow(unused_imports)]
pub(crate) use define_regression_test_with_extra_args;

/// Define a regression test for an example model
#[allow(unused_macros)]
macro_rules! define_regression_test {
    ($example:ident) => {
        define_regression_test_with_extra_args!($example, &[]);
    };
}
#[allow(unused_imports)]
pub(crate) use define_regression_test;

/// Define a regression test for an example model, including debug output files
#[allow(unused_macros)]
macro_rules! define_regression_test_with_debug_files {
    ($example:ident) => {
        define_regression_test_with_extra_args!($example, &["--debug-model"]);
    };
}
#[allow(unused_imports)]
pub(crate) use define_regression_test_with_debug_files;

/// Define a regression test for a patched example
#[allow(unused_macros)]
macro_rules! define_regression_test_with_patches {
    ($example:ident) => {
        define_regression_test_with_extra_args!($example, &["--patch"]);
    };
}
#[allow(unused_imports)]
pub(crate) use define_regression_test_with_patches;
