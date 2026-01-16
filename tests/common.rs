use assert_cmd::cargo_bin_cmd;

macro_rules! muse2_cmd {
    ($args:expr) => {
        cargo_bin_cmd!("muse2")
            .env("MUSE2_USE_DEFAULT_SETTINGS", "1")
            .args($args)
    };
}

pub fn assert_muse2_runs(args: &[&str]) {
    muse2_cmd!(args).assert().success();
}

#[allow(dead_code)]
#[must_use]
pub fn get_muse2_stdout(args: &[&str]) -> String {
    let output = muse2_cmd!(args).output().expect("Failed to run muse2");
    assert!(output.status.success());

    str::from_utf8(&output.stdout)
        .expect("Non-unicode chars in stdout")
        .into()
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
