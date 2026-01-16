use assert_cmd::cargo_bin_cmd;

pub fn assert_muse2_runs(args: &[&str]) {
    cargo_bin_cmd!("muse2")
        .env("MUSE2_USE_DEFAULT_SETTINGS", "1")
        .args(args)
        .assert()
        .success();
}
