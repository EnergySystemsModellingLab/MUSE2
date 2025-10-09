use human_panic::{metadata, setup_panic};
use log::error;
use muse2::ISSUES_URL;
use muse2::cli::run_cli;
use muse2::log::is_logger_initialised;

fn main() {
    setup_panic!(metadata!().support(format!(
        "Open an issue on Github: {ISSUES_URL}/new?template=bug_report.md"
    )));

    if let Err(err) = run_cli() {
        if is_logger_initialised() {
            error!("{err:?}");
        } else {
            eprintln!("Error: {err:?}");
        }

        // Terminate program, signalling an error
        std::process::exit(1);
    }
}
