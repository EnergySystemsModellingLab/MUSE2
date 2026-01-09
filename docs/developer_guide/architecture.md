# Architecture and coding style

This document describes the overall architecture of the MUSE2 project, as well as the coding style
used for Rust. This document is intended to help new contributors get started with the codebase more
quickly rather than to be a set of prescriptions.

## User interface and deployment

MUSE2 is a command-line utility, designed to be built and run as a single, standalone executable
file (called `muse2` on Unix platforms and `muse2.exe` on Windows). The normal way in which users
will obtain MUSE2 is by downloading a pre-built binary for their platform (currently, Linux, Windows
and macOS ARM binaries are provided). The user should not need to install any additional
dependencies in order to use MUSE2. All assets (e.g. example models; see below) should be bundled
into this executable file.

For information on the command-line interface, see [the documentation][cli]. The [`clap`] crate is
used to provide this interface.

[cli]: ../command_line_help.md
[`clap`]: <https://docs.rs/clap/latest/clap/>

## Error handling

One of the distinctive features of the Rust programming language is its approach to error handling.
There are two usual ways to signal that an error has occurred: either by returning it from a
function like any other value (usually via the [`Result`] enum) or by "panicking", which usually
results in termination of the program[^1]. See [the official docs] for more information.

In the case of MUSE2, we use the [`anyhow`] crate for error handling, which provides some useful
helpers for passing error messages up the call stack and attaching additional context. For any
user-facing error (e.g. caused by a malformed input file), you should return an error wrapped in a
`Result`, so that it can be logged. For purely developer-facing errors, such as functions called
with bad arguments, you should instead [`panic!`]. Note that users should **NEVER** be able to
trigger a panic in MUSE2 and any case where this happens should be treated as a bug. We use the
[`human-panic`] crate to direct users to report the bug if a panic occurs in a release build.

[^1]: Technically, panics [can be caught](https://doc.rust-lang.org/std/panic/fn.catch_unwind.html),
      but this is unusual and we don't do it in MUSE2

[`Result`]: https://doc.rust-lang.org/std/result/
[the official docs]: https://doc.rust-lang.org/book/ch09-00-error-handling.html
[`anyhow`]: https://docs.rs/anyhow
[`panic!`]: https://doc.rust-lang.org/std/macro.panic.html
[`human-panic`]: https://docs.rs/human-panic/

## Logging

MUSE2 makes use of the [`log`] crate, which provides a number of macros for logging at different
levels (e.g. `info!`, `warn!` etc.). This crate just provides the helper macros and does not provide
a logging backend. For the backend, we use [`fern`], which deals with formatting and (in the case of
simulation runs) writing to log files. A few simple commands print to the console directly without
using the logging framework (e.g. `muse2 example list`), but for code run as part of model
validation and simulation runs, you should use the `log` macros rather than printing to the console
directly. Note that you should generally not use the `error!` macro directly, but should instead
pass these errors up the callstack via `anyhow` (see above).

Note that the log level is configurable at runtime; see [user guide][logging-docs] for details.

[`log`]: https://docs.rs/log
[`fern`]: https://docs.rs/fern
[logging-docs]: ../user_guide.md#setting-the-log-level
