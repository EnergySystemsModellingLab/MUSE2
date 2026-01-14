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
[`clap`]: https://docs.rs/clap/latest/clap/

## Using external crates

With Rust it is easy to add external dependencies (called "crates") from [crates.io]. It is
preferable to make use of external crates for additional required functionality rather than
reimplementing this by hand.

Some crates which we make heavy use of are:

- [`anyhow`] - Ergonomic error handling in Rust (see below)
- [`float-cmp`] - For approximate comparison of floating-point types
- [`indexmap`] - Provides hash table and set types which preserve insertion order
- [`itertools`] - Provides extra features for iterator types (consider using to simplify code using
  iterators)

[crates.io]: https://crates.io/
[`anyhow`]: https://docs.rs/anyhow/
[`float-cmp`]: https://docs.rs/float-cmp/
[`indexmap`]: https://docs.rs/indexmap/
[`itertools`]: https://docs.rs/itertools/

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
pass these errors up the call stack via `anyhow` (see above).

Note that the log level is configurable at runtime; see [user guide][logging-docs] for details.

## Writing tests

This repository includes tests for many aspects of MUSE2's functionality (both unit tests and
integration tests). These can be run with `cargo test`. All tests must pass for submitted code; this
is enforced via a [GitHub Actions workflow][ci-workflow]. Newly added code should include tests,
wherever feasible. Code coverage is tracked with [Codecov]. There is good documentation on how to
write tests in Rust [in the Rust book][tests-docs].

You may wish to use [test fixtures] for your unit tests. While Rust's built-in testing framework
does not support test fixtures directly, the [`rstest`] crate, which is already included as a
dependency for MUSE2, provides this functionality. You should prefer adding test fixtures over
copy-pasting the same data structures between different tests. For common data structures (e.g.
commodities, assets etc.), there are fixtures for these already provided in [`fixture.rs`]. You
should use these where possible rather than creating new fixtures.

As the fixtures needed for many tests are potentially complicated, there are also helper macros for
testing that validation/running fails/succeeds for modified versions of example models. For more
information, see [`fixture.rs`]. As this method is likely to lead to terser code compared to using
fixtures, it should be preferred for new tests.

We check whether each of the bundled example models (see below) runs successfully to completion as
regression tests. We also check that the output has not substantially changed (i.e. that the numbers
in the outputs are within a tolerance), which helps catch accidental changes to the behaviour of
MUSE2. Of course, often we _do_ want to change the behaviour of MUSE2 as the model evolves. In this
case, you can regenerate test data by running:

```sh
just regenerate_test_data
```

If you do so, please verify that the changes to the output files are at least roughly what was
expected, before you commit these updated test files.

[`log`]: https://docs.rs/log
[`fern`]: https://docs.rs/fern
[logging-docs]: ../user_guide.md#setting-the-log-level
[ci-workflow]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/.github/workflows/cargo-test.yml
[Codecov]: https://about.codecov.io/
[tests-docs]: https://doc.rust-lang.org/book/ch11-01-writing-tests.html
[test fixtures]: https://en.wikipedia.org/wiki/Test_fixture
[`rstest`]: https://docs.rs/rstest
[`fixture.rs`]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/src/fixture.rs

## Example models

MUSE2 provides a number of example models, to showcase its functionality and help users get started
with creating their own. These models live in the [`examples`] folder of the repository and are also
bundled with the MUSE2 executable ([see user guide for more detail][user-guide-example-models]).

As these are intended as both a kind of documentation and templates, they should ideally be kept as
simple as possible.

If you add a new example model, please also add a regression test ([see here for an
example][regression-test-example]).

[`examples`]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/examples/
[user-guide-example-models]: https://energysystemsmodellinglab.github.io/MUSE2/user_guide.html#example-models
[regression-test-example]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/tests/regression_muse1_default.rs

## Unit types

We define a number of types for units used commonly in MUSE2, such as activity, capacity etc. These
are simple wrappers around `f64`s, but provide additional type safety, ensuring that the wrong types
are not passed to functions, for example. Certain arithmetic operations involving types are also
defined: for example, if you divide a variable of type [`Money`] by one of type [`Activity`], you
get a result of type [`MoneyPerActivity`].

These types should be used in preference to plain `f64`s, where possible. For variables which are
unitless, there is a [`Dimensionless`] type to make this explicit.

For more information, consult [the documentation for the `units` module][units-module-docs].

[`Money`]: https://energysystemsmodellinglab.github.io/MUSE2/api/muse2/units/struct.Money.html
[`Activity`]: https://energysystemsmodellinglab.github.io/MUSE2/api/muse2/units/struct.Activity.html
[`MoneyPerActivity`]: https://energysystemsmodellinglab.github.io/MUSE2/api/muse2/units/struct.MoneyPerActivity.html
[`Dimensionless`]: https://energysystemsmodellinglab.github.io/MUSE2/api/muse2/units/struct.Dimensionless.html
[units-module-docs]: https://energysystemsmodellinglab.github.io/MUSE2/api/muse2/units/index.html

## Input and output files

File formats for MUSE2 input and output files are described [in the
documentation][file-format-docs]. This documentation is generated from schema files ([JSON schemas]
for TOML files and [table schemas] for CSV files); these schemas **MUST** be updated when the file
format changes (i.e. when a field is added/removed/changed). (For details of how to generate this
documentation locally, see [Developing the documentation].)

When it comes to reading input files, we try to perform as much validation as possible within the
input layer, so that we can provide users with detailed error messages, rather than waiting until
errors in the input data become apparent in the simulation run (or, worse, are missed altogether!).
A certain amount of type safety is given by the [`serde`] crate (e.g. checking that fields which
should be integers are really integers), but we also carry out many other validation checks (e.g.
checking that there is a producer for every required commodity in the first year).

[file-format-docs]: https://energysystemsmodellinglab.github.io/MUSE2/file_formats/
[JSON schemas]: https://json-schema.org/
[table schemas]: https://specs.frictionlessdata.io/table-schema/
[`serde`]: https://serde.rs/
[Developing the documentation]: ./docs.md#documenting-file-formats
