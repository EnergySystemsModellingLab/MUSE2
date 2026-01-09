# Architecture and coding style

This document describes the overall architecture of the MUSE2 project, as well as the coding style
used for Rust. This document is intended to help new contributors get started with the codebase more
quickly rather than to be a set of prescriptions.

TODO: WIP!!!!!!

## Deployment

- deployed as single executable automatically on release (CD), with pre-built binaries provided for
  Windows, Linux and macOS (ARM)
- _no_ other files are included, so template config files, example models (see below) etc. **MUST**
  be bundled into the executable (with the exception of documentation, which is available online,
  though we could provide offline documentation too)

## Extra dependencies

- built using `clap`, with subcommands
- `itertools`
- use `float-cmp` crate for approx comparisons

## Error handling

- we use `anyhow`
- _user-facing_ errors should be passed up the call stack and will be logged (see below)
- _coding_ errors should lead to panic (use `human-panic` crate for releases)
- docs: <https://doc.rust-lang.org/book/ch09-00-error-handling.html>

## Logging

- (see user guide)
- use macros from `log` crate; don't print to console directly
- we use `fern` to handle formatting and also to write to output folder

## Unit types

- custom unit types (wrapper for `f64`) to provide type safety
- defined in `units.rs`; may need to define additional operations there
- use in preference to plain `f64`s where possible

## ID types

- (see `id.rs`)

## Time

- (explain MUSE2 concept of time? see `time_slice.rs`)

## Input layer

- (link to file format docs)
- big and complex!
- use `serde` for deserialising CSV and TOML files, which provides a certain amount of validation
  upfront
- however, also need to do additional validation. we endeavour to do as much validation as poss
  before the model is actually running, because a) errors might go unnoticed and lead to broken
  results, b) often you can give better diagnostics this way and c) it means users can get errors
  quickly, rather than partway through a very long simulation run

## Testing

- test functions should not start with `test_`
- for tests with fixtures, use the `rstest` crate (see also `fixture.rs`)
- test using "patched" examples (see `patch.rs`)
- (describe regression tests and how to work with them)

## Example models

- (see user guide)
- partly there as documentation for users (hence, should be simple and tidy)
- also to be used as templates for users (bundled with executable)
- also used for regression tests
