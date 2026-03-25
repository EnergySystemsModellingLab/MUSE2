# AGENTS.md

## Code style and architecture

- When generating or reviewing code, please consult the guidelines in
  `docs/developer_guide/architecture_quickstart.md`
- If adding a new feature or fixing a bug that was present in the last release of MUSE2, add a note
  to `docs/release_notes/upcoming.md`
- Prefer UK spelling in code and documentation

For Rust code:

- Prefer `use` imports to fully qualified paths
- Prefer named format arguments (e.g. `"{x}"`) over positional formatting (e.g. `"{}", x`)
- Test function names should not be prefixed with `test_`
- Prefer using parameterised tests (using `rstest`) over separate ones where testing similar
  functionality

## Building and testing

```sh
cargo build          # build the project
cargo test           # run all tests
cargo test --quiet   # run all tests with minimal output
```

To regenerate data for regression tests after intentionally changing MUSE2's behaviour:

```sh
just regenerate_test_data
```

## Error handling

- Use the [`anyhow`] crate for error handling
- Return a `Result` (via `?`) for any user-facing error (e.g. malformed input files), so errors
  can be logged with context
- Use `panic!` only for developer-facing errors (e.g. functions called with invalid arguments)
- Users should **never** be able to trigger a panic — any such case is a bug
- Do not use the `error!` log macro directly; propagate errors via `anyhow` instead

[`anyhow`]: https://docs.rs/anyhow/

## Unit types

Prefer the unit wrapper types from the `units` module (e.g. `Activity`, `Capacity`, `Money`,
`MoneyPerActivity`, `Dimensionless`) over plain `f64`s. This provides additional type safety and
prevents accidentally mixing incompatible quantities.

## Writing tests

- Use fixtures from [`src/fixture.rs`] for common data structures (commodities, assets, etc.)
- For tests that check validation or simulation of modified example models, prefer the helper
  macros in `src/fixture.rs` over manual fixture construction:
  - `assert_validate_ok_simple!(patches)` — asserts that patched model passes validation
  - `assert_validate_fails_with_simple!(patches, msg)` — asserts validation fails with a message
  - `assert_patched_runs_ok_simple!(patches)` — asserts that patched model runs successfully
  - `assert_error!(result, msg)` — asserts that a `Result` is an error with the given message
- When adding a new example model, also add a regression test to [`tests/regression.rs`]

[`src/fixture.rs`]: src/fixture.rs
[`tests/regression.rs`]: tests/regression.rs

## Input/output file formats

When changing an input or output file format (adding, removing, or renaming fields), you **must**
also update the corresponding schema file in `schemas/`. The schema files are used to generate the
file format documentation.
