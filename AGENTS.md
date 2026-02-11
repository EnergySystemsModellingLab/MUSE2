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
