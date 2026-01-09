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
