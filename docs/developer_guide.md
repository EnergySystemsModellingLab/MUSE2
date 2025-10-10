# Developer Guide

This is a guide for those who wish to contribute to the MUSE2 project or make local changes to
the code.

[The API documentation is available here.](./api/muse2)

## Background: The Rust programming language

MUSE2 is written using [Rust], which is a high-performance, compiled language. If you have used
other compiled languages, such as C++, many of the concepts will be familiar. One feature which
distinguishes it from other languages like C and C++, however, is that [undefined behaviour], such
as [memory safety] bugs, are not possible, provided you keep to the [safe subset] of the language.
This means you can have the performance benefits of using a low-level language like C, with the
safety guarantees and much of the convenience of a higher-level language like Python.

There is much high quality documentation available for learning Rust, but it is probably best to
start with [The Rust Programming Language book], which is freely available online.

[Rust]: https://www.rust-lang.org/
[undefined behaviour]: https://en.wikipedia.org/wiki/Undefined_behavior
[memory safety]: https://www.memorysafety.org/docs/memory-safety/
[safe subset]: https://doc.rust-lang.org/nomicon/meet-safe-and-unsafe.html
[The Rust Programming Language book]: https://doc.rust-lang.org/book/

## Installing developer tools

To develop MUSE2 locally you will need the following components:

- [Git]
- Rust development tools
- C++ development tools (needed for bindings to the [HiGHS] solver)

Optional requirements:

- [Just] (recommended)
- [uv] (required for `pre-commit` and file format documentation)

You can either install the necessary developer tools locally on your machine manually (a bare metal
installation) or use the provided [development container]. Bare metal installation should generally
be preferred if you plan on doing substantial development, as you should get better performance
locally (and therefore shorter compile times), as well as making it easier to interact with your
local filesystem. Development containers provide a mechanism for installing all the tools you will
need (into a [Docker] container) and are generally used either via [Visual Studio Code] running
locally or [GitHub Codespaces], which run on GitHub-provided virtual machines running remotely.

We provide a [justfile] for some common developer tasks.

[Git]: https://git-scm.com/
[HiGHS]: https://highs.dev/
[Just]: https://github.com/casey/just
[uv]: https://docs.astral.sh/uv/
[development container]: https://devcontainers.github.io/
[Docker]: https://www.docker.com/
[Visual Studio Code]: https://code.visualstudio.com/
[GitHub Codespaces]: https://github.com/features/codespaces
[justfile]: ../justfile

### Option 1: Bare metal installation

#### Installing the Rust toolchain

We recommend that developers use `rustup` to install the Rust toolchain. Follow the instructions on
[the `rustup` website](https://rustup.rs/).

Once you have done so, select the `stable` toolchain (used by this project) as your default with:

```sh
rustup default stable
```

As the project uses the latest stable toolchain, you may see build errors if your toolchain is out
of date. You can update to the latest version with:

```sh
rustup update stable
```

#### Installing C++ tools for HiGHS

The [`highs-sys`] crate requires a C++ compiler and CMake to be installed on your system.
These may be installed already, but if you encounter errors during the build process for `highs-sys`
(e.g. "Unable to find libclang"), you should follow [the instructions in the `highs-sys`
repository][highs-sys-repo].

[`highs-sys`]: https://crates.io/crates/highs-sys
[highs-sys-repo]: https://github.com/rust-or/highs-sys#building-highs

### Option 2: Developing inside a container

If you wish to use the development container locally with Visual Studio Code, you should first
install the [Dev Containers extension]. Note you will also need Docker to be installed locally. For
more information, see [the documentation].

You can use GitHub Codespaces to develop directly from your web browser. For more information,
please see [GitHub's guide]. When you first create your Codespace, you will be asked whether you
wish to install the recommended extensions and you should choose "yes".

[Dev Containers extension]: https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers
[the documentation]: https://code.visualstudio.com/docs/devcontainers/containers
[GitHub's guide]: https://docs.github.com/en/codespaces/developing-in-a-codespace/developing-in-a-codespace

## Downloading the MUSE2 source code

Unless you are developing in a GitHub Codespace (see above), you will need to download the MUSE2
source code to your local machine before you can develop it. Like many projects, MUSE2 is stored
in a Git repository [hosted on GitHub]. Many IDEs, such as Visual Studio Code, provide an interface
to clone Git repositories, but you can also use the Git command-line tool ([see installation
instructions]), like so:

```sh
git clone https://github.com/EnergySystemsModellingLab/MUSE2
```

The source code will now be available in a folder named `MUSE2`.

[hosted on GitHub]: https://github.com/EnergySystemsModellingLab/MUSE2
[see installation instructions]: https://git-scm.com/downloads

## Working with the project

To build the project, run:

```sh
cargo build
```

Note that if you just want to build-test the project (i.e. check for errors and warnings) without
building an executable, you can use the `cargo check` command, which is much faster.

To run MUSE2 with the "simple" example, you can run:

```sh
cargo run run examples/simple
```

(Note the two `run`s. The first is for `cargo` and the second is passed as an argument to the built
`muse2` program.)

Tests can be run with:

```sh
cargo test
```

More information is available in [the official `cargo` book](https://doc.rust-lang.org/cargo/).

## Checking test coverage

We use [Codecov](https://about.codecov.io/) to check whether pull requests introduce code without
tests.

To check coverage locally (i.e. to make sure newly written code has tests), we recommend using
[cargo-llvm-cov](https://github.com/taiki-e/cargo-llvm-cov).

It can be installed with:

```sh
cargo install cargo-llvm-cov
```

Once installed, you can use it like so:

```sh
cargo llvm-cov --open
```

Alternatively, you can use Just:

```sh
just coverage --open
```

This will generate a report in HTML format showing which lines are not currently covered by tests
and open it in your default browser.

## Developing the documentation

You can make changes to the documentation without having any additional tools installed, however,
you will if you wish to view your changes locally.

For developing the main documentation, you will need [mdBook] installed (see below) and for the file
format documentation, you will need [uv].

If you have all the necessary tools installed and wish to build all of the documentation, you can
run:

```sh
just build-docs
```

However, you will likely want to build only part of the documentation, which you can do by passing
another argument to this command, e.g.:

```sh
just build-docs file-format
```

To see the possible recipes, run:

```sh
just --list build-docs
```

[mdBook]: https://rust-lang.github.io/mdBook/

### The book

We use [mdBook](https://rust-lang.github.io/mdBook/) for generating technical documentation.

If you are developing the documentation locally, you may want to check that your changes render
correctly (especially if you are working with equations).

To do this, you first need to install mdBook:

```sh
cargo install mdbook
```

You can then view the documentation in your browser like so:

```sh
mdbook serve -o
```

### Documenting file formats

Documentation for file formats of different input and output files used by MUSE2 is automatically
generated from schemas, stored in the
[`schemas/`](https://github.com/EnergySystemsModellingLab/MUSE2/tree/main/schemas) folder.

Files are either in [TOML](https://toml.io/en/) or
[CSV](https://en.wikipedia.org/wiki/Comma-separated_values) format. For TOML files, we use [JSON
schemas](https://json-schema.org/) and for CSV files we use [table
schemas](https://specs.frictionlessdata.io//table-schema/) (a similar format).

The documentation is generated with the
[`docs/file_formats/generate_docs.py`](https://github.com/EnergySystemsModellingLab/MUSE2/tree/main/docs/file_formats/generate_docs.py)
script. To generate all file format docs, run:

```sh
just build-docs file-format
```

To generate just one kind of docs (e.g. for input files only), run:

```sh
just build-docs file-format input
```

### Recreate the `command_line_help.md` file

This file is created automatically. In order to examine the output locally, run:

```sh
just build-docs cli-help
```

The file will be written to `docs/command_line_help.md`.

## Pre-Commit hooks

We use [`pre-commit`] to automatically run a number of hooks for this repository when a new Git
commit is made. You can run `pre-commit` via our `justfile`, provided you have `uv` installed.

You can enable `pre-commit` for this repository with:

```sh
just pre-commit install
```

Thereafter, a series of checks should be run every time you commit with Git. In addition, the
`pre-commit` hooks are also run as part of the CI pipeline.

Note: you may get errors due to the [`clippy`] hook failing. In this case, you may be able to
automatically fix them by running `cargo clipfix` (which we have defined as an alias in
[`.cargo/config.toml`]).

[`clippy`]: https://doc.rust-lang.org/clippy
[`.cargo/config.toml`]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/.cargo/config.toml
