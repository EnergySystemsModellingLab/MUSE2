# Setting up your development environment

To develop MUSE2 locally you will need the following components:

- [Git]
- Rust development tools
- C++ development tools (needed for bindings to the [HiGHS] solver)

Optional requirements:

- [Just] (recommended)
- [uv] (required for [pre-commit] and file format documentation)

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
[pre-commit]: https://pre-commit.com/
[development container]: https://devcontainers.github.io/
[Docker]: https://www.docker.com/
[Visual Studio Code]: https://code.visualstudio.com/
[GitHub Codespaces]: https://github.com/features/codespaces
[justfile]: ../../justfile

## Installing tools

### Option 1: Bare metal installation

#### Installing the Rust toolchain

We recommend that developers use `rustup` to install the Rust toolchain. Follow the instructions on
[the `rustup` website](https://rustup.rs/).

This project defines the required version of the Rust toolchain in [`rust-toolchain.toml`]. The
first time you attempt to run `cargo` (see [Building and developing MUSE2]), this toolchain should
be downloaded and installed automatically by `rustup`.

[`rust-toolchain.toml`]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/rust-toolchain.toml
[Building and developing MUSE2]: ./coding.md

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

## Pre-Commit hooks

We use [pre-commit] to automatically run a number of hooks for this repository when a new Git
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
