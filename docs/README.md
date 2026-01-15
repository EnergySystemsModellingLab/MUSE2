# MUSE2

## Overview

> ⚠️ **Please note that MUSE2 currently only works with simple models and is not yet suitable
for use in research.** ⚠️

MUSE2 (**M**od**U**lar energy systems **S**imulation **E**nvironment) is a tool for running
simulations of energy systems, written in Rust. Its purpose is to provide users with a framework to
simulate pathways of energy system transition, usually in the context of climate change mitigation.

It is the successor to [MUSE], which is written in Python. It was developed following re-design of
MUSE to address a range of legacy issues that are challenging to address via upgrades to the
existing MUSE framework, and to implement the framework in the high-performance Rust language.

[MUSE]: https://github.com/EnergySystemsModellingLab/MUSE_OS

## Getting started

You will first need to install MUSE2. We recommend downloading the latest version for your platform
on [the releases page]. The archive file you download should contain a readme file containing
instructions on how to run MUSE2. Alternatively, you can install the [`muse2` crate from
crates.io][muse2-crate] using [`cargo`] (see [Setting up your development environment][dev-setup]).

Once you have installed MUSE2, you can look at the [user guide] and the [command line help] for
details on how to get started with creating and running your own models.

Detailed information about the model used by MUSE2 is provided in [Model Description]. For a list of
relevant terms, see the [glossary].

If you want to work with the MUSE2 source code, for instance to submit a change to this repository,
please see the [developer guide].

[the releases page]: https://github.com/EnergySystemsModellingLab/MUSE2/releases
[`cargo`]: https://doc.rust-lang.org/cargo/
[dev-setup]: ./developer_guide/setup.md
[muse2-crate]: https://crates.io/crates/muse2
[user guide]: ./user_guide.md
[command line help]: ./command_line_help.md
[Model Description]: ./model/
[glossary]: ./glossary.md
[developer guide]: ./developer_guide/
