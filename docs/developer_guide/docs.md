# Developing the documentation

You can make changes to the documentation without installing any additional tools, however, you may
wish to do so in order to view your changes locally.

For developing the main documentation, you will need [mdBook] installed (see below) and for the file
format documentation, you will need [uv]. We also recommend that you install [Just] (the
instructions below assume it is present).

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
[uv]: https://docs.astral.sh/uv/
[Just]: https://github.com/casey/just

## The book

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

## Documenting file formats

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

## Generating the `command_line_help.md` file

This file is created automatically. In order to examine the output locally, run:

```sh
just build-docs cli-help
```

The file will be written to `docs/command_line_help.md`.
