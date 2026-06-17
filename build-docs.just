# Make rustdoc warnings fatal
export RUSTDOCFLAGS := "-D warnings"

# Build all documentation, except old docs
all: cli-help file-format examples header book api

# Build all documentation, including old docs
all_with_old: all old

# Build book
book:
    @echo Building book
    @mdbook build

# API documentation for Rust code
api:
    @echo Generating API documentation
    @cargo doc --no-deps --document-private-items

    @# Put API documentation in with book
    @rm -rf book/api
    @mkdir -p book/api
    @cp -r target/doc/* book/api

# Help for muse2 program
cli-help:
    @echo Generating command-line help
    @cargo run -- --markdown-help > docs/command_line_help.md

# Build documentation for file formats
file-format *ARGS:
    @echo Building docs for file formats
    @uv run docs/file_formats/generate_docs.py {{ARGS}}

# Build documentation for examples
examples:
    @echo Building docs for examples
    @uv run docs/generate_example_docs.py

# Generate theme file for version banner and widget for docs
header:
    @echo Generating theme/header.hbs
    @uv run docs/generate_header.py

# Build documentation for previous releases
old:
    @# Clean output dir
    @rm -rf book/release

    @uv run docs/build_old_docs.py
