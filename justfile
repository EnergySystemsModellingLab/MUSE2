# This is a justfile containing a number of useful recipes for developers. To use it,
# you need to install the `just` program, which you can do with cargo:
#
#     cargo install just
#
# To see the list of available recipes, just run:
#
#     just

# Build documentation
mod build-docs

# Display list of just commands
help:
    @just --list

# Generate test coverage in HTML format
coverage *ARGS:
    @cargo llvm-cov --html {{ARGS}}

# Regenerate data for regression tests
regenerate_test_data:
    @tests/regenerate_test_data.sh

# Run the pre-commit tool
pre-commit *ARGS:
    @uv tool run pre-commit {{ARGS}}
