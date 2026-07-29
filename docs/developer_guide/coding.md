# Building and developing MUSE2

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

## Building MUSE2

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

## Working with the benchmarking framework

MUSE2 includes a benchmarking framework to measure the performance of various components using the
[Criterion.rs] library. To run all the available benchmarks, use the following command:

```sh
cargo bench --features bench
```

The `--features bench` flag disables some aspects of the code that are incompatible with the
benchmarking, like singleton objects that can be only instantiated once and throw an error otherwise.
See `rust` documentation for more information about [features].

You can also run specific benchmarks by specifying their names:

```sh
cargo bench --features bench --bench <benchmark_name>
```

For example, to run the `examples` benchmark, you would use:

```sh
cargo bench --features bench --bench examples
```

If run more than once, the benchmarking framework will compare the results of the current run with the
previous run and report the differences. This is useful for checking whether a change has
introduced a performance regression.

[Criterion.rs] automatically generates an HTML report that can be found in the
`target/criterion/report` directory. You can open it with your web browser to see the results and
navigate through the different benchmarks.

### Adding new benchmarks

To add a new benchmark, i.e. a collection of related performance tests, creat a new file in the
`benches` directory with the name of your benchmark. For example, to add a benchmark for the `foo`
functionality, you would create a file called `benches/foo.rs`. Then `foo` will need to be added to
`cargo.toml` under the `[bench]` section, like so:

```toml
[[bench]]
name = "foo"
harness = false
```

The `harness = false` line is necessary because Criterion.rs provides its own test harness, which
is incompatible with the default Rust test harness. Within the new benchmark file, you can then add
your performance tests using the Criterion.rs API. Check `benches/examples.rs` for an example of how
to do this. More information about writing benchmarks can be found in the [Criterion.rs] documentation.

[Criterion.rs]: https://bheisler.github.io/criterion.rs/book/index.html
[features]: https://doc.rust-lang.org/cargo/reference/features.html
