use criterion::{Criterion, criterion_group, criterion_main};
use muse2::cli::RunOpts;
use muse2::cli::example::handle_example_run_command;
use muse2::example::get_example_names;
use std::hint::black_box;

/// Benchmark the example run command for all examples
fn criterion_benchmark(c: &mut Criterion) {
    let example_names = get_example_names();
    let output_root = tempfile::TempDir::new()
        .expect("Failed to create temporary output directory for benchmarks");
    let output_dir = output_root.path().join("muse2-example-bench-output");
    let options = RunOpts {
        output_dir: Some(output_dir),
        overwrite: true,
        debug_model: false,
        no_copy_input_files: false,
    };
    let mut group = c.benchmark_group("example_run");
    group
        .noise_threshold(0.05) // Set a noise threshold of 5%
        .sample_size(20); // Set the sample size to 20 iterations

    for name in example_names {
        group.bench_function(format!("{name} example"), |b| {
            b.iter(|| {
                handle_example_run_command(black_box(name), black_box(false), black_box(&options))
                    .expect("Example run benchmark failed");
            });
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
