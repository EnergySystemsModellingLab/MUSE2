use criterion::{Criterion, criterion_group, criterion_main};
use muse2::cli::RunOpts;
use muse2::cli::example::handle_example_run_command;
use muse2::example::get_example_names;
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    let example_names = get_example_names();
    let options = RunOpts {
        output_dir: None,
        overwrite: true,
        debug_model: false,
        no_copy_input_files: false,
    };
    for name in example_names {
        c.bench_function(&format!("{name} example"), |b| {
            b.iter(|| {
                handle_example_run_command(black_box(name), black_box(false), black_box(&options))
                    .unwrap_or(());
            });
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
