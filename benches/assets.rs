//! Benchmark for [`select_best_assets`], which is responsible for iteratively choosing the best
//! assets (from among existing and candidate assets) to meet demand for a given commodity/region
//! market during agent investment.
use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group, criterion_main};
use muse2::asset::{AssetPool, AssetRef};
use muse2::example::Example;
use muse2::input::load_model;
use muse2::model::Model;
use muse2::output::DataWriter;
use muse2::process::{Process, ProcessID};
use muse2::simulation::candidate_assets_for_next_year;
use muse2::simulation::investment::{flatten_preset_demands_for_year, select_best_assets};
use muse2::simulation::market::{
    collect_investment_limits_for_candidates, get_asset_options, get_demand_portion_for_market,
    get_responsible_agents,
};
use muse2::simulation::optimisation::DispatchRun;
use muse2::simulation::prices::{Prices, calculate_prices};
use std::hint::black_box;
use std::rc::Rc;
use std::time::Duration;
use tempfile::TempDir;

/// The example model used as the basis for this benchmark
const EXAMPLE_NAME: &str = "two_outputs";
/// The base (first) milestone year of the example model
const BASE_YEAR: u32 = 2020;
/// The milestone year for which assets are selected in this benchmark
const YEAR: u32 = 2030;
/// The commodity market used for this benchmark: passenger transport demand, which has several
/// competing candidate technologies (petrol, diesel, electric and hybrid cars), making it a
/// representative, non-trivial case for asset selection.
const COMMODITY_ID: &str = "TPASKM";
/// The range of numbers of competing candidate technologies to benchmark, to see how
/// [`select_best_assets`] scales with the size of the search space.
const N_TECHNOLOGIES_RANGE: std::ops::RangeInclusive<usize> = 1..=20;

/// Extract the `two_outputs` example model to a temporary directory, load it, and create a
/// (non-debug) [`DataWriter`] for it.
///
/// The returned [`TempDir`]s must be kept alive for as long as `model` and `writer` are in use.
fn load_bench_model() -> (Model, DataWriter, TempDir, TempDir) {
    let example_dir = TempDir::new().expect("Failed to create temp dir for example");
    let model_path = example_dir.path().join(EXAMPLE_NAME);
    Example::from_name(EXAMPLE_NAME)
        .and_then(|example| example.extract(&model_path))
        .expect("Failed to extract example");
    let model = load_model(&model_path).expect("Failed to load model");

    let output_dir = TempDir::new().expect("Failed to create temp dir for output");
    let writer = DataWriter::create(output_dir.path(), &model_path, false)
        .expect("Failed to create data writer");

    (model, writer, example_dir, output_dir)
}

/// Commission base-year assets, age them up to `YEAR`, and build the candidate assets available
/// for investment in `YEAR`, mirroring the start of `simulation::run`.
fn build_assets_for_investment_year(
    model: &Model,
) -> (Vec<AssetRef>, Vec<AssetRef>, Vec<AssetRef>) {
    let mut user_assets = model.user_assets.clone();
    let mut asset_pool = AssetPool::new();
    asset_pool.commission_new(BASE_YEAR, &mut user_assets);

    // Keep a copy of the assets commissioned for the base year, which are used to seed prices for
    // the investment year (mirroring the first iteration of `simulation::run`, in which the base
    // year is dispatched before any investment has taken place)
    let base_year_assets: Vec<AssetRef> = asset_pool.to_vec();

    // Age the asset pool up to the investment year, dropping any assets that have been
    // decommissioned by then
    asset_pool.decommission_old(YEAR);
    let existing_assets = asset_pool.take();

    // Candidate assets for the investment year
    let candidates = candidate_assets_for_next_year(
        &model.processes,
        Some(YEAR),
        model.parameters.candidate_asset_capacity,
    );

    (base_year_assets, existing_assets, candidates)
}

/// Dispatch the base year (with and without the investment-year candidates) to obtain realistic
/// seed prices for investment appraisal, as per `simulation::run_dispatch_for_year` (this is the
/// dispatch performed before any agent investment has taken place).
fn calculate_seed_prices(
    model: &Model,
    base_year_assets: &[AssetRef],
    candidates: &[AssetRef],
    writer: &mut DataWriter,
) -> Prices {
    let solution_existing = DispatchRun::new(model, base_year_assets, BASE_YEAR)
        .run("bench setup: without candidates", writer)
        .expect("Dispatch without candidates failed");
    let solution_with_candidates = DispatchRun::new(model, base_year_assets, BASE_YEAR)
        .with_candidates(candidates)
        .run("bench setup: with candidates", writer)
        .expect("Dispatch with candidates failed");

    calculate_prices(
        model,
        &solution_existing,
        &solution_with_candidates,
        BASE_YEAR,
    )
    .expect("Failed to calculate prices")
}

/// Build `n` synthetic candidate processes, based on cycling through `templates`, so that we can
/// benchmark how [`select_best_assets`] scales with the number of competing technologies.
///
/// Each synthetic process is a copy of one of `templates`, but given a unique ID so that it is
/// treated as a distinct candidate technology.
fn build_synthetic_processes(templates: &[Rc<Process>], n: usize) -> Vec<Rc<Process>> {
    (0..n)
        .map(|i| {
            let template = &templates[i % templates.len()];
            Rc::new(Process {
                id: ProcessID::new(&format!("{}#{i}", template.id)),
                ..(**template).clone()
            })
        })
        .collect()
}

/// Benchmark [`select_best_assets`] using inputs derived from the `two_outputs` example model,
/// scaling the number of competing candidate technologies over `N_TECHNOLOGIES_RANGE`.
fn criterion_benchmark(c: &mut Criterion) {
    let (model, mut writer, _example_dir, _output_dir) = load_bench_model();
    let (base_year_assets, existing_assets, candidates) = build_assets_for_investment_year(&model);
    let prices = calculate_seed_prices(&model, &base_year_assets, &candidates, &mut writer);

    // The market of interest: agent, commodity and region for which assets will be selected
    let commodity = &model.commodities[COMMODITY_ID];
    let region_id = model
        .regions
        .keys()
        .next()
        .expect("Model must have at least one region");
    let (agent, commodity_portion) =
        get_responsible_agents(model.agents.values(), &commodity.id, region_id, YEAR)
            .next()
            .expect("No agent found responsible for the target commodity/region/year");

    let net_demand =
        flatten_preset_demands_for_year(&model.commodities, &model.time_slice_info, YEAR);
    let demand = get_demand_portion_for_market(
        &model.time_slice_info,
        &net_demand,
        &commodity.id,
        region_id,
        commodity_portion,
    );

    // Real candidate technologies for this market, used as templates to build up to
    // `N_TECHNOLOGIES_RANGE.end()` synthetic competing technologies
    let templates: Vec<Rc<Process>> = agent
        .iter_search_space(region_id, &commodity.id, YEAR)
        .cloned()
        .collect();

    let mut group = c.benchmark_group("select_best_assets");
    group
        .noise_threshold(0.05)
        .sample_size(20)
        .measurement_time(Duration::from_secs(3));

    for n in N_TECHNOLOGIES_RANGE {
        // Give the agent a synthetic search space of `n` competing technologies for this market
        let mut agent = agent.clone();
        agent.search_space.insert(
            (commodity.id.clone(), region_id.clone(), YEAR),
            Rc::new(build_synthetic_processes(&templates, n)),
        );

        let opt_assets: Vec<AssetRef> = get_asset_options(
            &existing_assets,
            &demand,
            &agent,
            commodity,
            region_id,
            YEAR,
            model.parameters.capacity_limit_factor,
        )
        .collect();
        let investment_limits =
            collect_investment_limits_for_candidates(&opt_assets, commodity_portion);

        group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, _| {
            b.iter_batched(
                || {
                    (
                        opt_assets.clone(),
                        investment_limits.clone(),
                        demand.clone(),
                    )
                },
                |(opt_assets, investment_limits, demand)| {
                    select_best_assets(
                        black_box(&model),
                        opt_assets,
                        investment_limits,
                        black_box(commodity),
                        black_box(&agent),
                        black_box(region_id),
                        black_box(&prices),
                        demand,
                        black_box(YEAR),
                        &mut writer,
                    )
                    .expect("select_best_assets failed")
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
