//! Benchmark for [`select_best_assets`], which is responsible for iteratively choosing the best
//! assets (from among existing and candidate assets) to meet demand for a given commodity/region
//! market during agent investment.
use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group, criterion_main};
use muse2::asset::{AssetIterator, AssetPool, AssetRef};
use muse2::commodity::CommodityID;
use muse2::example::Example;
use muse2::input::load_model;
use muse2::model::Model;
use muse2::output::DataWriter;
use muse2::process::{Process, ProcessID};
use muse2::region::RegionID;
use muse2::simulation::investment::collect_preset_demands_for_year;
use muse2::simulation::investment::competitive::{get_candidate_assets, select_best_assets};
use muse2::simulation::market::{
    collect_agent_limits, get_demand_portion_for_market, get_responsible_agents,
};
use muse2::simulation::prices::{PriceMap, Prices};
use muse2::time_slice::TimeSliceID;
use muse2::units::MoneyPerFlow;
use rayon::ThreadPoolBuilder;
use serde::Deserialize;
use std::hint::black_box;
use std::sync::Arc;
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
/// The numbers of competing candidate technologies to benchmark, stepped in fives, to see how
/// [`select_best_assets`] scales with the size of the search space. Values are zero-padded to
/// two digits when used as benchmark IDs so that names sort correctly on the filesystem
/// (e.g. `05` before `10` rather than `10` before `5`).
const N_TECHNOLOGIES: &[usize] = &[1, 5, 10, 15, 20];

/// Extract the `two_outputs` example model to a temporary directory, load it, and create a
/// (non-debug) [`DataWriter`] for it.
///
/// The returned [`TempDir`]s must be kept alive for as long as `model` and `writer` are in use.
fn load_bench_model() -> (Model, DataWriter, TempDir, TempDir) {
    let example_dir = TempDir::new().expect("Failed to create temp dir for example");
    let model_path = example_dir.path().join(EXAMPLE_NAME);
    Example::from_name(EXAMPLE_NAME)
        .expect("Invalid example name")
        .extract(&model_path)
        .expect("Failed to extract example");
    let model = load_model(&model_path).expect("Failed to load model");

    let output_dir = TempDir::new().expect("Failed to create temp dir for output");
    let writer = DataWriter::create(output_dir.path(), &model_path, false)
        .expect("Failed to create data writer");

    (model, writer, example_dir, output_dir)
}

/// Commission base-year assets and age them up to `YEAR`, mirroring the start of `simulation::run`.
fn build_assets_for_investment_year(model: &Model) -> Vec<AssetRef> {
    let mut user_assets = model.user_assets.clone();
    let mut asset_pool = AssetPool::new();
    asset_pool.commission_new(BASE_YEAR, &mut user_assets);

    // Age the asset pool up to the investment year, dropping any assets that have been
    // decommissioned by then
    asset_pool.decommission_old(YEAR);
    asset_pool.take()
}

/// Load recorded base-year prices so benchmark setup does not require a simulation.
///
/// The example uses the same full-cost-average strategy for market and fallback prices.
fn load_seed_prices() -> Prices {
    #[derive(Deserialize)]
    struct PriceRow {
        milestone_year: u32,
        commodity_id: CommodityID,
        region_id: RegionID,
        time_slice: TimeSliceID,
        price: MoneyPerFlow,
    }

    let mut reader = csv::Reader::from_reader(
        include_bytes!("../tests/data/two_outputs/commodity_prices.csv").as_slice(),
    );
    let mut market = PriceMap::default();
    for row in reader.deserialize::<PriceRow>() {
        let row = row.expect("Failed to read benchmark seed prices");
        if row.milestone_year == BASE_YEAR {
            market.insert(
                &row.commodity_id,
                &row.region_id,
                &row.time_slice,
                row.price,
            );
        }
    }
    assert!(
        market.iter().next().is_some(),
        "Benchmark seed prices must include the base year"
    );

    Prices {
        fallback: market.clone(),
        market,
    }
}

/// Build `n` synthetic candidate processes, based on cycling through `templates`, so that we can
/// benchmark how [`select_best_assets`] scales with the number of competing technologies.
///
/// Each synthetic process is a copy of one of `templates`, but given a unique ID so that it is
/// treated as a distinct candidate technology.
fn build_synthetic_processes(templates: &[Arc<Process>], n: usize) -> Vec<Arc<Process>> {
    (0..n)
        .map(|i| {
            let template = &templates[i % templates.len()];
            Arc::new(Process {
                id: ProcessID::new(&format!("{}#{i}", template.id)),
                ..(**template).clone()
            })
        })
        .collect()
}

/// Benchmark [`select_best_assets`] using inputs derived from the `two_outputs` example model,
/// scaling the number of competing candidate technologies over `N_TECHNOLOGIES_RANGE`.
///
/// Two groups are run:
/// - `parallel`: uses the default Rayon global thread pool (all available cores).
/// - `sequential`: installs a single-thread Rayon pool so that `par_iter` degenerates to serial
///   execution, giving a fair like-for-like comparison with the overhead of parallelism removed.
#[allow(clippy::too_many_lines)]
fn criterion_benchmark(c: &mut Criterion) {
    let (model, mut writer, _example_dir, _output_dir) = load_bench_model();
    let existing_assets = build_assets_for_investment_year(&model);
    let prices = load_seed_prices();

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

    let net_demand = collect_preset_demands_for_year(&model.commodities, YEAR);
    let demand = get_demand_portion_for_market(
        &model.time_slice_info,
        &net_demand,
        commodity,
        region_id,
        commodity_portion,
    );

    // Real candidate technologies for this market, used as templates to build up to
    // `N_TECHNOLOGIES` synthetic competing technologies
    let templates: Vec<Arc<Process>> = agent
        .iter_search_space(region_id, &commodity.id, YEAR)
        .cloned()
        .collect();

    // Single-thread pool used to run `select_best_assets` sequentially for comparison.
    // Because `select_best_assets` uses `par_iter` internally, installing this pool makes it
    // degenerate to serial execution while keeping all other code paths identical.
    let sequential_pool = ThreadPoolBuilder::new()
        .num_threads(1)
        .build()
        .expect("Failed to build sequential thread pool");

    for (group_name, use_parallel) in &[("parallel", true), ("sequential", false)] {
        let mut group = c.benchmark_group(format!("select_best_assets/{group_name}"));
        group
            .noise_threshold(0.05)
            .sample_size(20)
            .measurement_time(Duration::from_secs(3));

        for &n in N_TECHNOLOGIES {
            // Give the agent a synthetic search space of `n` competing technologies
            let mut agent = agent.clone();
            agent.search_space.insert(
                (commodity.id.clone(), region_id.clone(), YEAR),
                Arc::new(build_synthetic_processes(&templates, n)),
            );

            let processes: Vec<_> = agent
                .iter_search_space(region_id, &commodity.id, YEAR)
                .cloned()
                .collect();
            let mut opt_assets: Vec<AssetRef> = existing_assets
                .iter()
                .filter_agent(&agent.id)
                .filter_region(region_id)
                .filter_primary_producers_of(&commodity.id)
                .cloned()
                .collect();
            opt_assets.extend(get_candidate_assets(
                &demand,
                &processes,
                region_id,
                commodity,
                YEAR,
                model.parameters.capacity_tranche_fraction,
            ));
            let agent_addition_limits = collect_agent_limits(
                &agent,
                region_id,
                &commodity.id,
                YEAR,
                commodity_portion,
                Process::agent_addition_limit,
            );
            let agent_total_limits = collect_agent_limits(
                &agent,
                region_id,
                &commodity.id,
                YEAR,
                commodity_portion,
                Process::agent_total_limit,
            );

            group.bench_with_input(
                BenchmarkId::from_parameter(format!("{n:02}")),
                &n,
                |b, _| {
                    b.iter_batched(
                        || {
                            (
                                opt_assets.clone(),
                                agent_addition_limits.clone(),
                                agent_total_limits.clone(),
                                demand.clone(),
                            )
                        },
                        |(opt_assets, agent_addition_limits, agent_total_limits, demand)| {
                            let run = || {
                                select_best_assets(
                                    black_box(&model),
                                    &[],
                                    opt_assets,
                                    agent_addition_limits,
                                    agent_total_limits,
                                    black_box(commodity),
                                    black_box(&agent),
                                    black_box(region_id),
                                    black_box(&prices),
                                    &demand,
                                    black_box(YEAR),
                                    &mut writer,
                                )
                                .expect("select_best_assets failed")
                            };
                            if *use_parallel {
                                run()
                            } else {
                                sequential_pool.install(run)
                            }
                        },
                        BatchSize::SmallInput,
                    );
                },
            );
        }
        group.finish();
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
