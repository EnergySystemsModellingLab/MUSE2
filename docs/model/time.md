# Time

MUSE2 represents time in two related ways: over the long-term model horizon and within each
milestone year.

```text
Time horizon
└── Milestone years
      └── Seasons
          └── Time slices
```

Milestone years describe when the model makes long-term investment decisions and records results.
Time slices describe how operation is represented within each of those years.

## Time horizon and milestone years

The time horizon is the overall period covered by a model run. It is represented by the ordered list
of `milestone_years` in `model.toml`, for example:

```toml
milestone_years = [2020, 2030, 2040]
```

Milestone years must be positive, sorted and unique. MUSE2 evaluates the system at each milestone
year. The first milestone year is the base year: existing assets and base-year demand describe the
initial system state. Subsequent milestone years are investment and reporting points, at which
assets, demand, dispatch and prices are evaluated.

Milestone years need not cover every calendar year in the horizon. Use additional milestone years
when technology, demand or policy changes need to be represented at a finer long-term resolution.

## Seasons and time slices

Each milestone year is divided into user-defined seasons and times of day. A *time slice* combines
one season and one time of day, and is written as `season.time_of_day`, for example `winter.day` or
`summer.peak`.

The names are labels supplied by the modeller. MUSE2 does not attach a built-in meaning to names
such as `winter`, `day` or `peak`.

Time slices are defined in `time_slices.csv`, for example:

```csv
season,time_of_day,fraction
winter,night,0.25
winter,day,0.25
summer,night,0.25
summer,day,0.25
```

The `fraction` is the share of the year represented by a time slice. Every fraction must be
positive, and the fractions for all slices must sum to one. If `time_slices.csv` is omitted, MUSE2
uses one `all-year.all-day` slice covering the whole year.

The input-file reference contains the complete [time-slice format](../file_formats/input_files.md#time-slices).

## Time-slice selections

A time-slice selection is the period over which MUSE2 applies a balance or limit. It can cover the
whole year (`annual`), a season (`winter`), or one time slice (`winter.day`). Time-slice selections
apply to the following, at granularities that depend either on the `time_slice_level` of a
commodity, or the `time_slice` of an explicitly provided constraint:

- **SED balance:** Supply and demand for an SED commodity are balanced at the commodity's
  `time_slice_level`. `annual` creates one balance for the whole year, `season` creates one balance
  per season, and `daynight` creates one balance per time slice. See
  [commodity balance constraints](./dispatch_optimisation.md#commodity-balance-constraints). In
  other words, `time_slice_level` controls how time slices are grouped for balancing; it does not
  change the time slices defined in `time_slices.csv`.
- **SVD demand:** Annual service demand is distributed using
  [`demand_slicing.csv`](../file_formats/input_files.md#demandslicingcsv). Demand-slicing data
  should be provided at the granularity of the SVD commodity's `time_slice_level`. Its `fraction`
  is the share of annual demand assigned to a selection, and is distinct from the duration
  `fraction` in `time_slices.csv`. See [commodity balance constraints](./dispatch_optimisation.md#commodity-balance-constraints).
- **Commodity constraints:** A production or consumption limit applies to the selection specified
  in its own `time_slice` field, which may differ from the commodity's balance level. For example,
  a commodity balanced annually can have a production limit that applies only to `winter`. See
  [commodity consumption/production constraints](./dispatch_optimisation.md#commodity-consumptionproduction-constraints).
- **Availability constraints:** A process activity limit applies to the selection specified in its
  own `time_slice` field. It can apply to one time slice, a season, or the whole year,
  independently of the balance level of the commodities produced or consumed by the process. See
  [asset activity limits](./dispatch_optimisation.md#asset-activity-limits).

## Choosing a resolution

- **Define the time slices for the most detailed requirement:** The shared time-slice definition
  must support the most detailed commodity balance or operational constraint in the model.
- **Choose each commodity's balance level separately:** Set `time_slice_level` to `annual`, `season`,
  or `daynight` according to the resolution needed for that commodity. Demand-slicing data should
  use the same granularity.
- **Keep explicit constraints in mind:** Availability limits and commodity constraints specify their
  own time-slice selection, which can be more detailed than the balance level of the commodities
  involved.

More time slices provide more detail but increase the size of the optimisation problem, so use only
the temporal resolution needed by the model.
