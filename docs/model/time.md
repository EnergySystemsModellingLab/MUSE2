# Time

MUSE2 represents time in two related ways: over the long-term *time horizon* (the overall period
covered by a model run, from the first to the last milestone year) and within each milestone year.

The model proceeds through the time horizon one milestone year at a time. For each milestone year,
MUSE2 decides which new assets to invest in, then simulates how the system operates over a single
representative year, divided into time slices. Results are recorded for that milestone year before
moving on to the next.

## Time horizon and milestone years

The time horizon is represented by the ordered list of `milestone_years` in `model.toml`, for
example:

```toml
milestone_years = [2020, 2030, 2040]
```

MUSE2 evaluates the system at each milestone year in turn. The first milestone year is referred to
as the "base year": existing assets and base-year demand describe the initial system state.
Subsequent milestone years are investment and reporting points, at which assets, demand, dispatch
and prices are evaluated.

Milestone years need not cover every calendar year in the horizon. Note that the model is not run
for the years in between milestone years: no investment or dispatch takes place in them and no
results are produced for them. In the example above, nothing is simulated for 2021–2029. Use
additional milestone years when technology, demand or policy changes need to be represented at a
finer long-term resolution.

## Seasons and time slices

Each milestone year is divided into user-defined seasons and times of day. A *time slice* combines
one season and one time of day, and is written as `season.time_of_day`, for example `winter.day` or
`summer.peak`.

The names are labels supplied by the modeller. MUSE2 does not attach a built-in meaning to names
such as `winter`, `day` or `peak`. The order in which seasons, times of day and time slices are
listed has no effect on the results: they are treated as independent periods, with no notion of one
following another.

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

## Temporal granularity

Although the time slices are defined once for the whole model, not every balance or limit in MUSE2
has to be applied at the finest level. Depending on the case, it can be applied to a single time
slice, to a whole season, or to the whole year. Input files that need to refer to such a period
(for example, to say when a limit applies) use a common syntax:

- a single time slice, written `season.time_of_day` (e.g. `winter.day`)
- a whole season, written as the season name (e.g. `winter`)
- the whole year, written `annual`

For commodities, the `time_slice_level` field in `commodities.csv` sets the granularity at which
supply and demand are balanced: `annual` creates one balance for the whole year, `season` one per
season, and `daynight` one per time slice. It controls how time slices are grouped for balancing; it
does not change the time slices defined in `time_slices.csv`. Service demand data in
[`demand_slicing.csv`](../file_formats/input_files.md#demandslicingcsv) should be provided at the
same granularity. Other inputs, such as process availability limits and commodity constraints,
specify their own time-slice period, which may differ from the balance level of the commodities
involved. See the [input file reference](../file_formats/input_files.md) for the fields which accept
this syntax, and the [dispatch optimisation](./dispatch_optimisation.md) documentation for how they
are applied.

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
