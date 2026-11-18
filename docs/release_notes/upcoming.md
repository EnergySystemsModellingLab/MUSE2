# Upcoming release of MUSE2

## New features

- Seasonal/annual utilisation penalties are now included in the investment mini-dispatch ([#1548])
- The number of price calculation iterations for cyclically-dependent markets can now be configured
 with `price_cycle_iterations` in `model.toml` (default: 1)
- Circular commodity dependencies are now handled with a two-pass investment algorithm ([#1557])
- Agents now have an agent type, set with the new optional `agent_type` column in `agents.csv`. The
  new `portfolio` agent type selects assets for each market by solving a single optimisation,
  rather than appraising options one at a time ([#1574])
- A new `population` agent type represents a population of discrete entities with identical needs,
  such as households, and selects assets in merit order ([#1603])
- A new optional `simulation_protocol` setting in `model.toml` selects how each milestone year is
  simulated. The new default, `agent_dispatch`, calculates flows and prices directly from the
  activities determined by agents' own optimisations, without a system-wide dispatch.
  `system_dispatch` selects the previous behaviour

## Breaking changes

- Investment constraints are now defined per agent in `agent_investment_constraints.csv` instead
  of per process in `process_investment_constraints.csv`.
- The `capacity_margin` parameter has been removed ([#1557])
- `portfolio` is now the default agent type. To keep the previous investment behaviour, set
  `agent_type` to `competitive` in `agents.csv` ([#1574])
- The `shadow` and `scarcity` pricing strategies have been removed. Commodities using them in
  `commodities.csv`, or `fallback_pricing_strategy` in `model.toml`, must switch to another strategy
- Prices in cyclically-dependent markets are now seeded with the previous year's (or previous
  ironing-out iteration's) prices, or zero if there are none, rather than with shadow prices

## Bug fixes

[#1548]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1548
[#1557]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1557
[#1574]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1574
[#1603]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1603
