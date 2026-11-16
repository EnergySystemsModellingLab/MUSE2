# Upcoming release of MUSE2

## New features

- Seasonal/annual utilisation penalties are now included in the investment mini-dispatch ([#1548])
- The number of price calculation iterations for cyclically-dependent markets can now be configured
 with `price_cycle_iterations` in `model.toml` (default: 1)
- Circular commodity dependencies are now handled with a two-pass investment algorithm ([#1557])
- Agents now have an agent type, set with the new optional `agent_type` column in `agents.csv`. The
  new `portfolio` agent type selects assets for each market by solving a single optimisation,
  rather than appraising options one at a time ([#1574])

## Breaking changes

- The `capacity_margin` parameter has been removed ([#1557])
- `portfolio` is now the default agent type. To keep the previous investment behaviour, set
  `agent_type` to `competitive` in `agents.csv` ([#1574])

## Bug fixes

[#1548]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1548
[#1557]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1557
[#1574]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1574
