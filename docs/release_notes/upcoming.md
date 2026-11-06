# Upcoming release of MUSE2

## New features

- Seasonal/annual utilisation penalties are now included in the investment mini-dispatch ([#1548])
- The number of price calculation iterations for cyclically-dependent markets can now be configured
 with `price_cycle_iterations` in `model.toml` (default: 1)
- Circular commodity dependencies are now handled with a two-pass investment algorithm, which
  automatically identifies feedback processes ([#1557])

## Breaking changes

- The `capacity_margin` parameter has been removed ([#1557])

## Bug fixes

[#1548]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1548
[#1557]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1557
