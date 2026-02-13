# Upcoming release of MUSE2

<!-- Instructions for developers:

This file contains information about the current in-progress version of MUSE2. Once that version is
ready to be released, carry out the following steps:

- Copy this file, giving it a name corresponding to its version (e.g. v2.3.4)
- Change the heading to be in the form "Release notes for MUSE2 v2.3.4 (January 3, 2027)"
- Remove this comment 😀
- Add the new file to docs/SUMMARY.md and docs/release_notes/README.md
- Empty the sections below in *this* file (upcoming.md) ready for us to add changes for the next
  version

-->

## New features

- You can now set both upper and lower bounds for process availabilities ([#1018])
- Default output root path is now configurable in `settings.toml` ([#1003])
- If demand cannot be satisfied by the simulation, we now inform users what the offending
  commodities and time slices are ([#767])
- Trim whitespace from fields when reading in CSV files ([#976])
- Assets can now be commissioned after a specified number of years with the `mothball_years`
  parameter ([#1022])
- Default to no availability limits if user doesn't provide any for a process ([#1018])
- Allow user to specify ranges of years in input files ([#1017])
- Users can now omit empty CSV files ([#961])
- Users can now optionally specify an explicit decommission year for an asset in `assets.csv` input
  file ([#966])
- Allow for adding both a `prod` and `cons` levy to a commodity ([#969])
- Availability limits can now be provided at multiple levels for a process ([#1018])
- Pricing strategy can now vary by commodity ([#1021])

## Experimental features

- Assets can now be made divisible to represent many individual assets, such as a fleet of gas
  boilers (albeit the current implementation is slow; [#1030]). These fleets can be partially
  decommissioned.
- Users can now have circular dependencies between commodities, such as a hydrogen power plant that
  itself requires electricity (though the current solution likely won't work in every situation;
  [#986])
- There are new options for pricing strategy (`full` and `marginal`), which take capital costs into
  account ([#1021])

## Bug fixes

- Fix: process availability constraints were wrongly being applied to individual time slices,
  regardless of time slice level ([#1018])
- Various fixes to process flows and availabilities input code for non-milestone years ([#868],
  [#1000], [#1010])
- Users can now set demand to zero in `demand.csv` ([#871])
- Fix: sign for levies of type `net` was wrong for inputs ([#969])
- Fix `--overwrite` option for `save-graphs` command ([#1001])
- Skip assets with zero activity when appraising with LCOX ([#1129])

[#767]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/767
[#868]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/868
[#871]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/871
[#961]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/961
[#966]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/966
[#969]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/969
[#976]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/976
[#986]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/986
[#1000]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1000
[#1001]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1001
[#1003]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1003
[#1010]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1010
[#1017]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1017
[#1018]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1018
[#1021]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1021
[#1022]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1022
[#1030]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1030
[#1129]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1129
