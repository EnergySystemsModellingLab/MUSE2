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

- Users can now optionally pass [custom options][highs-opts-docs] to the HiGHS solver [#1276]

## Breaking changes

- Changed the default `pricing_strategy` for SED/SVD commodities from "shadow" to "full_average" ([#1281])

## Bug fixes

- Fix misleading warning message for assets decommissioned before simulation start ([#1259])

[highs-opts-docs]: https://energysystemsmodellinglab.github.io/MUSE2/developer_guide/custom_highs_options.html
[#1259]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1259
[#1281]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1281
[#1276]: https://github.com/EnergySystemsModellingLab/MUSE2/pull/1276
