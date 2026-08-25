# Making a release

This page describes the steps for making a new MUSE2 release.

## Before you start

Before preparing the release, manually run the [Test with MUSE2 workflow] on the `main` branch of
the [muse2_data_analysis repository] and check that all jobs pass. This workflow tests the analysis
repository's `main` branch against MUSE2's `main` branch.

## Choose a version number

MUSE2 version numbers have the form `2.X.Y`. The first digit is fixed at `2` to distinguish MUSE2
releases from the Python-based [MUSE_OS]. The second digit represents major changes, while the third
digit represents minor changes. This is a looser versioning scheme than Semantic Versioning, where
"major" and "minor" have stricter definitions.

## Prepare the release

1. Review `docs/release_notes/upcoming.md` and make sure the entries are complete and correct.
1. Copy `docs/release_notes/upcoming.md` to `docs/release_notes/v2.X.Y.md`.
1. Update the heading in the new file to include the version and the date on which the release will
   be published, for example:

   ```md
   # Release notes for MUSE2 v2.3.4 (January 3, 2027)
   ```

1. Remove the developer instructions from the versioned release notes.
1. Add the new release notes to [the documentation summary][documentation summary] and [the
   release notes index][release notes index].
1. Empty the release-note sections in `docs/release_notes/upcoming.md`, while retaining its
   developer instructions as a placeholder for the next release.

## Update version metadata

Update all release metadata together:

- Set the package version in `Cargo.toml`.
- Set `version` and `date-released` in `CITATION.cff`.

## Publish the release

Once the preparation changes have been merged to `main` and CI has passed, create and publish a
GitHub Release with the exact `v2.X.Y` tag, such as `v2.3.4`.

The published release triggers the workflows that build and attach binaries, publish the crate to
[crates.io], and deploy the documentation. Tags must use the
`v2.X.Y` format so that the documentation tooling recognises them as MUSE2 releases.

After publishing, manually check the [GitHub releases page], the [MUSE2 crates.io page], and the
[published documentation] to confirm that the release is available in each location.

## Create a release for the analysis repository

After publishing the MUSE2 release, create and publish a release in the
[muse2_data_analysis repository] with the same name as the MUSE2 release, such as `v2.3.4`.
The analysis repository derives its package version from the Git tag, so this tag must exactly
match the MUSE2 release tag.

[GitHub releases page]: https://github.com/EnergySystemsModellingLab/MUSE2/releases
[MUSE2 crates.io page]: https://crates.io/crates/muse2
[published documentation]: https://energysystemsmodellinglab.github.io/MUSE2/
[muse2_data_analysis repository]: https://github.com/EnergySystemsModellingLab/muse2_data_analysis
[MUSE_OS]: https://github.com/EnergySystemsModellingLab/MUSE_OS
[Test with MUSE2 workflow]: https://github.com/EnergySystemsModellingLab/muse2_data_analysis/actions/workflows/ci.yml
[crates.io]: https://crates.io/crates/muse2
[documentation summary]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/docs/SUMMARY.md
[release notes index]: https://github.com/EnergySystemsModellingLab/MUSE2/blob/main/docs/release_notes/README.md
