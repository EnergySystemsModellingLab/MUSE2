//! Code for working with years.
use super::{is_sorted_and_unique, is_sorted_and_unique_with, parse_range_parts};
use anyhow::{Context, Result, bail, ensure};
use itertools::Itertools;
use std::ops::RangeInclusive;

/// Return any valid years in the specified range
fn get_valid_years_in_range(
    range: &RangeInclusive<u32>,
    valid_years: &[u32],
) -> impl Iterator<Item = u32> {
    valid_years
        .iter()
        .copied()
        .filter(move |year| range.contains(year))
}

/// Parse a string of years separated by semicolons into a vector of u32 years.
///
/// The string can be either "all" (case-insensitive) or year ranges (optionally) separated with
/// semicolons. A year range can be a single year (e.g. 2020) or a range with a start year and/or
/// end year (e.g. 2020.., ..2020, 2020..2025).
///
/// # Arguments
///
/// - `s` - Input string to parse
/// - `valid_years` - The possible years which can be referenced in `s` (must be sorted and unique)
///
/// # Returns
///
/// A [`Vec`] of years or an error.
///
/// # Panics
///
/// If `valid_years` is empty, unsorted or contains duplicates.
pub fn parse_year_str(s: &str, valid_years: &[u32]) -> Result<Vec<u32>> {
    assert!(!valid_years.is_empty(), "`valid_years` cannot be empty");
    assert!(
        is_sorted_and_unique(valid_years),
        "`valid_years` must be sorted and unique"
    );

    let s = s.trim();
    ensure!(!s.is_empty(), "No years provided");

    if s.eq_ignore_ascii_case("all") {
        return Ok(Vec::from_iter(valid_years.iter().copied()));
    }

    // Get ranges of years, separated by semicolons. Note that a range can be a single year.
    let ranges: Vec<_> = s
        .split(';')
        .map(|s| {
            let (start, end) = s.split_once("..").unwrap_or((s, s));
            parse_range_parts(
                start,
                end,
                u32::MIN..=u32::MAX,
                *valid_years.first().unwrap(),
                *valid_years.last().unwrap(),
            )
            .with_context(|| format!("Invalid year range: {s}"))
        })
        .try_collect()?;

    ensure!(
        is_sorted_and_unique_with(ranges.iter(), |a, b| {
            a.start() < b.start() && a.end() < b.start()
        }),
        "Year ranges must be sorted and non-overlapping"
    );

    let mut years = Vec::new();
    for range in ranges {
        let old_len = years.len();
        years.extend(get_valid_years_in_range(&range, valid_years));

        // No valid years in range
        if years.len() == old_len {
            // For readability, provide different error messages for single year vs range
            if range.start() == range.end() {
                bail!("Invalid year: {}", range.start());
            }
            bail!(
                "No valid years in year range: {}..{}",
                range.start(),
                range.end()
            );
        }
    }

    Ok(years)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use rstest::rstest;

    #[rstest]
    #[case("2020", &[2020, 2021], &[2020])]
    #[case("all", &[2020, 2021], &[2020, 2021])]
    #[case("ALL", &[2020, 2021], &[2020, 2021])]
    #[case(" ALL ", &[2020, 2021], &[2020, 2021])]
    #[case("2020;2021", &[2020, 2021], &[2020, 2021])]
    #[case("  2020;  2021", &[2020, 2021], &[2020, 2021])] // whitespace should be stripped
    #[case("2019..2026", &[2020, 2025], &[2020, 2025])]
    #[case("..2023", &[2020, 2025], &[2020])] // Empty start
    #[case("2021..", &[2020, 2025], &[2025])] // Empty end
    #[case("2020;2021..2022", &[2020, 2021, 2022], &[2020, 2021, 2022])] // Can have multiple ranges
    fn parse_year_str_valid(
        #[case] input: &str,
        #[case] milestone_years: &[u32],
        #[case] expected: &[u32],
    ) {
        assert_eq!(parse_year_str(input, milestone_years).unwrap(), expected);
    }

    #[rstest]
    #[case("", &[2020], "No years provided")]
    #[case("2021", &[2020], "Invalid year: 2021")]
    #[case("a;2020", &[2020], "Invalid year range: a")]
    #[case("2021;2020", &[2020, 2021], "Year ranges must be sorted and non-overlapping")] // out of order
    #[case("2021;2020;2021", &[2020, 2021], "Year ranges must be sorted and non-overlapping")] // duplicate
    #[case("2021..2020", &[2020, 2021], "Invalid year range: 2021..2020")] // out of order
    #[case("2021..2024", &[2020, 2025], "No valid years in year range: 2021..2024")]
    #[case("..2020..2025", &[2020, 2025], "Invalid year range: ..2020..2025")]
    #[case("2020...2025", &[2020, 2025], "Invalid year range: 2020...2025")]
    #[case("..", &[2020, 2025], "Invalid year range: ..")]
    fn parse_year_str_invalid(
        #[case] input: &str,
        #[case] milestone_years: &[u32],
        #[case] error_msg: &str,
    ) {
        assert_error!(parse_year_str(input, milestone_years), error_msg);
    }
}
