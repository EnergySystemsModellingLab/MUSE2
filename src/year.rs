//! Code for working with years.
use crate::input::is_sorted_and_unique;
use anyhow::{Context, Result, ensure};
use itertools::Itertools;

/// Parse a single year from a string and check it is in `valid_years`
fn parse_and_validate_year(s: &str, valid_years: &[u32]) -> Option<u32> {
    let year = s.trim().parse::<u32>().ok()?;
    if valid_years.binary_search(&year).is_ok() {
        Some(year)
    } else {
        None
    }
}

/// Parse a string of years separated by semicolons into a vector of u32 years.
///
/// The string can be either "all" (case-insensitive), a single year, or a semicolon-separated list
/// of years (e.g. "2020;2021;2022" or "2020; 2021; 2022")
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
/// If `valid_years` is unsorted or non-unique.
pub fn parse_year_str(s: &str, valid_years: &[u32]) -> Result<Vec<u32>> {
    // We depend on this in `parse_and_validate_year`
    assert!(
        is_sorted_and_unique(valid_years),
        "`valid_years` must be sorted and unique"
    );

    let s = s.trim();
    ensure!(!s.is_empty(), "No years provided");

    if s.eq_ignore_ascii_case("all") {
        return Ok(Vec::from_iter(valid_years.iter().copied()));
    }

    ensure!(
        !(s.contains(';') && s.contains("..")),
        "Both ';' and '..' found in year string {s}. Discrete years and ranges cannot be mixed."
    );

    // We first process ranges
    let years: Vec<_> = if s.contains("..") {
        parse_years_range(s, valid_years)?
    } else {
        s.split(';')
            .map(|y| {
                parse_and_validate_year(y, valid_years)
                    .with_context(|| format!("Invalid year: {y}"))
            })
            .try_collect()?
    };

    ensure!(
        is_sorted_and_unique(&years),
        "Years must be in order and unique"
    );

    Ok(years)
}

/// Parse a year string that is defined as a range, selecting the valid years within that range.
///
/// It should be of the form start..end. If either of the limits are omitted, they will default to
/// the first and last years of the `valid_years`. If both limits are missing, this is equivalent to
/// passing all.
fn parse_years_range(s: &str, valid_years: &[u32]) -> Result<Vec<u32>> {
    // Require exactly one ".." separator so only forms start..end, start.. or ..end are allowed.
    let parts: Vec<&str> = s.split("..").collect();
    ensure!(
        parts.len() == 2,
        "Year range must be of the form 'start..end', 'start..' or '..end'. Invalid: {s}"
    );
    let left = parts[0].trim();
    let right = parts[1].trim();

    // If the range start is open, we assign the first valid year
    let start = if left.is_empty() {
        valid_years[0]
    } else {
        left.parse::<u32>()
            .ok()
            .with_context(|| format!("Invalid start year in range: {left}"))?
    };

    // If the range end is open, we assign the last valid year
    let end = if right.is_empty() {
        *valid_years.last().unwrap()
    } else {
        right
            .parse::<u32>()
            .ok()
            .with_context(|| format!("Invalid end year in range: {right}"))?
    };

    ensure!(
        end > start,
        "End year must be bigger than start year in range {s}"
    );
    let years: Vec<_> = (start..=end).filter(|y| valid_years.contains(y)).collect();
    ensure!(
        !years.is_empty(),
        "No valid years found in year range string {s}"
    );
    Ok(years)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture::assert_error;
    use rstest::rstest;

    #[rstest]
    #[case("2020", &[2020, 2021], &[2020])]
    #[case("all", &[2020, 2021], &[2020,2021])]
    #[case("ALL", &[2020, 2021], &[2020,2021])]
    #[case(" ALL ", &[2020, 2021], &[2020,2021])]
    #[case("2020;2021", &[2020, 2021], &[2020,2021])]
    #[case("  2020;  2021", &[2020, 2021], &[2020,2021])] // whitespace should be stripped
    #[case("2019..2026", &[2020,2025], &[2020,2025])]
    #[case("..2023", &[2020,2025], &[2020])] // Empty start
    #[case("2021..", &[2020,2025], &[2025])] // Empty end
    #[case("..", &[2020,2025], &[2020,2025])]
    fn test_parse_year_str_valid(
        #[case] input: &str,
        #[case] milestone_years: &[u32],
        #[case] expected: &[u32],
    ) {
        assert_eq!(parse_year_str(input, milestone_years).unwrap(), expected);
    }

    #[rstest]
    #[case("", &[2020], "No years provided")]
    #[case("2021", &[2020], "Invalid year: 2021")]
    #[case("a;2020", &[2020], "Invalid year: a")]
    #[case("2021;2020", &[2020, 2021],"Years must be in order and unique")] // out of order
    #[case("2021;2020;2021", &[2020, 2021],"Years must be in order and unique")] // duplicate
    #[case("2021;2020..2021", &[2020, 2021],"Both ';' and '..' found in year string 2021;2020..2021. Discrete years and ranges cannot be mixed.")]
    #[case("2021..2020", &[2020, 2021],"End year must be bigger than start year in range 2021..2020")] // out of order
    #[case("2021..2024", &[2020,2025], "No valid years found in year range string 2021..2024")]
    #[case("..2020..2025", &[2020,2025], "Year range must be of the form 'start..end', 'start..' or '..end'. Invalid: ..2020..2025")]
    #[case("2020...2025", &[2020,2025], "Invalid end year in range: .2025")]
    fn test_parse_year_str_invalid(
        #[case] input: &str,
        #[case] milestone_years: &[u32],
        #[case] error_msg: &str,
    ) {
        assert_error!(parse_year_str(input, milestone_years), error_msg);
    }
}
