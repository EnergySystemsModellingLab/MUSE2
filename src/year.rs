//! Code for working with years.
use crate::input::is_sorted_and_unique;
use anyhow::{Context, Result, ensure};
use itertools::Itertools;

/// Parse a string of years separated by semicolons into a vector of u32 years.
///
/// The string can be either "all" (case-insensitive), a single year, or a semicolon-separated list
/// of years (e.g. "2020;2021;2022" or "2020; 2021; 2022")
///
/// # Arguments
///
/// - `s` - Input string to parse
/// - `valid_years` - The possible years which can be referenced in `s`
///
/// # Returns
///
/// A [`Vec`] of years or an error.
pub fn parse_year_str<I, J>(s: &str, valid_years: I) -> Result<Vec<u32>>
where
    I: IntoIterator<Item = u32, IntoIter = J> + Clone,
    J: Iterator<Item = u32> + Clone,
{
    let s = s.trim();
    ensure!(!s.is_empty(), "No years provided");
    let valid_years = valid_years.into_iter();

    if s.eq_ignore_ascii_case("all") {
        return Ok(Vec::from_iter(valid_years));
    }

    let parse_and_validate_year = |s: &str| {
        let year = s.trim().parse::<u32>().ok()?;
        valid_years.clone().contains(&year).then_some(year)
    };
    let years: Vec<_> = s
        .split(';')
        .map(|y| parse_and_validate_year(y).with_context(|| format!("Invalid year: {y}")))
        .try_collect()?;

    ensure!(
        is_sorted_and_unique(&years),
        "Years must be in order and unique"
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
    fn test_parse_year_str_valid(
        #[case] input: &str,
        #[case] milestone_years: &[u32],
        #[case] expected: &[u32],
    ) {
        assert_eq!(
            parse_year_str(input, milestone_years.iter().copied()).unwrap(),
            expected
        );
    }

    #[rstest]
    #[case("", &[2020], "No years provided")]
    #[case("2021", &[2020], "Invalid year: 2021")]
    #[case("a;2020", &[2020], "Invalid year: a")]
    #[case("2021;2020", &[2020, 2021],"Years must be in order and unique")] // out of order
    #[case("2021;2020;2021", &[2020, 2021],"Years must be in order and unique")] // duplicate
    fn test_parse_year_str_invalid(
        #[case] input: &str,
        #[case] milestone_years: &[u32],
        #[case] error_msg: &str,
    ) {
        assert_error!(
            parse_year_str(input, milestone_years.iter().copied()),
            error_msg
        );
    }
}
