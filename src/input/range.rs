//! Provides a helper for parsing range-type parameters from input files.
use anyhow::{Result, ensure};
use std::error::Error;
use std::fmt::Display;
use std::ops::RangeInclusive;
use std::str::FromStr;

/// Try to divide a string into two parts at the specified delimiter.
///
/// # Returns
///
/// - `None` if `delimiter` is not present
/// - `Some` tuple of the two strings if it is
fn partition<'a>(s: &'a str, delimiter: &str) -> Option<(&'a str, &'a str)> {
    let idx = s.find(delimiter)?;

    #[allow(clippy::string_slice)]
    Some((&s[..idx], &s[idx + delimiter.len()..]))
}

/// Parse a range from an input string, using values in `limits` as defaults.
///
/// See [`parse_range_with_defaults`].
pub fn parse_range<T>(s: &str, limits: RangeInclusive<T>) -> Result<RangeInclusive<T>>
where
    T: FromStr + Copy + PartialOrd + Display,
    <T as FromStr>::Err: Error + Sync + Send + 'static,
{
    parse_range_with_defaults(s, limits.clone(), *limits.start(), *limits.end())
}

/// Parse a range from an input string.
///
/// Start and end values must be a type that is parseable from a string. Ranges are inclusive.
/// Whitespace is trimmed from start and end values before parsing.
///
/// If start or end values are omitted, the values in `defaults` will be used.
///
/// Valid ranges:
///
/// - Single value (e.g. 2000)
/// - Range of values (e.g. 1990..2000)
/// - Range with no upper limit (e.g. 1990..)
/// - Range with no lower limit (e.g. ..2000)
///
/// # Panics
///
/// Panics if `limits` has a start after its end or `default_lower` is greater than
/// `default_upper`.
pub fn parse_range_with_defaults<T>(
    s: &str,
    limits: RangeInclusive<T>,
    default_lower: T,
    default_upper: T,
) -> Result<RangeInclusive<T>>
where
    T: FromStr + Copy + PartialOrd + Display,
    <T as FromStr>::Err: Error + Sync + Send + 'static,
{
    assert!(
        limits.start() <= limits.end(),
        "Start of limits must be before end"
    );
    assert!(
        default_lower <= default_upper,
        "default_lower must be less than default_upper"
    );

    let (start, end) = if let Some((str1, str2)) = partition(s, "..") {
        let str1 = str1.trim();
        let str2 = str2.trim();
        ensure!(
            !str1.is_empty() || !str2.is_empty(),
            "Start and end of range cannot both be omitted"
        );

        let value1 = if str1.is_empty() {
            default_lower
        } else {
            str1.parse()?
        };
        let value2 = if str2.is_empty() {
            default_upper
        } else {
            str2.parse()?
        };

        (value1, value2)
    } else {
        let value = s.trim().parse()?;
        (value, value)
    };
    ensure!(
        start <= end,
        "Start value must be less than or equal to end value"
    );
    ensure!(
        start >= *limits.start(),
        "Start value must be >= {}",
        limits.start()
    );
    ensure!(
        end <= *limits.end(),
        "End value must be <= {}",
        limits.end()
    );

    Ok(start..=end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("1,2", ",", Some(("1","2")))]
    #[case("hello world", " ", Some(("hello", "world")))]
    #[case("a..b", "..", Some(("a","b")))]
    #[case("a", "", Some(("", "a")))]
    #[case("", "", Some(("", "")))]
    #[case("a..b", "c", None)]
    #[case("🙂😐😞", "😐", Some(("🙂", "😞")))]
    fn partition_works(
        #[case] input: &str,
        #[case] delim: &str,
        #[case] expected: Option<(&str, &str)>,
    ) {
        assert_eq!(partition(input, delim), expected);
    }

    #[rstest]
    #[case("1..2", 1..=2)]
    #[case("1..1", 1..=1)]
    #[case("..2", 0..=2)]
    #[case("1..", 1..=100)]
    fn parse_range_ok(#[case] input: &str, #[case] expected: RangeInclusive<i32>) {
        assert_eq!(parse_range(input, 0..=100).unwrap(), expected);
    }

    #[rstest]
    #[case("..")] // can't omit start and end
    #[case("-1..10")] // start out of range
    #[case("0..101")] // end out of range
    #[case("2..1")] // start greater than end
    fn parse_range_error(#[case] input: &str) {
        parse_range(input, 0..=100).unwrap_err();
    }
}
