//! Code for working with time slices.
//!
//! Time slices provide a mechanism for users to indicate production etc. varies with the time of
//! day and time of year.
use crate::id::{IDCollection, define_id_type};
use crate::units::{Dimensionless, Year};
use anyhow::{Context, Result};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use serde::de::Error;
use serde::{Deserialize, Serialize};
use serde_string_enum::DeserializeLabeledStringEnum;
use std::fmt::Display;
use std::iter;

define_id_type! {Season}
define_id_type! {TimeOfDay}

/// An ID describing season and time of day
#[derive(Hash, Eq, PartialEq, Ord, PartialOrd, Clone, Debug)]
pub struct TimeSliceID {
    /// The name of each season.
    pub season: Season,
    /// The name of each time slice within a day.
    pub time_of_day: TimeOfDay,
}

/// Only implement for tests as this is a bit of a footgun
#[cfg(test)]
impl From<&str> for TimeSliceID {
    fn from(value: &str) -> Self {
        let (season, time_of_day) = value
            .split('.')
            .collect_tuple()
            .expect("Time slice not in form season.time_of_day");
        TimeSliceID {
            season: season.into(),
            time_of_day: time_of_day.into(),
        }
    }
}

impl Display for TimeSliceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.season, self.time_of_day)
    }
}

impl<'de> Deserialize<'de> for TimeSliceID {
    fn deserialize<D>(deserialiser: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserialiser)?;
        let (season, time_of_day) = s.split('.').collect_tuple().ok_or_else(|| {
            D::Error::custom(format!(
                "Invalid input '{s}': Should be in form season.time_of_day"
            ))
        })?;
        Ok(Self {
            season: season.into(),
            time_of_day: time_of_day.into(),
        })
    }
}

impl Serialize for TimeSliceID {
    fn serialize<S>(&self, serialiser: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serialiser.collect_str(self)
    }
}

/// Represents a time slice read from an input file, which can be all
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum TimeSliceSelection {
    /// All year and all day
    Annual,
    /// Only applies to one season
    Season(Season),
    /// Only applies to a single time slice
    Single(TimeSliceID),
}

impl TimeSliceSelection {
    /// The [`TimeSliceLevel`] to which this [`TimeSliceSelection`] corresponds
    pub fn level(&self) -> TimeSliceLevel {
        match self {
            Self::Annual => TimeSliceLevel::Annual,
            Self::Season(_) => TimeSliceLevel::Season,
            Self::Single(_) => TimeSliceLevel::DayNight,
        }
    }

    /// Iterate over the subset of time slices in this selection
    pub fn iter<'a>(
        &'a self,
        time_slice_info: &'a TimeSliceInfo,
    ) -> Box<dyn Iterator<Item = (&'a TimeSliceID, Year)> + 'a> {
        let ts_info = time_slice_info;
        match self {
            Self::Annual => Box::new(ts_info.iter()),
            Self::Season(season) => {
                Box::new(ts_info.iter().filter(move |(ts, _)| ts.season == *season))
            }
            Self::Single(ts) => Box::new(iter::once((ts, ts_info.time_slices[ts]))),
        }
    }

    /// Iterate over this [`TimeSliceSelection`] at the specified level.
    ///
    /// For example, this allows you to iterate over a [`TimeSliceSelection::Season`] at the level
    /// of either seasons (in which case, the iterator will just contain the season) or time slices
    /// (in which case it will contain all time slices for that season).
    ///
    /// Note that you cannot iterate over a [`TimeSliceSelection`] with coarser temporal granularity
    /// than the [`TimeSliceSelection`] itself (for example, you cannot iterate over a
    /// [`TimeSliceSelection::Season`] at the [`TimeSliceLevel::Annual`] level). In this case, the
    /// function will return `None`.
    pub fn iter_at_level<'a>(
        &'a self,
        time_slice_info: &'a TimeSliceInfo,
        level: TimeSliceLevel,
    ) -> Option<Box<dyn Iterator<Item = (Self, Year)> + 'a>> {
        if level > self.level() {
            return None;
        }

        let ts_info = time_slice_info;
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Annual => match level {
                TimeSliceLevel::Annual => Box::new(iter::once((Self::Annual, Year(1.0)))),
                TimeSliceLevel::Season => Box::new(
                    ts_info
                        .seasons
                        .iter()
                        .map(|(season, duration)| (season.clone().into(), *duration)),
                ),
                TimeSliceLevel::DayNight => Box::new(
                    ts_info
                        .time_slices
                        .iter()
                        .map(|(ts, duration)| (ts.clone().into(), *duration)),
                ),
            },
            Self::Season(season) => match level {
                TimeSliceLevel::Season => {
                    Box::new(iter::once((self.clone(), ts_info.seasons[season])))
                }
                TimeSliceLevel::DayNight => Box::new(
                    ts_info
                        .time_slices
                        .iter()
                        .filter(move |(ts, _)| &ts.season == season)
                        .map(|(ts, duration)| (ts.clone().into(), *duration)),
                ),
                TimeSliceLevel::Annual => unreachable!(),
            },
            Self::Single(time_slice) => Box::new(iter::once((
                time_slice.clone().into(),
                ts_info.time_slices[time_slice],
            ))),
        };

        Some(iter)
    }
}

impl From<TimeSliceID> for TimeSliceSelection {
    fn from(value: TimeSliceID) -> Self {
        Self::Single(value)
    }
}

impl From<Season> for TimeSliceSelection {
    fn from(value: Season) -> Self {
        Self::Season(value)
    }
}

impl Display for TimeSliceSelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Annual => write!(f, "annual"),
            Self::Season(season) => write!(f, "{season}"),
            Self::Single(ts) => write!(f, "{ts}"),
        }
    }
}

/// The time granularity for a particular operation
#[derive(
    PartialEq, PartialOrd, Copy, Clone, Debug, DeserializeLabeledStringEnum, strum::EnumIter,
)]
pub enum TimeSliceLevel {
    /// Treat individual time slices separately
    #[string = "daynight"]
    DayNight,
    /// Whole seasons
    #[string = "season"]
    Season,
    /// The whole year
    #[string = "annual"]
    Annual,
}

/// Information about the time slices in the simulation, including names and durations
#[derive(PartialEq, Debug)]
pub struct TimeSliceInfo {
    /// Names of times of day (e.g. "evening")
    pub times_of_day: IndexSet<TimeOfDay>,
    /// Names and fraction of year occupied by each season
    pub seasons: IndexMap<Season, Year>,
    /// The fraction of the year that this combination of season and time of day occupies
    pub time_slices: IndexMap<TimeSliceID, Year>,
}

impl Default for TimeSliceInfo {
    /// The default `TimeSliceInfo` is a single time slice covering the whole year
    fn default() -> Self {
        let id = TimeSliceID {
            season: "all-year".into(),
            time_of_day: "all-day".into(),
        };
        let time_slices = [(id.clone(), Year(1.0))].into_iter().collect();

        Self {
            seasons: iter::once((id.season, Year(1.0))).collect(),
            times_of_day: iter::once(id.time_of_day).collect(),
            time_slices,
        }
    }
}

impl TimeSliceInfo {
    #[allow(clippy::doc_markdown)]
    /// Get the `TimeSliceID` corresponding to the `time_slice`.
    ///
    /// `time_slice` must be in the form "season.time_of_day".
    pub fn get_time_slice_id_from_str(&self, time_slice: &str) -> Result<TimeSliceID> {
        let (season, time_of_day) = time_slice
            .split('.')
            .collect_tuple()
            .context("Time slice must be in the form season.time_of_day")?;
        let season = self
            .seasons
            .get_id(season)
            .with_context(|| format!("{season} is not a known season"))?;
        let time_of_day = self
            .times_of_day
            .get_id(time_of_day)
            .with_context(|| format!("{time_of_day} is not a known time of day"))?;

        Ok(TimeSliceID {
            season: season.clone(),
            time_of_day: time_of_day.clone(),
        })
    }

    /// Get a `TimeSliceSelection` from the specified string.
    ///
    /// If the string is empty, the default value is `TimeSliceSelection::Annual`.
    pub fn get_selection(&self, time_slice: &str) -> Result<TimeSliceSelection> {
        if time_slice.eq_ignore_ascii_case("annual") {
            Ok(TimeSliceSelection::Annual)
        } else if time_slice.contains('.') {
            let time_slice = self.get_time_slice_id_from_str(time_slice)?;
            Ok(TimeSliceSelection::Single(time_slice))
        } else {
            let season = self
                .seasons
                .get_id(time_slice)
                .with_context(|| format!("'{time_slice}' is not a valid season"))?
                .clone();
            Ok(TimeSliceSelection::Season(season))
        }
    }

    /// Iterate over all [`TimeSliceID`]s
    pub fn iter_ids(&self) -> indexmap::map::Keys<'_, TimeSliceID, Year> {
        self.time_slices.keys()
    }

    /// Iterate over all seasons
    pub fn iter_seasons(&self) -> indexmap::map::Keys<'_, Season, Year> {
        self.seasons.keys()
    }

    /// Iterate over all time slices
    pub fn iter(&self) -> impl Iterator<Item = (&TimeSliceID, Year)> {
        self.time_slices
            .iter()
            .map(|(ts, duration)| (ts, *duration))
    }

    /// Iterate over the different time slice selections for a given time slice level.
    ///
    /// For example, if [`TimeSliceLevel::Season`] is specified, this function will return an
    /// iterator of [`TimeSliceSelection`]s covering each season.
    pub fn iter_selections_at_level(
        &self,
        level: TimeSliceLevel,
    ) -> Box<dyn Iterator<Item = TimeSliceSelection> + '_> {
        match level {
            TimeSliceLevel::Annual => Box::new(iter::once(TimeSliceSelection::Annual)),
            TimeSliceLevel::Season => {
                Box::new(self.seasons.keys().cloned().map(TimeSliceSelection::Season))
            }
            TimeSliceLevel::DayNight => {
                Box::new(self.iter_ids().cloned().map(TimeSliceSelection::Single))
            }
        }
    }

    /// Iterate over a subset of time slices calculating the relative duration of each.
    ///
    /// The relative duration is specified as a fraction of the total time covered by `selection`.
    ///
    /// # Arguments
    ///
    /// * `selection` - A subset of time slices
    ///
    /// # Returns
    ///
    /// An iterator of time slices along with the fraction of the total selection.
    pub fn iter_selection_share<'a>(
        &'a self,
        selection: &'a TimeSliceSelection,
        level: TimeSliceLevel,
    ) -> Option<impl Iterator<Item = (TimeSliceSelection, Dimensionless)> + use<>> {
        // Store selections as we have to iterate twice
        let selections = selection.iter_at_level(self, level)?.collect_vec();

        // Total duration covered by selection
        let total_duration: Year = selections.iter().map(|(_, duration)| *duration).sum();

        // Calculate share
        let iter = selections
            .into_iter()
            .map(move |(selection, duration)| (selection, duration / total_duration));
        Some(iter)
    }

    /// Calculate the total length of a selection of time slices.
    pub fn length_for_selection(&self, selection: &TimeSliceSelection) -> Result<Year> {
        let length: Year = selection.iter(self).map(|(_, duration)| duration).sum();
        Ok(length)
    }

    /// Share a value between a subset of time slices in proportion to their lengths.
    ///
    /// For instance, you could use this function to compute how demand is distributed between the
    /// different time slices of winter.
    ///
    /// # Arguments
    ///
    /// * `selection` - A subset of time slices
    /// * `value` - The value to be shared between the time slices
    ///
    /// # Returns
    ///
    /// An iterator of time slices along with a fraction of `value`.
    pub fn calculate_share<'a>(
        &'a self,
        selection: &'a TimeSliceSelection,
        level: TimeSliceLevel,
        value: Dimensionless,
    ) -> Option<impl Iterator<Item = (TimeSliceSelection, Dimensionless)> + use<>> {
        let iter = self
            .iter_selection_share(selection, level)?
            .map(move |(selection, share)| (selection, value * share));
        Some(iter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::units::UnitType;
    use itertools::assert_equal;
    use rstest::{fixture, rstest};

    #[fixture]
    fn time_slices1() -> [TimeSliceID; 2] {
        [
            TimeSliceID {
                season: "winter".into(),
                time_of_day: "day".into(),
            },
            TimeSliceID {
                season: "summer".into(),
                time_of_day: "night".into(),
            },
        ]
    }

    #[fixture]
    fn time_slice_info1(time_slices1: [TimeSliceID; 2]) -> TimeSliceInfo {
        TimeSliceInfo {
            seasons: [("winter".into(), Year(0.5)), ("summer".into(), Year(0.5))]
                .into_iter()
                .collect(),
            times_of_day: ["day".into(), "night".into()].into_iter().collect(),
            time_slices: time_slices1.map(|ts| (ts, Year(0.5))).into_iter().collect(),
        }
    }

    #[fixture]
    fn time_slice_info2() -> TimeSliceInfo {
        let time_slices = [
            TimeSliceID {
                season: "winter".into(),
                time_of_day: "day".into(),
            },
            TimeSliceID {
                season: "winter".into(),
                time_of_day: "night".into(),
            },
            TimeSliceID {
                season: "summer".into(),
                time_of_day: "day".into(),
            },
            TimeSliceID {
                season: "summer".into(),
                time_of_day: "night".into(),
            },
        ];
        TimeSliceInfo {
            times_of_day: ["day".into(), "night".into()].into_iter().collect(),
            seasons: [("winter".into(), Year(0.5)), ("summer".into(), Year(0.5))]
                .into_iter()
                .collect(),
            time_slices: time_slices
                .iter()
                .map(|ts| (ts.clone(), Year(0.25)))
                .collect(),
        }
    }

    #[rstest]
    fn test_ts_selection_iter_annual(
        time_slice_info1: TimeSliceInfo,
        time_slices1: [TimeSliceID; 2],
    ) {
        assert_equal(
            TimeSliceSelection::Annual.iter(&time_slice_info1),
            time_slices1.iter().map(|ts| (ts, Year(0.5))),
        );
    }

    #[rstest]
    fn test_ts_selection_iter_season(
        time_slice_info1: TimeSliceInfo,
        time_slices1: [TimeSliceID; 2],
    ) {
        assert_equal(
            TimeSliceSelection::Season("winter".into()).iter(&time_slice_info1),
            iter::once((&time_slices1[0], Year(0.5))),
        );
    }

    #[rstest]
    fn test_ts_selection_iter_single(
        time_slice_info1: TimeSliceInfo,
        time_slices1: [TimeSliceID; 2],
    ) {
        let ts = time_slice_info1
            .get_time_slice_id_from_str("summer.night")
            .unwrap();
        assert_equal(
            TimeSliceSelection::Single(ts).iter(&time_slice_info1),
            iter::once((&time_slices1[1], Year(0.5))),
        );
    }

    fn assert_selection_equal<I, T>(actual: Option<I>, expected: Option<Vec<(&str, T)>>)
    where
        T: UnitType,
        I: Iterator<Item = (TimeSliceSelection, T)>,
    {
        let Some(actual) = actual else {
            assert!(expected.is_none());
            return;
        };

        let ts_info = time_slice_info2();
        let expected = expected
            .unwrap()
            .into_iter()
            .map(move |(sel, frac)| (ts_info.get_selection(sel).unwrap(), frac));
        assert_equal(actual, expected);
    }

    #[rstest]
    #[case(TimeSliceSelection::Annual, TimeSliceLevel::Annual, Some(vec![("annual", Year(1.0))]))]
    #[case(TimeSliceSelection::Annual, TimeSliceLevel::Season, Some(vec![("winter", Year(0.5)), ("summer", Year(0.5))]))]
    #[case(TimeSliceSelection::Annual, TimeSliceLevel::DayNight,
           Some(vec![("winter.day", Year(0.25)), ("winter.night", Year(0.25)), ("summer.day", Year(0.25)), ("summer.night", Year(0.25))]))]
    #[case(TimeSliceSelection::Season("winter".into()), TimeSliceLevel::Annual, None)]
    #[case(TimeSliceSelection::Season("winter".into()), TimeSliceLevel::Season, Some(vec![("winter", Year(0.5))]))]
    #[case(TimeSliceSelection::Season("winter".into()), TimeSliceLevel::DayNight,
           Some(vec![("winter.day", Year(0.25)), ("winter.night", Year(0.25))]))]
    #[case(TimeSliceSelection::Single("winter.day".into()), TimeSliceLevel::Annual, None)]
    #[case(TimeSliceSelection::Single("winter.day".into()), TimeSliceLevel::Season, None)]
    #[case(TimeSliceSelection::Single("winter.day".into()), TimeSliceLevel::DayNight, Some(vec![("winter.day", Year(0.25))]))]
    fn test_ts_selection_iter_at_level(
        time_slice_info2: TimeSliceInfo,
        #[case] selection: TimeSliceSelection,
        #[case] level: TimeSliceLevel,
        #[case] expected: Option<Vec<(&str, Year)>>,
    ) {
        let actual = selection.iter_at_level(&time_slice_info2, level);
        assert_selection_equal(actual, expected);
    }

    #[rstest]
    #[case(TimeSliceSelection::Annual, TimeSliceLevel::Annual, Some(vec![("annual", Dimensionless(8.0))]))]
    #[case(TimeSliceSelection::Annual, TimeSliceLevel::Season, Some(vec![("winter", Dimensionless(4.0)), ("summer", Dimensionless(4.0))]))]
    #[case(TimeSliceSelection::Annual, TimeSliceLevel::DayNight,
           Some(vec![("winter.day", Dimensionless(2.0)), ("winter.night", Dimensionless(2.0)), ("summer.day", Dimensionless(2.0)), ("summer.night", Dimensionless(2.0))]))]
    #[case(TimeSliceSelection::Season("winter".into()), TimeSliceLevel::Annual, None)]
    #[case(TimeSliceSelection::Season("winter".into()), TimeSliceLevel::Season, Some(vec![("winter", Dimensionless(8.0))]))]
    #[case(TimeSliceSelection::Season("winter".into()), TimeSliceLevel::DayNight,
           Some(vec![("winter.day", Dimensionless(4.0)), ("winter.night", Dimensionless(4.0))]))]
    #[case(TimeSliceSelection::Single("winter.day".into()), TimeSliceLevel::Annual, None)]
    #[case(TimeSliceSelection::Single("winter.day".into()), TimeSliceLevel::Season, None)]
    #[case(TimeSliceSelection::Single("winter.day".into()), TimeSliceLevel::DayNight, Some(vec![("winter.day", Dimensionless(8.0))]))]
    fn test_calculate_share(
        time_slice_info2: TimeSliceInfo,
        #[case] selection: TimeSliceSelection,
        #[case] level: TimeSliceLevel,
        #[case] expected: Option<Vec<(&str, Dimensionless)>>,
    ) {
        let actual = time_slice_info2.calculate_share(&selection, level, Dimensionless(8.0));
        assert_selection_equal(actual, expected);
    }
}
