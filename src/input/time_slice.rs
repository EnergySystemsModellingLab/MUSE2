//! Code for reading time slice info from a CSV file.
use super::{
    IDLike, check_values_sum_to_one_approx, deserialise_proportion_nonzero, input_err_msg, read_csv,
};
use crate::id::IDCollection;
use crate::time_slice::{Season, TimeOfDay, TimeSliceID, TimeSliceInfo};
use crate::units::Year;
use anyhow::{Context, Result, ensure};
use indexmap::{IndexMap, IndexSet};
use serde::Deserialize;
use std::path::Path;

const TIME_SLICES_FILE_NAME: &str = "time_slices.csv";

/// A time slice record retrieved from a CSV file
#[derive(PartialEq, Debug, Deserialize)]
struct TimeSliceRaw {
    season: Season,
    time_of_day: TimeOfDay,
    #[serde(deserialize_with = "deserialise_proportion_nonzero")]
    fraction: Year,
}

/// Get the specified ID from `set` or insert if it doesn't exist.
///
/// The purpose of returning an ID is to deduplicate values and reduce memory usage.
fn get_or_insert<T: IDLike>(id: T, set: &mut IndexSet<T>) -> T {
    // Sadly there's no entry API for HashSets: https://github.com/rust-lang/rfcs/issues/1490
    if let Ok(id) = set.get_id(&id) {
        id.clone()
    } else {
        set.insert(id.clone());
        id
    }
}

/// Read time slice information from an iterator of raw time slice records
fn read_time_slice_info_from_iter<I>(iter: I) -> Result<TimeSliceInfo>
where
    I: Iterator<Item = TimeSliceRaw>,
{
    let mut times_of_day = IndexSet::new();
    let mut seasons = IndexMap::new();
    let mut time_slices = IndexMap::new();
    for time_slice in iter {
        let time_of_day = get_or_insert(time_slice.time_of_day, &mut times_of_day);
        let season = match seasons.entry(time_slice.season) {
            indexmap::map::Entry::Occupied(mut entry) => {
                *entry.get_mut() += time_slice.fraction;
                entry.key().clone()
            }
            indexmap::map::Entry::Vacant(entry) => {
                let key = entry.key().clone();
                entry.insert(time_slice.fraction);
                key
            }
        };
        let id = TimeSliceID {
            season,
            time_of_day,
        };

        ensure!(
            time_slices
                .insert(id.clone(), time_slice.fraction)
                .is_none(),
            "Duplicate time slice entry for {id}",
        );
    }

    // Validate data
    check_values_sum_to_one_approx(time_slices.values().copied())
        .context("Invalid time slice fractions")?;

    Ok(TimeSliceInfo {
        times_of_day,
        seasons,
        time_slices,
    })
}

/// Read time slices from a CSV file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
///
/// # Returns
///
/// This function returns a `TimeSliceInfo` struct or, if the file doesn't exist, a single time
/// slice covering the whole year (see `TimeSliceInfo::default()`).
pub fn read_time_slice_info(model_dir: &Path) -> Result<TimeSliceInfo> {
    let file_path = model_dir.join(TIME_SLICES_FILE_NAME);
    if !file_path.exists() {
        return Ok(TimeSliceInfo::default());
    }

    let time_slices_csv = read_csv(&file_path)?;
    read_time_slice_info_from_iter(time_slices_csv).with_context(|| input_err_msg(file_path))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;
    use std::path::Path;
    use tempfile::tempdir;

    /// Create an example time slices file in dir_path
    fn create_time_slices_file(dir_path: &Path) {
        let file_path = dir_path.join(TIME_SLICES_FILE_NAME);
        let mut file = File::create(file_path).unwrap();
        writeln!(
            file,
            "season,time_of_day,fraction
winter,day,0.25
peak,night,0.25
summer,peak,0.25
autumn,evening,0.25"
        )
        .unwrap();
    }

    #[test]
    fn test_read_time_slice_info() {
        let dir = tempdir().unwrap();
        create_time_slices_file(dir.path());

        let info = read_time_slice_info(dir.path()).unwrap();
        assert_eq!(
            info,
            TimeSliceInfo {
                seasons: [
                    ("winter".into(), Year(0.25)),
                    ("peak".into(), Year(0.25)),
                    ("summer".into(), Year(0.25)),
                    ("autumn".into(), Year(0.25))
                ]
                .into_iter()
                .collect(),
                times_of_day: [
                    "day".into(),
                    "night".into(),
                    "peak".into(),
                    "evening".into()
                ]
                .into_iter()
                .collect(),
                time_slices: [
                    (
                        TimeSliceID {
                            season: "winter".into(),
                            time_of_day: "day".into(),
                        },
                        Year(0.25),
                    ),
                    (
                        TimeSliceID {
                            season: "peak".into(),
                            time_of_day: "night".into(),
                        },
                        Year(0.25),
                    ),
                    (
                        TimeSliceID {
                            season: "summer".into(),
                            time_of_day: "peak".into(),
                        },
                        Year(0.25),
                    ),
                    (
                        TimeSliceID {
                            season: "autumn".into(),
                            time_of_day: "evening".into(),
                        },
                        Year(0.25),
                    ),
                ]
                .into_iter()
                .collect()
            }
        );
    }

    #[test]
    fn test_read_time_slice_info_non_existent() {
        let actual = read_time_slice_info(tempdir().unwrap().path());
        assert_eq!(actual.unwrap(), TimeSliceInfo::default());
    }
}
