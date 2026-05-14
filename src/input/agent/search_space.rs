//! Code for reading agent search space data from a CSV file.
use super::super::{input_err_msg, read_csv_optional, try_insert};
use crate::agent::{Agent, AgentID, AgentMap, AgentSearchSpaceMap};
use crate::commodity::CommodityID;
use crate::id::IDCollection;
use crate::process::{Process, ProcessMap};
use crate::year::parse_year_str;
use anyhow::{Context, Result, ensure};
use itertools::Itertools;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

const AGENT_SEARCH_SPACE_FILE_NAME: &str = "agent_search_space.csv";

type ProducersMap = HashMap<(AgentID, CommodityID, u32), Rc<Vec<Rc<Process>>>>;

#[derive(PartialEq, Debug, Deserialize)]
struct SearchSpaceEntry {
    /// The agent to which this search space applies
    agent_id: String,
    /// The commodity to which this search space applies
    commodity_id: String,
    /// The year(s) to which the search space applies
    years: String,
    /// The processes that the agent will consider investing in.
    ///
    /// This can be process IDs separated by semicolons or "all".
    search_space: String,
}

/// Add a new entry to the search space map.
///
/// # Returns
///
/// Returns an error if the entry is invalid or there is already an existing entry for the same
/// key in `map`.
fn add_entry_to_search_space_map(
    entry: &SearchSpaceEntry,
    agents: &AgentMap,
    processes: &ProcessMap,
    commodity_ids: &HashSet<CommodityID>,
    milestone_years: &[u32],
    producers: &ProducersMap,
    map: &mut HashMap<AgentID, AgentSearchSpaceMap>,
) -> Result<()> {
    let (agent_id, agent) = agents
        .get_key_value(entry.agent_id.as_str())
        .with_context(|| format!("Invalid agent ID '{}'", &entry.agent_id))?;
    let commodity_id = commodity_ids.get_id(&entry.commodity_id)?;
    let years = parse_year_str(&entry.years, milestone_years)?;
    ensure!(
        years.iter().all(|year| agent
            .commodity_portions
            .contains_key(&(commodity_id.clone(), *year))),
        "Agent '{agent_id}' is not responsible for commodity '{commodity_id}' in at least some of \
        the specified years: {years:?}",
    );

    let map = map.entry(agent.id.clone()).or_default();
    for_each_year_in_search_space(
        &entry.search_space,
        agent_id,
        commodity_id,
        &years,
        processes,
        producers,
        |commodity_id, year, search_space| {
            try_insert(map, &(commodity_id, year), search_space)
                .context("Overlapping entries in search space file")
        },
    )?;

    Ok(())
}

/// Parse the search space string and iterate over the processed search space for each year
fn for_each_year_in_search_space<F>(
    search_space: &str,
    agent_id: &AgentID,
    commodity_id: &CommodityID,
    years: &[u32],
    processes: &ProcessMap,
    producers: &ProducersMap,
    mut f: F,
) -> Result<()>
where
    F: FnMut(CommodityID, u32, Rc<Vec<Rc<Process>>>) -> Result<()>,
{
    ensure!(!search_space.is_empty(), "No processes provided");

    if search_space.eq_ignore_ascii_case("all") {
        // Iterate over all possible producers for each year
        for &year in years {
            let search_space = &producers[&(agent_id.clone(), commodity_id.clone(), year)];
            f(commodity_id.clone(), year, search_space.clone())?;
        }
    } else {
        // Check each process ID in turn
        let search_space: Rc<Vec<_>> = Rc::new(search_space
            .split(';')
            .map(|process_id_str| {
                let process = processes
                    .get(process_id_str.trim())
                    .with_context(|| format!("Invalid process ID '{process_id_str}'"))?;

                // Check that the specified process is a possibility for all specified years
                for &year in years {
                    let producers = &producers[&(agent_id.clone(), commodity_id.clone(), year)];
                    ensure!(
                        producers.iter().any(|producer| producer.id == process.id),
                        "Process '{}' does not produce commodity '{commodity_id}' in year {year} \
                        in any of the valid regions for agent '{agent_id}'",
                        &process.id
                    );
                }

                Ok(process.clone())
            })
            .try_collect()?);

        for &year in years {
            f(commodity_id.clone(), year, search_space.clone())?;
        }
    }

    Ok(())
}

/// Read agent search space info from the `agent_search_spaces.csv` file.
///
/// # Arguments
///
/// * `model_dir` - Folder containing model configuration files
/// * `agents` - Map of agents
/// * `processes` - Map of processes
/// * `commodity_ids` - All possible valid commodity IDs
/// * `milestone_years` - The milestone years of the simulation
///
/// # Returns
///
/// A `HashMap` mapping `AgentID` to `AgentSearchSpaceMap`.
pub fn read_agent_search_space(
    model_dir: &Path,
    agents: &AgentMap,
    processes: &ProcessMap,
    commodity_ids: &HashSet<CommodityID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentSearchSpaceMap>> {
    let file_path = model_dir.join(AGENT_SEARCH_SPACE_FILE_NAME);
    let iter = read_csv_optional::<SearchSpaceEntry>(&file_path)?;
    read_agent_search_space_from_iter(iter, agents, processes, commodity_ids, milestone_years)
        .with_context(|| input_err_msg(&file_path))
}

fn read_agent_search_space_from_iter<I>(
    iter: I,
    agents: &AgentMap,
    processes: &ProcessMap,
    commodity_ids: &HashSet<CommodityID>,
    milestone_years: &[u32],
) -> Result<HashMap<AgentID, AgentSearchSpaceMap>>
where
    I: Iterator<Item = SearchSpaceEntry>,
{
    let producers = get_producers_map(agents, processes);
    let mut search_spaces = HashMap::new();
    for entry in iter {
        add_entry_to_search_space_map(
            &entry,
            agents,
            processes,
            commodity_ids,
            milestone_years,
            &producers,
            &mut search_spaces,
        )?;
    }

    for (agent_id, agent) in agents {
        // Get or create search space map
        let search_space = search_spaces
            .entry(agent_id.clone())
            .or_insert_with(AgentSearchSpaceMap::new);

        // Add missing entries for commodities/years
        fill_missing_search_space_entries(agent, &producers, search_space);
    }

    Ok(search_spaces)
}

/// Fill missing entries for the search space map for all commodities/milestone years.
///
/// The entries are filled will all producers of the given commodity in the given year. Only
/// producers which operate in at least one of the same regions as the agent are considered.
fn fill_missing_search_space_entries(
    agent: &Agent,
    producers: &ProducersMap,
    search_space: &mut AgentSearchSpaceMap,
) {
    // Agents all have commodity portions and this field should have been assigned already
    assert!(!agent.commodity_portions.is_empty());

    for (commodity_id, year) in agent.commodity_portions.keys() {
        let key = (commodity_id.clone(), *year);
        search_space
            .entry(key)
            .or_insert_with(|| producers[&(agent.id.clone(), commodity_id.clone(), *year)].clone());
    }
}

/// Get a map of all the producers for each agent, for each commodity and year combination
fn get_producers_map(agents: &AgentMap, processes: &ProcessMap) -> ProducersMap {
    let mut map = HashMap::new();
    for (agent_id, agent) in agents {
        for (commodity_id, year) in agent.commodity_portions.keys() {
            let producers = processes
                .values()
                .filter(move |process| {
                    process.active_for_year(*year)
                        && process.primary_output.as_ref() == Some(commodity_id)
                        && !process.regions.is_disjoint(&agent.regions)
                })
                .cloned()
                .collect_vec();

            try_insert(
                &mut map,
                &(agent_id.clone(), commodity_id.clone(), *year),
                Rc::new(producers),
            )
            .expect("Unexpected duplicate element");
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        fixture::{
            agent_id, assert_error, assert_patched_runs_ok_simple,
            assert_validate_fails_with_simple, commodity_id, process, processes,
        },
        patch::FilePatch,
    };
    use indexmap::indexmap;
    use rstest::{fixture, rstest};

    #[fixture]
    fn process1(process: Process) -> Rc<Process> {
        Rc::new(process)
    }

    #[fixture]
    fn process2(process: Process) -> Rc<Process> {
        Rc::new(Process {
            id: "process2".into(),
            ..process
        })
    }

    #[rstest]
    fn empty_search_space_returns_error(agent_id: AgentID, commodity_id: CommodityID) {
        let result = for_each_year_in_search_space(
            "",
            &agent_id,
            &commodity_id,
            &[2020],
            &ProcessMap::new(),
            &ProducersMap::new(),
            |_, _, _| Ok(()),
        );
        assert_error!(result, "No processes provided");
    }

    #[rstest]
    #[case("all")]
    #[case("ALL")]
    #[case("All")]
    fn all_is_case_insensitive(
        #[case] search_space: &str,
        agent_id: AgentID,
        commodity_id: CommodityID,
        process: Process,
    ) {
        let mut producers = ProducersMap::new();
        producers.insert(
            (agent_id.clone(), commodity_id.clone(), 2020),
            Rc::new(vec![Rc::new(process)]),
        );
        let result = for_each_year_in_search_space(
            search_space,
            &agent_id,
            &commodity_id,
            &[2020],
            &ProcessMap::new(),
            &producers,
            |_, _, _| Ok(()),
        );
        result.unwrap();
    }

    #[rstest]
    fn all_calls_f_for_each_year(
        agent_id: AgentID,
        commodity_id: CommodityID,
        process1: Rc<Process>,
        process2: Rc<Process>,
    ) {
        let mut producers = ProducersMap::new();
        producers.insert(
            (agent_id.clone(), commodity_id.clone(), 2020),
            Rc::new(vec![process1.clone()]),
        );
        producers.insert(
            (agent_id.clone(), commodity_id.clone(), 2030),
            Rc::new(vec![process2.clone()]),
        );
        let mut calls: Vec<(u32, Rc<Vec<Rc<Process>>>)> = Vec::new();
        for_each_year_in_search_space(
            "all",
            &agent_id,
            &commodity_id,
            &[2020, 2030],
            &ProcessMap::new(),
            &producers,
            |_, year, search_space| {
                calls.push((year, search_space));
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].0, 2020);
        assert_eq!(calls[0].1.len(), 1);
        assert_eq!(calls[0].1[0].id, process1.id);
        assert_eq!(calls[1].0, 2030);
        assert_eq!(calls[1].1.len(), 1);
        assert_eq!(calls[1].1[0].id, process2.id);
    }

    #[rstest]
    fn specific_process_calls_f_for_each_year(
        agent_id: AgentID,
        commodity_id: CommodityID,
        processes: ProcessMap,
    ) {
        let process = processes.values().next().unwrap().clone();
        let mut producers = ProducersMap::new();
        producers.insert(
            (agent_id.clone(), commodity_id.clone(), 2020),
            Rc::new(vec![process.clone()]),
        );
        producers.insert(
            (agent_id.clone(), commodity_id.clone(), 2030),
            Rc::new(vec![process.clone()]),
        );
        let mut calls: Vec<(u32, Rc<Vec<Rc<Process>>>)> = Vec::new();
        for_each_year_in_search_space(
            "process1",
            &agent_id,
            &commodity_id,
            &[2020, 2030],
            &processes,
            &producers,
            |_, year, search_space| {
                calls.push((year, search_space));
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].0, 2020);
        assert_eq!(calls[1].0, 2030);
        assert_eq!(calls[0].1.len(), 1);
        assert_eq!(calls[0].1[0].id, process.id);
        // Both years receive the same Rc-wrapped search space
        assert!(Rc::ptr_eq(&calls[0].1, &calls[1].1));
    }

    #[rstest]
    fn multiple_process_ids_calls_f_with_all_processes(
        agent_id: AgentID,
        commodity_id: CommodityID,
        process1: Rc<Process>,
        process2: Rc<Process>,
    ) {
        let mut producers = ProducersMap::new();
        producers.insert(
            (agent_id.clone(), commodity_id.clone(), 2020),
            Rc::new(vec![process1.clone(), process2.clone()]),
        );
        let processes: ProcessMap = indexmap! {
            process1.id.clone() => process1.clone(),
            process2.id.clone() => process2.clone(),
        };
        let mut calls: Vec<(u32, Rc<Vec<Rc<Process>>>)> = Vec::new();
        for_each_year_in_search_space(
            "process1;process2",
            &agent_id,
            &commodity_id,
            &[2020],
            &processes,
            &producers,
            |_, year, search_space| {
                calls.push((year, search_space));
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].1.len(), 2);
        assert_eq!(calls[0].1[0].id, process1.id);
        assert_eq!(calls[0].1[1].id, process2.id);
    }

    #[test]
    fn model_runs_with_search_space_file1() {
        // Check that it runs with everything set to all
        assert_patched_runs_ok_simple!(vec![
            FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                "agent_id,commodity_id,years,search_space",
                "A0_GEX,GASPRD,all,all",
                "A0_GPR,GASNAT,all,all",
                "A0_ELC,ELCTRI,all,all",
                "A0_RES,RSHEAT,all,all",
            ])
        ]);
    }

    #[test]
    fn model_runs_with_search_space_file2() {
        // Check that it runs with a more complex file
        assert_patched_runs_ok_simple!(vec![
            FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                "agent_id,commodity_id,years,search_space",
                "A0_GEX,GASPRD,all,GASDRV",
                "A0_GPR,GASNAT,2020,all",
                "A0_GPR,GASNAT,2030,GASPRC",
                "A0_GPR,GASNAT,2040,all",
                "A0_ELC,ELCTRI,all,all",
                "A0_RES,RSHEAT,2020,RGASBR;RELCHP",
                "A0_RES,RSHEAT,2030,RGASBR ; RELCHP",
                "A0_RES,RSHEAT,2040,RGASBR;RELCHP",
            ])
        ]);
    }

    #[test]
    fn not_responsible_for_commodity() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "A0_GEX,ELCTRI,all,all",
                ]),
            ],
            "Agent 'A0_GEX' is not responsible for commodity 'ELCTRI' in at least some of the \
            specified years: [2020, 2030, 2040]"
        );
    }

    #[test]
    fn unknown_agent_id_fails() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "UNKNOWN_AGENT,GASPRD,all,all",
                ])
            ],
            "Invalid agent ID 'UNKNOWN_AGENT'"
        );
    }

    #[test]
    fn unknown_commodity_id_fails() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "A0_GEX,UNKNOWN_COMMODITY,all,all",
                ])
            ],
            "Unknown ID UNKNOWN_COMMODITY found"
        );
    }

    #[test]
    fn invalid_year_fails() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "A0_GEX,GASPRD,9999,all",
                ])
            ],
            "Invalid year: 9999"
        );
    }

    #[test]
    fn overlapping_entries_fails() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "A0_GEX,GASPRD,2020,all",
                    "A0_GEX,GASPRD,2020,GASDRV",
                ])
            ],
            "Overlapping entries in search space file"
        );
    }

    #[test]
    fn invalid_search_space_fails() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "A0_GEX,GASPRD,all,NONEXISTENT_PROCESS",
                ])
            ],
            "Invalid process ID 'NONEXISTENT_PROCESS'"
        );
    }

    #[test]
    fn process_not_valid_producer_fails() {
        assert_validate_fails_with_simple!(
            vec![
                FilePatch::new(AGENT_SEARCH_SPACE_FILE_NAME).with_replacement(&[
                    "agent_id,commodity_id,years,search_space",
                    "A0_GEX,GASPRD,all,GASPRC",
                ])
            ],
            "Process 'GASPRC' does not produce commodity 'GASPRD' in year 2020 \
            in any of the valid regions for agent 'A0_GEX'"
        );
    }
}
