//! Code for creating sets of markets.
use crate::commodity::CommodityID;
use crate::process::ProcessID;
use crate::region::RegionID;
use indexmap::IndexSet;
use itertools::Itertools;
use std::fmt::Display;

/// Represents a set of markets which are invested in together.
#[derive(PartialEq, Debug, Clone, Eq)]
pub enum MarketSet {
    /// Assets are selected for a single market using
    /// [`crate::simulation::orchestration::run_agents_for_single_market`].
    Single((CommodityID, RegionID)),
    /// Assets are selected for a group of markets which forms a cycle using
    /// [`crate::simulation::orchestration::run_agents_for_cycle`].
    Cycle {
        /// The markets in the cycle, in investment order
        markets: Vec<(CommodityID, RegionID)>,
        /// Producers of cycle markets which consume a market from earlier in the investment order
        feedback_processes: IndexSet<(ProcessID, RegionID)>,
    },
    /// Assets are selected for a layer of independent [`MarketSet`]s
    Layer(Vec<MarketSet>),
}

impl MarketSet {
    /// Recursively iterate over all markets contained in this `MarketSet`.
    pub fn iter_markets<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = &'a (CommodityID, RegionID)> + 'a> {
        match self {
            MarketSet::Single(market) => Box::new(std::iter::once(market)),
            MarketSet::Cycle { markets, .. } => Box::new(markets.iter()),
            MarketSet::Layer(set) => Box::new(set.iter().flat_map(|s| s.iter_markets())),
        }
    }
}

impl Display for MarketSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MarketSet::Single((commodity_id, region_id)) => {
                write!(f, "{commodity_id}|{region_id}")
            }
            MarketSet::Cycle { markets, .. } => {
                write!(
                    f,
                    "({})",
                    markets.iter().map(|(c, r)| format!("{c}|{r}")).join(", ")
                )
            }
            MarketSet::Layer(ids) => {
                write!(f, "[{}]", ids.iter().join(", "))
            }
        }
    }
}
