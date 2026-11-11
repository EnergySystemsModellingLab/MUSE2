<!-- markdownlint-disable MD033 -->
<!-- allow inline html -->
<!-- markdownlint-disable MD028 -->
<!-- allow adjacent block elements -->

# Model Description

The purpose of MUSE2 (**M**od**U**lar energy systems **S**imulation **E**nvironment) is to
provide users with a framework to simulate pathways of energy system transition, usually in the
context of climate change mitigation.

## Model Concept

MUSE2 is a bottom-up engineering-economic modelling framework that computes a price-induced
supply-demand equilibrium on a set of user-defined commodities.
It does this for each milestone time period within a user-defined time horizon (see documentation on
[Time](time.md)).
This is a "partial equilibrium" in the sense that the framework equilibrates only the user-defined
commodities, as opposed to a whole economy.

MUSE2 is data-driven in the sense that model processing and data are entirely independent, and
user-defined data is at the heart of how the model behaves. It is also "bottom-up" in nature, which
means that it requires users to characterise each individual process that produces or consumes each
commodity, along with a range of other physical, economic and agent parameters.

MUSE2 does not require a single set of physical units, but the units used within each model must be
consistent. See [Units and Dimensions](units_and_dimensions.md) for how capacity, activity,
commodity flows, and monetary values are related.

At a high level, the user defines:

1) The overall temporal arrangements, including the base time period, milestone time periods and
   time horizon, and within-period time slice lengths.

2) The service demands for each end-use (e.g. residential heating, steel production), for each
   region, and how that demand is distributed between the user-defined time slices within the year.
   Service demands must be given a value for the base time period and all milestone time periods in
   each region.

3) The existing capacity of each process (i.e. assets) in the base time period, and the year in
   which it was commissioned or will be decommissioned.

4) The techno-economic attributes (e.g. capital cost, operating costs, efficiency, lifetime, input
   and output commodities, etc) of each process. This must include attributes of processes existing
   in the base time period (i.e. assets) and possible future processes that could be adopted in
   future milestone time periods.

5) The agents that choose between processes by applying search spaces, objectives and decision
   rules. Portions of demand for each commodity must be assigned to an agent, and the sum of these
   portions must be one.

## Framework Overview

The model operates sequentially across a series of milestone years (MSYs). For the base year,
existing assets are commissioned and the model establishes the [commodity prices][prices] used as
the starting point for the first investment year. For each subsequent MSY, the model performs the
following steps:

1. **Decommission end-of-life assets.** Assets whose scheduled decommissioning year has been
   reached are removed from the active pool.

2. **Agent investment.** Agents select assets to meet commodity demand for the current MSY.
   Investment decisions follow a pre-computed **investment order** derived from the topology of the
   [commodity graph][commodity-graphs]: markets are processed deepest-first (most downstream to most
   upstream), grouped into **layers** of independent markets at the same depth. As markets are
   settled, the commodity flows of the newly selected assets are used to propagate demand upstream:
   input commodity flows consumed by newly committed assets become demand targets for the markets
   not yet invested in. See [Investment Appraisal][investment] for full details.

3. **Ironing out.** Investment is repeated iteratively with updated prices to resolve any price
   instability introduced by new capacity commitments. The loop terminates when time-slice-weighted
   market prices have converged within `price_tolerance`, or when `max_ironing_out_iterations` is
   reached (both defined in [`model.toml`][model-toml]).

4. **Mothballing and decommissioning.** Previously commissioned assets that were not selected for
   retention are mothballed. Any asset that has remained mothballed for longer than
   `mothball_years` (defined in [`model.toml`][model-toml]) is permanently decommissioned.

5. **Final flows and prices.** The commodity flows and prices for the MSY are calculated for the
   settled asset pool, written to the output files, and carried forward as the price basis for the
   next MSY's investment appraisal.

How commodity flows and prices are obtained in each of these steps is determined by the
[simulation protocol](#simulation-protocols).

## Simulation protocols

The simulation protocol is selected with the `simulation_protocol` setting in
[`model.toml`][model-toml]. If it is not specified, `system_dispatch` is used.

### `system_dispatch`

In this protocol, agents choose capacity and the flows and prices for each year are obtained from a
system-wide [dispatch optimisation][dispatch-optimisation]. It works as follows:

- **Base year:** Dispatch optimisations are run over the existing assets, with and without candidate
  assets, to establish commodity flows and prices.
- **Agent investment:** After all markets in each layer are settled, a **partial system dispatch** is
  run over all assets selected so far. For markets in a cycle, a partial dispatch is run after each
  market is settled. This provides the flows used to propagate demand upstream.
- **Ironing out:** Each iteration runs the full agent investment pass followed by a full system
  dispatch of the selected assets, from which updated prices are calculated.
- **Final dispatch:** A full dispatch optimisation is run over the settled asset pool. The
  resulting flows and commodity prices are written to the output files.

The `value_of_lost_load`, `commodity_balance_epsilon` and `highs.dispatch_options` settings in
[`model.toml`][model-toml] are only used by this protocol.

[investment]: ./investment.md
[commodity-graphs]: ./commodity_graphs.md
[dispatch-optimisation]: ./dispatch_optimisation.md
[prices]: ./prices.md
[model-toml]: ../file_formats/input_files.md#model-parameters-modeltoml
