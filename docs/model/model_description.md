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
It does this for each milestone time period within a user-defined time horizon.
This is a "partial equilibrium" in the sense that the framework equilibrates only the user-defined
commodities, as opposed to a whole economy.

MUSE2 is data-driven in the sense that model processing and data are entirely independent, and
user-defined data is at the heart of how the model behaves. It is also "bottom-up" in nature, which
means that it requires users to characterise each individual process that produces or consumes each
commodity, along with a range of other physical, economic and agent parameters.

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

The model framework is designed to operate sequentially across several distinct milestone years
(MSY). For each MSY, it endogenously determines asset decommissioning (both scheduled and
economically-driven) and guides new capacity investments. The overarching objective is to simulate
agent decision-making to serve commodity (and service) demand.

A fundamental premise for the investment appraisal is that prices for balanced commodities (Supply
Equals Demand: SED) from the previous milestone year (\\( \pi_{prevMSY} \\)) are considered reliable
for economic evaluations. Service demand commodity (SVD) prices from \\( MSY_{prev} \\) may or may
not be reliable (as defined by the user), guiding the choice of appraisal method for assets
producing them. It is designed as a recursive dynamic model with imperfect foresight.

The workflow is structured as follows:

1. **Dispatch is executed for a calibrated base year.** Dispatch is executed using the formulation
   shown in [Dispatch Optimisation Formulation]. All existing assets are included, and all candidate
   assets for the \\( MSY_{next} \\) are included with capacities set to zero. This ensures that all
   commodity shadow prices and reduced costs for candidate asset are generated for use in the \\(
   MSY_{next} \\). \\( VoLL \\) load shedding variables should be zero after completion, and if they
   are non-zero then throw an error as the model is not properly calibrated.

2. **Time-travel loop:** Move to the next milestone year (\\( MSY \\)).

   1. **Decommission assets that reached their end of life.** This establishes the existing asset
      fleet for the current \\( MSY \\) by accounting for initial retirements. \\( ExistingCapacity
      \\) is the set of existing assets and their capacities available after EOL decommissioning.

   2. **Determine SVD demand profiles and run investment appraisal tools for them.** SVD demand
      profiles for the \\( MSY \\) are determined from user input data. The [investment appraisal
      tools] are applied to determine portfolios of existing and new assets to meet demand for each
      SVD commodity. Prices of input commodities are known from the final dispatch of the previous
      milestone year. This finalises \\( NewCapacity \\) and \\( AssetChosen \\) for each SVD.

   3. **Build System Layer-by-Layer loop: Completes the investment pass for the milestone year,
      progressively adding commodities layer by layer.** This step determines new asset capacities
      (\\( NewCapacity \\)) and selects existing assets that remain competitive (\\( AssetChosen
      \\)). Other existing assets are decommissioned if they are not utilised for \\( MothballYears
      \\) years (a user-input asset parameter). This inner loop continues until no different SED
      commodities (that have not been processed using the investment appraisal tools) are added as
      commodities of interest for this iteration.

      1. **Determine the commodities of interest for investment.** In the base year this is all
         service demand (SVD) commodities. After the base year, the commodities of interest are
         determined dynamically; they are the set of commodities that are consumed by the assets
         invested/chosen in the last iteration (layer) of this loop.

      2. **Dispatch to determine commodity of interest demand profile.** Dispatch is executed using
         the formulation shown in [Dispatch Optimisation Formulation], but only including system
         elements downstream of the commodities of interest. Commodity prices for upstream/unknown
         inputs/outputs from assets serving the commodities of interest and assets downstream of the
         commodities of interest with unknown commodity prices (if any) are assumed to take on
         commodity prices from the previous MSY. Care must be taken to avoid any double-counting of
         prices and e.g. commodity levies. Demand profiles for commodities of interest are recorded
         (\\( D[c,r,t] \\)).

      3. **Run investment appraisal tools for each commodity of interest.** The [investment
         appraisal tools] are applied to determine portfolios of existing and new assets to meet
         demand for each commodity of interest. It is necessary to consider the complete demand
         profile of each commodity of interest, as even where demand can be served with existing
         assets in the MSY without new investment, economic decommissioning is still possible.

      4. **Finalise new capacity and retained assets that produce the commodities of interest.** \\(
         NewCapacity \\) and \\( AssetChosen \\) are finalised for the layer.

      5. **Check if there are SED commodities that the investment appraisal tools have not been run
         for.** If yes, move to next layer of the layering loop at step 2(c). If no, **layering loop
         ends**, break and continue at step 2(d).

   4. **Ironing-out loop**, with iteration limit \\( k_{max} \\). For each \\( k \\):

      1. Execute dispatch as per [Dispatch Optimisation Formulation] with the complete system, with
         all candidate assets for the \\( MSY_{next} \\) included with capacities set to zero to
         generate prices and reduced costs for the \\( MSY_{next} \\).

      2. Check if load-weighted average prices for any SED commodity has changed (or changed more
         than a tolerance) since the last loop (also, possibly check if the 95th percentile of price
         has changed more than a tolerance). If yes, continue at 2(d)iii. If no, this MSY is
         complete, and if further MSY exist continue time-travel loop from step 2, or if no further
         MSY exist then go to step 3.

      3. If \\( k = k_{max} \\) break with a warning telling the user that this loop did not
         converge, identifying out-of-balance commodities. If further MSY exist continue time-travel
         loop from step 2, or if no further MSY exist then go to step 3.

      4. Re-run investment appraisal tools for the assets and commodities that are contributing to
         the price instability.

3. **Outer loop ends when no further milestone years exist.**

[investment appraisal tools]: ./investment.md#tools
[Dispatch Optimisation Formulation]: ./dispatch_optimisation.md
