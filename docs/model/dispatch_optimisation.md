# Dispatch Optimisation Formulation

<!-- We sometimes need a space after an underscore to make equations render properly -->
<!-- markdownlint-disable MD037 -->

This dispatch optimisation model calculates the least-cost operation of the energy system for a
given configuration of assets and capacities, subject to demands and constraints. It is the core
engine used for each dispatch run referenced in the overall MUSE2 workflow. A key general
assumption is that SVD commodities represent final demands only and are not consumed as inputs by
any asset.

## General Sets

These define the fundamental categories used to define the energy system.

- \\( \mathbf{R} \\): Set of Regions (indexed by \\( r \\)). Represents distinct geographical or
  modelling areas.

- \\( \mathbf{T} \\): Set of Time Slices (indexed by \\( t \\)). Discrete operational periods within
  a year.

- \\( \mathbf{H} \\): Set of Seasons (indexed by \\( h \\)). Collections of time slices.

- \\( \mathbf{A} \\): Set of All Assets (indexed by \\( a \\)). All existing and candidate
  production, consumption, or conversion technologies.

- \\( \mathbf{A}^{flex} \subseteq \mathbf{A} \\): Subset of Flexible Assets (variable input/output
  ratios).

- \\( \mathbf{A}^{std} = \mathbf{A} \setminus \mathbf{A}^{flex} \\): Subset of Standard Assets
  (fixed input/output coefficients).

- \\( \mathbf{C} \\): Set of Commodities (indexed by \\( c \\)). All energy carriers, materials, or
  tracked flows. Partitioned into:

  - \\( \mathbf{C}^{\mathrm{SVD}} \\): Supply-Driven Commodities (final demands; not consumed by
    assets).

  - \\( \mathbf{C}^{\mathrm{SED}} \\): Supply-Equals-Demand Commodities (intermediate system flows
    like grid electricity).

  - \\( \mathbf{C}^{\mathrm{OTH}} \\): Other Tracked Flows (e.g., losses, raw emissions).

- \\( \mathbf{C}^{VoLL} \subseteq \mathbf{C}^{\mathrm{SVD}} \cup \mathbf{C}^{\mathrm{SED}} \\):
  Subset of commodities where unserved demand is modelled with a penalty.

- \\( \mathbf{P} \\): Set of External Pools/Markets (indexed by \\( p \\)).

- \\( \mathbf{S} \\): Set of Scopes (indexed by \\( s \\)). Sets of \\( (r,t) \\) pairs for policy
  application.

## A. Core Model (Standard Assets: \\( a \in \mathbf{A}^{std} \\))

**Purpose:** Defines the operation of assets with fixed, predefined input-output relationships.

### A.1. Parameters (for \\( a \in \mathbf{A}^{std} \\) or global)

- \\( duration[t] \\): Duration of time slice \\( t \\) as a fraction of the year (\\( \in (0,1]
  \\)). Represents the portion of the year covered by this slice.

- \\( season\\_ slice[h,t] \\): Binary indicator; \\( 1 \\) if time slice \\( t \\) is in season \\(
  h \\), \\( 0 \\) otherwise. Facilitates seasonal aggregation.

- \\( balance\\_ level[c,r] \\): Defines the temporal resolution (’timeslice’, ’seasonal’, ’annual’)
  at which the supply-demand balance for commodity \\( c \\) in region \\( r \\) must be enforced.

- \\( demand[r,c] \\): Total annual exogenously specified demand (\\( \ge 0 \\)) for commodity \\( c
  \in \mathbf{C}^{\mathrm{SVD}} \\) in region \\( r \\). This is the final demand to be met.

- \\( timeslice\\_ share[c,t] \\): Fraction (\\( \in [0,1] \\)) of the annual \\( demand[r,c] \\)
  for \\( c \in \mathbf{C}^{\mathrm{SVD}} \\) that occurs during time slice \\( t \\). (\\(
  \sum_{t}timeslice\\_ share[c,t]=1 \\)). Defines the demand profile.

- \\( capacity[a,r] \\): Installed operational capacity (\\( \ge 0 \\)) of asset \\( a \\) in region
  \\( r \\) (e.g., MW for power plants). This value is an input to each dispatch run.

- \\( cap2act[a] \\): Conversion factor (\\( >0 \\)) from asset capacity units to activity units,
  ensuring consistency between capacity (e.g., MW) and activity (e.g., MWh produced in a slice)
  considering \\( duration[t] \\).

- \\( avail_{UB}[a,r,t], avail_{LB}[a,r,t], avail_{EQ}[a,r,t] \\): Availability factors (\\( \in
  [0,1] \\)) for asset \\( a \\) in time slice \\( t \\). \\( UB \\) is maximum availability, \\( LB
  \\) is minimum operational level, \\( EQ \\) specifies exact operation if required.

- \\( cost_{var}[a,r,t] \\): Variable operating cost (\\( \ge 0 \\)) per unit of activity for asset
  \\( a \\) (e.g., non-fuel O&M).

- \\( input_{coeff}[a,c] \\): Units (\\( \ge 0 \\)) of commodity \\( c \in
  (\mathbf{C}^{\mathrm{SED}} \cup \mathbf{C}^{\mathrm{OTH}}) \\) consumed by asset \\( a \\) per
  unit of its activity. (By assumption, \\( input_{coeff}[a,c]=0 \\) if \\( c \in
  \mathbf{C}^{\mathrm{SVD}} \\)).

- \\( output_{coeff}[a,c] \\): Units (\\( \ge 0 \\)) of commodity \\( c \in \mathbf{C} \\) produced
  by asset \\( a \\) per unit of its activity.

- \\( cost_{input}[a,c] \\): Specific cost (\\( \ge 0 \\)) per unit of input commodity \\( c \\)
  consumed by asset \\( a \\). Useful if \\( c \\) attracts a levy/incentive \\( only \\) if it is
  consumed by this type of asset.

- \\( cost_{output}[a,c] \\): Specific cost (if positive) or revenue (if negative) per unit of
  output commodity \\( c \\) produced by asset \\( a \\). Useful if levy/incentive applies \\( only
  \\) when the commodity is produced by this type of asset.

- \\( VoLL[c,r] \\): Value of Lost Load. A very high penalty cost applied per unit of unserved
  demand for \\( c \in \mathbf{C}^{VoLL} \\) in region \\( r \\).

### A.2. Decision Variables

These are the quantities the dispatch optimisation model determines.

- \\( act[a,r,t]\ge0 \\): Activity level of asset \\( a \\) in region \\( r \\) during time slice
  \\( t \\). This is the primary operational decision for each asset.

- \\( UnmetD[c,r,t]\ge0 \\): Unserved demand for commodity \\( c \in \mathbf{C}^{VoLL} \\) in region
  \\( r \\) during time slice \\( t \\). This variable allows the model to find a solution even if
  capacity is insufficient.

### A.3. Objective Contribution (for standard assets \\( a \in \mathbf{A}^{std} \\))

This term represents the sum of operational costs associated with standard assets, forming a
component of the overall system cost that the model seeks to minimise.

\\[
  \sum_{a\in \mathbf{A}^{std}}\sum_{r,t} act[a,r,t]
  \Biggl(
    cost_{var}[a,r,t] +
    \sum_{c \notin \mathbf{C}^{\mathrm{SVD}}} cost_{input}[a,c]\\,input_{coeff}[a,c] +
    \sum_{c \in \mathbf{C}} cost_{output}[a,c]\\,output_{coeff}[a,c]
  \Biggr)
\\]

### A.4. Constraints (Capacity & Availability for standard assets \\( a \in \mathbf{A}^{std} \\))

These constraints ensure that each standard asset’s operation respects its physical capacity and
time-varying availability limits. For all \\( a \in \mathbf{A}^{std}, r, t \\):

- Asset activity \\( act[a,r,t] \\) is constrained by its available capacity, considering its
  minimum operational level (lower bound, LB) and maximum availability (upper bound, UB):

    \\[
      \begin{aligned}
        capacity[a,r]\\,cap2act[a]\\,avail_{LB}[a,t]\\,duration[t] &\le act[a,r,t] \\\\
        act[a,r,t] &\le capacity[a,r]\\,cap2act[a]\\,avail_{UB}[a,t]\\,duration[t]
      \end{aligned}
    \\]

- If an exact operational level is mandated (e.g., for some renewables based on forecast, or fixed
  generation profiles for specific assets):

  \\[ act[a,r,t] = capacity[a,r]\\,cap2act[a]\\,avail_{EQ}[a,t]\\,duration[t] \\]

## B. Full Model Construction

> Note: This section includes references to many features that are not described elsewhere in this
> document or implemented yet (e.g. region-to-region trade), but these are included for
> completeness. This represents the roadmap for future MUSE2 development.

This section describes how all preceding components are integrated to form the complete dispatch
optimisation problem. 1. **Sets, Parameters, Decision Variables:** The union of all previously
defined elements. 2. **Objective Function:** The overall objective is to minimise the total system
cost, which is the sum of all operational costs from assets (standard and flexible), financial
impacts from policy scopes (taxes minus credits), costs of inter-regional trade, costs of pool-based
trade, and importantly, the high economic penalties associated with any unserved demand for critical
commodities:

\\[
  \begin{aligned}
    \text{Minimise: } &(\text{Core Asset Operational Costs from A.3 and E.4}) \\\\
    &+ (\text{Scope Policy Costs/Credits from B.4}) \\\\
    &+ (\text{Region-to-Region Trade Costs from C.4}) + (\text{Pool-Based Trade Costs from D.4}) \\\\
    &+ \sum_{c \in \mathbf{C}^{VoLL},r,t} UnmetD[c,r,t] \cdot VoLL[c,r]
    \quad \text{(Penalty for Unserved Demand)}
  \end{aligned}
\\]

Note that the unmet demand variables (\\( UnmetD[c,r,t] \\)) are normally not included in the
optimisation and are currently only used to diagnose the source of errors when running the model.

### Constraints

The complete set of constraints that the optimisation must satisfy includes:

- Capacity & Availability constraints for all assets \\( a \in \mathbf{A} \\)
  (as per A.4 and E.5).

- Scope policy constraints (B.5).

- Region-to-Region Trade Limits (C.5.A).

- Pool-Based Trade Limits (D.5.A).

- Flexible Asset operational constraints (E.5).

### Demand Satisfaction for \\( c\in \mathbf{C}^{\mathrm{SVD}} \\)

These constraints ensure that exogenously defined final demands for SVDs are met in each region \\(
r \\) and time slice \\( t \\), or any shortfall is explicitly accounted for.

For all \\( r,t,c \in \mathbf{C}^{\mathrm{SVD}} \\): Let \\( TotalSystemProduction_{SVD}[c,r,t] \\)
be the sum of all production of \\( c \\) from standard assets (\\( output_{coeff}[a,c]\\,act[a,r,t]
\\)) and flexible assets (the relevant \\( OutputSpec[a,c,r,t] \\) if \\( c \in
\mathbf{C}\_a^{eff\\_out} \\), or \\( act[a,r,t] \cdot coeff\_{aux\\_out}[a,c] \\) if \\( c \in
\mathbf{C}^{aux\\_out}\_a \\)).

Let \\( NetImports_{SVD}[c,r,t] \\) be net imports of \\( c \\) from R2R and Pool trade if SVDs are
tradeable. If \\( c \in \mathbf{C}^{VoLL} \\) (meaning unserved demand for this SVD is permitted at
a penalty):

\\[
  TotalSystemProduction_{SVD}[c,r,t] + NetImports_{SVD}[c,r,t] + UnmetD[c,r,t]
    \ge demand[r,c] \times timeslice\\_ share[c,t]
\\]

Else (if SVD \\( c \\) must be strictly met and is not included in \\( \mathbf{C}^{VoLL} \\)):

\\[
  TotalSystemProduction_{SVD}[c,r,t] + NetImports_{SVD}[c,r,t]
    \ge demand[r,c] \times timeslice\\_ share[c,t]
\\]

### Commodity Balance for \\( c\in \mathbf{C}^{\mathrm{SED}} \\)

These constraints ensure that for all intermediate SED commodities, total supply equals total demand
within each region \\( r \\) and for each balancing period defined by \\( balance\\_ level[c,r] \\)
(e.g., timeslice, seasonal, annual).

For a timeslice balance (\\( \forall r,t,c \in \mathbf{C}^{\mathrm{SED}} \\)):

Total Inflows (Local Production by all assets + Imports from other regions and pools + Unserved SED
if \\( c \in \mathbf{C}^{VoLL} \\)) = Total Outflows (Local Consumption by all assets + Exports to
other regions).

\\[
  \begin{aligned}
    &\sum\_{a \in \mathbf{A}^{std}} output_{coeff}[a,c]\\,act[a,r,t]
      && \text{(Std Asset Production)} \\\\
    &+ \sum\_{a \in \mathbf{A}^{flex}}
      \left(
        \begin{cases}
          OutputSpec[a,c,r,t] & \text{if } c \in \mathbf{C}^{eff\\_out}\_a \\\\
          act[a,r,t] \cdot coeff\_{aux\\_out}[a,c] & \text{if } c \in \mathbf{C}^{aux\_out}\_a \\ 0
            & \text{otherwise}
        \end{cases}
      \right)
      && \text{(Flex Asset Production)} \\\\
    &-\sum\_{a \in \mathbf{A}^{std}} input\_{coeff}[a,c]\\,act[a,r,t]
      && \text{(Std Asset Consumption)} \\\\
    &- \sum\_{a \in \mathbf{A}^{flex}}
      \left(
        \begin{cases}
          InputSpec[a,c,r,t] & \text{if } c \in \mathbf{C}^{eff\\_in}\_a \\\\
          act[a,r,t] \cdot coeff\_{aux\\_in}[a,c] & \text{if } c \in \mathbf{C}^{aux\\_in}\_a \\\\
          0 & \text{otherwise}
        \end{cases}
      \right)
      && \text{(Flex Asset Consumption)} \\\\
    &+ \sum\_{r'\neq r, c \in \mathbf{C}^R} ship\_{R2R}[r',r,c,t](1 - loss\_{R2R}[r',r,c,t])
      && \text{(R2R Imports)} \\\\
    &+ \sum\_{p, c \in \mathbf{C}^P} ship\_{pool}[p,r,c,t](1 - loss\_{pool}[p,r,c,t])
      && \text{(Pool Imports)} \\\\
    &- \sum\_{r'\neq r, c \in \mathbf{C}^R} ship\_{R2R}[r,r',c,t]
      && \text{(R2R Exports)} \\\\
    &+ \mathbb{I}(c \in \mathbf{C}^{VoLL}) \cdot UnmetD[c,r,t]
      && \text{(Unserved SED, if modelled)} \\\\
    &\ge 0
  \end{aligned}
\\]

(where \\( \mathbb{I}(c \in \mathbf{C}^{VoLL}) \\) is an indicator function, \\( 1 \\) if \\( c \\)
is in \\( \mathbf{C}^{VoLL} \\), \\( 0 \\) otherwise. Note that SVDs are not consumed by assets, so
\\( input_{coeff}[a,c] \\) and related terms for SVDs on the consumption side are zero).
