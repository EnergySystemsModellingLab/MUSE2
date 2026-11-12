# Investment

<!-- markdownlint-disable MD049 -->

This section describes the investment and asset retention process applied at each milestone year
(MSY). Commodity markets (commodity–region pairs) are visited in **investment order** (see below).
For each market, every [agent][agents] responsible for it selects assets to meet its share of the
market's demand, deciding which of its existing assets to retain and which new assets to build. How
an agent selects assets is determined by its **agent type**; see [Agents][agents] for a description
of each asset selection algorithm. Demands for `ServiceDemand` (`SVD`) commodities are fixed from
the input data, while demands for `SupplyEqualsDemand` (`SED`) commodities accumulate as assets are
selected for markets earlier in the investment order.

## Investment Order

Investment decisions are made sequentially, starting from the most downstream commodity markets
and moving upstream. For example, in a model where gas may be used to generate electricity,
investment in electricity generation would happen before investment in gas production.

This ordering ensures that when an upstream market is being invested in, the
demand created by already-committed downstream assets is already known.

Markets are processed in **layers**: a layer is a set of commodity markets that are independent of
one another (i.e. at the same depth in the commodity graph and with no dependencies between them).
All markets within a layer are settled before moving to the next layer. After each layer is
settled, a single [partial system dispatch](#partial-system-dispatch) is run over all assets
selected so far. This quantifies the input commodity flows consumed by newly committed assets —
for example, a gas generator committed during electricity market investment will consume gas,
creating demand that the gas market investment must subsequently meet.

Only commodities of type `ServiceDemand` and `SupplyEqualsDemand` are subject to
investment decisions. Other commodity types (e.g. `OTH`) are excluded.

> Note: the investment order is the reverse of the [price calculation order][prices], where prices
> are computed upstream first.

For details of how the investment order is determined from the commodity graph, including a worked
example, see [Commodity Graphs](./commodity_graphs.md#investment-and-price-order).

### Circularities

When markets form a circularity (e.g. electricity → hydrogen → electricity), they can't be put in an
order that respects every dependency. When the model is loaded, MUSE2 chooses an order for the
markets in each circularity, which determines its **feedback processes**: processes that consume a
market which is invested in *before* the market they supply (see
[Commodity Graphs][graph-circularities]). If assets are selected for a feedback process, they create
extra demand for a market that has already been invested in.

To deal with this, MUSE2 invests in the circularity in two passes over its markets, running a
[partial system dispatch](#partial-system-dispatch) after each market in each pass:

1. **First pass (provisional).** The markets are visited in order, with all processes available,
   including feedback processes. Any feedback assets selected will leave some demand for earlier
   markets unmet. Only the assets selected for feedback processes are kept; all other selections
   are discarded.
2. **Second pass (final).** The kept feedback assets are added to the system: their inputs add to
   the demand for the markets they consume, and their outputs reduce the demand for the markets
   they supply. The markets are then visited in the same order again, but this time feedback
   processes are excluded. Without them, demand only flows upstream, so the demand for each market
   is fully known when it is invested in.

The final selection is the feedback assets from the first pass plus all assets from the second
pass.

## Agent Shares

Each commodity market may be served by multiple agents, each responsible for a defined share
(or *portion*) of the total demand. An agent's portion determines:

- The fraction of the total demand that the agent is responsible for meeting.
- The scaling applied to any `addition_limit` and `total_capacity_limit` investment constraints
  (see [Investment Constraints](#investment-constraints)).

Agent portions for each commodity and milestone year are defined in the agent input files. Each
responsible agent selects assets for its own share of demand, using the algorithm for its
[agent type][agents].

## Investment Options

For each commodity market, agents consider two categories of supply option:

- **Existing assets**: already-commissioned assets owned by the agent in the market's region that
  produce the commodity of interest as their primary output. This includes any mothballed capacity,
  which can be reactivated if it is selected (see
  [Mothballing and Decommissioning](#mothballing-and-decommissioning)).
- **Candidate assets**: processes in the agent's search space with the commodity of interest as
  their primary output, available to be newly built.

### Annualised Fixed Cost

The annualised fixed cost (AFC) per unit of capacity differs between the two categories:

- **Existing assets**: AFC comprises only the fixed operations and maintenance (O&M) cost
(\\( \mathrm{FOM} \\)):
  \\[
    \mathrm{AFC}_{\mathrm{existing}} = \mathrm{FOM}
  \\]

- **Candidate assets**: AFC includes annualised capital expenditure plus fixed O&M:
  \\[
    \mathrm{AFC}_{\mathrm{candidate}} = \mathrm{CAPEX} \times \mathrm{CRF} + \mathrm{FOM}
  \\]

  where the Capital Recovery Factor (CRF) annualises the upfront capital cost over the asset's
  lifetime \\( L \\) at discount rate \\( d \\):
  \\[
    \mathrm{CRF} = \frac{d \cdot (1 + d)^L}{(1 + d)^L - 1}
  \\]

  If \\( d = 0 \\), then \\( \mathrm{CRF} = 1/L \\).

## Asset Capacities

Every asset consists of either a continuous capacity or one or more equal-capacity tranches.

- For assets defined in `assets.csv`, an explicitly supplied `num_tranches` determines the tranche size.
  Otherwise, a process `tranche_size` determines the tranche size. If neither is supplied, the asset
  has continuous capacity.
- Assets invested in _by MUSE_ are made up of tranches of the process `tranche_size`, if defined.
  Otherwise, their capacity depends on the agent type: competitive agents build new capacity in
  tranches whose size is inferred from demand at the time of investment, while portfolio agents
  build a continuous capacity (see [Agents][agents]).

The tranches of a multi-tranche asset can be retained or mothballed independently. A
continuous-capacity asset is retained or mothballed as a whole by competitive agents, whereas
portfolio agents may retain only part of its capacity (see
[Mothballing and Decommissioning](#mothballing-and-decommissioning)).

## Investment Constraints

Processes may be subject to investment constraints defined in
[`process_investment_constraints.csv`][process-investment-constraints-csv]: an `addition_limit`
constraining new-build capacity, and a `total_capacity_limit` constraining total installed
capacity.

In multi-agent simulations, both limits are scaled by \\( \mathrm{AgentPortion} \\), the fraction
of the commodity market for which an agent is responsible, to give a limit for each agent. Each
agent type enforces these limits as part of its asset selection algorithm (see [Agents][agents]).
One agent falling short of its own limits does **not** allow others to exceed theirs. Therefore, the
overall limits can only ever be reached if *all* agents max out their own individual limits.

### Addition limits

The `addition_limit` specifies the maximum new capacity that can be built *per year*, and applies
only to **candidate** (new-build) assets. The installable capacity limit for a given process
(\\( p \\)), agent (\\( g \\)), region (\\( r \\)), commodity (\\( c \\)) and milestone year
(\\( y \\)) is:

\\[
  \mathrm{MaxInstallableCapacity}\_{p,r,y,g} = \mathrm{AdditionLimit}\_{p,r,y}
    \cdot \Delta\_{\mathrm{MSY}} \cdot \mathrm{AgentPortion}\_{c,r,y,g}
\\]

where \\( \Delta_{\mathrm{MSY}} \\) is the number of years since the previous milestone year.

### Total capacity limits

The `total_capacity_limit` specifies a hard upper limit on the **total capacity** of a process that
may exist at any given time. This includes both existing assets which are considered for retention,
and candidate assets which are considered for commissioning. The total capacity limit for a given
process, agent, region, commodity and milestone year is:

\\[
  \mathrm{MaxTotalCapacity}\_{p,r,y,g} = \mathrm{TotalCapacityLimit}\_{p,r,y}
    \cdot \mathrm{AgentPortion}\_{c,r,y,g}
\\]

## Commodity Prices

Agents use commodity prices when selecting assets. These prices come from the previous MSY's
dispatch (or from the previous [ironing-out loop][framework-overview] iteration, if enabled). See
[Commodity Prices][prices] for how market, shadow and fallback prices are calculated, and
[Agents][agents] for how each agent type uses them.

## Asset Selection

For each market, the asset selection algorithm of each responsible agent returns the assets that it
has selected to meet its share of demand. Selected existing assets are retained (reactivating any
selected mothballed capacity), and selected candidate assets are commissioned as new assets owned by
the agent.

If an agent is unable to meet its share of demand, the outcome depends on its agent type. A
competitive agent logs a warning and stops early, keeping the assets selected so far; the unmet
demand may still be satisfied during the full system dispatch, but this is not guaranteed. If
investment constraints are so restrictive that it has no options left, the simulation terminates
with an error. For a portfolio agent, the simulation terminates with an error.

> **Note:** only production of the *primary output* commodity is counted against demand. If a
> selected asset also produces other commodities as secondary outputs, that side-production does
> not reduce the demand targets of those other commodity markets. This behaviour may be revised in a
> future release.

## Mothballing and Decommissioning

After investment is complete for a given MSY, any capacity of previously commissioned assets that
was not selected for retention is *mothballed*. For assets made up of tranches, whole tranches are
mothballed, whereas assets with continuous capacity may be partially mothballed. Mothballed capacity
is not dispatched, but it remains in the asset pool, together with the year in which it was
mothballed.

Mothballed capacity is available for selection in future MSYs (see
[Investment Options](#investment-options)). If an asset is selected again, its mothballed capacity
is reactivated as needed, starting with the capacity that has been mothballed for the longest.

Capacity that has been mothballed for `mothball_years` years (as defined in
[`model.toml`][model-toml]) is *decommissioned*: it is permanently removed from the asset. Assets
with no remaining capacity are removed from the asset pool and excluded from all future investment
and dispatch.

## Partial System Dispatch

After each layer of commodity markets is settled during the investment loop, a **partial system
dispatch** is run over all assets selected so far. Within a [circularity](#circularities), a partial
dispatch is instead run after each market in each pass, over the assets just selected for that
market. This is a standard [dispatch optimisation][dispatch-optimisation] solve, with two key
modifications described below.

### Market subset

Only the commodity markets visited so far — i.e. the current market and all those settled in
earlier investment rounds — are subject to commodity balance constraints. Markets that are upstream
of the current investment frontier are not yet constrained, because no assets have been committed
to serve them yet.

### Input prices for upstream commodities

Because upstream markets are unconstrained, their commodities have no balance constraints and
therefore no shadow prices in this dispatch. Without any signal on the cost of upstream inputs,
those inputs would appear free to the solver.

To avoid this, the shadow prices \\( \lambda_{c,r,t} \\) from the previous MSY (see
[Commodity Prices](#commodity-prices)) are passed as explicit
cost penalties on any input flows corresponding to upstream, unconstrained commodities. This ensures
the dispatch correctly accounts for the cost of consuming upstream resources, even before those
markets have been invested in.

### Using the partial dispatch result

The flow map from the partial dispatch is used to update the **net demand** seen by each upstream
market. For each newly committed asset, its input commodity flows are added to the demand for the
corresponding upstream markets, and its primary output flows reduce the remaining demand for the
current market. Only primary output flows are counted against demand; secondary outputs do not
affect the demand targets of other commodity markets. These updated demands then drive the next
investment decisions as the process moves along the investment order, with each market's committed
assets shaping the demand seen by those upstream.

[agents]: ./agents.md
[framework-overview]: index.html#framework-overview
[prices]: ./prices.md
[model-toml]: ../file_formats/input_files.md#model-parameters-modeltoml
[process-investment-constraints-csv]: ../file_formats/input_files.md#process_investment_constraintscsv
[dispatch-optimisation]: ./dispatch_optimisation.md
[graph-circularities]: ./commodity_graphs.md#circularities
