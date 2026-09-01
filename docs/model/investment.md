# Investment Appraisal

<!-- markdownlint-disable MD049 -->

This section describes the investment and asset retention process applied at each milestone year
(MSY). For each commodity market (a commodity–region pair), processed in **investment order**
(see below), agents evaluate every available supply option — existing commissioned assets and new
candidate assets from their search space — and select the best option to commit. The committed
asset's production is subtracted from the remaining demand for that market, and the process
repeats until demand is met or no feasible options remain. Demands for `ServiceDemand` (`SVD`)
commodities are fixed from the input data, while demands for `SupplyEqualsDemand` (`SED`)
commodities accumulate as assets are committed earlier in the investment order.

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

### Circularities

When commodity markets form a cycle (e.g. electricity → hydrogen → electricity), the markets in
the cycle are resolved in sequence within one pass. After each market in the cycle is visited, a
[partial system dispatch](#partial-system-dispatch) is run to rebalance demand. Newly committed
assets within the cycle are given limited capacity flexibility, controlled by the `capacity_margin`
parameter (defined in [`model.toml`][model-toml]), to absorb small demand shifts caused by later
markets in the cycle. If these shifts exceed `capacity_margin`, the simulation terminates with an
error, and the user should increase this parameter.

## Commodity Prices Used in Appraisal

Investment appraisal uses two distinct price sets, both sourced from the previous MSY's dispatch
(or from the previous [ironing-out loop][framework-overview] iteration if enabled):

- **Shadow prices** \\( \lambda\_{c,r,t} \\): used for activity coefficients in the mini dispatch
  optimisation step of each appraisal (see [Mini Dispatch Optimisation](#mini-dispatch-optimisation)).
- **Market prices** \\( \pi\_{c,r,t} \\): used to calculate the investment metric (Cost Index or
  SNAS) after dispatch.
- **Fallback prices** \\( \phi\_{c,r,t} \\): used to incentivise dispatch in the mini dispatch
  optimisation when shadow prices alone are insufficient (see
  [Mini Dispatch Optimisation](#mini-dispatch-optimisation)). Calculated using the strategy defined
  by `fallback_pricing_strategy` in [`model.toml`][model-toml].

See [Commodity Prices][prices] for how these price sets are calculated.

## Agent Shares

Each commodity market may be served by multiple agents, each responsible for a defined share
(or *portion*) of the total demand. An agent's portion determines:

- The fraction of the total demand that the agent is responsible for meeting.
- The scaling applied to any `addition_limit` investment constraints
  (see [Investment Constraints](#investment-constraints)).

Agent portions for each commodity and milestone year are defined in the agent input files.

## Investment Options

For each commodity market, agents consider two categories of supply option:

- **Existing assets**: already-commissioned assets owned by the agent that produce the commodity
  of interest as their primary output.
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

Every asset consists of one or more equal-capacity units. A single-unit asset is retained or
mothballed as a whole, while the units of a multi-unit asset can be retained or mothballed
independently (see [Mothballing and Decommissioning](#mothballing-and-decommissioning)).

- For assets defined in `assets.csv`, an explicitly supplied `num_units` determines the unit size.
  Otherwise, a process `tranche_size` determines the unit size. If neither is supplied, the asset
  consists of one unit with its full capacity.
- Assets invested in _by MUSE_ will use the process `tranche_size`, if defined, or will use a capacity
  based on demand at the time of investment (see "trial capacity" below).

### Existing assets

Existing assets (i.e assets that have already been commissioned, whether via `assets.csv` or by
MUSE) are appraised one unit at a time to decide how many units to retain. This allows partial
retention — for example, some units of a multi-unit plant may be retained while others are
mothballed.

### Candidate assets

Before a candidate asset for new investment can be appraised, it is assigned a trial capacity which
defines how much capacity can be installed in a single investment round.

If a process has a defined `tranche_size`, the trial capacity is set to one unit. Otherwise, it
calculated based on the capacity that would satisfy the total remaining demand if the asset operated
at its maximum annual rate:

\\[
  \mathrm{TrialCapacity} = \frac{\sum_t \mathrm{Demand}_t}{\mathrm{MaxAnnualSupplyPerCapacity}}
    \\times \mathrm{CapacityLimitFactor}
\\]

`capacity_limit_factor` (set in [`model.toml`][model-toml], must be > 0 and <= 1) controls the
size of investment increments relative to total demand. Lower values produce smaller investment
increments (requiring more investment rounds), while higher values produce larger increments.

### Investment constraints

Processes may have an `addition_limit` (see
[`process_investment_constraints.csv`][process-investment-constraints-csv]) specifying the
maximum new capacity that can be built per year. The installable capacity limit for a given MSY is:

\\[
  \mathrm{MaxInstallableCapacity} = \mathrm{AdditionLimit} \times \Delta_{\mathrm{MSY}}
    \times \mathrm{AgentPortion}
\\]

where \\( \Delta_{\mathrm{MSY}} \\) is the number of years since the previous MSY and
\\( \mathrm{AgentPortion} \\) is the fraction of the commodity market for which this agent is
responsible.

If the remaining installable capacity is exhausted, the candidate is excluded from further
consideration.

## Mini Dispatch Optimisation

For each supply option being appraised, a small linear programme (LP) is solved to determine the
optimal activity profile given the current remaining demand.

### Activity coefficients

The mini dispatch optimisation implicitly frames each time slice as a choice: the asset can either
produce the commodity of interest, or it can be treated as procured from an alternative source at
the **fallback price** \\( \phi_{c,r,t} \\). Each unit of activity produces
\\( f_{c,\mathrm{primary}} \\) units of output, displacing that quantity from the fallback source.
The optimiser therefore dispatches the asset whenever doing so is cheaper than procuring from
elsewhere.

This is captured by the activity coefficient \\( \alpha_t \\), which combines two components:

\\[
  \mathrm{NetOperatingCost}_t = \mathrm{OperatingCost}(t) - \mathrm{RevenueFromFlows}(\lambda, t)
\\]

\\[
  \mathrm{FallbackCost} = \phi_{c,r,t} \cdot f_{c,\mathrm{primary}}
\\]

\\[
  \alpha_t = \mathrm{FallbackCost} - \mathrm{NetOperatingCost}_t + \varepsilon
\\]

where \\( \mathrm{RevenueFromFlows} \\) is the sum of all commodity flow revenues and costs (positive
for outputs, negative for inputs) valued at shadow prices, \\( \mathrm{OperatingCost} \\) is the
variable operating cost plus levies and flow costs, \\( f_{c,\mathrm{primary}} \\) is the primary
output flow coefficient, and \\( \varepsilon \\) is a small positive constant added to ensure that
break-even assets are still dispatched.

**NetOperatingCost** is the net cost of running the asset for one unit of activity at shadow prices
— negative when the asset is profitable (revenues exceed costs).

**FallbackCost** is the cost of procuring one unit of activity's worth of primary output from an
alternative source at the fallback price.

The asset dispatches when \\( \alpha_t > 0 \\), i.e. when:

\\[
  \mathrm{NetOperatingCost}_t < \mathrm{FallbackCost}
\\]

\\( \phi_{c,r,t} \\) is calculated according to the strategy defined by `fallback_pricing_strategy`
in [`model.toml`][model-toml].

### Constraints

- **Activity bounds**: the sum of activity within each time-slice selection is bounded by the
  asset's availability limits multiplied by its capacity.
- **Demand constraints**: demand for a commodity is balanced at the commodity's defined
  *time-slice level* (e.g. annual, seasonal, or time-slice). The total supply (activity × flow
  coefficient) within each balance bucket must not exceed the remaining demand for that bucket.

### Objective

The optimisation maximises the total net revenue across all time slices, subject to the above
constraints:

\\[
  \max \sum_t \alpha_t \cdot \mathrm{Activity}_t
\\]

## Metric Calculation

After the dispatch LP is solved, an investment metric is calculated from the resulting activity
profile using **market prices**.

### Market costs per time slice

The market cost \\( \mu_t \\) is calculated differently depending on the objective type:

- **LCOX**: the net cost of operating, excluding revenues from the primary output commodity:
  \\[
    \mu_t^{\mathrm{LCOX}} = \mathrm{OperatingCost}(t) -
      \mathrm{RevenueFromNonPrimaryFlows}(\pi, t)
  \\]

- **NPV**: the net cost of operating, including all commodity flows (so negative values represent
  profit):
  \\[
    \mu_t^{\mathrm{NPV}} = \mathrm{OperatingCost}(t) - \mathrm{RevenueFromFlows}(\pi, t)
  \\]

### LCOX metric (`objective_type = "lcox"`)

The LCOX metric is calculated as the total annualised cost divided by total annual output, using
the above defined market costs which *exclude* the primary output commodity:

\\[
  \mathrm{LCOXMetric} = \frac{\mathrm{AFC} \times \mathrm{Capacity} + \sum_t \mathrm{Activity}_t
  \times \mu_t^{\mathrm{LCOX}}}
    {\sum_t \mathrm{Activity}_t}
\\]

Lower values indicate lower-cost investments.

### NPV metric (`objective_type = "npv"`)

The NPV metric is based on the Specific Net Annualised Surplus (SNAS). This the net surplus per
unit of activity, using market costs that *include* the primary output commodity:

\\[
  \mathrm{SNAS} = \frac{-\left(\mathrm{AFC} \times \mathrm{Capacity} + \sum_t \mathrm{Activity}_t \times
    \mu_t^{\mathrm{NPV}}\right)}{\sum_t \mathrm{Activity}_t}
\\]

Higher values indicate more profitable investments.

> For both metrics, any option with zero total activity after the mini dispatch LP is excluded from
> consideration, as it cannot contribute to meeting demand. This will generally happen if all
> time slices have negative activity coefficients, unless the process has lower-bound activity
> constraints that force activity.

## Asset Selection

### Sorting and tie-breaking

All feasible options are appraised and ranked by their metric. When two options have approximately
equal metrics, the following tie-breaking rules are applied in order:

1. Existing commissioned assets are preferred over new candidates.
2. Among existing assets, newer assets (commissioned more recently) are preferred.
3. If the tie is still unresolved, the first option in the ordering is selected arbitrarily, and a
   `debug`-level log message is emitted.

### Selection loop

The best-ranked asset is committed. Its production profile from the mini dispatch optimisation is
subtracted from the remaining demand, and the loop repeats with the updated demand profile. This
continues until:

- The remaining demand falls below `remaining_demand_absolute_tolerance`
  (in [`model.toml`][model-toml]), or
- No feasible options remain. In this case, a warning is logged and the loop ends early. The
  unmet demand may still be satisfied during the full system dispatch, but is not guaranteed.

If demand cannot be met at all due to overly restrictive investment constraints, the simulation
terminates with an error.

> **Note:** only production of the *primary output* commodity is counted against the remaining
> demand. If a committed asset also produces other commodities as secondary outputs, that
> side-production does not reduce the demand targets of those other commodity markets. This
> behaviour may be revised in a future release.

## Mothballing and Decommissioning

After investment is complete for a given MSY, any previously commissioned assets (or individual
units making up the asset) that were not selected for retention are *mothballed*: their mothball
year is recorded and they are removed from the active asset pool. They remain available for
potential re-selection in future MSYs.

A mothballed asset that remains unused for `mothball_years` consecutive years (as defined in
[`model.toml`][model-toml]) is *decommissioned* — permanently removed from the asset pool and
excluded from all future investment and dispatch.

## Example: Gas Power Plant

The following illustrates how LCOX and NPV metrics are calculated for a gas combined-cycle power
plant, evaluated across two time slices: \\( t_0 \\) (peak) and \\( t_1 \\) (off-peak).

### Parameters

#### Asset flows and operating costs
<!-- markdownlint-disable MD013 -->
| Flow | Value | Description |
| ------ | ------- | ------------- |
| Electricity output | \\( +1.0 \\) MWh/MWh activity | Primary output |
| Heat output | \\( +0.5 \\) MWh/MWh activity | By-product |
| Natural gas input | \\( -2.5 \\) MWh/MWh activity | Fuel |
| \\( \mathrm{OperatingCost} \\) | £5/MWh activity | Constant across time slices |
<!-- markdownlint-enable MD013 -->

All per-flow costs (\\( \mathrm{cost}\_{\mathrm{input}} \\),
\\( \mathrm{cost}\_{\mathrm{output}} \\)) are zero.

#### Fixed costs and capacity

| Parameter | Value     |
|-----------|-----------|
| AFC       | £1,000/MW |
| Capacity  | 100 MW    |

#### Prices (both shadow and market prices are equal in this example)

| Commodity | \\( t_0 \\) (Peak) | \\( t_1 \\) (Off-peak) |
| ----------- | -------------------- | ------------------------ |
| Electricity | £90/MWh | £50/MWh |
| Heat | £25/MWh | £15/MWh |
| Natural gas | £35/MWh | £25/MWh |

### Mini Dispatch Optimisation (identical for LCOX and NPV)

Activity coefficients use shadow prices:

**\\( t_0 \\):**
\\[
\alpha_{t_0} = (1.0 \times 90) + (0.5 \times 25) + (-2.5 \times 35) - 5
= \text{£10/MWh}
\\]

**\\( t_1 \\):**
\\[
\alpha_{t_1} = (1.0 \times 50) + (0.5 \times 15) + (-2.5 \times 25) - 5
= \text{£}{-10}\text{/MWh}
\\]

The optimiser maximises \\( 10 \cdot \mathrm{Activity}_{t_0} + (-10) \cdot \mathrm{Activity}_{t_1} \\),
so it prefers to dispatch during \\( t_0 \\) and minimise activity during \\( t_1 \\), subject to
demand and availability constraints.

Suppose the optimiser determines \\( \mathrm{Activity}_{t_0} = 80 \\) MWh and
\\( \mathrm{Activity}_{t_1} = 20 \\) MWh.

### LCOX Metric

**Market costs (excluding primary output):**

\\[
\begin{aligned}
\mu_{t_0}^{\mathrm{LCOX}} &= 5 + (2.5 \times 35) - (0.5 \times 25) = \text{£80/MWh} \\\\
\mu_{t_1}^{\mathrm{LCOX}} &= 5 + (2.5 \times 25) - (0.5 \times 15) = \text{£60/MWh}
\end{aligned}
\\]

**Cost Index:**
\\[
\begin{aligned}
\mathrm{CostIndex} &= \frac{(1{,}000 \times 100) + (80 \times 80) + (20 \times 60)}{80 + 20} \\\\
&= \text{£1,076/MWh}
\end{aligned}
\\]

### NPV Metric

**Market costs (including primary output):**

\\[
\begin{aligned}
\mu_{t_0}^{\mathrm{NPV}} &= 5 - (1.0 \times 90) - (0.5 \times 25) + (2.5 \times 35)
= \text{£}{-10}\text{/MWh} \\\\
\mu_{t_1}^{\mathrm{NPV}} &= 5 - (1.0 \times 50) - (0.5 \times 15) + (2.5 \times 25) = \text{£10/MWh}
\end{aligned}
\\]

**SNAS:**
\\[
\begin{aligned}
\mathrm{SNAS} &= \frac{-\left[(1{,}000 \times 100) + (80 \times (-10)) + (20 \times 10)\right]}
{80 + 20} \\\\
&= \text{£}{-994}\text{/MWh}
\end{aligned}
\\]

The negative SNAS indicates that at current market prices, this asset does not generate a surplus
over its annualised costs. It would still be selected if it has the highest SNAS among all
available options.

## Partial System Dispatch

After each layer of commodity markets is settled during the investment loop, a **partial system
dispatch** is run over all assets selected so far. This is a standard
[dispatch optimisation][dispatch-optimisation] solve, with three key modifications described below.

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
[Commodity Prices Used in Appraisal](#commodity-prices-used-in-appraisal)) are passed as explicit
cost penalties on any input flows corresponding to upstream, unconstrained commodities. This ensures
the dispatch correctly accounts for the cost of consuming upstream resources, even before those
markets have been invested in.

### Capacity flexibility in circularities

When markets form a cycle, the partial dispatch after each market in the cycle uses **flexible
capacity variables** for all newly committed assets in the cycle. Rather than fixing the capacity of
these assets at their committed value, the solver may adjust capacity within the bounds:

\\[
  \bigl[(1 - \mathrm{capacity\_margin}) \cdot \mathrm{Capacity}_a, \space
        (1 + \mathrm{capacity\_margin}) \cdot \mathrm{Capacity}_a\bigr]
\\]

where \\( \mathrm{Capacity}\_a \\) is the committed capacity of asset \\( a \\). The upper bound is
additionally capped by the asset's `MaxInstallableCapacity`. This allows the dispatch to absorb
small demand shifts caused by subsequent markets in the cycle.

Each flexible capacity variable enters the dispatch objective with a cost coefficient equal to the
asset's AFC (annualised capital cost plus fixed O&M).

If the demand shift for a previously settled market in the cycle exceeds what the flexible capacity
can accommodate, the simulation terminates with an error. The user should increase the
`capacity_margin` parameter (defined in [`model.toml`][model-toml]) in this case.

### Using the partial dispatch result

The flow map from the partial dispatch is used to update the **net demand** seen by each upstream
market. For each newly committed asset, its input commodity flows are added to the demand for the
corresponding upstream markets, and its primary output flows reduce the remaining demand for the
current market. Only primary output flows are counted against demand; secondary outputs do not
affect the demand targets of other commodity markets. These updated demands then drive the next
investment decisions as the process moves along the investment order, with each market's committed
assets shaping the demand seen by those upstream.

[framework-overview]: index.html#framework-overview
[prices]: ./prices.md
[model-toml]: ../file_formats/input_files.md#model-parameters-modeltoml
[process-investment-constraints-csv]: ../file_formats/input_files.md#process_investment_constraintscsv
[dispatch-optimisation]: ./dispatch_optimisation.md
