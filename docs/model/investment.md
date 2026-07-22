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

After each commodity market is settled, a dispatch is run over all assets selected so far. This
quantifies the input commodity flows consumed by newly committed assets — for example, a gas
generator committed during electricity market investment will consume gas, creating demand that
the gas market investment must subsequently meet.

Only commodities of type `ServiceDemand` and `SupplyEqualsDemand` are subject to
investment decisions. Other commodity types (e.g. `OTH`) are excluded.

> Note: the investment order is the reverse of the [price calculation order][prices], where prices
> are computed upstream first.

### Circularities

When commodity markets form a cycle (e.g. electricity → hydrogen → electricity), the markets in
the cycle are resolved in sequence within one pass. After each market in the cycle is visited, a
dispatch is run to rebalance demand. Newly committed assets within the cycle are given limited
capacity flexibility, controlled by the `capacity_margin` parameter (defined in
[`model.toml`][model-toml]), to absorb small demand shifts caused by later markets in the cycle.
If these shifts exceed `capacity_margin`, the simulation terminates with an error, and the user
should increase this parameter.

## Commodity Prices Used in Appraisal

Investment appraisal uses two distinct price sets, both sourced from the previous MSY's dispatch
(or from the previous [ironing-out loop][framework-overview] iteration if enabled):

- **Shadow prices** \\( \lambda\_{c,r,t} \\): used for activity coefficients in the mini dispatch
  optimisation step of each appraisal (see [Mini Dispatch Optimisation](#mini-dispatch-optimisation)).
- **Market prices** \\( \pi\_{c,r,t} \\): used to calculate the investment metric (Cost Index or
  SNAS) after dispatch.
- **Fallback prices** \\( \phi_{c,r,t} \\): used to incentivise dispatch in the mini dispatch
  optimisation when shadow prices alone are insufficient (see
  [Mini Dispatch Optimisation](#mini-dispatch-optimisation)). Calculated using the strategy defined
  by `fallback_price_strategy` in [`model.toml`][model-toml].

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
(\\( \text{FOM} \\)):
  \\[
    \text{AFC}_\text{existing} = \text{FOM}
  \\]

- **Candidate assets**: AFC includes annualised capital expenditure plus fixed O&M:
  \\[
    \text{AFC}_\text{candidate} = \text{CAPEX} \times \text{CRF} + \text{FOM}
  \\]

  where the Capital Recovery Factor (CRF) annualises the upfront capital cost over the asset's
  lifetime \\( L \\) at discount rate \\( d \\):
  \\[
    \text{CRF} = \frac{d \cdot (1 + d)^L}{(1 + d)^L - 1}
  \\]

  If \\( d = 0 \\), then \\( \text{CRF} = 1/L \\).

## Asset Capacity

A process is either **divisible** or **non-divisible**:

- A **divisible** process has a fixed `unit_size` (defined in [`processes.csv`][processes-csv]).
  Assets of this type consist of one or more discrete units, each of size `unit_size`. When
  commissioned, a divisible asset is split into individual units, each of which is appraised and
  retained or mothballed independently.
- A **non-divisible** process has no `unit_size`. Assets of this type are treated as a single
  entity: capacities may take any value within their allowable range, but cannot be split into
  independently appraised or mothballed units.

### Existing assets

- **Non-divisible**: the asset is appraised as a whole at its full installed capacity.
- **Divisible**: each individual unit is appraised separately, one at a time. This allows partial
  retention — for example, some units of a multi-unit plant may be retained while others are
  mothballed.

### Candidate assets

Before a candidate asset can be appraised, it is assigned a trial capacity which defines how much
capacity can be installed in a single investment round (subject to further
[demand-limiting capacity](#demand-limiting-capacity-dlc) and
[investment constraints](#investment-constraints), described below)

- **Divisible**: the trial capacity is set to one unit (one `unit_size`), representing a single
  unit being considered for investment.
- **Non-divisible**: the trial capacity is based on the capacity that would satisfy the
  total remaining demand if the asset operated at its maximum annual rate:

  \\[
    \text{TrialCapacity} = \frac{\sum_t \text{Demand}_t}{\text{MaxAnnualSupplyPerCapacity}}
    \times \text{CapacityLimitFactor}
  \\]

  `capacity_limit_factor` (set in [`model.toml`][model-toml], must be > 0 and <= 1) controls the
  size of investment increments relative to total demand. Lower values produce smaller investment
  increments (requiring more investment rounds), while higher values produce larger increments.

### Demand-limiting capacity (DLC)

In each investment round, a candidate's trial capacity is further capped by the
*demand-limiting capacity*, which is the minimum capacity required to satisfy the remaining demand
across all time-slice selections:

\\[
  \text{DLC} = \max_{\text{selection}} \frac{\sum_{t \in \text{selection}} \text{Demand}\_t}
    {\text{MaxSupplyPerCapacity}_\text{selection}}
\\]

Selections where the asset has zero maximum supply are excluded. The cap prevents over-investment
(i.e. building more capacity than needed to meet remaining demand).

### Investment constraints

Processes may have an `addition_limit` (see
[`process_investment_constraints.csv`][process-investment-constraints-csv]) specifying the
maximum new capacity that can be built per year. The installable capacity limit for a given MSY is:

\\[
  \text{MaxInstallableCapacity} = \text{AdditionLimit} \times \Delta_\text{MSY}
    \times \text{AgentPortion}
\\]

where \\( \Delta_\text{MSY} \\) is the number of years since the previous MSY and
\\( \text{AgentPortion} \\) is the fraction of the commodity market for which this agent is
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
\\( f_{c,\text{primary}} \\) units of output, displacing that quantity from the fallback source.
The optimiser therefore dispatches the asset whenever doing so is cheaper than procuring from
elsewhere.

This is captured by the activity coefficient \\( \alpha_t \\), which combines two components:

\\[
  \text{NetOperatingCost}_t = \text{OperatingCost}(t) - \text{RevenueFromFlows}(\lambda, t)
\\]

\\[
  \text{FallbackCost} = \phi_{c,r,t} \cdot f_{c,\text{primary}}
\\]

\\[
  \alpha_t = \text{FallbackCost} - \text{NetOperatingCost}_t + \varepsilon
\\]

where \\( \text{RevenueFromFlows} \\) is the sum of all commodity flow revenues and costs (positive
for outputs, negative for inputs) valued at shadow prices, \\( \text{OperatingCost} \\) is the
variable operating cost plus levies and flow costs, \\( f_{c,\text{primary}} \\) is the primary
output flow coefficient, and \\( \varepsilon \\) is a small positive constant added to ensure that
break-even assets are still dispatched.

**NetOperatingCost** is the net cost of running the asset for one unit of activity at shadow prices
— negative when the asset is profitable (revenues exceed costs).

**FallbackCost** is the cost of procuring one unit of activity's worth of primary output from an
alternative source at the fallback price.

The asset dispatches when \\( \alpha_t > 0 \\), i.e. when:

\\[
  \text{NetOperatingCost}_t < \text{FallbackCost}
\\]

\\( \phi_{c,r,t} \\) is calculated according to the strategy defined by `fallback_price_strategy`
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
  \max \sum_t \alpha_t \cdot \text{Activity}_t
\\]

## Metric Calculation

After the dispatch LP is solved, an investment metric is calculated from the resulting activity
profile using **market prices**.

### Market costs per time slice

The market cost \\( \mu_t \\) is calculated differently depending on the objective type:

- **LCOX**: the net cost of operating, excluding revenues from the primary output commodity:
  \\[
    \mu_t^\text{LCOX} = \text{OperatingCost}(t) -
      \text{RevenueFromNonPrimaryFlows}(\pi, t)
  \\]

- **NPV**: the net cost of operating, including all commodity flows (so negative values represent
  profit):
  \\[
    \mu_t^\text{NPV} = \text{OperatingCost}(t) - \text{RevenueFromFlows}(\pi, t)
  \\]

### LCOX metric (`objective_type = "lcox"`)

The LCOX metric is calculated as the total annualised cost divided by total annual output, using
the above defined market costs which *exclude* the primary output commodity:

\\[
  \text{LCOXMetric} = \frac{\text{AFC} \times \text{cap} + \sum_t \text{Activity}_t \times \mu_t^\text{LCOX}}
    {\sum_t \text{Activity}_t}
\\]

Lower values indicate lower-cost investments.

### NPV metric (`objective_type = "npv"`)

The NPV metric is based on the Specific Net Annualised Surplus (SNAS). This the net surplus per
unit of activity, using market costs that *include* the primary output commodity:

\\[
  \text{SNAS} = \frac{-\left(\text{AFC} \times \text{cap} + \sum_t \text{Activity}_t \times
    \mu_t^\text{NPV}\right)}{\sum_t \text{Activity}_t}
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

After investment is complete for a given MSY, any previously commissioned assets that were not
selected for retention are *mothballed*: their mothball year is recorded and they are removed from
the active asset pool. They remain available for potential re-selection in future MSYs.

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
| \\( \text{OperatingCost} \\) | £5/MWh activity | Constant across time slices |
<!-- markdownlint-enable MD013 -->

All per-flow costs (\\( cost_\text{input} \\), \\( cost_\text{output} \\)) are zero.

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

The optimiser maximises \\( 10 \cdot act_{t_0} + (-10) \cdot act_{t_1} \\), so it prefers to
dispatch during \\( t_0 \\) and minimise activity during \\( t_1 \\), subject to demand and
availability constraints.

Suppose the optimiser determines \\( act_{t_0} = 80 \\) MWh and \\( act_{t_1} = 20 \\) MWh.

### LCOX Metric

**Market costs (excluding primary output):**

\\[
\begin{aligned}
\mu_{t_0}^\text{LCOX} &= 5 + (2.5 \times 35) - (0.5 \times 25) = \text{£80/MWh} \\\\
\mu_{t_1}^\text{LCOX} &= 5 + (2.5 \times 25) - (0.5 \times 15) = \text{£60/MWh}
\end{aligned}
\\]

**Cost Index:**
\\[
\begin{aligned}
\text{CostIndex} &= \frac{(1{,}000 \times 100) + (80 \times 80) + (20 \times 60)}{80 + 20} \\\\
&= \text{£1,076/MWh}
\end{aligned}
\\]

### NPV Metric

**Market costs (including primary output):**

\\[
\begin{aligned}
\mu_{t_0}^\text{NPV} &= 5 - (1.0 \times 90) - (0.5 \times 25) + (2.5 \times 35)
= \text{£}{-10}\text{/MWh} \\\\
\mu_{t_1}^\text{NPV} &= 5 - (1.0 \times 50) - (0.5 \times 15) + (2.5 \times 25) = \text{£10/MWh}
\end{aligned}
\\]

**SNAS:**
\\[
\begin{aligned}
\text{SNAS} &= \frac{-\left[(1{,}000 \times 100) + (80 \times (-10)) + (20 \times 10)\right]}
{80 + 20} \\\\
&= \text{£}{-994}\text{/MWh}
\end{aligned}
\\]

The negative SNAS indicates that at current market prices, this asset does not generate a surplus
over its annualised costs. It would still be selected if it has the highest SNAS among all
available options.

[framework-overview]: index.html#framework-overview
[prices]: ./prices.md
[model-toml]: ../file_formats/input_files.md#model-parameters-modeltoml
[processes-csv]: ../file_formats/input_files.md#processescsv
[process-investment-constraints-csv]: ../file_formats/input_files.md#process_investment_constraintscsv
