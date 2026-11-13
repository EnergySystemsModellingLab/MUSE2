# Agents

<!-- markdownlint-disable MD049 -->

Agents are the decision-making entities in MUSE2. Each agent operates in one or more regions and is
responsible for a portion of the demand for one or more commodities. Agents invest in, retain and
operate assets in order to meet their share of demand.

Each agent has:

- **Commodity portions**: the share of demand for each commodity that the agent is responsible for
  in each milestone year (see [`agent_commodity_portions.csv`][agent-commodity-portions-csv]).
- A **search space**: the processes that the agent may invest in for each commodity, region and
  milestone year (see [`agent_search_spaces.csv`][agent-search-spaces-csv]).
- An **objective** for each milestone year, such as levelised cost (`lcox`) or net present value
  (`npv`) (see [`agent_objectives.csv`][agent-objectives-csv]), and a **decision rule** for
  combining objectives (see [`agents.csv`][agents-csv]). Currently only the `single` decision rule
  is supported.
- An **agent type**, which determines the algorithm that the agent uses to select assets (see
  [`agents.csv`][agents-csv]).

During [investment][investment], the asset selection algorithm of each agent is called once for
every market (commodity–region pair) that the agent is responsible for, with the agent's share of
demand and its share of any [investment constraints][investment-constraints]. It returns the assets
selected by the agent: the existing assets to retain and the new candidate assets to build. The rest
of the investment process (the investment order, handling of circularities, partial system
dispatch, mothballing, etc.) is the same for all agent types.

There are two agent types, set with the `agent_type` column of [`agents.csv`][agents-csv]:
competitive agents (`competitive`) and portfolio agents (`portfolio`)

## Competitive agents

Competitive agents select assets in a series of rounds. In each round, every remaining option (a
tranche of an existing asset or of a candidate asset) is appraised alongside the assets already
selected in previous rounds. A small [dispatch optimisation](#mini-dispatch-optimisation) of the
already-selected assets plus the option determines how the option would operate, and an
[investment metric](#metric-calculation) is then calculated for the option's
[marginal activity](#marginal-activity) according to the agent's objective. The best option is
selected and added to the set of selected assets, and the process repeats until demand is met or no
feasible options remain.

### Commodity prices used in appraisal

Investment appraisal uses two price sets calculated from the previous MSY's dispatch (or from the
previous [ironing-out loop][framework-overview] iteration if enabled):

- **Market prices** \\( \pi\_{c,r,t} \\): used for activity coefficients in the mini dispatch and
  for the investment metric (Cost Index or SNAS).
- **Fallback prices** \\( \phi\_{c,r,t} \\): used to incentivise dispatch in the mini dispatch
  optimisation when market prices alone are insufficient (see
  [Mini Dispatch Optimisation](#mini-dispatch-optimisation)). Calculated using the strategy defined
  by `fallback_pricing_strategy` in [`model.toml`][model-toml].

See [Commodity Prices][prices] for how prices are calculated.

### Capacities of options

#### Existing assets

Existing assets (i.e assets that have already been commissioned, whether via `assets.csv` or by
MUSE) are appraised one tranche at a time to decide how many tranches to retain. This allows partial
retention — for example, some tranches of a multi-tranche plant may be retained while others are
mothballed. Assets with continuous capacity are appraised as a single unit, so they are either
retained or mothballed as a whole.

#### Candidate assets

Before a candidate asset for new investment can be appraised, it is assigned a trial capacity. This
is the capacity of one tranche, or the amount of capacity that can be installed in a single
investment round.

If a process has a defined `tranche_size`, the trial capacity is set to that tranche size. Otherwise,
the tranche size is inferred from the capacity that would satisfy the total annual demand if the
asset operated at its maximum annual rate:

\\[
  \mathrm{TrialCapacity} = \frac{\sum_t \mathrm{Demand}_t}{\mathrm{MaxAnnualSupplyPerCapacity}}
    \\times \mathrm{CapacityTrancheFraction}
\\]

`capacity_tranche_fraction` (set in [`model.toml`][model-toml], must be > 0 and <= 1) controls the
size of inferred investment tranches relative to this demand-based capacity. Lower values produce
smaller tranches and therefore require more investment rounds, while higher values produce larger
tranches.

### Investment constraints

Competitive agents apply the agent's [investment constraints][investment-constraints] one tranche
at a time.

Each selection of a candidate asset tranche counts towards the
\\( \mathrm{MaxInstallableCapacity} \\) of that process. A candidate asset tranche is excluded
from consideration if its capacity would exceed the remaining limit.

Each selection of a candidate asset tranche, or retention of an existing asset tranche, counts
towards the \\( \mathrm{MaxTotalCapacity} \\) of their respective process. Any tranche (whether for
a candidate asset or an existing asset) is excluded from consideration if it would exceed the
remaining limit.

### Mini Dispatch Optimisation

For each supply option being appraised, a small linear programme (LP) is solved to determine the
optimal activity profile. The LP includes every asset selected so far in the current market
(whether existing or candidate) as well as the option being appraised, and is solved against the
market's full demand rather than the demand remaining after earlier selections. The option is
therefore appraised on how it would operate alongside the assets already selected, rather than in
isolation.

#### Activity coefficients

Each asset in the LP has its own activity coefficients, calculated as described below.

The mini dispatch optimisation implicitly frames each time slice as a choice: the asset can either
produce the commodity of interest, or it can be treated as procured from an alternative source at
the **fallback price** \\( \phi_{c,r,t} \\). Each unit of activity produces
\\( f_{c,\mathrm{primary}} \\) units of output, displacing that quantity from the fallback source.
The optimiser therefore dispatches the asset whenever doing so is cheaper than procuring from
elsewhere.

This is captured by the activity coefficient \\( \alpha_t \\), which combines two components:

\\[
  \mathrm{NetOperatingCost}_t = \mathrm{OperatingCost}(t) - \mathrm{RevenueFromFlows}(\pi, t)
\\]

\\[
  \mathrm{FallbackCost} = \phi_{c,r,t} \cdot f_{c,\mathrm{primary}}
\\]

\\[
  \alpha_t = \mathrm{FallbackCost} - \mathrm{NetOperatingCost}_t + \varepsilon
\\]

where \\( \mathrm{RevenueFromFlows} \\) is the sum of all commodity flow revenues and costs (positive
for outputs, negative for inputs) valued at market prices, \\( \mathrm{OperatingCost} \\) is the
variable operating cost plus levies and flow costs, \\( f_{c,\mathrm{primary}} \\) is the primary
output flow coefficient, and \\( \varepsilon \\) is a small positive constant added to ensure that
break-even assets are still dispatched.

**NetOperatingCost** is the net cost of running the asset for one unit of activity at market prices
— negative when the asset is profitable (revenues exceed costs).

**FallbackCost** is the cost of procuring one unit of activity's worth of primary output from an
alternative source at the fallback price.

The asset dispatches when \\( \alpha_t > 0 \\), i.e. when:

\\[
  \mathrm{NetOperatingCost}_t < \mathrm{FallbackCost}
\\]

\\( \phi_{c,r,t} \\) is calculated according to the strategy defined by `fallback_pricing_strategy`
in [`model.toml`][model-toml].

#### Constraints

- **Activity bounds**: for each asset, the sum of activity within each time-slice selection is
  bounded by the asset's availability limits multiplied by its capacity.
- **Demand constraints**: demand for a commodity is balanced at the commodity's defined
  *time-slice level* (e.g. annual, seasonal, or time-slice). The total supply (activity × flow
  coefficient) of all assets in the LP within each balance bucket must not exceed the demand for
  that bucket.
- **Equal utilisation of equivalent assets**: assets with equivalent dispatch properties are
  constrained to have the same utilisation in every time slice, as in the full-system dispatch (see
  [Equal Utilisation of Equivalent Assets][dispatch-equal-utilisation]).

#### Objective

The optimisation maximises the total net revenue across all assets and time slices, subject to the
above constraints:

\\[
  \max \sum_{a} \sum_t \alpha_{a,t} \cdot \mathrm{Activity}_{a,t}
\\]

In addition, seasonal/annual utilisation penalties are included to encourage even spread of
utilisation within and across seasons. These are equivalent to the
[Seasonal/Annual Utilisation Penalties][dispatch-utilisation-penalties] used in the full-system
dispatch optimisation, and are applied to each asset separately.

### Metric Calculation

After the dispatch LP is solved, an investment metric is calculated for the option using **market
prices**. The metric is based on the option's marginal activity, while the fixed costs and market
costs are those of the option itself. Fallback revenue and epsilon are not included in the metric
costs, though they can affect the activity profile used to calculate the metric.

#### Marginal activity

Because the LP includes previously selected assets, the option's activity cannot be read directly
from the solution. Instead, the metric uses the *marginal activity*
\\( \Delta\mathrm{Activity}_t \\): the change in activity, relative to the previous round's
solution, of all assets equivalent to the option (including the option itself):

\\[
  \Delta\mathrm{Activity}_t = \sum_{a \in G} \left(\mathrm{Activity}_{a,t}^{(k)} -
    \mathrm{Activity}_{a,t}^{(k-1)}\right)
\\]

where \\( G \\) is the set of assets in the LP that are equivalent to the option (see
[Equal Utilisation of Equivalent Assets][dispatch-equal-utilisation]), and \\( k \\) is the current
round. Activity in the previous solution is zero for any asset not present in it, so in the first
round the marginal activity is simply the option's activity. All of the marginal activity is
attributed to the option being appraised.

#### Market costs per time slice

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

#### LCOX metric (`objective_type = "lcox"`)

The LCOX metric is calculated as the total annualised cost divided by total annual output, using
the above defined market costs which *exclude* the primary output commodity:

\\[
  \mathrm{LCOXMetric} = \frac{\mathrm{AFC} \times \mathrm{Capacity} + \sum_t
  \Delta\mathrm{Activity}_t \times \mu_t^{\mathrm{LCOX}}}
    {\sum_t \Delta\mathrm{Activity}_t}
\\]

Lower values indicate lower-cost investments.

#### NPV metric (`objective_type = "npv"`)

The NPV metric is based on the Specific Net Annualised Surplus (SNAS). This the net surplus per
unit of activity, using market costs that *include* the primary output commodity:

\\[
  \mathrm{SNAS} = \frac{-\left(\mathrm{AFC} \times \mathrm{Capacity} + \sum_t
    \Delta\mathrm{Activity}_t \times \mu_t^{\mathrm{NPV}}\right)}{\sum_t \Delta\mathrm{Activity}_t}
\\]

Higher values indicate more profitable investments.

> For both metrics, any option with zero total marginal activity after the mini dispatch LP is
> excluded from consideration, as it cannot contribute to meeting demand. This will generally
> happen if all time slices have negative activity coefficients, unless the process has lower-bound
> activity constraints that force activity.

### Asset selection

#### Sorting and tie-breaking

All feasible options are appraised and ranked by their metric. When two options have approximately
equal metrics, the following tie-breaking rules are applied in order:

1. Existing commissioned assets are preferred over new candidates.
2. Among existing assets, newer assets (commissioned more recently) are preferred.
3. If the tie is still unresolved, the first option in the ordering is selected arbitrarily, and a
   `debug`-level log message is emitted.

#### Selection loop

The best-ranked asset is committed and added to the set of selected assets. The solution of its mini
dispatch optimisation is kept as the baseline for calculating
[marginal activity](#marginal-activity) in the next round. The remaining demand is calculated as the
market's demand minus the supply from all selected assets in that solution, and the loop repeats
with the updated set of selected assets. This continues until:

- The remaining demand falls below `remaining_demand_absolute_tolerance`
  (in [`model.toml`][model-toml]), or
- No feasible options remain. In this case, a warning is logged and the loop ends early. The
  unmet demand may still be satisfied during the full system dispatch, but is not guaranteed.

If demand cannot be met at all due to overly restrictive investment constraints, the simulation
terminates with an error.

### Example: Gas Power Plant

The following illustrates how LCOX and NPV metrics are calculated for a gas combined-cycle power
plant, evaluated across two time slices: \\( t_0 \\) (peak) and \\( t_1 \\) (off-peak).

#### Parameters

##### Asset flows and operating costs
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

##### Fixed costs and capacity

| Parameter | Value     |
|-----------|-----------|
| AFC       | £1,000/MW |
| Capacity  | 100 MW    |

##### Market prices

| Commodity | \\( t_0 \\) (Peak) | \\( t_1 \\) (Off-peak) |
| ----------- | -------------------- | ------------------------ |
| Electricity | £90/MWh | £50/MWh |
| Heat | £25/MWh | £15/MWh |
| Natural gas | £35/MWh | £25/MWh |

#### Mini Dispatch Optimisation (identical for LCOX and NPV)

Activity coefficients use market prices:

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

For simplicity, assume this is the first round of selection, so that no assets have been selected
yet and the plant is the only asset in the LP. Suppose the optimiser determines
\\( \mathrm{Activity}_{t_0} = 80 \\) MWh and \\( \mathrm{Activity}_{t_1} = 20 \\) MWh. As there is
no previous solution, the marginal activity equals the activity:
\\( \Delta\mathrm{Activity}_{t_0} = 80 \\) MWh and \\( \Delta\mathrm{Activity}_{t_1} = 20 \\) MWh.
In later rounds, the marginal activity would instead be the change in activity relative to the
previous round's solution.

#### LCOX Metric

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

#### NPV Metric

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

## Portfolio agents

Portfolio agents select all of their assets for a market at once, by solving a single linear
programme (LP). This chooses the capacity and activity of every option together, so as to meet the
agent's share of demand at the lowest total cost.

### Options

The options considered are the agent's [investment options][investment-options]:

- each of the agent's existing assets in the region which have the commodity as their primary
  output, including any mothballed capacity; and
- one candidate asset for each process in the agent's search space for the commodity, region and
  milestone year.

### Decision variables

For each option \\( a \\), the LP has:

- a capacity variable \\( K_a \\), which is continuous; and
- an activity variable \\( A_{a,t} \ge 0 \\) for each time slice \\( t \\).

The capacity variables are bounded as follows:

- **Candidate assets**: \\( 0 \le K_a \le \mathrm{MaxInstallableCapacity}\_p \\), where
  \\( \mathrm{MaxInstallableCapacity}\_p \\) is the agent's
  [addition limit][investment-constraints] for the candidate's process \\( p \\). If the process
  has no addition limit, there is no upper bound.
- **Existing assets**: \\( 0 \le K_a \le \mathrm{Capacity}\_a \\), where
  \\( \mathrm{Capacity}\_a \\) is the asset's current capacity, including any mothballed
  capacity. Existing assets can therefore be reduced in size, but not expanded.

### Optimisation objective

The LP minimises the total annual cost of the selected assets:

\\[
  \min \sum_a \mathrm{AFC}\_a \cdot K_a
    + \sum_a \sum_t \left( \mathrm{OperatingCost}\_{a,t} + \mathrm{InputCost}\_{a,t} \right)
      \cdot A_{a,t}
    + \mathrm{UtilisationPenalties}
\\]

where:

- \\( \mathrm{AFC}\_a \\) is the [annualised fixed cost][annualised-fixed-cost] of option
  \\( a \\), so the capacity of candidate assets incurs capital and fixed O&M costs, while that
  of existing assets incurs only fixed O&M costs;
- \\( \mathrm{OperatingCost}\_{a,t} \\) is the variable operating cost plus levies and flow
  costs, per unit of activity; and
- \\( \mathrm{InputCost}\_{a,t} \\) is the cost of the option's input commodities per unit of
  activity, valued at **market prices** \\( \pi\_{c,r,t} \\):

  \\[
    \mathrm{InputCost}\_{a,t} = \sum\_{c \in \mathrm{inputs}(a)} \left| f\_{a,c} \right|
      \cdot \pi\_{c,r,t}
  \\]

  Inputs without a market price are treated as free.

### Optimisation constraints

- **Activity limits**: for each option and each time-slice selection \\( s \\) for which the
  process defines availability limits, the total activity in the selection must lie within the
  limits per unit of capacity (\\( L\_{a,s} \\) and \\( U\_{a,s} \\)) multiplied by the
  capacity:

  \\[
    L\_{a,s} \cdot K_a \le \sum\_{t \in s} A\_{a,t} \le U\_{a,s} \cdot K_a
  \\]

- **Demand**: for each time-slice selection \\( s \\) at the commodity's time-slice level, the
  total production of the commodity \\( c \\) by all options must equal the agent's
  share of demand:

  \\[
    \sum_a \sum\_{t \in s} f\_{a,c} \cdot A\_{a,t} = \mathrm{Demand}\_{c,s}
  \\]

  where \\( f\_{a,c} \\) is the flow coefficient of the commodity for option \\( a \\). Minimum
  activity limits can make the optimisation infeasible if demand cannot be met exactly.

- **Total capacity limits**: for each process \\( p \\) with a
  [total capacity limit][investment-constraints], the summed capacity of all options for that
  process (the candidate and any existing assets) must not exceed the agent's limit:

  \\[
    \sum\_{a \in p} K_a \le \mathrm{MaxTotalCapacity}\_p
  \\]

### Selecting assets from the solution

Once the LP has been solved, the selected assets are constructed from the capacity variables:

1. Any small negative capacities, caused by solver tolerances, are treated as zero.
2. For assets made up of tranches (including candidates for processes with a `tranche_size`), the
   capacity is rounded **up** to a whole number of tranches. Continuous capacities are used as
   they are.
3. Options with zero capacity are discarded. Existing assets that are discarded, and any capacity
   of existing assets beyond the selected capacity, are
   [mothballed][mothballing].
4. Selected existing assets have any mothballed capacity that is needed reactivated, and selected
   candidate assets are assigned to the agent, to be commissioned.

[investment]: ./investment.md
[investment-options]: ./investment.md#investment-options
[investment-constraints]: ./investment.md#investment-constraints
[annualised-fixed-cost]: ./investment.md#annualised-fixed-cost
[mothballing]: ./investment.md#mothballing-and-decommissioning
[framework-overview]: index.html#framework-overview
[prices]: ./prices.md
[model-toml]: ../file_formats/input_files.md#model-parameters-modeltoml
[agents-csv]: ../file_formats/input_files.md#agentscsv
[agent-commodity-portions-csv]: ../file_formats/input_files.md#agent_commodity_portionscsv
[agent-search-spaces-csv]: ../file_formats/input_files.md#agent_search_spacescsv
[agent-objectives-csv]: ../file_formats/input_files.md#agent_objectivescsv
[dispatch-utilisation-penalties]: ./dispatch_optimisation.md#seasonalannual-utilisation-penalties
[dispatch-equal-utilisation]: ./dispatch_optimisation.md#equal-utilisation-of-equivalent-assets
