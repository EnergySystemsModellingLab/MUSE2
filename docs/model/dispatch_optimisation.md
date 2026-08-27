# Dispatch Optimisation

This section describes the formulation of the dispatch optimisation model in MUSE2. For a given
milestone year (MSY), the dispatch model calculates the least-cost operation of the
energy system assets to satisfy commodity demands. It is solved as a Linear Programme (LP) using the
HiGHS solver.

## Sets and Indices

The energy system is defined using the following sets:

- \\( r \in \mathbf{R} \\): Regions. Represents distinct geographical areas.
- \\( t \in \mathbf{T} \\): Time Slices. Discrete operational sub-periods within a year.
- \\( s \in \mathbf{S} \\): Time Slice Selections. Collections of time slices representing temporal
groupings at different levels (e.g. a single time slice, a season, or the entire year).
- \\( a \in \mathbf{A} \\): Assets. All commissioned production, consumption, or conversion technologies.
- \\( c \in \mathbf{C} \\): Commodities. All energy carriers, emissions, or materials, partitioned into:
  - \\( \mathbf{C}^{\mathrm{SVD}} \\): Service Demand commodities (representing final end-use demands).
  - \\( \mathbf{C}^{\mathrm{SED}} \\): Supply-Equals-Demand commodities (intermediate carrier flows
  like electricity or hydrogen).
  - \\( \mathbf{C}^{\mathrm{OTH}} \\): Other commodities (e.g. emissions or tracked flows not
  subject to supply-demand balancing).

## Decision Variables

The dispatch model determines the activity level of all assets \\( a \in \mathbf{A} \\) in all time
slices \\( t \in \mathbf{T}\\):

\\[
  \mathrm{Activity}_{a, t} \ge 0 \text{ for all } a \in \mathbf{A}, t \in \mathbf{T}
\\]

Activity is a dimensionless quantity representing the level of operation of the asset. It is
multiplied by flow coefficients to give the corresponding commodity flows in the units of that
commodity (e.g. PJ of energy for an energy flow).

## Objective Function

The objective is to minimise the total operating cost of the energy system:

\\[
  \text{Minimise } \sum\_{a \in \mathbf{A}} \sum\_{t \in \mathbf{T}} \mathrm{Activity}\_{a, t} \cdot
  \mathrm{Cost}\_{\mathrm{Activity},a,t}
\\]

### Activity Cost Coefficient \\( \mathrm{Cost}_{\mathrm{Activity},a,t} \\)

The cost per unit of activity for asset \\( a \\) in time slice \\( t \\) is composed of variable
operating costs, flow costs, and levies:

\\[
  \mathrm{Cost}\_{\mathrm{Activity},a,t} = \mathrm{VariableOpex}\_a +
  \sum\_{f \in \mathrm{Flows}\_a} |f\_{\mathrm{coeff}}| \cdot
  \left( f\_{\mathrm{cost}} + \mathrm{Levy}\_{f,t} \right)
\\]

- \\( \mathrm{VariableOpex} \\): The non-fuel variable O&M cost of the process.
- \\( f_{\mathrm{coeff}} \\): The flow coefficient for a commodity flow associated with the asset
(positive for output/production, negative for input/consumption).
- \\( f_{\mathrm{cost}} \\): The cost per unit of commodity flow.
- \\( \mathrm{Levy}_{f,t} \\): The regional levy (or subsidy, if negative) per unit of commodity flow
in time slice \\( t \\).

## Constraints

### Asset Activity Limits

For each asset \\( a \\) and every time slice selection \\( s \\):

\\[
  \mathrm{Avail}\_{\mathrm{LB},a,s} \cdot \Delta\_s \cdot \mathrm{Capacity}\_a \cdot \mathrm{cap2act}\_a
  \le \sum\_{t \in s} \mathrm{Activity}\_{a, t} \le
  \mathrm{Avail}\_{\mathrm{UB},a,s} \cdot \Delta\_s \cdot \mathrm{Capacity}\_a \cdot \mathrm{cap2act}\_a
\\]

where:

- \\( \mathrm{Capacity}_a \\) is the fixed installed capacity of the asset.
- \\( \mathrm{cap2act}_a \\) is the conversion factor from capacity to activity units.
- \\( \Delta_s = \sum_{t \in s} \Delta_t \\) is the total duration of selection \\( s \\) as a
fraction of the year.
- \\( \mathrm{Avail}\_{\mathrm{LB},a,s} \\) and \\( \mathrm{Avail}\_{\mathrm{UB},a,s} \\) are the
lower and upper availability fractions from `process_activity_limits.csv`, defaulting to
\\( 0 \\) and \\( 1 \\) respectively for any selection not explicitly defined.

### Equal Utilisation of Equivalent Assets

To avoid arbitrarily utilising one asset over another, the dispatch model adds additional
constraints to equalise the utilisation of assets with equivalent dispatch properties. Assets are
considered equivalent when they are in the same region and have the same variable operating cost,
activity limits, and commodity flows. Their capacity, state, and process identity are not considered
when determining equivalence.

For an asset \\(a\\) in time slice \\(t\\), utilisation is defined as

\\[
  \\mathrm{Utilisation}\_{a,t} =
  \\frac{\\mathrm{Activity}\_{a,t}}{\\mathrm{Capacity}_a \\cdot \\mathrm{cap2act}_a}
\\]

For every pair of equivalent assets \\( x \\) and \\( y \\), and for every time slice \\( t \\),
the optimisation model imposes:

\\[
  \\mathrm{Utilisation}\_{x,t} = \\mathrm{Utilisation}\_{y,t}
\\]

### Commodity Balance Constraints

For each balanced commodity \\( c \in \mathbf{C}^{\mathrm{SED}} \cup \mathbf{C}^{\mathrm{SVD}} \\)
in region \\( r \\) and time slice selection \\( s \\) at the commodity's temporal resolution
(`time_slice_level`), the sum of production across all assets minus consumption must satisfy
demands:

\\[
  \sum_{a \in \mathbf{A}\_r} f_{\mathrm{coeff},a,c} \cdot
  \sum_{t \in s} \mathrm{Activity}_{a, t} \ge \mathrm{Bound}\_{c, r, s}
\\]

where:

- \\( f_{\mathrm{coeff},a,c} \\) is the flow coefficient of commodity \\( c \\) for asset
\\( a \\) (positive for outputs, negative for inputs).
- \\( \mathrm{Bound}\_{c, r, s} \\) is the constraint lower bound:
  - For **Service Demand** (`SVD`): \\( \mathrm{Demand}\_{c, r, s} \\)
  - For **Supply-Equals-Demand** (`SED`): \\( 0 \\)

### Commodity Consumption/Production Constraints

Commodity constraints impose lower and upper limits on the total production or consumption of a
commodity in a region over a specified time slice selection:

\\[
  L\_{c,r,s} \leq
  \sum\_{a \in \mathbf{A}\_r^d} |f\_{\mathrm{coeff},a,c}| \cdot
  \sum\_{t \in s} \mathrm{Activity}\_{a,t}
  \leq U\_{c,r,s}
\\]

where:

- \\( d \\) is the balance type: production (`prod`) or consumption (`cons`).
- \\( \mathbf{A}\_r^d \\) contains assets in region \\( r \\) with flows in direction \\( d \\).
- \\( L\_{c,r,s} \\) and \\( U\_{c,r,s} \\) are the lower and upper limits.

These constraints are defined in the optional `commodity_constraints.csv` file. They can apply to
`SED` and `OTH` commodities, but not `SVD` commodities. The feature is experimental and requires
`please_give_me_broken_results = true` in `model.toml`.

## Shadow Prices

The dual values (shadow prices) of the commodity balance constraints represent the marginal cost of
satisfying an additional unit of demand for that commodity in region \\( r \\) during selection
 \\( s \\). These shadow prices are critical outputs of the dispatch model and are used to seed and
 guide investment appraisal in subsequent steps.

---

## Seasonal/Annual Utilisation Penalties

MUSE2 optionally applies small penalties to the peak capacity required by each asset within a season
and across the whole year. These penalties encourage activity to be distributed across time slices
within a season and across seasons, respectively. They are particularly useful when commodities are
balanced at the seasonal/annual levels and the balance constraint otherwise leaves the
intra-seasonal or inter-seasonal production profile undetermined. In real-world terms, this
represents a preference to avoid concentrating an asset's operation into short periods of high
utilisation, which may reduce cycling, wear, start-up requirements, or the need to maintain capacity
for seasonal peaks.

For asset \\(a\\) and time slice \\(t\\), the capacity required to support its activity is

\\[
  \mathrm{RequiredCapacity}\_{a,t} =
  \frac{\mathrm{Activity}\_{a,t}}
  {\mathrm{cap2act}\_a \cdot \Delta\_t}
\\]

For each asset and season, MUSE2 introduces an auxiliary variable \\(U_{a,s}\\), representing the
peak capacity required by the asset during that season. It is constrained by

\\[
  U_{a,s} \geq \mathrm{RequiredCapacity}_{a,t}
\\]

for every time slice \\(t\\) in season \\(s\\). In addition, MUSE2 introduces an auxiliary variable
\\(U_{a,\\mathrm{annual}}\\), representing the greatest seasonal peak capacity required by the asset
during the year. It is constrained by

\\[
  U_{a,\\mathrm{annual}} \\geq U_{a,s}
\\]

for every season \\(s\\).

When enabled, the penalties add the following term to the optimisation objective:

\\[
  \\lambda_{\\mathrm{seasonal}}
  \\sum_{a \\in \\mathbf{A}} \\sum_{s \\in \\mathbf{S}}
  \\Delta_s U_{a,s}
  +
  \\lambda_{\\mathrm{annual}}
  \\sum_{a \\in \\mathbf{A}} U_{a,\\mathrm{annual}}
\\]

Here, \\(\\lambda_{\\mathrm{seasonal}}\\) and \\(\\lambda_{\\mathrm{annual}}\\) are set by the
`seasonal_utilisation_penalty` and `annual_utilisation_penalty` model parameters, respectively.
Setting either parameter to zero disables its corresponding penalty. The seasonal parameter controls
how strongly activity is spread within seasons, while the annual parameter controls how strongly it
is spread across seasons. Both are weighted objective terms, so their values should be small enough
that smoothing dispatch does not outweigh meaningful differences in operating cost (default for
both = `1e-6`).

---

## Candidate Dispatch Run

After the primary dispatch run, MUSE2 performs a second dispatch run that includes
**candidate assets** — technologies not yet commissioned but available for investment — alongside
the existing assets. The purpose of this run is to obtain shadow prices for commodities that are
not consumed or produced in the primary dispatch solution.

In the primary dispatch, a commodity balance constraint will only have a non-zero shadow price if
the corresponding constraint is binding (i.e. supply exactly meets demand). For a commodity that is
neither produced nor consumed by any active asset, the constraint is trivially satisfied and its
dual value is zero. This gives no signal to the investment model about the potential value of new
capacity for that commodity.

By including candidate assets in the second run, MUSE2 ensures that every commodity served by at
least one candidate process has a balance constraint that can be binding, yielding a meaningful
shadow price. To guarantee a non-zero shadow price even when there is no existing demand for the
commodity, a small epsilon is added to the lower bound of commodity balance constraints where
candidate assets are present:

\\[
  \mathrm{Bound}\_{c, r, s} = \begin{cases}
    \max\left( \mathrm{Demand}\_{c, r, s}\, \epsilon \right) &
      \text{if } c \in \mathbf{C}^{\mathrm{SVD}} \text{ and candidate assets serve } (c, r, s) \\\\
    \epsilon &
      \text{if } c \in \mathbf{C}^{\mathrm{SED}} \text{ and candidate assets serve } (c, r, s) \\\\
    \mathrm{Demand}\_{c, r, s} &
      \text{if } c \in \mathbf{C}^{\mathrm{SVD}} \\\\
    0 &
      \text{if } c \in \mathbf{C}^{\mathrm{SED}}
  \end{cases}
\\]

where \\( \epsilon \\) is the `commodity_balance_epsilon` parameter. The shadow prices from this
candidate dispatch run are then used to seed and guide investment appraisal in subsequent steps.

---

## Diagnosing Infeasible Models

In practice, a dispatch optimisation run may be **infeasible** for several reasons, such as
insufficient installed asset capacity to meet demand or incompatible explicit commodity constraints.
When this occurs, MUSE2 performs additional dispatch runs with modified optimisation problems to
help identify the cause. The resulting diagnostic information is included in error messages and
saved in the dispatch debug files.

### Unmet Demand Diagnostic

1. **Diagnostic Re-Run:** MUSE2 reruns the dispatch optimisation with a set of slack variables
representing unmet demand, \\( \\mathrm{UnmetD}\_{c, r, t} \\ge 0 \\), added to the commodity balance
constraints:
   \\[
     \sum_{a \in \mathbf{A}\_r} f\_{\mathrm{coeff},a,c} \cdot \sum\_{t \in s}
     \mathrm{Activity}\_{a, t} +
     \sum\_{t \in s} \mathrm{UnmetD}\_{c, r, t} \ge \mathrm{Bound}\_{c, r, s}
   \\]
1. **Objective Penalty:** To ensure the solver only leaves demand unmet if it is mathematically
impossible to satisfy it, these variables are heavily penalised in the diagnostic objective function
using the `value_of_lost_load` parameter (\\( \mathrm{VoLL} \\)):
   \\[
     \text{Minimise } \sum\_{a \in \mathbf{A}} \sum\_{t \in \mathbf{T}} \mathrm{Activity}\_{a, t} \cdot
      \mathrm{Cost}\_{\mathrm{Activity},a,t} +
    \mathrm{VoLL} \cdot \sum\_{c, r, t} \mathrm{UnmetD}\_{c, r, t}
   \\]
1. **Isolating Shortfalls:** The addition of \\( \mathrm{UnmetD}\_{c, r, t} \\) guarantees that the
LP remains mathematically feasible. When solved, any time slice, region, or commodity with a
shortfall will have \\( \mathrm{UnmetD}_{c, r, t} > 0 \\).
1. **Error Reporting:** MUSE2 scans the solution, identifies all balanced markets \\( (c, r) \\)
where unmet demand occurred, outputs detailed diagnostic CSV files, and aborts the simulation with
an error identifying the exact out-of-balance markets.

### Commodity Constraints Diagnostic

If the dispatch optimisation remains infeasible, MUSE2 reruns it with the explicit commodity
constraints disabled. If this rerun succeeds, the infeasibility is likely caused by one or more
constraints defined in `commodity_constraints.csv`.

If the rerun remains infeasible, commodity constraints are not identified as the cause. Since
commodity constraints are an experimental feature, this diagnosis should be treated as indicative
rather than definitive.
