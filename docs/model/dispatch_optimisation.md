# Dispatch Optimisation

This section describes the formulation of the dispatch optimisation model in MUSE2. For a given
milestone year (MSY) and region, the dispatch model calculates the least-cost operation of the
energy system assets to satisfy commodity demands. It is solved as a Linear Programme (LP) using the
HiGHS solver.

## Sets and Indices

The energy system is defined using the following sets:

- \\( r \in \mathbf{R} \\): Regions. Represents distinct geographical areas.
- \\( t \in \mathbf{T} \\): Time Slices. Discrete operational sub-periods within a year.
- \\( S \in \mathbf{S} \\): Time Slice Selections. Collections of time slices representing temporal
groupings at different levels (e.g. a single time slice, a season, or the entire year).
- \\( a \in \mathbf{A} \\): Assets. All commissioned production, consumption, or conversion technologies.
- \\( c \in \mathbf{C} \\): Commodities. All energy carriers, emissions, or materials, partitioned into:
  - \\( \mathbf{C}^{\mathrm{SVD}} \\): Service Demand commodities (representing final end-use demands).
  - \\( \mathbf{C}^{\mathrm{SED}} \\): Supply-Equals-Demand commodities (intermediate carrier flows
  like electricity or hydrogen).
  - \\( \mathbf{C}^{\mathrm{OTH}} \\): Other commodities (e.g. emissions or tracked flows not
  subject to supply-demand balancing).

## Decision Variables

The dispatch model determines the following variables:

- \\( act_{a, t} \ge 0 \\): Activity level of asset \\( a \\) during time slice \\( t \\) (in units
of activity, e.g. PJ/year or MW).

## Objective Function

The objective is to minimise the total operating cost of the energy system:

\\[
  \text{Minimise } \sum_{a \in \mathbf{A}} \sum_{t \in \mathbf{T}} act_{a, t} \cdot
  c_{\mathrm{act}}(a, t)
\\]

### Activity Cost Coefficient \\( c_{\mathrm{act}}(a, t) \\)

The cost per unit of activity for asset \\( a \\) in time slice \\( t \\) is composed of variable
operating costs, flow costs, and levies:

\\[
  c\_{\mathrm{act}}(a, t) = \text{VariableOpex}\_a +
  \sum\_{f \in \text{Flows}\_a} |f\_{\mathrm{coeff}}| \cdot
  \left( f\_{\mathrm{cost}} + \mathrm{levy}\_f(t) \right)
\\]

- **VariableOpex**: The non-fuel variable O&M cost of the process.
- \\( f_{\mathrm{coeff}} \\): The flow coefficient for a commodity flow associated with the asset
(positive for output/production, negative for input/consumption).
- \\( f_{\mathrm{cost}} \\): The cost per unit of commodity flow.
- \\( \mathrm{levy}_f(t) \\): The regional levy (or subsidy, if negative) per unit of commodity flow
in time slice \\( t \\).

## Constraints

### Asset Activity Limits

For each asset \\( a \\) and every time slice selection \\( S \\):

\\[
  avail\_{\mathrm{LB}}(a, S) \cdot \Delta\_S \cdot cap\_a \cdot \text{cap2act}\_a \le
  \sum\_{t \in S} act\_{a, t} \le
  avail\_{\mathrm{UB}}(a, S) \cdot \Delta\_S \cdot cap\_a \cdot \text{cap2act}\_a
\\]

where:

- \\( cap_a \\) is the fixed installed capacity of the asset.
- \\( \text{cap2act}_a \\) is the conversion factor from capacity to activity units.
- \\( \Delta_S = \sum_{t \in S} \Delta_t \\) is the total duration of selection \\( S \\) as a
fraction of the year.
- \\( avail_{\mathrm{LB}}(a, S) \\) and \\( avail_{\mathrm{UB}}(a, S) \\) are the lower and upper
availability fractions from `process_activity_limits.csv`, defaulting to \\( 0 \\) and \\( 1 \\)
respectively for any selection not explicitly defined.

### Commodity Balance Constraints

For each balanced commodity \\( c \in \mathbf{C}^{\mathrm{SED}} \cup \mathbf{C}^{\mathrm{SVD}} \\)
in region \\( r \\) and time slice selection \\( S \\) at the commodity's temporal resolution
(`time_slice_level`), the sum of production across all assets minus consumption must satisfy
demands:

\\[
  \sum_{a \in \mathbf{A}(r)} f_{\mathrm{coeff}}(a, c) \cdot
  \sum_{t \in S} act_{a, t} \ge \text{Bound}_{c, r, S}
\\]

where:

- \\( f_{\mathrm{coeff}}(a, c) \\) is the flow coefficient of commodity \\( c \\) for asset
\\( a \\) (positive for outputs, negative for inputs).
- \\( \text{Bound}\_{c, r, S} \\) is the constraint lower bound:
  - For **Service Demand** (`SVD`): \\( \text{Demand}\_{c,r,S} \\)
  - For **Supply-Equals-Demand** (`SED`): \\( 0 \\)

## Shadow Prices

The dual values (shadow prices) of the commodity balance constraints represent the marginal cost of
satisfying an additional unit of demand for that commodity in region \\( r \\) during selection
 \\( S \\). These shadow prices are critical outputs of the dispatch model and are used to seed and
 guide investment appraisal in subsequent steps.

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
  \text{Bound}\_{c, r, S} = \begin{cases}
    \max\left( \text{Demand}\_{c, r, S}\, \epsilon \right) &
      \text{if } c \in \mathbf{C}^{\mathrm{SVD}} \text{ and candidate assets serve } (c, r, S) \\\\
    \epsilon &
      \text{if } c \in \mathbf{C}^{\mathrm{SED}} \text{ and candidate assets serve } (c, r, S) \\\\
    \text{Demand}\_{c, r, S} &
      \text{if } c \in \mathbf{C}^{\mathrm{SVD}} \\\\
    0 &
      \text{if } c \in \mathbf{C}^{\mathrm{SED}}
  \end{cases}
\\]

where \\( \epsilon \\) is the `commodity_balance_epsilon` parameter. The shadow prices from this
candidate dispatch run are then used to seed and guide investment appraisal in subsequent steps.

---

## Diagnosing Infeasible Models

In practice, a dispatch optimisation run can fail if the problem is **infeasible** — typically
because the installed asset capacity in the region is insufficient to meet the required exogenous or
intermediate commodity demands.

To help debug and pinpoint the exact source of failure, MUSE2 employs a diagnostic mechanism using
**unmet demand variables**:

1. **First-Pass Run:** MUSE2 first attempts to solve the dispatch model in its standard form
(without unmet demand variables).
2. **Diagnostic Re-Run:** If the solver reports that the problem is infeasible, MUSE2 automatically
spawns a second, diagnostic dispatch run. In this run, a set of slack variables representing unmet
demand, \\( UnmetD_{c, r, t} \ge 0 \\), is added to the commodity balance constraints:
   \\[
     \sum_{a \in \mathbf{A}(r)} f_{\mathrm{coeff}}(a, c) \cdot \sum_{t \in S} act_{a, t} +
      \sum_{t \in S} UnmetD_{c, r, t} \ge \text{Bound}_{c, r, S}
   \\]
3. **Objective Penalty:** To ensure the solver only leaves demand unmet if it is physically
impossible to satisfy it, these variables are heavily penalised in the diagnostic objective function
using the `value_of_lost_load` parameter (\\( \mathrm{VoLL} \\)):
   \\[
     \text{Minimise } \sum\_{a \in \mathbf{A}} \sum\_{t \in \mathbf{T}} act\_{a, t} \cdot
      c\_{\mathrm{act}}(a, t) +
    \mathrm{VoLL} \cdot \sum\_{c, r, t} UnmetD\_{c, r, t}
   \\]
4. **Isolating Shortfalls:** The addition of \\( UnmetD_{c, r, t} \\) guarantees that the LP remains
mathematically feasible. When solved, any time slice, region, or commodity with a shortfall
will have \\( UnmetD_{c, r, t} > 0 \\).
5. **Error Reporting:** MUSE2 scans the solution, identifies all balanced markets \\( (c, r) \\)
where unmet demand occurred, outputs detailed diagnostic CSV files, and aborts the simulation with
an error identifying the exact out-of-balance markets.
