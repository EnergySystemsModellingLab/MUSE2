# Investment Appraisal Approach

<!-- markdownlint-disable MD049 -->

This section details the investment and asset retention decision process, which is applied within
step 2 of the [overall MUSE2 workflow][framework-overview]. This process determines which new assets
to build and which existing assets to retain to meet system needs over time. In the overall
workflow, dispatch optimisation is used to identify *physical needs* by quantifying demand profiles
for commodities of interest.

## Commodity prices

The economic evaluation and selection of all supply options — new candidate assets and
contributions from existing assets — consistently use prices formed in the *previous*
milestone year (\\( \lambda\_{c,r,t} \\)). This approach models investment and retention
decisions as being based on recent, known economic conditions while responding to immediate
commodity demands. A core assumption is that all commodities, except specific user-identified SVD
commodities, have reliable \\( \lambda\_{c,r,t} \\) values for these economic evaluations.

When `pricing_strategy` is `shadow_prices`, these are the shadow prices for each commodity
\\( c \\), in each region \\( r \\), for each time slice \\( t \\), taken from the final dispatch of
the preceding MSY. When the `pricing_strategy` option is set to `scarcity_adjusted`, these are the
shadow prices for each commodity adjusted to remove the impact of binding capacity constraints.

Note: there is an option to iterate over each year so that investment decisions are based on
equilibrium prices in the _current year_, in what's referred to as the "[ironing-out loop][framework-overview]".
In this case, \\( \lambda\_{c,r,t} \\) will reflect prices from previous iteration of the
ironing-out loop.

## Candidate and existing asset data

Asset economic data for investment appraisal calculations, drawn from user inputs and previous
investments.

- For all assets:

  - All relevant operational parameters for \\( opt \\) as defined in [Dispatch Optimisation
    Formulation] (e.g., availability \\( avail_{UB} \\), variable costs \\( cost_{var} \\), yield
    coefficients \\( output_{coeff} \\), etc.).

  - \\( \text{FOM}_{opt,r} \\): Annual fixed Operations & Maintenance costs per unit of capacity for
    \\( opt \\) in \\( r \\).

  - \\( FinancingInDecomDec_{ex} \\) (binary flag). This user-defined option specifies whether to
    include estimated financing costs in the economic viability threshold when considering the
    decommissioning of an existing asset. This can only be used on profit-evaluable assets. Used
    with \\( PercentDebt_{ex} \\). Where financing costs are included, the percentage debt is
    multiplied by the original capex, and the result is annualised.

- For new candidate assets:

  - \\( \text{CAPEX}_{ca,r} \\): Upfront capital expenditure required per unit of new capacity for
    candidate \\( ca \\) in region \\( r \\).

  - \\( \text{Life}_{ca} \\): Expected operational lifetime of the new asset \\( ca \\) (in years).

  - \\( \text{WACC}_{ca} \\): Weighted Average Cost of Capital (discount rate) used for appraising
    candidate \\( ca \\).

  - \\( CapMaxBuild_{ca,r} \\): Maximum buildable new capacity for candidate asset type \\( ca \\)
    in region \\( r \\) during this MSY investment phase (an exogenous physical, resource, or policy
    limit).

## Investment Appraisal

The main MUSE2 workflow invokes the portfolio construction methods detailed in tools A and B
below. These tools select the best asset from the pool of candidate and existing assets, thereby
providing investment and dynamic decommissioning decisions.

### Pre-calculation of metrics for each supply option

> Note: This section contains a reference to "scopes", a feature that is not yet implemented

- Annualised fixed costs per unit of capacity (\\( AFC_{opt,r} \\)): For new candidates, this is
  their annualised CAPEX plus FOM. For existing assets, the relevant fixed cost is its FOM.

- Costs per unit of activity in each time slice, calculated as follows:

  \\[
    \begin{aligned}
          AC_t = & \quad cost\_{\text{var}}[t] \\\\
            &+ \sum\_{c} \Big( cost\_{\text{input}}[c] \cdot input\_{\text{coeff}}[c]
              + cost\_{\text{output}}[c] \cdot output\_{\text{coeff}}[c] \Big) \\\\
            &+ \sum\_{c} \Big( input\_{\text{coeff}}[c] - output\_{\text{coeff}}[c] \Big)
              \cdot \lambda\_{c,r,t} \\\\
            &+ \sum\_{s,c} in\\_scope[s] \cdot \Big\\{ \\\\
            &\quad \quad (cost\_{\text{prod}}[s,c] - \mu\_{s,c}^{\text{prod}})
              \cdot output\_{\text{coeff}}[c] \\\\
            &\quad \quad + (cost\_{\text{cons}}[s,c] - \mu\_{s,c}^{\text{cons}})
              \cdot input\_{\text{coeff}}[c] \\\\
            &\quad \quad + (cost\_{\text{net}}[s,c] - \mu\_{s,c}^{\text{net}})
              \cdot (output\_{\text{coeff}}[c] - input\_{\text{coeff}}[c]) \\\\
            &\Big\\}
    \end{aligned}
  \\]

  When using the LCOX objective, the calculation is adjusted to exclude the commodity of interest
  (\\( \lambda\_{c,r,t} \\) are set to zero).

### Initialise demand profiles for commodity of interest

- Initialise \\( D[c,t] \\) from the MSY dispatch run output \\( U_c \\).

- We break down the demand profile into tranches. The first tranche for investment consideration is
  that with the highest load factor. The size of this tranche is the overall peak demand divided by
  an input parameter (which can vary between 2 and 6). This assumption should be revisited!

### Iteratively construct asset portfolio to meet target \\( U_c \\)

> Note: The current implementation of MUSE2 doesn't use tranches

1. Start with the first tranche of the demand.

2. Loop over available options \\( opt \\) (new or existing or import), applying either tool A or B
   to check the supply option.

3. Result includes all options \\( opt^\* \\) (new or existing or import) from which we select the
   one that is the best. The related capacity to commit is returned from the tool, as is its
   production profile related to the tranche. Save key information, including investment/retention
   metric for all options, even the ones not invested/retained.

4. \\( D[c] \\) is updated to remove the production profile of the committed asset. The next tranche
   profile is then calculated (note that \\( opt^\* \\) may not serve all demand in the current
   tranche).

5. Keep going until there is no \\( D[c] \\) left. Will need to handle a situation where we run out
   of candidate and existing assets and demand is still present.

### Tools

#### Tool A: NPV

This method is used when decision rule is single objective and objective is annualised profit for
agents' serving commodity \\( c \\). This method iteratively builds a supply portfolio by selecting
options that offer the highest annualised profit for serving the current commodity demand. The
economic evaluation uses \\( \pi_{prevMSY} \\) prices and takes account of asset-specific
operational constraints (e.g., minimum load levels) and the balance level of the target commodity
(time slice profile, seasonal or annual). For each asset option:

- **Optimise capacity and dispatch to maximise annualised profit:** Solve a small optimisation
  sub-problem to maximise the asset's surplus, subject to its operational rules and the specific
  demand tranche it is being asked to serve. \\(\varepsilon \approx 1×10^{-14}\\) is added to each
  \\(AC_t \\) to allow assets which are breakeven (or very close to breakeven) to be dispatched.

  \\[
    maximise \Big\\{ - \sum_t act_t \* (AC_t + \varepsilon)
    \Big\\}
  \\]

  Where \\( act_t \\) is a decision variable, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_t \\).

  - A demand constraint, where output cannot exceed demand in the tranche, which adapts based on the
    commodity's balance level (time slice, season, annual).

  - Capacity is constrained to \\( CapMaxBuild \\) for candidates, and to known capacity for
    existing assets.

- **Calculate a profitability index:** This is the total annualised surplus (\\( - \sum_t
  act_t \* AC \\)) divided by the annualised fixed cost (\\(
  AFC \* cap \\)).

#### Tool B: LCOX

This method is used when decision rule is single objective and objective is LCOX for agents' serving
commodity \\( c \\). This method constructs a supply portfolio (from new candidates \\( ca \\), new
import infrastructure \\( ca_{import} \\), and available existing assets \\( ex \\)) to meet target
\\( U_{c} \\) at the lowest cost for the investor. As above, the appraisal for each option
explicitly accounts for its own operational constraints and adapts based on the \\( balance\_level
\\) of \\( c \\). Inputs and outputs for all options are valued using prices from the previous
milestone year (\\( \pi_{prevMSY} \\)), for priced commodities. Inputs and outputs for unpriced
commodities are set to zero, and the commodity of interest is assumed to have zero value.
For each asset option:

- **Optimise capacity and dispatch to minimise annualised cost:** Solve a small optimisation
  sub-problem to maximise the asset's surplus, subject to its operational rules and the specific
  demand tranche it is being asked to serve.

  \\[
    minimise \Big\\{
      AF \* cap + \sum_t act_t \* AC_t + VoLL \* UnmetD_t
    \Big\\}
  \\]

  Where \\( cap \\) and \\( act_t \\) are decision variables, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_t \\).

  - A demand constraint, where output from the asset plus VoLL-related outputs must equal demand in
    each timeslice of the tranche, which adapts based on the commodity's balance level (time slice,
    season, annual).

  - Capacity is constrained to \\( CapMaxBuild \\) for candidates, and to known capacity for
    existing assets.

  - VoLL variables are active to ensure a feasible solution alongside maximum operation of the
    asset.

- **Calculate a cost index:** This is the total annualised cost (\\(
  AFC \* cap_r + \sum_{t} act_t \* AC_t \\)), divided by the annual output
  \\( \sum_t act_t \\).

[framework-overview]: https://energysystemsmodellinglab.github.io/MUSE2/model/index.html#framework-overview
[Dispatch Optimisation Formulation]: ./dispatch_optimisation.md
