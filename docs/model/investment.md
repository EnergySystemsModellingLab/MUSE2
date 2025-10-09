# Investment Appraisal Approach

<!-- markdownlint-disable MD049 -->

This section details the investment and asset retention decision process, which is applied within
step 2 of the [overall MUSE2 workflow]. This process determines which new assets to build and
which existing assets to retain to meet system needs over time. In the overall workflow, dispatch
optimisation is used to identify *physical needs* by quantifying demand profiles for commodities of
interest.

The economic evaluation and selection of all supply options—new candidate assets (\\( ca \\)) and
contributions from existing assets (\\( ex \\))—consistently use prices formed in the *previous*
milestone year (\\( \pi_{prevMSY} \\)). This approach models investment and retention decisions as
being based on recent, known economic conditions while responding to immediate commodity demands. A
core assumption is that all commodities, except specific user-identified SVD commodities, have
reliable \\( \pi_{prevMSY} \\) values for these economic evaluations.

## Data for economic evaluation of supply options

This data provides the primary economic context for appraisal calculations, drawn from the settled
state of the preceding milestone year.

- Previous MSY prices: \\( \pi_{prevMSY}[c,r,t] \\). When `pricing_strategy` is `shadow_prices`,
  these are the shadow prices for each commodity \\( c \\), in each region \\( r \\), for each time
  slice \\( t \\), taken from the final dispatch of the preceding MSY. When the `pricing_strategy`
  option is set to `scarcity_adjusted`, these are the shadow prices for each commodity adjusted to
  remove the impact of binding capacity constraints.

- Previous MSY reduced costs for new candidates: \\( rc_{prevMSY}[ca,r,t] \\). If candidate assets
  \\( ca \\) were included (at zero capacity) in the \\( \text{MSY}\_{prev} \\)’s final dispatch run,
  their reduced costs were generated and can be used as a measure of their profitability in \\(
  \text{MSY}\_{next} \\).

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

- **Determine candidate asset reduced cost or equivalent for existing assets:** For candidate
  assets, if `pricing_strategy` is `scarcity_adjusted`, candidate asset reduced costs are as
  provided by the solver. If `pricing_strategy` is `shadow_prices`, candidate asset reduced costs
  must be calculated as follows:

  \\[
    \begin{aligned}
          RC^*\_{ca,r,t} = RC\_{ca,r,t}
            &- \sum\_{c} \Big( input\_{\text{coeff}}[ca,c] - output\_{\text{coeff}}[ca,c] \Big)
              \cdot \lambda\_{c,r,t} \\\\
            &+ \sum\_{c} \Big( input\_{\text{coeff}}[ca,c] - output\_{\text{coeff}}[ca,c] \Big)
              \cdot \lambda^\*\_{c,r,t}
    \end{aligned}
  \\]

  Where \\( RC^\*\_{ca,r,t} \\) is the adjusted reduced cost, \\( RC\_{ca,r,t} \\) is the
  solver-provided reduced cost, \\( \lambda\_{c,r,t} \\) is the solver-provided commodity shadow
  price, and \\( \lambda^\*\_{c,r,t} \\) is the adjusted commodity price (which removes the impact
  of scarcity pricing).

  For existing assets, an equivalent to reduced cost is calculated as follows:

  \\[
    \begin{aligned}
          RC^\*\_{ex,r,t} = & \quad cost\_{\text{var}}[ex,r,t] \\\\
            &+ \sum\_{c} \Big( cost\_{\text{input}}[ex,c] \cdot input\_{\text{coeff}}[ex,c]
              + cost\_{\text{output}}[ex,c] \cdot output\_{\text{coeff}}[ex,c] \Big) \\\\
            &+ \sum\_{c} \Big( input\_{\text{coeff}}[ex,c] - output\_{\text{coeff}}[ex,c] \Big)
              \cdot \lambda^\*\_{c,r,t} \\\\
            &+ \sum\_{s,c} in\\_scope[s] \cdot \Big\\{ \\\\
            &\quad \quad (cost\_{\text{prod}}[s,c] - \mu\_{s,c}^{\text{prod}})
              \cdot output\_{\text{coeff}}[ex,c] \\\\
            &\quad \quad + (cost\_{\text{cons}}[s,c] - \mu\_{s,c}^{\text{cons}})
              \cdot input\_{\text{coeff}}[ex,c] \\\\
            &\quad \quad + (cost\_{\text{net}}[s,c] - \mu\_{s,c}^{\text{net}})
              \cdot (output\_{\text{coeff}}[ex,c] - input\_{\text{coeff}}[ex,c]) \\\\
            &\Big\\}
    \end{aligned}
  \\]

  Where \\( RC^\*\_{ex,r,t} \\) is the marginal surplus, \\( \lambda^\*\_{c,r,t} \\) is the adjusted
  commodity price (which removes the impact of scarcity pricing) or the solver-provided shadow price
  (including scarcity pricing) as appropriate.

  For the case of LCOX objective, \\( RC^\*\_{ex,r,t} \\) must also be adjusted to remove the prices
  of non-priced commodities, and the price of the commodity of interest. For these commodities \\(
  \lambda^\*\_{c,r,t} \\) and \\( \lambda\_{c,r,t} \\) are set to zero, and \\( RC^\*\_{ex,r,t} \\)
  adjusted as a result.

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
agents’ serving commodity \\( c \\). This method iteratively builds a supply portfolio by selecting
options that offer the highest annualised profit for serving the current commodity demand. The
economic evaluation uses \\( \pi_{prevMSY} \\) prices and takes account of asset-specific
operational constraints (e.g., minimum load levels) and the balance level of the target commodity
(time slice profile, seasonal or annual).

- **Choose capacity and dispatch to maximise annualised profit:** Solve a small optimisation
  sub-problem to maximise the asset’s surplus, subject to its operational rules and the specific
  demand tranche it is being asked to serve. Define \\( SurplusPerAct_{opt,t} = - RC^*_{opt,r,t}
  \\).

  \\[
    maximise \Big\\{ -AFC\_{opt,r}\*cap_{opt,r} + \sum\_{t} act\_{opt,t}\*SurplusPerAct_{opt,t}
    \Big\\}
  \\]

  Where \\( cap_{opt,r} \\) and \\( act_{opt,t} \\) are decision variables, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_{opt,t} \\).

  - A demand constraint, where output cannot exceed demand in the tranche, which adapts based on the
    commodity’s balance level (time slice, season, annual).

  - Capacity is constrained \\( <CapMaxBuild_{opt,r} \\) for candidates, and to known capacity for
    existing assets.

- **Calculate a profitability index:** This is the total annualised surplus (\\( \sum_{t}
  act_{opt,t}*SurplusPerAct_{opt,t} \\)) divided by the annualised capital cost (\\(
  AFC_{opt,r}*cap_{opt,r} \\)).

- **Save information:** Save \\( opt \\) information. If this is the last \\( opt \\) then exit this
  loop.

#### Tool B: LCOX

This method is used when decision rule is single objective and objective is LCOX for agents’ serving
commodity \\( c \\). This method constructs a supply portfolio (from new candidates \\( ca \\), new
import infrastructure \\( ca_{import} \\), and available existing assets \\( ex \\)) to meet target
\\( U_{c} \\) at the lowest cost for the investor. As above, the appraisal for each option
explicitly accounts for its own operational constraints and adapts based on the \\( balance\_level
\\) of \\( c \\). Inputs and outputs for all options are valued using prices from the previous
milestone year (\\( \pi_{prevMSY} \\)), for priced commodities. Inputs and outputs for unpriced
commodities are set to zero, and the commodity of interest is assumed to have zero value.

- **Choose capacity and dispatch to minimise annualised cost:** Solve a small optimisation
  sub-problem to maximise the asset’s surplus, subject to its operational rules and the specific
  demand tranche it is being asked to serve. Define \\( CostPerAct_{opt,t} = RC^*_{opt,r,t} \\).

  \\[
    minimise \Big\\{
      AFC\_{opt,r}\*cap\_{opt,r} + \sum\_{t} act\_{opt,t}\*CostPerAct\_{opt,t} + VoLL*UnmetD\_{r,c,t}
    \Big\\}
  \\]

  Where \\( cap_{opt,r} \\) and \\( act_{opt,t} \\) are decision variables, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_{opt,t} \\).

  - A demand constraint, where output from the asset plus VoLL-related outputs must equal demand in
    each timeslice of the tranche, which adapts based on the commodity’s balance level (time slice,
    season, annual).

  - Capacity is constrained \\( <CapMaxBuild_{opt,r} \\) for candidates, and to known capacity for
    existing assets.

  - VoLL variables are active to ensure a feasible solution alongside maximum operation of the
    asset.

- **Calculate a cost index:** This is the total annualised cost (\\(
  AFC_{opt,r}*cap_{opt,r}+\sum_{t} act_{opt,t}*CostPerAct_{opt,t} \\)), divided by the annual output
  \\( \sum_{t} act_{opt,t} \\).

- **Save information:** Save \\( opt \\) information. If this is the last \\( opt \\) then exit this
  loop.

[overall MUSE2 workflow]: ./model_description.md#framework-overview
[Dispatch Optimisation Formulation]: ./dispatch_optimisation.md
