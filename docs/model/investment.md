# Investment Appraisal Approach

<!-- markdownlint-disable MD049 -->

This section details the investment and asset retention decision process, which is applied within
step 2 of the [overall MUSE2 workflow]. This process determines which new assets to build and
which existing assets to retain to meet system needs over time. In the overall workflow, dispatch
optimisation is used to identify *physical needs* by quantifying demand profiles for commodities of
interest.

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
equilibrium prices in the _current year_, in what's referred to as the "[ironing-out loop]".
In this case, \\( \lambda\_{c,r,t} \\) will reflect prices from the previous iteration of the
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

- Annualised fixed costs per unit of capacity (\\( AFC_{opt,r} \\)): For new candidates, this is
  their annualised CAPEX plus FOM. For existing assets, the relevant fixed cost is its FOM.

- Calculate net revenue per unit of activity \\(AC_{t}^{NPV} \\) (Tool A):
  \\[
    \begin{aligned}
      AC_{t}^{NPV} = &-cost\_{\text{var}}[t] \\\\
      &- \sum\_{c} \Big( cost\_{\text{input}}[c] \cdot input\_{\text{coeff}}[c] +
      cost\_{\text{output}}[c] \cdot output\_{\text{coeff}}[c] \Big) \\\\
      &+ \sum\_{c} \Big( output\_{\text{coeff}}[c] - input\_{\text{coeff}}[c] \Big)
        \cdot \lambda\_{c,r,t} \\\\
      &+ \sum\_{s,c} in\\_scope[s] \cdot \Big\\{ \\\\
      &\quad \quad (\mu\_{s,c}^{\text{prod}} - cost\_{\text{prod}}[s,c])
        \cdot output\_{\text{coeff}}[c] \\\\
      &\quad \quad + (\mu\_{s,c}^{\text{cons}} - cost\_{\text{cons}}[s,c])
        \cdot input\_{\text{coeff}}[c] \\\\
      &\quad \quad + (\mu\_{s,c}^{\text{net}} - cost\_{\text{net}}[s,c])
        \cdot (output\_{\text{coeff}}[c] - input\_{\text{coeff}}[c]) \\\\
      &\Big\\}
    \end{aligned}
  \\]

- Calculate cost per unit of activity \\( AC_{t}^{LCOX} \\) (Tool B). Note that the commodity
  of interest (primary output \\( c_{primary} \\)) is excluded from the price term:
  \\[
    \begin{aligned}
      AC_{t}^{LCOX} = & \quad cost\_{\text{var}}[t] \\\\
      &+ \sum\_{c} \Big( cost\_{\text{input}}[c] \cdot input\_{\text{coeff}}[c]+
      cost\_{\text{output}}[c] \cdot output\_{\text{coeff}}[c] \Big) \\\\
      &- \sum\_{c \neq c_{primary}} \Big( output\_{\text{coeff}}[c] - input\_{\text{coeff}}
      [c] \Big)
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

 > Note: "scopes" are not implemented in the current model.

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

This method is used when the decision rule is `single` and the objective is annualised profit for
agents serving commodity \\( c \\). It iteratively builds a supply portfolio by selecting
options that offer the highest annualised profit for serving the current commodity demand. The
economic evaluation uses \\( \pi_{prevMSY} \\) prices and takes account of asset-specific
operational constraints (e.g., minimum load levels) and the balance level of the target commodity
(time slice profile, seasonal or annual). For each asset option:

- **Optimise capacity and dispatch to maximise annualised profit:** Solve a small optimisation
  sub-problem to maximise the asset's surplus, subject to its operational rules and the specific
  demand tranche it is being asked to serve. \\(\varepsilon \approx 1×10^{-14}\\) is added to each
  \\(AC_{t}^{NPV} \\) to allow assets which are breakeven (or very close to breakeven) to be dispatched.

  \\[
    maximise \Big\\{\sum_t act_t (AC_{t}^{NPV} + \varepsilon)
    \Big\\}
  \\]

  Where \\( act_t \\) is a decision variable, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_t \\).

  - A demand constraint, where output cannot exceed demand in the tranche, which adapts based on the
    commodity's balance level (time slice, season, annual).

  - Capacity is constrained to \\( CapMaxBuild \\) for candidates, and to known capacity for
    existing assets.

- **Calculate a profitability index:** This is the total annualised surplus divided by the
 annualised fixed cost.
  \\[
  \text{Profitability Index} =
  \frac{\sum_t \text{act}_t \cdot \text{AC}_t^{\text{NPV}}}{\text{AFC} \cdot \text{cap}}
  \\]

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
      AF \times cap + \sum_t act_t \times AC_{t}^{LCOX} + VoLL \times UnmetD_t
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

- **Calculate a cost index:** This is the total annualised cost divided by the annual output.
  \\[
  \text{Cost Index} = \frac{\text{AFC} \times \text{cap}_r + \sum_t \text{act}_t
   \times \text{AC}_t^{\text{LCOX}}}{\sum_t \text{act}_t}
  \\]

## Example: Gas Power Plant

The following is an illustrative example of how the NPV and LCOX approaches work, using a simple
gas combined-cycle power plant as the supply option under consideration.
This example demonstrates the evaluation across two time periods
t1 (peak period) and t2 (off-peak period) with variable operating costs \\( cost\_{var}[t] \\)
fixed.

### Model Parameters

#### Asset Parameters
<!-- markdownlint-disable MD013 -->
| Parameter | Notation | Value | Description |
|-----------|----------|-------|-------------|
| Primary output (Electricity) | \\( output\_{coeff}[c_{primary}] \\) | 1.0 MWh per unit activity | Main commodity produced |
| By-product output (Waste heat) | \\( output\_{coeff}[c_{heat}] \\) | 0.5 MWh per unit activity | Co-product from generation |
| Input (Natural gas) | \\( input\_{coeff}[c_{gas}] \\) | 2.5 MWh per unit activity | Fuel consumption |
| Variable operating cost | \\( cost\_{var}[t] \\) | £5/MWh of activity | Operating costs per unit activity |
<!-- markdownlint-enable MD013 -->

#### Investment Parameters

| Parameter | Notation | Value |
|-----------|----------|-------|
| Annualised fixed cost | \\( AFC\_{opt,r} \\) | £1,000/MW |
| Capacity | \\( cap \\) | 100 MW |

#### Market Prices by Time Period

| Commodity | Notation | t1 (Peak) | t2 (Off-peak) |
|-----------|----------|-----------|---------------|
| Electricity | \\( \lambda\_{c_{elec},r,t} \\) | £90/MWh | £50/MWh |
| Heat | \\( \lambda\_{c_{heat},r,t} \\) | £25/MWh | £15/MWh |
| Natural gas | \\( \lambda\_{c_{gas},r,t} \\) | £35/MWh | £25/MWh |

### NPV Approach (Tool A)

#### Calculate Net Revenue per Unit of Activity

The net revenue calculation follows the general form:

\\[
AC_t^{NPV} = \sum_{c} \Big( output\_{coeff}[c] - input\_{coeff}[c] \Big) \cdot \lambda_{c,r,t} - cost\_{var}[t]
\\]

**For t1 (peak period):**

\\[
\begin{aligned}
AC_{t1}^{NPV} &= (1.0 \times 90) + (0.5 \times 25) + (-2.5 \times 35) - 5 \\\\
&= 90 + 12.5 - 87.5 - 5 \\\\
&= \text{£10/MWh}
\end{aligned}
\\]

The asset earns £10 profit for every MWh it operates during peak periods.

**For t2 (off-peak period):**

\\[
\begin{aligned}
AC_{t2}^{NPV} &= (1.0 \times 50) + (0.5 \times 15) + (-2.5 \times 25) - 5 \\\\
&= 50 + 7.5 - 62.5 - 5 \\\\
&= \text{£} -10 \text{/MWh}
\end{aligned}
\\]

The asset loses £10 for every MWh it operates during off-peak periods.

#### Dispatch Optimisation

The optimisation maximises total net revenue across all time periods:

\\[
\max \sum_t act_t \cdot AC_t^{NPV} = act_{t1} \cdot 10 + act_{t2} \cdot (-10)
\\]

where \\( act_t \\) is the activity (operational level) in each time slice, subject to operational
 constraints and demand requirements.

In this case, the optimiser will prefer to dispatch the asset during t1 (profitable) and
 minimise operation during t2 (unprofitable), subject to technical constraints such as minimum
load requirements.

#### Profitability Index

The profitability index is calculated as:

\\[
\text{Profitability Index} = \frac{\sum_t act_t \cdot AC_t^{NPV}}{AFC \times cap}
\\]

Suppose the dispatch optimiser determines \\( act_{t1} = 80 \\) MWh and \\( act_{t2} = 20 \\) MWh are
the optimal activity levels:

\\[
\begin{aligned}
\text{Profitability Index} &= \frac{(80 \times 10) + (20 \times (-10))}{1{,}000 \times 100} \\\\
&= \frac{800 - 200}{100{,}000} \\\\
&= \frac{600}{100{,}000} \\\\
&= 0.006
\end{aligned}
\\]

The profitability index is then compared against all other options to determine which asset provides
 the best return on investment for serving the demand.

### LCOX Approach (Tool B)

#### Net Cost per Unit of Activity

The net cost excludes the primary output (electricity) and is calculated as:

\\[
AC_t^{LCOX} = cost\_{var}[t] + \sum_{c \neq c_{primary}} \Big( input\_{coeff}[c] -
 output\_{coeff}[c] \Big) \cdot \lambda_{c,r,t}
\\]

**For t1 (peak period):**

\\[
\begin{aligned}
AC_{t1}^{LCOX} &= 5 + (2.5 \times 35) - (0.5 \times 25) \\\\
&= 5 + 87.5 - 12.5 \\\\
&= \text{£80/MWh}
\end{aligned}
\\]

It costs £80 per MWh to operate during peak periods (net of heat by-product sales).

**For t2 (off-peak period):**

\\[
\begin{aligned}
AC_{t2}^{LCOX} &= 5 + (2.5 \times 25) - (0.5 \times 15) \\\\
&= 5 + 62.5 - 7.5 \\\\
&= \text{£60/MWh}
\end{aligned}
\\]

It costs £60 per MWh to operate during off-peak periods, reflecting lower gas prices
 and lower heat by-product value.

#### Capacity and Dispatch Optimisation

The optimiser determines the most cost-effective capacity and dispatch pattern to meet demand across
 both time periods by minimising the total annualised cost with respect to decision variables
 \\( cap \\) and \\( act_t \\):

\\[
AFC \cdot cap + \sum_t act_t \cdot AC_t^{LCOX} = 1{,}000 \cdot cap + act_{t1}
 \cdot 80 + act_{t2} \cdot 60
\\]

#### Cost Index (Levelised Cost of X)

The cost index is calculated as:

\\[
\text{cost index} = \frac{AFC \cdot cap + \sum_t act_t \cdot AC_t^{LCOX}}{\sum_t act_t}
\\]

Suppose the optimiser determines \\( cap = 100 \\) MW, \\( act_{t1} = 150 \\) MWh,
 and \\( act_{t2} = 80 \\) MWh are the optimal capacity and activity levels:

\\[
\begin{aligned}
\text{cost index} &= \frac{(1{,}000 \times 100) + (150 \times 80) + (80 \times 60)}{150 + 80} \\\\
&= \frac{100{,}000 + 12{,}000 + 4{,}800}{230} \\\\
&= \frac{116{,}800}{230} \\\\
&= \text{£508/MWh}
\end{aligned}
\\]

The cost index is £508 per MWh of electricity produced.
 This metric is compared across all supply options to identify
 the lowest-cost solution for meeting demand.

[overall MUSE2 workflow]: ./README.md#framework-overview
[Dispatch Optimisation Formulation]: ./dispatch_optimisation.md
[ironing-out loop]: ./README.md#framework-overview
