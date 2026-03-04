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

- Calculate the specific process and commodity flow costs (\\(\text{SPCF}\_{t})\\):

  \\[
    \text{SPCF}\_{t} = \sum\_{c} \Big( cost\_{\text{input}}[c] \cdot input\_{\text{coeff}}[c] +
            cost\_{\text{output}}[c] \cdot output\_{\text{coeff}}[c] \Big)
  \\]

#### Coefficients of activity

- Calculate net revenue per unit of activity \\(AC_{t}^{NPV} \\) (Tool A):
  \\[
    \begin{aligned}
          AC\_{t}^{NPV} = &-cost\_{\text{var}}[t] \\\\
            &- \text{SPCF}\_{t} \\\\
            &+ \sum\_{c} \Big( output\_{\text{coeff}}[c] - input\_{\text{coeff}}[c] \Big)
              \cdot \lambda\_{c,r,t} \\\\
            &+ \varepsilon \\\\
    \end{aligned}
  \\]
  \\(\varepsilon \approx 1\times 10^{-14}\\) is added to
  each \\(AC_{t}^{NPV} \\) to allow assets which are breakeven (or very close to breakeven) to be
  dispatched.

- Calculate cost per unit of activity \\( AC_{t}^{LCOX} \\) (Tool B). Note that the commodity
  of interest (primary output \\( c_{primary} \\)) is excluded from the price term:
  \\[
    \begin{aligned}
          AC\_{t}^{LCOX} = & \quad cost\_{\text{var}}[t] \\\\
            &+ \text{SPCF}\_{t} \\\\
            &- \sum\_{c \neq c_{primary}} \Big( output\_{\text{coeff}}[c] - input\_{\text{coeff}}
            [c] \Big)
              \cdot \lambda\_{c,r,t} \\\\
    \end{aligned}
  \\]

- The third term in both activity coefficients accounts for commodity price flow costs, which are
  the net costs or revenues associated with the commodity flows. In the LCOX case the commodity of
  interest is excluded from this term because the cost of production shouldn't depend on the market
  price of the commodity being produced.

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
  demand tranche it is being asked to serve.

  \\[
    maximise \Big\\{\sum_t act_t AC\_{t}^{NPV}
    \Big\\}
  \\]

  Where \\( act_t \\) is a decision variable, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_t \\).

  - A demand constraint, where output cannot exceed demand in the tranche, which adapts based on the
    commodity's balance level (time slice, season, annual).

  - Capacity is constrained up to \\( CapMaxBuild \\) for candidates, and to known capacity for
    existing assets.

- **Decide on metric:** The type of metric used to compare profitability is dependent on the value of
  \\(\text{AFC}\\). If \\(\text{AFC} = 0\\) within the tolerance provided by the `float_cmp` crate,
  the associated investment option is always prioritised over options with \\(\text{AFC} > 0\\).

- **If \\(\text{AFC} > 0\\), Use the profitability index \\(\text{PI}\\) metric:** This is the total
 annualised surplus divided by the annualised fixed cost.
  \\[
  \text{PI} =
  \frac{\sum\_t act\_t \cdot \text{AC}\_t^{\text{NPV}}}{\text{AFC} \cdot \text{cap}}
  \\]

- **If \\(\text{AFC} = 0\\), Use the total annualised surplus metric \\(\text{TAS}\\):**
    \\[
  \text{TAS} =
  \sum\_t act\_t \cdot \text{AC}\_t^{\text{NPV}}
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
  sub-problem to minimise the asset's annualised cost, subject to its operational rules and the specific
  demand tranche it is being asked to serve.

  \\[
    minimise \Big\\{
      \text{AFC} \times cap + \sum\_t act\_t \times AC\_{t}^{LCOX} + VoLL \times UnmetD\_t
    \Big\\}
  \\]

  Where \\( cap \\) and \\( act_t \\) are decision variables, and subject to:

  - The asset operational constraints (e.g., \\( avail_{LB}, avail_{EQ} \\), etc.), activity less
    than capacity, applied to its activity profile \\( act_t \\).

  - A demand constraint, where output from the asset plus VoLL-related outputs must equal demand in
    each timeslice of the tranche, which adapts based on the commodity's balance level (time slice,
    season, annual).

  - Capacity is constrained up to \\( CapMaxBuild \\) for candidates, and to known capacity for
    existing assets.

  - VoLL variables are active to ensure a feasible solution alongside maximum operation of the
    asset.

- **Calculate a Cost Index Metric:** This is the total annualised cost divided by the annual output.
  \\[
  \text{Cost Index} = \frac{\text{AFC} \times \text{cap}_r + \sum_t act_t
   \times \text{AC}_t^{\text{LCOX}}}{\sum_t act_t}
  \\]

#### Equal-Metric Fallback

If two or more investment options from the same tool have equal metrics, the following tie-breaking
rules are applied in order:

1. Assets which are already commissioned are preferred over new candidate assets.
2. Newer (commissioned later) assets are preferred over older assets.
3. If there is still a tie, the first option in the data structure storing the metrics is selected,
   which is an arbitrary choice. A `debug` level log message is emitted in this case.

## Example: Gas Power Plant

The following is an illustrative example of how the NPV and LCOX approaches work, using a simple
gas combined-cycle power plant as the supply option under consideration.
This example demonstrates the evaluation across two time periods
\\(t\_0\\) (peak period) and \\(t\_1\\) (off-peak period) with variable operating costs
 \\( cost\_{var}[t] \\) constant in all time periods.

### Model Parameters

#### Asset Parameters
<!-- markdownlint-disable MD013 -->
| Parameter | Notation | Value | Description |
| ----------- | ---------- | ------- | ------------- |
| Primary output (Electricity) | \\( output\_{coeff}[c_{primary}] \\) | 1.0 MWh per unit activity | Main commodity produced |
| By-product output (Waste heat) | \\( output\_{coeff}[c_{heat}] \\) | 0.5 MWh per unit activity | Co-product from generation |
| Input (Natural gas) | \\( input\_{coeff}[c_{gas}] \\) | 2.5 MWh per unit activity | Fuel consumption |
| Variable operating cost | \\( cost\_{var}[t] \\) | £5/MWh of activity | Operating costs per unit activity |
<!-- markdownlint-enable MD013 -->

All per-flow costs represented in the general formulas as \\( cost\_{input} \\) and
\\( cost\_{output} \\) are assumed to be zero.

#### Investment Parameters

| Parameter | Notation | Value |
| ----------- | ---------- | ------- |
| Annualised fixed cost | \\( AFC\_{opt,r} \\) | £1,000/MW |
| Capacity | \\( cap \\) | 100 MW |

#### Market Prices by Time Period

| Commodity | Notation | \\(t_0\\) (Peak) | \\(t_1\\) (Off-peak) |
| ----------- | ---------- | ----------- | --------------- |
| Electricity | \\( \lambda\_{c\_{primary},r,t} \\) | £90/MWh | £50/MWh |
| Heat | \\( \lambda\_{c\_{heat},r,t} \\) | £25/MWh | £15/MWh |
| Natural gas | \\( \lambda\_{c\_{gas},r,t} \\) | £35/MWh | £25/MWh |

### NPV Approach (Tool A)

#### Calculate Net Revenue per Unit of Activity

**For \\(t\_0\\) (peak period):**

\\[
\begin{aligned}
AC_{t_{0}}^{NPV} &= (1.0 \times 90) + (0.5 \times 25) + (-2.5 \times 35) - 5 \\\\
&= 90 + 12.5 - 87.5 - 5 \\\\
&= \text{£10/MWh}
\end{aligned}
\\]

The asset earns £10 profit for every MWh it operates during peak periods.

**For \\(t\_1\\) (off-peak period):**

\\[
\begin{aligned}
AC_{t\_1}^{NPV} &= (1.0 \times 50) + (0.5 \times 15) + (-2.5 \times 25) - 5 \\\\
&= 50 + 7.5 - 62.5 - 5 \\\\
&= \text{£} -10 \text{/MWh}
\end{aligned}
\\]

The asset loses £10 for every MWh it operates during off-peak periods.

#### Dispatch Optimisation

The optimisation maximises total net revenue across all time periods:

\\[
\max \sum\_t act\_t \cdot AC\_t^{NPV} = act\_{t_{0}} \cdot 10 + act\_{t\_1} \cdot (-10)
\\]

where \\( act\_t \\) is the activity (operational level) in each time slice, subject to operational
 constraints and demand requirements.

In this case, the optimiser will prefer to dispatch the asset during \\(t\_0\\) (profitable) and
minimise operation during \\(t\_1\\) (unprofitable), subject to technical constraints such as minimum
load requirements.

#### Profitability Index

The profitability index is calculated as:

\\[
\text{PI} = \frac{\sum\_t act\_t \cdot AC\_t^{NPV}}{AFC \times cap}
\\]

Suppose the dispatch optimiser determines \\( act\_{t\_{0}} = 80 \\) MWh and \\( act\_{t\_1} = 20 \\)
MWh are the optimal activity levels:

\\[
\begin{aligned}
\text{PI} &= \frac{(80 \times 10) + (20 \times (-10))}{1{,}000 \times 100} \\\\
&= \frac{800 - 200}{100{,}000} \\\\
&= \frac{600}{100{,}000} \\\\
&= 0.006
\end{aligned}
\\]

The profitability index is then compared against all other options to determine which asset provides
 the best return on investment for serving the demand.

### LCOX Approach (Tool B)

#### Net Cost per Unit of Activity

**For \\(t\_0\\) (peak period):**

\\[
\begin{aligned}
AC\_{t\_{0}}^{LCOX} &= 5 + (2.5 \times 35) - (0.5 \times 25) \\\\
&= 5 + 87.5 - 12.5 \\\\
&= \text{£80/MWh}
\end{aligned}
\\]

It costs £80 per MWh to operate during peak periods (net of heat by-product sales).

**For \\(t_1\\) (off-peak period):**

\\[
\begin{aligned}
AC\_{t\_1}^{LCOX} &= 5 + (2.5 \times 25) - (0.5 \times 15) \\\\
&= 5 + 62.5 - 7.5 \\\\
&= \text{£60/MWh}
\end{aligned}
\\]

It costs £60 per MWh to operate during off-peak periods, reflecting lower gas prices
 and lower heat by-product value.

#### Capacity and Dispatch Optimisation

The optimiser determines the most cost-effective capacity and dispatch pattern to meet demand across
both time periods by minimising the total annualised cost with respect to decision variables
\\( cap \\) and \\( act\_t \\):

\\[
AFC \cdot cap + \sum\_t act\_t \cdot AC\_t^{LCOX} = 1{,}000 \cdot cap + act\_{t\_{0}}
 \cdot 80 + act\_{t\_1} \cdot 60
\\]

#### Cost Index (Levelised Cost of X)

The Cost Index is calculated as:

\\[
\text{Cost Index} = \frac{AFC \cdot cap + \sum\_t act\_t \cdot AC\_t^{LCOX}}{\sum\_t act\_t}
\\]

Suppose the optimiser determines \\( cap = 100 \\) MW, \\( act\_{t\_{0}} = 150 \\) MWh,
 and \\( act\_{t\_1} = 80 \\) MWh are the optimal capacity and activity levels:

\\[
\begin{aligned}
\text{Cost Index} &= \frac{(1{,}000 \times 100) + (150 \times 80) + (80 \times 60)}{150 + 80} \\\\
&= \frac{100{,}000 + 12{,}000 + 4{,}800}{230} \\\\
&= \frac{116{,}800}{230} \\\\
&= \text{£508/MWh}
\end{aligned}
\\]

The Cost Index is £508 per MWh of electricity produced.
 This metric is compared across all supply options to identify
 the lowest-cost solution for meeting demand.

[framework-overview]: https://energysystemsmodellinglab.github.io/MUSE2/model/index.html#framework-overview
[Dispatch Optimisation Formulation]: ./dispatch_optimisation.md
