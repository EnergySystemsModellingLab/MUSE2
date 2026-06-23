# Commodity Prices

This section describes how commodity prices are calculated in MUSE2 at the end of each
milestone year iteration. The pricing algorithm operates on the results of the dispatch
optimisation, translating flows, capacities, and solver dual values (shadow prices) into commodity
market prices.

## Pricing Strategies

Each commodity is configured with a specific `pricing_strategy` via [`commodities.csv`].
The options are:

- **`marginal`**: Prices are set to the marginal cost of the highest-cost active asset producing the
commodity.
- **`marginal_average`**: Prices are set to the output-weighted average marginal cost across all
active assets.
- **`full`**: Prices are set to the full cost (marginal cost + annual fixed/capital costs) of the highest-cost active asset producing the commodity.
- **`shadow`**: Prices are taken directly from the shadow prices (dual values) of the commodity
balance constraints in the dispatch optimisation.
- **`scarcity`**: Prices are set to the shadow price plus the highest activity dual of the assets
producing the commodity in that region and time slice.
- **`unpriced`**: No prices are calculated for the commodity.

### Cost Calculations

Cost-based prices (`marginal`, `marginal_average`, `full`, `full_average`) are calculated based on
process cost parameters, asset capacities and dispatch flows. For assets producing multiple output
commodities (e.g. an asset producing both oil and gas), generic activity costs and fixed costs are
shared between outputs according to output flow coefficients.

#### Generic Activity Cost

The generic activity cost comprises all operating expenditures, input purchases, and flow costs/levies
not associated with specific SED/SVD outputs:
\[
\text{GenericActivityCost} = \text{VariableOperatingCost} + \text{InputPurchases} + \sum \text{GenericFlowCosts}
\]

This is shared equally over all SED/SVD outputs in proportion to their output coefficients to
compute a generic cost per unit of output flow:
\\[
\text{GenericCostPerOutput} = \frac{\text{GenericActivityCost}}{\sum_{c \in \text{SED, SVD}} \text{OutputCoefficient}_c}
\\]

#### Marginal Cost

The marginal cost of an output commodity \\( c \\) is the sum of the generic cost per output and any
commodity-specific costs (e.g., production levies, flow costs):
\\[
\text{MarginalCost}_c = \text{GenericCostPerOutput} + \text{SpecificCostPerOutput}_c
\\]

#### Full Cost

The full cost includes the marginal cost plus annualised capital and fixed operating costs.

Annualised capital costs are calculated using the [Capital Recovery Factor] (CRF):
\\[
\text{AnnualCapitalCostPerCapacity} = \text{TotalCapitalCostPerCapacity} \times \text{CRF}
\\]

The annualised fixed cost (AFC) is:
\\[
\text{AFC} = (\text{AnnualCapitalCostPerCapacity} + \text{AnnualFixedOpex}) \times \text{TotalCapacity}
\\]

This is divided by annual activity to get a cost per unit of activity:
\\[
\text{AnnualFixedCostPerActivity} = \frac{\text{AFC}}{\text{AnnualActivity}}
\\]

This is divided again by the sum of SED/SVD output coefficients to get a cost per unit output:
\\[
\text{AnnualFixedCostPerOutput} = \frac{\text{AnnualFixedCostPerActivity}}{\sum_{c \in \text{SED,
SVD}} \text{OutputCoefficient}_c}
\\]

*Note: this only works if all output commodities are measured in the same energy units (e.g. PJ).
For this reason, MUSE disallows processes that have output commodities with differing units.*

The final full cost of output commodity \\( c \\) is:
\\[
\text{FullCost}_c = \text{MarginalCost}_c + \text{AnnualFixedCostPerOutput}
\\]

### Candidate asset fallback

For cost-based strategies (`marginal`, `marginal_average`, `full`, `full_average`), if no active
producers exist in the current year, prices are calculated by looking at candidate assets for
investment in the following milestone year. Prices are set by the candidate asset with the lowest
marginal/full cost, calculated assuming full utilisation (i.e. assuming annual activity equals the
maximum annual activity limit).

In the final milestone year, prices for commodities with no active producer are omitted.

### Time Slice Aggregation

For commodities defined at coarser time slice levels (e.g. seasonal or annual), prices are
calculated by weighting asset costs across time slices by activity to yield a flat price for the
season/year. For candidates, or assets with zero activity across the selection, upper activity
limits are used as weights.

## Price Calculation Order

Prices are calculated sequentially starting from upstream commodities (e.g. raw fuels) and moving
downstream to intermediate energy carriers and final service demands.

This sequence is the **reverse** of the investment order. This ensures that the prices of input
commodities (from upstream markets) are calculated and known before they are consumed as inputs by
downstream processes evaluating marginal or full cost pricing.

## Circularities

When the energy system includes circular commodity flows (e.g., electricity producing hydrogen,
which then generates electricity), the resulting cyclic dependencies are resolved as follows:

1. **Initial Seeding:** Cyclic markets are seeded with their shadow prices.
2. **Sequential Evaluation:** Commodities in the cycle are evaluated in sequence, starting with
those furthest upstream relative to the rest of the system (i.e., furthest from the commodities
downstream of the SCC), and working down. Marginal cost calculations use newly updated prices for
input commodities already evaluated in the sequence, and fall back to the seeded shadow prices for
any not yet evaluated, which may occur due to circular dependencies.
<!-- Currently a hidden option, TBD whether we want to open this up: -->
<!-- 2. **Iterative Refinement:** An iterative loop runs for a fixed number of iterations,
re-evaluating each market in the cycle in reverse order (starting with those
furthest from commodities downstream of the SCC) to propagate the feedback effects through the
circular markets. -->

<!-- markdownlint-disable MD024 -->
## Example

The following example demonstrates the calculation of full-cost prices for two commodities using the
`full` pricing strategy.

### GASPRD

In this scenario, GASPRD is produced by a single GASDRV asset. This asset produces 1 unit of GASPRD
per unit of activity, along with 5.113 units of CO₂ emissions. Suppose an annual utilisation
of 0.20786471743 (based on dispatch results).

#### Marginal Cost

The asset incurs a CO₂ levy (0.04/unit) and a variable operating cost of 2.0:

\\[
\text{MarginalCost}_{\text{GASPRD}} = ( 5.113 \times 0.04) + 2.0 = 2.20452
\\]

#### Full Cost

Using a lifetime of 25 years, a discount rate of 10%, annual fixed costs of 0.3 per unit capacity,
and a total capital cost of 10 per unit capacity:

\\[
\text{CRF} = 0.11016807219
\\]

\\[
\text{AnnualFixedCostPerCapacity} = (10 \times 0.11016807219 + 0.3) = 1.4016807219
\\]

Accounting for annual utilisation:

\\[
\text{AnnualFixedCostPerActivity} = \frac{1.4016807219}{0.20786471743} = 6.7432354044
\\]

Accounting for the output coefficient:

\\[
\text{AnnualFixedCostPerOutput} = \frac{6.7432354044}{1} = 6.7432354044
\\]

The full cost is:

\\[
\text{FullCost}_{\text{GASPRD}} = 2.20452 + 6.7432354044 = 8.9477554044
\\]

### GASNAT

Following on from the above scenario, GASNAT is produced by a single GASPRC asset, which consumes
GASPRD as an input fuel. This asset consumes 1.05 units of GASPRD and produces 1 unit of GASNAT
per unit of activity, along with 2.5565 units of CO₂ emissions. Suppose an annual utilisation
of 0.2094885671 (based on dispatch results).

#### Marginal Cost

The asset incurs a CO₂ levy (0.04/unit), a variable operating cost of 0.5 and fuel purchases:

\\[
\text{CO2Levy} = 2.5565 \times 0.04 = 0.10226
\\]

\\[
\text{InputPurchases} = 8.9477554044 \times 1.05 = 9.39514317462
\\]

\\[
\text{MarginalCost}_{\text{GASNAT}} = 0.10226 + 0.5 + 9.39514317462 = 9.99740317462
\\]

#### Full Cost

Using the same lifetime and discount rate, along with annual fixed costs of 0.21 per unit capacity,
and a total capital cost of 7 per unit capacity:

\\[
\text{CRF} = 0.11016807219
\\]

\\[
\text{AnnualFixedCostPerCapacity} = (7 \times 0.11016807219 + 0.21) = 0.98117650533
\\]

Accounting for annual utilisation:

\\[
\text{AnnualFixedCostPerActivity} = \frac{0.98117650533}{0.2094885671} = 4.6836756722
\\]

Accounting for the output coefficient:

\[
\text{AnnualFixedCostPerOutput} = \frac{4.6836756722}{1} = 4.6836756722
\]

The full cost is:

\\[
\text{FullCost}_{\text{GASNAT}} = 9.99740317462 + 4.6836756722 = 14.6810788468
\\]

This example illustrates the sequential nature of price calculation: the GASPRD price is
calculated first and then used as an input cost when calculating the GASNAT price.

[Capital Recovery Factor]: https://homerenergy.com/products/pro/docs/latest/capital_recovery_factor.html
[`commodities.csv`]: ../file_formats/input_files.md#commoditiescsv
