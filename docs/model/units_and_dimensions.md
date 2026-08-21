# Units and Dimensions

MUSE2 does not impose one universal system of physical units. Instead, the input files define the
unit conventions used by each model. Commodity units are provided in the `units` field of
`commodities.csv`, while the optional `currency` field in `model.toml` documents the currency and,
if needed, its reference year. Process flow coefficients and `capacity_to_activity` then define the
relationships between activity, commodity flows, and capacity.

MUSE2 requires commodity unit labels to be present and uses them to check consistency, but it does
not interpret their contents, convert between units, or perform general dimensional analysis. For
example, `PJ`, `tonnes`, and `ktCO2` are simply labels to MUSE2; their physical meanings come from
the conventions chosen by the modeller. Similarly, the `currency` value is just a label: MUSE2
does not perform currency conversions or adjust monetary values for inflation.

The main relationships are:

- **Activity** describes how much a *Process* operates.
- **Commodity units** label the quantities produced and consumed by a *Process*.
- **Flow coefficients** convert activity into input and output flows of each *Commodity*.
- **Capacity-to-activity** relates installed capacity to maximum annual activity.
- **Capacity** limits the activity an *Asset* can perform.
- **Monetary units** describe how costs and prices are expressed.

For the example models, energy commodities are measured in PJ, generation capacity is expressed in
GW, emissions are measured in ktCO2, and monetary values are expressed in millions of US dollars.

## Commodity flows and flow coefficients

Every commodity must provide a unit label in the `units` field of `commodities.csv`. The
label is useful for documentation purposes, and is only used by MUSE2 for consistency checks; MUSE2
does not parse it or convert between labels. The simple example uses:

| Commodity | Type | Unit in the simple example |
| --- | --- | --- |
| GASPRD | Supply Equals Demand | PJ |
| GASNAT | Supply Equals Demand | PJ |
| ELCTRI | Supply Equals Demand | PJ |
| RSHEAT | Service Demand | PJ |
| CO2EMT | Other | ktCO2 |

> If any process has multiple SED or SVD output commodities, these commodities **must** all use
> exactly the same unit label - this is checked and enforced during input file loading.
> Inputs and OTH outputs are not subject to this check.

A *process flow coefficient* converts one activity unit into the corresponding commodity flow.
Positive coefficients represent production and negative coefficients represent consumption. A flow
coefficient therefore maps a dimensionless activity quantity to the corresponding commodity-flow
quantity. The coefficient must be interpreted together with the commodity's unit label.

For example, the simple example defines the gas combined-cycle turbine with these flows:

| Flow | Coefficient | Units and meaning |
| --- | ---: | --- |
| Natural gas input | `-1.5` | -1.5 PJ/activity unit |
| Electricity output | `1.0` | 1.0 PJ/activity unit |
| CO2 emissions | `76.695` | 76.695 ktCO2/activity unit |

## Activity

Activity is a dimensionless, process-specific quantity used by MUSE2 to describe how much an asset
operates in a time slice. It has no intrinsic physical unit. Its physical interpretation is supplied
by the process's flow coefficients and commodity-unit labels, while its maximum annual amount is
constrained by the asset's capacity and `capacity_to_activity`.

## Capacity and capacity-to-activity

Capacity is the installed size of an *Asset*. Its unit depends on the technology and the model.
For example, a power station might have capacity in MW or GW, while a material-processing process
might use tonnes per year.

The `capacity_to_activity` factor gives the maximum annual activity per unit of capacity. The factor,
flow coefficients, and commodity-unit labels work together to define what the capacity unit
represents physically.

In the simple example, the wind-farm and gas-turbine processes use `capacity_to_activity = 31.54`.
Since electricity output is labelled `PJ` and has a coefficient of `1.0`, this value was chosen so that
one capacity unit corresponds to 1 GW of electricity-generating capacity, able to produce up to
31.54 PJ of electricity per year. This follows from:

`1 GW x 31,536,000 seconds per year = 31,536,000,000,000,000 J per year = 31.536 PJ per year`

The model's value of `31.54` is rounded from `31.536`. At full activity, the gas-turbine coefficients
(above) then imply 47.31 PJ of natural-gas consumption per year, and approximately 2,419 ktCO2 of
emissions. In practice, availability limits may reduce actual activity below this maximum.

Conversely, a coal-producing process where coal is measured in tonnes, with an output
coefficient of `1.0`, might use `capacity_to_activity = 1`, so that one capacity unit represents an
amount of capacity able to produce up to one tonne of coal per year.

## Monetary units

MUSE2 does not convert currencies or adjust monetary values for inflation. Therefore, all monetary
inputs and outputs must be expressed using a consistent currency and reference year. For
documentation purposes, the currency and reference year can be included in the `currency` label in
`model.toml`, such as:

```toml
currency = "MUSD2020"
```

This is a free-text label for documenting the monetary
convention used by a model. It has no prescribed format, so modellers can use it to record a
currency, reference year, or any other relevant information.

Here, `MUSD2020` means million US dollars expressed in 2020 terms. The label is descriptive metadata
only; it does not determine the units of monetary data or affect model calculations.

For example, with `currency = "MUSD2020"` and capacity measured in GW, the monetary quantities would
be expressed as follows:

| Quantity | Units |
| --- | --- |
| Capital cost | MUSD2020/GW |
| Fixed operating cost | MUSD2020/GW/year |
| Variable operating cost | MUSD2020 per unit of activity |
| Flow cost or levy | MUSD2020 per unit of commodity flow (e.g. MUSD2020/PJ) |
| Commodity price | MUSD2020 per unit of commodity flow (e.g. MUSD2020/PJ) |

For example, a `variable_operating_cost` of `0.55` in a model defined with `currency = "MUSD2020"`
means that one activity unit incurs a variable operating cost of `0.55 MUSD2020` (i.e. $550,000 in
2020 terms).

## Choosing consistent units

When building a model:

1. Choose units for each commodity, such as PJ for energy and ktCO2 for emissions.
2. Choose a capacity unit for each process, such as GW for a power plant.
3. Set `capacity_to_activity` so that capacity converts to the appropriate annual activity scale.
4. Define flow coefficients so that activity produces and consumes the intended commodity quantities.
5. Express all costs and prices using the same currency convention and the relevant capacity,
   activity, or commodity-flow denominator.

MUSE2 validates some unit relationships, but it does not perform general unit conversion. A model
can use different physical units for different commodities, provided that each process's capacity,
activity, flow, and cost data are dimensionally consistent.
