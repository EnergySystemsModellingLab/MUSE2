# Glossary

**Activity:** The flow of input/s or output/s of a *Process* that are limited by its capacity. For
example, a 500MW power station can output 500MWh per hour of electrical power, or a 50MW
electrolyser consumes up to 50MWh per hour of electrical power to produce hydrogen.

**Agent:** A decision-making entity in the system. An *Agent* is responsible for serving a
user-specified portion of a *Commodity* demand or *Service Demand*. *Agents* invest in and operate
*Assets* to serve demands and produce commodities.

**Agent Objective/s:** One or more objectives that an *Agent* considers when deciding which
*Process* to invest in. Objectives can be economic, environmental, or others.

**Asset:** Once an *Agent* makes an investment, the related capacity of their chosen *Process*
becomes an *Asset* that they own and operate. An *Asset* is an instance of a *Process*, it has a
specific capacity, and a decommissioning year. A set of *Assets* must exist in the base year
sufficient to serve base year demands (i.e. a calibrated base year, based on user input data).

**Availability:** The maximum, minimum or fixed percentage of maximum output (or input) that a
*Process* delivers over a period. The time period could be a single time slice, a season, or a year.

**Base Year:** The starting year of a model run. The base year is typically calibrated to known
data, including **Process** stock and commodity consumption/production.

**Calibration:** The act of ensuring that the model represents the system being modelled in a
historical base year.

**Capacity:** The maximum output (or input) of an *Asset*.

**Capital Cost:** The overnight capital cost of a process.

<!-- markdownlint-disable-next-line MD033 -->
**Commodity:** A substance (e.g. CO<sub>2</sub>) or form of energy (e.g. electricity) that can be
produced and/or consumed by *Process*es* in the model. A *Service Demand* is a type of commodity
that is defined at the end point of the system.

**Commodity Levy:** Represents a tax, levy or other external cost on a commodity. Levies can be
applied to all commodity production (sum of output of all processes for that commodity), net
production (sum of output and input for all processes), or all consumption (sum of input for all
processes). It can also be negative, indicating an incentive on commodity
production/consumption/net.

**Decision Rule:** The rule via which an *Agent* uses the *Objective/s* to decide between *Process*
options to invest in. Examples include single objective, weighted sum between multiple objectives,
or epsilon constraint where a secondary objective is considered if two options with similar primary
objectives are identified.

**Dispatch:** The way in which *Assets* are operated to serve demand. MUSE2 uses merit order
dispatch, subject to *Availability* and other constraints that can be defined by the user.

**End Year:** The final year in the model time horizon.

**Equivalent Annual Cost (EAC):** An *Agent* objective, representing the annualised cost of serving
all or part of an *Agent's* demand for a year, considering the *Asset's* entire lifetime.

**Fixed Operating Cost:** The *Asset* or *Process* annual operating cost charged per unit of
capacity.

**Input Commodity/ies:** The commodities that flow into a *Process*.

**Levelised Cost of X (LCOX):** An *Agent* objective, representing the discounted cost of 1 unit of
output commodity X from a process over its lifetime under a specified discount rate.

**Lifetime:** The lifetime of a *Process*, measured in years.

**Milestone Years:** A set of years in the model time horizon where model results are recorded. For
example, with a 2025 Base Year and End Year 2100, a user might choose to record outputs in
5-year steps.

**Merit Order:** A method of operating *Assets* when the cheapest is dispatched first, followed by
the next most expensive, etc, until demand is served. Also called “unit commitment.”

**Output Commodity/ies:** The commodities that flow out of a *Process*.

**Process:** A blueprint of an available *Process* that converts input commodities to output
commodities. *Process*es have economic attributes of capital cost, fixed operating cost per unit
capacity, non-fuel variable operating cost per unit activity, and risk discount rate. They have
physical attributes of quantity and type of input and output commodities (which implicitly specify
efficiency), *Availability* limits (by time slice, season and/or year), lifetime (years). When a
*Process* is selected by an *Agent* for investment an instance of it called an *Asset* is created.

**Region:** A geographical area that is modelled. Regions primarily determine trade boundaries.

**Season:** A year is usually broken down into seasons in the model. For example, summer, winter,
other.

**Sector:** Models are often broken down into sectors, each of which is associated with specific
*Service Demands* or specific *Commodity* production. For example, the residential sector, the power
sector, etc.

**Service Demand:** A Service Demand is a type of commodity that is consumed at the boundary of the
modelled system. For example, tonne-kilometers of road freight, PJ of useful heat demand, etc.

**Discount Rate:** The discount rate used to calculate any process-specific agent economic
objectives that require a discount rate. For example, Equivalent Annual Cost, Net Present Value,
Levelised Cost of X, etc.

**Time Horizon:** The overall period modelled. For example, 2025&ndash;2100.

**Time Period:** Refers to a specific *Milestone Year* in the time horizon.

**Time Slice:** The finest time period in the model. The maximum time slice length is 1 year (where
a model does not represent seasons or within-day (diurnal) variation). A typical model will have
several diurnal time slices, and several seasonal time slices.

**Utilisation:** The percentage of an *Asset*'s capacity that is actually used to produce its
commodities. Must be between 0 and 1, and can be measured at time slice, season, or year level.

**Variable Operating Cost:** The variable operating cost charged per unit of activity of the
 *Process*.
