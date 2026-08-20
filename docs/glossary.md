# Glossary

**Activity:** The operational quantity describing how much a *Process* is operated. *Activity* is
limited by an *Asset*'s *Capacity* and *Availability*, and is multiplied by the *Process*'s flow
coefficients to determine its input and output flows. For example, a 500MW power station can
output 500MWh per hour of electrical power, or a 50MW electrolyser consumes up to 50MWh per hour
of electrical power to produce hydrogen.

**Agent:** A decision-making entity in the system. An *Agent* is responsible for serving a
user-specified portion of a *Commodity* demand or *Service Demand*. *Agents* invest in and operate
*Assets* to serve demands and produce *Commodities*.

**Agent Objective:** One or more objectives that an *Agent* considers when deciding which *Process*
to invest in. Objectives can be economic, environmental, or others. An *Agent* can use multiple
objectives when applying a *Decision Rule*.

**Agent Share:** The portion of a *Commodity* demand for which an *Agent* is responsible. *Agent*
shares determine how total demand is divided between agents and can also scale investment limits.
For example, if total electricity demand is 100 PJ and an *Agent* has a 25% *Agent Share*, that
*Agent* is responsible for 25 PJ of demand.

**Annualised Capital Cost:** The annual cost equivalent of an upfront *Capital Cost*, calculated
using the *Capital Recovery Factor* for the relevant *Asset* *Lifetime* and *Discount Rate*. For
example, an *Asset* with a £1 million *Capital Cost*, a 10-year *Lifetime*, and a 5% *Discount Rate*
has an annualised *Capital Cost* of approximately £129,500 per year before any *Fixed Operating Cost*
is added.

**Asset:** Once an *Agent* makes an investment, the related capacity of their chosen *Process*
becomes an *Asset* that they own and operate. An *Asset* is an instance of a *Process*, it has a
specific *Capacity*, and a decommissioning *Year*. A set of *Assets* must exist in the *Base Year*
sufficient to serve *Base Year* demands (i.e. a calibrated *Base Year*, based on user input data).

**Asset Commissioning:** The point at which an *Asset* becomes active and available for *Dispatch*.
An *Asset* is commissioned according to its commission *Year*, either from user input or following
an investment decision.

**Asset Decommissioning:** The permanent removal of an *Asset* from the active model. An *Asset* can
be decommissioned when it reaches the end of its allowed *Lifetime* or after remaining mothballed for
the configured period.

**Asset Mothballing:** The temporary withdrawal of an *Asset* or some of its units from the active
pool when it is not selected for retention. A mothballed *Asset* is not dispatched, but may be
reactivated in a later investment period; it is permanently decommissioned if it remains mothballed
for longer than a configured number of years.

**Availability:** The maximum, minimum or fixed percentage of maximum output (or input) that a
*Process* can deliver over a period. *Availability* limits the *Activity* of an *Asset* and can be
specified for a single *Time Slice*, a *Season*, or a *Year*. For example, an *Asset* with 100 MW of
*Capacity* and 50% *Availability* can provide at most 50 MW during the specified period. This is a
limit, not a measure of how much the *Asset* is actually used; that is its *Utilisation*.

**Base Year:** The starting *Year* of a model run. The *Base Year* is typically calibrated to known
data, including *Process* stock and *Commodity* consumption/production.

**Calibration:** The act of ensuring that the model represents the system being modelled in a
historical base year.

**Candidate Asset:** A hypothetical *Asset* representing a *Process* that is available for an
*Agent* to invest in, but has not yet been selected or commissioned. Its *Capacity* is assigned for
investment appraisal and can be adjusted during the investment process.

**Capacity:** The installed size of an *Asset*, used with its *Process*'s *Capacity-to-activity factor*
to determine the maximum *Activity* it can perform. In general, maximum *Activity* equals *Capacity*
multiplied by the *Capacity-to-activity factor*. The resulting *Activity*, multiplied by the
*Process*'s flow coefficients, determines the maximum input and output flows.

**Capacity-to-activity factor:** The factor that converts an *Asset*'s capacity into its maximum
*Activity*. It expresses the maximum *Activity* that one unit of capacity can perform over the relevant
period. The factor must use the units of the relevant *Commodity*. For example, if *Capacity* is
measured in GW and commodities are expressed in PJ, one GW operating for a full year can produce
8,760 GWh, equivalent to approximately 31.54 PJ. Its capacity-to-activity factor is therefore
31.54 PJ per GW-year (or 0.03154 PJ per MW-year).

**Capital Cost:** The overnight capital cost of a *Process*. It is a cost of establishing capacity,
rather than a short-term cost that varies directly with *Activity*.

**Capital Recovery Factor:** The factor used to convert an upfront *Capital Cost* into an annualised
cost over an *Asset*'s *Lifetime* at a given *Discount Rate*. It represents the proportion of the
upfront capital cost that is paid each year to spread that cost over the asset's lifetime. The
factor increases with the discount rate and decreases as the lifetime increases. When the discount
rate is zero, it is the reciprocal of the lifetime.

**Circular Commodity Dependency:** A dependency cycle in which the production or consumption of one
*Commodity* depends, directly or indirectly, on another *Commodity* that eventually depends on the
first. For example, electricity can be used to produce hydrogen, which is then used to produce
electricity.

<!-- markdownlint-disable-next-line MD033 -->
**Commodity:** A substance (e.g. CO<sub>2</sub>) or form of energy (e.g. electricity) that can be
produced and/or consumed by *Process*es in the model. A *Service Demand* is a type of *Commodity* that
is defined at the end point of the system.

**Commodity Balance:** The constraint applied to balanced *Commodities* at their *Time-slice Level*
and separately by *Region* and *Time-slice Selection*. For *Service Demand* (SVD) commodities, the
balance must meet the fixed demand specified in the input data. For *Supply Equals Demand* (SED)
commodities, production must meet the consumption created by processes in the system. *OTH
(other)-type commodity* entries, such as emissions or side products, are not subject to *Commodity
Balance* constraints.

**Commodity Levy:** Represents a tax, levy or other external cost on a *Commodity*. Levies can be
applied to all *Commodity* production (sum of output of all *Processes* for that *Commodity*), net
production (sum of output and input for all *Processes*), or all consumption (sum of input for all
*Processes*). It can also be negative, indicating an incentive on *Commodity*
production/consumption/net.

**Commodity Market:** The market for a particular *Commodity* in a *Region*. MUSE2 evaluates each
market during investment and calculates prices for the *Commodity* based on the operation of
relevant *Assets* and the applicable *Pricing Strategy*.

**Decision Rule:** The rule via which an *Agent* uses the *Agent Objective* to decide between *Process*
options to invest in. Examples include single objective, weighted sum between multiple objectives,
or epsilon constraint where a secondary objective is considered if two options with similar primary
objectives are identified.

**Demand Distribution:** The allocation of annual *Service Demand* across *Time Slices* within a
*Year*. It specifies how much of the annual demand occurs in each *Time Slice*, while the total
across the year remains equal to the annual demand.

**Discount Rate:** The discount rate used to calculate any *Process*-specific *Agent Objective*
that require a discount rate. For example, *Equivalent Annual Cost*, *Net Present Value*,
*Levelised Cost of X*, etc.

**Dispatch:** The way in which *Assets* are operated to serve demand. Within the limits imposed by
*Availability*, *Commodity* balances, and other user-defined constraints, MUSE2 effectively dispatches
*Assets* in *Merit Order*: cheaper *Assets* are used first, up to their available capacity or until
demand is met. The resulting operation is calculated by a least-cost linear programme.

**End Year:** The final year in the model *Time Horizon*.

**Equivalent Annual Cost (EAC):** An *Agent Objective*, representing the annualised cost of serving
all or part of an *Agent's* demand for a year, considering the *Asset's* entire *Lifetime*.

**Fixed Operating Cost:** The *Asset* or *Process* annual operating cost charged per unit of
*Capacity*. Unlike *Variable Operating Cost*, it does not vary directly with *Activity*.

**Flow Coefficient:** The factor that converts one unit of *Activity* into the corresponding input
or output flow of a *Commodity*. For example, a flow coefficient of 2 means that one unit of
*Activity* produces or consumes two units of that *Commodity*.

**Full Cost:** The cost of producing a unit of output including *Marginal Cost*, annualised *Capital
Cost*, and *Fixed Operating Cost*. For example, a *Process* with low fuel costs but high *Capital
Cost* may have a low *Marginal Cost* but a higher *Full Cost*.

**Input Commodity:** A *Commodity* that flows into a *Process*. A *Process* may have multiple input
commodities.

**Investment Appraisal:** The process of evaluating existing and *Candidate Asset*s to determine
which investments an *Agent* should make in a particular *Commodity Market* and *Milestone Year*.
Appraisal considers the *Agent Objective*, expected costs, prices, and the asset's operation.

**Investment Order:** The sequence in which *Commodity* markets are considered for investment.
MUSE2 generally processes downstream markets before upstream markets, so that demand for input
*Commodities* created by committed *Assets* is known when the upstream markets are evaluated. For
example, investment in electricity generation is considered before investment in gas production
when gas is used to generate electricity.

**Ironing-out Loop:** The repeated sequence of *Agent* investment and *Dispatch* used to resolve
price instability introduced by new capacity commitments. The loop stops when time-slice-weighted
market prices converge within the configured tolerance or the maximum number of iterations is reached.

**Levelised Cost of X (LCOX):** An *Agent Objective*, representing the discounted cost of 1 unit of
output *Commodity* X from a *Process* over its *Lifetime* under a specified *Discount Rate*.

**Lifetime:** The lifetime of a *Process*, measured in years.

**Marginal Cost:** The additional cost of producing one more unit of output from a *Process* or
*Asset*. It generally includes costs that vary with *Activity*, such as fuel and variable operating
costs, but not costs that do not change with short-term output. For example, the marginal cost of a
gas generator includes the cost of the gas used to produce an additional unit of electricity.

**Merit Order:** A method of operating *Assets* when the cheapest is dispatched first, followed by
the next most expensive, etc, until demand is served.

**Milestone Year:** A *Year* in the *Time Horizon* where model results are recorded. A model can
have multiple *Milestone Year*s. For example, with a 2025 *Base Year* and *End Year* 2100, a user
might choose to record outputs in 5-year steps.

**Net Present Value (NPV):** The value of an investment's future costs and benefits expressed in
the present, calculated by discounting them using a specified *Discount Rate*. NPV is an *Agent
Objective* used to compare investment options.

**OTH (other)-type commodity:** A *Commodity* that is not supply-demand balanced in the model.
Includes side products and emissions such as CO₂, or any raw materials that are not output by
modelled *Processes*.

**Output Commodity:** A *Commodity* that flows out of a *Process*. A *Process* may have multiple
output commodities.

**Partial Equilibrium:** An equilibrium in which only the user-defined *Commodities* and markets are
balanced. MUSE2 does not attempt to equilibrate the whole economy.

**Pricing Strategy:** The method used to calculate a *Commodity*'s price. This includes *Marginal
Cost* pricing, *Full Cost* pricing, and *Shadow Price* pricing.

**Process:** A blueprint of an available *Process* that converts *Input Commodities* to *Output
Commodities*. *Process*es have economic attributes of *Capital Cost*, *Fixed Operating Cost* per
unit *Capacity*, *Variable Operating Cost* per unit *Activity*, and risk *Discount Rate*. They have
physical attributes of quantity and type of *Input Commodities* and *Output Commodities* (which
implicitly specify efficiency), *Availability* limits (by *Time Slice*, *Season*, and/or *Year*), and
*Lifetime* (years).
When a *Process* is selected by an *Agent* for investment an instance of it called an *Asset* is
created.

**Region:** A geographical area represented in the model. Regions contain the *Assets*, *Processes*,
*Agents*, demands, and *Commodity* markets that apply in that area, and define the boundaries within
which trade and other regional interactions are modelled.

**Search Space:** The set of *Processes* an *Agent* is allowed to consider for investment, defined
for a particular *Commodity*, *Region*, and *Year*. It determines which *Candidate Assets* are
available during investment appraisal.

**Season:** A year is usually broken down into seasons in the model. For example, summer, winter,
other.

**Sector:** Models are often broken down into sectors, each of which is associated with specific
*Service Demands* or specific *Commodity* production. For example, the residential sector, the power
sector, etc.

**Service Demand (SVD):** A *Service Demand* is a type of *Commodity* that is consumed at the
boundary of the modelled system. For example, tonne-kilometers of road freight, PJ of useful heat demand,
etc.

**Shadow Price:** The value of relaxing a *Commodity* balance constraint by one unit, as calculated
by the *Dispatch* optimisation. For example, a high *Shadow Price* indicates that one additional
unit of a *Commodity* would substantially improve the optimisation objective because supply is scarce.

**Supply Equals Demand (SED):** An SED *Commodity* is a type of *Commodity* that is both consumed and
produced by *Processes* in the system. In fully resolved systems, supply of these *Commodities* is
constrained to be equal to or greater than demand.

**Time Horizon:** The overall period modelled. For example, 2025&ndash;2100.

**Time Period:** Refers to a specific *Milestone Year* in the *Time Horizon*.

**Time Slice:** A discrete operational sub-period within a *Year*, identified by a *Season* and
a time-of-day category. For example, a *Time Slice* might represent winter-day or summer-night.
*Time Slices* are the within-year periods over which *Activity*, demand, and flows are modelled. A
*Time Slice* can be grouped with other *Time Slices* in a *Time-slice Selection* to represent a
*Season*, an entire *Year*, or another temporal grouping.

**Time-slice Level:** The temporal resolution at which a *Commodity*'s balance constraints are
applied. MUSE2 supports annual, seasonal, and day-night levels, from a whole year to a particular
time of day. For example, an annual level balances total supply and demand across the year, whereas
a day-night level requires the balance to be met separately in each time-of-day group.

**Time-slice Selection:** A group of model *Time Slices* treated as one period when applying an
*Asset*'s *Activity* or *Availability* constraints, or balancing a *Commodity* at its
*Time-slice Level*. For example, a selection can group all winter *Time Slices* so that a seasonal
*Availability* limit applies to their combined *Activity*.

**Unit Size:** The capacity of one unit of an *Asset*. Every *Asset* has a *Unit Size*, either defined
when the *Asset* is created or inferred when an investment is made. *Unit Size* defines the granularity
at which retention, mothballing, and decommissioning decisions are made. For example, a 100 MW
*Asset* with a 25 MW *Unit Size* has four units, which can be retained or mothballed independently.

**Utilisation:** The percentage of an *Asset*'s *Capacity* that is actually used to produce its
*Commodities*. Must be between 0 and 1, and can be measured at *Time Slice*, *Season*, or *Year* level.
For example, an *Asset* operating at 25 MW from 100 MW of available *Capacity* has 0.25
*Utilisation*. *Availability* describes what the *Asset* could provide; *Utilisation* describes
what it actually provides.

**Variable Operating Cost:** The variable operating cost charged per unit of *Activity* of the
*Process*.

**Year:** A calendar year used to identify a *Base Year*, *Milestone Year*, or *End Year* and to
specify when an *Asset* is commissioned or decommissioned. A *Year* is distinct from a *Time Slice*,
which represents a subdivision within a year.
