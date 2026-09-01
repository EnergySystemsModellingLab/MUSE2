# Glossary

**Activity:** The quantity representing how much an *Asset* operates during a given period. Its flow
coefficients determine the resulting input and output flows of each *Commodity*.

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
has an annualised *Capital Cost* of approximately £129,500 per year.

**Asset:** A specific instance of a *Process*, representing installed or candidate capacity. It
carries the *Process*'s technical and economic properties, and adds instance-specific information
such as *Capacity*, ownership, location, commissioning *Year*, and operating state.

**Asset Commissioning:** The point at which an *Asset* becomes active and available for *Dispatch*.
An *Asset* is commissioned according to its commission *Year*, either from user input or following
an investment decision.

**Asset Decommissioning:** The permanent removal of an *Asset* from the active model. An *Asset* can
be decommissioned when it reaches the end of its allowed *Lifetime* or after remaining mothballed for
the configured period.

**Asset Mothballing:** The temporary withdrawal of an *Asset* or some of its tranches from the active
pool when it is not selected for retention. A mothballed *Asset* is not dispatched, but may be
reactivated in a later investment period; it is permanently decommissioned if it remains mothballed
for longer than a configured number of years.

**Asset Tranche:** A discrete portion of an *Asset*'s *Capacity*. An *Asset* can consist of multiple
*Asset Tranches* of equal size, which can be retained, mothballed, or decommissioned independently.

**Availability:** A *Process* property specifying the maximum, minimum, or fixed percentage of
maximum *Activity* that its *Assets* can deliver over a period. It can be specified for a single
*Time Slice*, a *Season*, or a *Year*. For example, an *Asset* with 100 MW of *Capacity* and 50%
annual *Availability* can provide at most 50 MW over the year. This is a limit, not a measure of
how much the *Asset* is actually used; that is its *Utilisation*.

**Base Year:** The starting *Year* of a model run. The *Base Year* is typically calibrated to known
data, including *Asset* stock and *Commodity* consumption/production.

**Calibration:** The act of ensuring that the model represents the system being modelled in a
historical base year. A set of *Assets* must exist in the *Base Year* sufficient to serve *Base Year*
demands.

**Candidate Asset:** A hypothetical *Asset* representing a *Process* that is available for an
*Agent* to invest in, but has not yet been selected or commissioned. Its *Capacity* is assigned for
investment appraisal and can be adjusted during the investment process.

**Capacity:** The installed size of an *Asset*. Together with its *Capacity-to-activity factor*, it
determines the maximum *Activity* the *Asset* can perform.

**Capacity-to-activity factor:** The factor that converts an *Asset*'s *Capacity* into its maximum
annual *Activity*.

**Capital Cost:** The upfront cost per unit of *Capacity* established in an *Asset*. It does not vary
directly with short-term *Activity*.

**Capital Recovery Factor:** The factor used to convert an upfront *Capital Cost* into an annualised
cost over an *Asset*'s *Lifetime* at a given *Discount Rate*. It represents the proportion of the
upfront capital cost that is paid each year to spread that cost over the asset's lifetime. The
factor increases with the discount rate and decreases as the lifetime increases. When the discount
rate is zero, it is the reciprocal of the lifetime.

**Circular Commodity Dependency:** A dependency cycle in which the production or consumption of one
*Commodity* depends, directly or indirectly, on another *Commodity* that eventually depends on the
first. For example, electricity can be used to produce hydrogen, which is then used to produce
electricity.

**Commodity:** A substance (e.g. CO₂) or form of energy (e.g. electricity) that can be
produced and/or consumed by *Processes* in the model. A *Service Demand* is a type of *Commodity* that
is defined at the end point of the system.

**Commodity Levy:** Represents a tax, levy or other external cost on a *Commodity*. Levies can be
applied to all *Commodity* production (sum of output of all *Assets* for that *Commodity*), net
production (sum of output and input for all *Assets*), or all consumption (sum of input for all
*Assets*). It can also be negative, indicating an incentive on *Commodity*
production/consumption/net.

**Commodity Market:** The market for a particular *Commodity* in a *Region*. MUSE2 evaluates each
market during investment and calculates prices for the *Commodity* based on the operation of
relevant *Assets* and the applicable *Pricing Strategy*.

**Decision Rule:** The rule via which an *Agent* uses the *Agent Objective(s)* to decide between
*Process* options to invest in.

**Demand Distribution:** The allocation of annual *Service Demand* across *Time Slices* within a
*Year*. It specifies how much of the annual demand occurs in each *Time Slice*, while the total
across the year remains equal to the annual demand.

**Discount Rate:** The proportion used to convert a *Process*'s future costs and benefits into
present-value terms. For example, a discount rate of 0.05 means that £100 received in one year's
time is valued at approximately £95.24 today (100 / (1 + 0.05)).

**Dispatch:** The way in which *Assets* are operated to serve demand. Within the limits imposed by
*Availability*, *Commodity* balances, and other user-defined constraints, MUSE2 effectively dispatches
*Assets* in *Merit Order*: cheaper *Assets* are used first, up to their available capacity or until
demand is met. The resulting operation is calculated by a least-cost linear programme.

**End Year:** The final year in the model *Time Horizon*.

**Equivalent Annual Cost (EAC):** The constant annual cost equivalent of an *Asset*'s upfront
capital cost and operating costs over its *Lifetime*, accounting for the *Discount Rate*.

**Fixed Operating Cost:** The annual operating cost charged per unit of an *Asset*'s *Capacity*.
Unlike *Variable Operating Cost*, it does not vary directly with *Activity*.

**Flow Coefficient:** The factor that converts one unit of *Activity* into the corresponding input
or output flow of a *Commodity*. Positive coefficients represent production and negative
coefficients represent consumption. For example, a flow coefficient of 2 means that one unit of
*Activity* produces two units of that *Commodity*.

**Full Cost:** The cost of producing a unit of output including *Marginal Cost*, annualised *Capital
Cost*, and *Fixed Operating Cost*. For example, a *Process* with low fuel costs but high *Capital
Cost* may have a low *Marginal Cost* but a higher *Full Cost*.

**Input Commodity:** A *Commodity* that flows into a *Process*. A *Process* may have multiple input
commodities.

**Investment Appraisal:** The process of evaluating existing and *Candidate Assets* to determine
which investments an *Agent* should make in a particular *Commodity Market* and *Milestone Year*.
Appraisal considers the *Agent Objective*, expected costs, commodity prices, and the asset's
expected operation.

**Investment Order:** The sequence in which *Commodity* markets are considered for investment.
MUSE2 generally processes downstream markets before upstream markets, so that demand for input
*Commodities* created by committed *Assets* is known when the upstream markets are evaluated. For
example, investment in electricity generation is considered before investment in gas production
when gas is used to generate electricity.

**Ironing-out Loop:** The repeated sequence of *Agent* investment and *Dispatch* used to resolve
price instability introduced by new capacity commitments. The loop stops when time-slice-weighted
market prices converge within the configured tolerance or the maximum number of iterations is reached.

**Levelised Cost of X (LCOX):** The cost per unit of output *Commodity* X from an *Asset*,
calculated by dividing its discounted lifetime costs by its discounted lifetime output under a
specified *Discount Rate*.

**Lifetime:** The expected operational duration of a *Process*, measured in years.

**Marginal Cost:** The cost of producing one additional unit of an output *Commodity* from an
*Asset*. It combines the *Process*'s *Variable Operating Cost* with activity-dependent costs
associated with the *Asset*'s commodity flows, such as input prices and levies. It excludes *Capital
Cost* and *Fixed Operating Cost*.

**Merit Order:** A method of operating *Assets* where the cheapest is dispatched first, followed by
the next most expensive, etc, until demand is served.

**Milestone Year:** A *Year* in the *Time Horizon* where model results are recorded. A model can
have multiple *Milestone Years*. For example, with a 2025 *Base Year* and *End Year* 2100, a user
might choose to record outputs in 5-year steps.

**Net Present Value (NPV):** The value of future costs and benefits expressed in the present,
calculated by discounting them using a specified *Discount Rate*.

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
Commodities*. *Processes* have economic attributes of *Capital Cost*, *Fixed Operating Cost* per
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

**Season:** A subdivision of a *Year* that groups related *Time Slices*. For example, summer,
winter, or other.

**Sector:** Models are often broken down into sectors, each of which is associated with specific
*Service Demands* or specific *Commodity* production. For example, the residential sector, the power
sector, etc.

**Service Demand (SVD):** A *Commodity* consumed at the boundary of the modelled system to represent
demand for a service. For example, tonne-kilometres of road freight or PJ of useful heat demand.

**Shadow Price:** The value of relaxing a *Commodity* balance constraint by one unit, as calculated
by the *Dispatch* optimisation. For example, a high *Shadow Price* indicates that one additional
unit of a *Commodity* would substantially improve the optimisation objective because supply is scarce.

**Supply Equals Demand (SED):** An SED *Commodity* is a type of *Commodity* that is both consumed and
produced by *Assets* in the system. In fully resolved systems, supply of these *Commodities* is
constrained to be equal to or greater than demand.

**Time Horizon:** The overall period modelled. For example, 2025&ndash;2100.

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

**Tranche Size:** The capacity of one tranche of an *Asset*. Every *Asset* has a *Tranche Size*,
either defined when the *Asset* is created or inferred when an investment is made. *Tranche Size*
defines the granularity at which retention, mothballing, and decommissioning decisions are made. For
example, a 100 MW *Asset* with a 25 MW *Tranche Size* has four tranches, which can be retained or
mothballed independently.

**Utilisation:** The proportion of an *Asset*'s *Capacity* that is actually used to produce its
*Commodities*, ranging from 0 to 1 inclusive. Can be measured at *Time Slice*, *Season*, or *Year* level.
For example, an *Asset* operating at 25 MW from 100 MW of available *Capacity* has 0.25
*Utilisation*. *Availability* describes what the *Asset* could provide; *Utilisation* describes
what it actually provides.

**Variable Operating Cost:** A *Process* parameter specifying the generic operating cost per unit
of *Activity*. It excludes costs associated with commodity flows, such as input prices and levies.

**Year:** A calendar year used to identify a *Base Year*, *Milestone Year*, or *End Year* and to
specify when an *Asset* is commissioned or decommissioned.
