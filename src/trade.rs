use crate::commodity::Commodity;
use crate::region::RegionID;
use crate::units::MoneyPerFlow;
use std::rc::Rc;

pub struct TradeLink {
    commodity: Rc<Commodity>,
    source_region: RegionID,
    destination_region: RegionID,
    import_levy: MoneyPerFlow,
    export_levy: MoneyPerFlow,
}

impl TradeLink {
    // for investment.rs
    pub fn is_commissioned(&self) -> bool {
        true
    }

    // need unit size for VariableMap::add_to_problem (could be represented with bool for integer/non-integer?)
    // also needed in perform_optimisation when interpreting results (so maybe shouldn't be bool)

    // need capacity bounds for add_capacity_constraint

    // need get_flow for add_demand_constraints
}
