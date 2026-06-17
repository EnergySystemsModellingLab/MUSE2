Reproducer: "No feasible investment options left".

This model is a variant of the `simple` example, modified so that running it
deliberately fails during agent investment with an error of the form:

    No feasible investment options left for commodity 'RSHEAT', region 'GBR',
    year '2030', agent 'A0_RES' after appraisal.

It demonstrates the case where, for a market with remaining unmet demand, every
candidate investment option appraises as non-feasible and is filtered out (see
`select_best_assets` in `src/simulation/investment.rs`).

How it is constructed (relative to `simple`):

  * `demand.csv`: residential heat (RSHEAT) demand for 2020 is set to 0. The
    first milestone year only runs dispatch (with no value-of-lost-load
    variables), so any unmet demand there would instead trigger a
    dispatch-infeasible error. Zeroing 2020 demand makes the first year
    trivially satisfiable, so the failure occurs during investment in 2030.

  * `assets.csv`: the existing RGASBR and RELCHP heating assets are removed, so
    meeting the (re-appearing) 2030 demand genuinely requires new investment.

  * `process_investment_constraints.csv`: both processes that can serve RSHEAT
    (RGASBR and RELCHP) are given an `addition_limit` of 0. They keep their
    normal availability, so they remain valid producers in the commodity graph,
    but no new capacity can be built. Each candidate therefore appraises with
    zero capacity and is discarded as non-feasible, leaving no feasible option
    to meet the 2030 demand.

Run it with:

    muse2 run reproducers/no_feasible_investment

The equivalent automated test is `no_feasible_investment_options` in
`src/simulation/investment.rs`.
