//! The competitive asset selection algorithm.
//!
//! Candidate and existing assets compete in rounds: each round every option is appraised with a
//! mini dispatch optimisation, and the best option according to the agent's objective is selected.
pub mod coefficients;
mod constraints;
mod costs;
pub mod metrics;
mod optimisation;
