//! Implement a context manager for timing the decorated function
use context_manager::{CallerContext, SyncWrapContext};
use lazy_static::lazy_static;
use log::debug;
use std::sync::RwLock;
use std::{marker::PhantomData, time::Instant};

// Global variable to store the total time spent in investment step
lazy_static! {
    static ref INVESTMENT_TIME: RwLock<f64> = RwLock::new(0.0);
}

// Global variable to store the total time spent in dispatch steps
lazy_static! {
    static ref DISPATCH_TIME: RwLock<f64> = RwLock::new(0.0);
}

#[doc(hidden)]
pub trait TimerLabel {
    fn label(caller_context: &CallerContext) -> String;
}

#[doc(hidden)]
pub trait Accumulator {
    fn accumulate(miliseconds: &f64) -> ();
}

/// Generic timing implementation shared by all timing contexts
pub struct TimerContext<L: TimerLabel = FnNameLabel, A: Accumulator = NullAccumulator> {
    start: Instant,
    _label: PhantomData<L>,
    _accumulator: PhantomData<A>,
}

impl<T, L: TimerLabel, A: Accumulator> SyncWrapContext<T> for TimerContext<L, A> {
    fn new() -> Self {
        Self {
            start: Instant::now(),
            _label: PhantomData,
            _accumulator: PhantomData,
        }
    }

    fn after(self, caller_context: &CallerContext, _result: &T) {
        let label = L::label(caller_context);
        let ms = self.start.elapsed().as_secs_f64() * 1000.0;
        A::accumulate(&ms);
        debug!("{label} completed in {ms:.1}ms");
    }
}

#[doc(hidden)]
pub struct FnNameLabel;
impl TimerLabel for FnNameLabel {
    fn label(caller_context: &CallerContext) -> String {
        let fn_name = caller_context.fn_name();
        format!("[Timer] '{fn_name}'")
    }
}

#[doc(hidden)]
pub struct InvestmentLabel;
impl TimerLabel for InvestmentLabel {
    fn label(_: &CallerContext) -> String {
        "Investment step".to_string()
    }
}

#[doc(hidden)]
pub struct DispatchLabel;
impl TimerLabel for DispatchLabel {
    fn label(_: &CallerContext) -> String {
        "Dispatch step".to_string()
    }
}

#[doc(hidden)]
pub struct NullAccumulator;
impl Accumulator for NullAccumulator {
    fn accumulate(_miliseconds: &f64) -> () {}
}

#[doc(hidden)]
pub struct InvestmentAccumulator;
impl Accumulator for InvestmentAccumulator {
    fn accumulate(miliseconds: &f64) -> () {
        let mut investment_time = INVESTMENT_TIME.write().unwrap();
        *investment_time += *miliseconds;
    }
}

#[doc(hidden)]
pub struct DispatchAccumulator;
impl Accumulator for DispatchAccumulator {
    fn accumulate(miliseconds: &f64) -> () {
        let mut dispatch_time = DISPATCH_TIME.write().unwrap();
        *dispatch_time += *miliseconds;
    }
}

/// Investment context for timing a function call
pub type InvestmentTimerContext = TimerContext<InvestmentLabel, InvestmentAccumulator>;

/// Dispatch context for timing a function call
pub type DispatchTimerContext = TimerContext<DispatchLabel, DispatchAccumulator>;

/// Get the total time spent in investment steps
pub fn get_investment_time() -> f64 {
    let investment_time = INVESTMENT_TIME.read().unwrap();
    *investment_time
}

/// Get the total time spent in dispatch steps
pub fn get_dispatch_time() -> f64 {
    let dispatch_time = DISPATCH_TIME.read().unwrap();
    *dispatch_time
}
