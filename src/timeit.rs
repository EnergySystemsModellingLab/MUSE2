//! Implement a context manager for timing the decorated function
use context_manager::{CallerContext, SyncWrapContext};
use log::debug;
use std::{marker::PhantomData, time::Instant};

#[doc(hidden)]
pub trait TimerLabel {
    fn label(caller_context: &CallerContext) -> String;
}

/// Generic timing implementation shared by all timing contexts
pub struct TimerContext<L: TimerLabel> {
    start: Instant,
    _label: PhantomData<L>,
}

impl<T, L: TimerLabel> SyncWrapContext<T> for TimerContext<L> {
    fn new() -> Self {
        Self {
            start: Instant::now(),
            _label: PhantomData,
        }
    }

    fn after(self, caller_context: &CallerContext, _result: &T) {
        let label = L::label(caller_context);
        let ms = self.start.elapsed().as_secs_f64() * 1000.0;
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

/// Generic context for timing a function call
pub type TimingContext = TimerContext<FnNameLabel>;

/// Investment context for timing a function call
pub type InvestmentContext = TimerContext<InvestmentLabel>;

/// Dispatch context for timing a function call
pub type DispatchContext = TimerContext<DispatchLabel>;
