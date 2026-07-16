//! Implement a context manager for timing the decorated function
use context_manager::{CallerContext, SyncWrapContext};
use log::info;
use std::time::Instant;

/// Context for time a function call
pub struct TimingContext {
    start: Instant,
}

impl<T> SyncWrapContext<T> for TimingContext {
    fn new() -> Self {
        Self {
            start: Instant::now(),
        }
    }

    fn after(self, caller_context: &CallerContext, _result: &T) {
        let elapsed = self.start.elapsed();
        info!(
            "[Timer] '{}' completed in {:.3}ms",
            caller_context.fn_name(),
            elapsed.as_secs_f64() * 1000.0,
        );
    }
}
