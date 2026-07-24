//! Implement a context manager for timing the decorated function
use context_manager::{CallerContext, SyncWrapContext};
use log::debug;
use std::marker::PhantomData;
use std::sync::{LazyLock, Mutex};
use std::time::Instant;

// Global variable to store the total time spent in investment step
static INVESTMENT_TIME: LazyLock<Mutex<f64>> = LazyLock::new(|| Mutex::new(0.0));

// Global variable to store the total time spent in dispatch steps
static DISPATCH_TIME: LazyLock<Mutex<f64>> = LazyLock::new(|| Mutex::new(0.0));

/// Trait providing the context of actions to be carried by the timer
pub trait Context {
    /// Adds the label to be used in the logs
    fn label(caller_context: &CallerContext) -> String;

    /// Includes de logic to accumulate the time in a global variable
    fn accumulate(milliseconds: &f64);
}

/// Generic timing implementation shared by all timing contexts
pub struct GenericTimer<C: Context = GenericContext> {
    start: Instant,
    _context: PhantomData<C>, // See https://doc.rust-lang.org/nomicon/phantom-data.html
}

impl<T, C: Context> SyncWrapContext<T> for GenericTimer<C> {
    fn new() -> Self {
        Self {
            start: Instant::now(),
            _context: PhantomData,
        }
    }

    fn after(self, caller_context: &CallerContext, _result: &T) {
        let label = C::label(caller_context);
        let ms = self.start.elapsed().as_secs_f64() * 1000.0;
        C::accumulate(&ms);
        debug!("{label} completed in {ms:.1}ms");
    }
}

/// Default context for timer, not accumularinging the time s
/// uaing the decorated function name in the logs
pub struct GenericContext;
impl Context for GenericContext {
    fn label(caller_context: &CallerContext) -> String {
        let fn_name = caller_context.fn_name();
        format!("[Timer] '{fn_name}'")
    }
    fn accumulate(_milliseconds: &f64) {}
}

/// Investment context, using a fixed label in logs and accumulating
/// the time in the `INVESTMENT_TIME` variable
pub struct InvestmentContext;
impl Context for InvestmentContext {
    fn label(_: &CallerContext) -> String {
        "Investment step".to_string()
    }
    fn accumulate(milliseconds: &f64) {
        let mut investment_time = INVESTMENT_TIME.lock().unwrap();
        *investment_time += *milliseconds;
    }
}

/// Dispatch context, using a fixed label in logs and accumulating
/// the time in the `DISPATCH_TIME` variable
pub struct DispatchContext;
impl Context for DispatchContext {
    fn label(_: &CallerContext) -> String {
        "Dispatch step".to_string()
    }
    fn accumulate(milliseconds: &f64) {
        let mut dispatch_time = DISPATCH_TIME.lock().unwrap();
        *dispatch_time += *milliseconds;
    }
}

/// Investment context for timing a function call
pub type InvestmentTimer = GenericTimer<InvestmentContext>;

/// Dispatch context for timing a function call
pub type DispatchTimer = GenericTimer<DispatchContext>;

/// Get the total time spent in investment steps
pub fn get_investment_time() -> f64 {
    let investment_time = INVESTMENT_TIME.lock().unwrap();
    *investment_time
}

/// Get the total time spent in dispatch steps
pub fn get_dispatch_time() -> f64 {
    let dispatch_time = DISPATCH_TIME.lock().unwrap();
    *dispatch_time
}
