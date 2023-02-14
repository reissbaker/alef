use crate::ast::trace::{Trace, Tracer, Tracers};
use crate::ast::span::Span;

pub struct ParseContext {
    pub collect_errors: bool,
    pub tracer: Tracers,
}

impl ParseContext {
    pub fn trace<'a>(&self, trace: Trace, span: &Span<'a>) {
        #[cfg(debug_assertions)]
        self.tracer.trace(trace, span);
    }
}
