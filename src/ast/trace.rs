use std::fmt::Debug;
use crate::ast::span::Span;

#[derive(Debug, Clone, Copy)]
pub enum Trace {
    StartParse,
    Err,
    Consume(usize),
    Ok(Option<()>),
}

pub trait Tracer<'a> where Self: Debug + Clone + Copy {
    #[cfg(debug_assertions)]
    fn trace(self, traced: Trace, span: &Span<'a>);
}

#[derive(Debug, Clone, Copy)]
pub enum Tracers {
    Println(PrintlnTracer),
    Nil,
}

impl<'a> Tracer<'a> for Tracers {
    #[cfg(debug_assertions)]
    fn trace(self, trace: Trace, span: &Span<'a>) {
        match self {
            Tracers::Println(tracer) => {
                tracer.trace(trace, span);
            }
            Tracers::Nil => {
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PrintlnTracer {}
impl<'a> Tracer<'a> for PrintlnTracer {
    #[cfg(debug_assertions)]
    fn trace(self, trace: Trace, span: &Span<'a>) {
        match trace {
            Trace::StartParse => {
                println!("Trace index {} ({}): Starting parse...", span.start, span.as_bytes()[0] as char);
            }
            Trace::Err => {
                println!("Trace index {} ({}): Errored", span.start, span.as_bytes()[0] as char);
            }
            Trace::Consume(count) => {
                println!("Trace index {} ({}): consuming {} ({})", span.start, span.as_bytes()[0] as char, count, span.get_consumed(span.start + count).as_str());
            }
            Trace::Ok(Some(_)) => {
                println!("Traced index {} ({}): Ok with trailing error", span.start, span.as_bytes()[0] as char);
            }
            Trace::Ok(None) => {
                println!("Traced index {} ({}): Ok", span.start, span.as_bytes()[0] as char);
            }
        }
    }
}
