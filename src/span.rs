use std::marker::PhantomData;
use std::fmt::Debug;

#[derive(Debug, Clone, Copy)]
pub enum Trace {
    StartParse,
    Err,
    Consume(usize),
    Ok(Option<()>),
}

pub trait Tracer<'a> where Self: Debug + Clone + Copy {
    #[cfg(debug_assertions)]
    fn trace(self, traced: Trace, span: &Span<'a, Self>);
}
impl<'a> Tracer<'a> for () {
    #[cfg(debug_assertions)]
    fn trace(self, _: Trace, _: &Span<'a, Self>) {}
}

#[derive(Debug, Clone, Copy)]
pub struct PrintlnTracer {}
impl<'a> Tracer<'a> for PrintlnTracer {
    #[cfg(debug_assertions)]
    fn trace(self, trace: Trace, span: &Span<'a, PrintlnTracer>) {
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

#[derive(Debug, Clone, Copy)]
pub struct Span<'a, T: Tracer<'a>> {
    pub start: usize,
    pub end: usize,
    pub input: &'a str,
    pub tracer: T,
}

impl<'a, T: Tracer<'a>> Span<'a, T> {
    pub fn new(input: &'a str, tracer: T) -> Span<'a, T> {
        Span {
            input,
            tracer,
            start: 0,
            end: input.len(),
        }
    }

    pub fn consume(&self, count: usize) -> Span<'a, T> {
        #[cfg(debug_assertions)]
        self.trace(Trace::Consume(count));

        Span {
            start: self.start + count,
            end: self.end,
            input: self.input,
            tracer: self.tracer,
        }
    }

    pub fn get_consumed(&self, next_start: usize) -> Span<'a, T> {
        Span {
            start: self.start,
            end: next_start,
            input: self.input,
            tracer: self.tracer,
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.input.as_bytes()[self.start..self.end]
    }
    pub fn as_str(&self) -> &'a str {
        &self.input[self.start..self.end]
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    #[cfg(debug_assertions)]
    pub fn trace(&self, trace: Trace) {
        self.tracer.trace(trace, self);
    }
}
