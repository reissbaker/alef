pub trait Tracer<'a> where Self: std::fmt::Debug + Clone + Copy {
    #[cfg(debug_assertions)]
    fn trace_consume(self, count: usize, span: &Span<'a, Self>);
    #[cfg(debug_assertions)]
    fn trace(self, msg: &str, span: &Span<'a, Self>);
}
impl<'a> Tracer<'a> for () {
    #[cfg(debug_assertions)]
    fn trace_consume(self, _: usize, _: &Span<'a, ()>) {}
    #[cfg(debug_assertions)]
    fn trace(self, _: &str, _: &Span<'a, Self>) {}
}

#[derive(Debug, Clone, Copy)]
pub struct PrintlnTracer {}
impl<'a> Tracer<'a> for PrintlnTracer {
    #[cfg(debug_assertions)]
    fn trace_consume(self, count: usize, span: &Span<'a, PrintlnTracer>) {
        println!("Consuming {} from index {}", count, span.start);
    }
    #[cfg(debug_assertions)]
    fn trace(self, msg: &str, _: &Span<'a, PrintlnTracer>) {
        println!("Trace: {}", msg);
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
            input, tracer,
            start: 0,
            end: input.len(),
        }
    }

    pub fn consume(&self, count: usize) -> Span<'a, T> {
        #[cfg(debug_assertions)]
        self.tracer.trace_consume(count, self);

        Span {
            start: self.start + count,
            end: self.end,
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

    #[cfg(debug_assertions)]
    pub fn trace(&self, msg: &str) {
        self.tracer.trace(msg, self);
    }
}
