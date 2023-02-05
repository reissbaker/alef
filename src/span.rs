use std::fmt::Debug;
use crate::trace::{Trace, Tracer};
use crate::parse_context::ParseContext;

#[derive(Debug, Clone, Copy)]
pub struct Span<'a> {
    pub start: usize,
    pub end: usize,
    pub input: &'a str,
}

impl<'a> Span<'a> {
    pub fn new(input: &'a str) -> Span<'a> {
        Span {
            input,
            start: 0,
            end: input.len(),
        }
    }

    pub fn consume(&self, ctx: &ParseContext, count: usize) -> Span<'a> {
        #[cfg(debug_assertions)]
        ctx.tracer.trace(Trace::Consume(count), self);

        Span {
            start: self.start + count,
            end: self.end,
            input: self.input,
        }
    }

    pub fn err_consume(&self, count: usize) -> Span<'a> {
        Span {
            start: self.start + count,
            end: self.end,
            input: self.input,
        }
    }

    pub fn get_consumed(&self, next_start: usize) -> Span<'a> {
        Span {
            start: self.start,
            end: next_start,
            input: self.input,
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

    pub fn slice(&self, start: usize, len: usize) -> Span<'a> {
        Span {
            start: self.start + start,
            end: self.start + len,
            input: self.input,
        }
    }
}
