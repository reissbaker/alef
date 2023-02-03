use std::fmt::Debug;
use crate::span::{Span};

pub fn format_error<'a, E: Debug + Clone + Copy>(e: FastError<'a, E>) -> String {
    let clone = e.clone();
    let span = clone.get_span();
    format!("Syntax error at index {}. Expected one of the following:\n{}", span.start, e.get_messages().into_iter().map(|e| {
        format!("{:?}", e)
    }).collect::<Vec<String>>().join("\n"))
}

#[derive(Debug, Clone, Copy)]
pub enum ParseError<E: Debug + Clone + Copy> {
    Char(char),
    Byte(u8),
    EOI,
    Kind(E),
}

#[derive(Debug, Clone)]
pub enum FastErrorList<'a, E: Debug + Clone + Copy> {
    Tail,
    Node(Box<FastError<'a, E>>),
}
#[derive(Debug, Clone)]
pub struct FastError<'a, E: Debug + Clone + Copy> {
    span: Span<'a>,
    error: ParseError<E>,
    list: FastErrorList<'a, E>,
}

impl<'a, E: Debug + Clone + Copy> FastError<'a, E> {
    pub fn new(span: Span<'a>, e: ParseError<E>) -> FastError<'a, E> {
        FastError {
            span,
            error: e,
            list: FastErrorList::Tail,
        }
    }

    pub fn get_span(&self) -> Span<'a> {
        self.span
    }

    pub fn get_messages(self) -> Vec<ParseError<E>> {
        let mut vec = vec![self.error];
        match self.list {
            FastErrorList::Tail => {
            }
            FastErrorList::Node(next) => {
                vec.extend(next.get_messages());
            }
        }
        vec
    }

    pub fn longest(self, other: FastError<'a, E>) -> FastError<'a, E> {
        let my_start = self.span.start;
        let other_start = other.span.start;
        if my_start > other_start {
            return self;
        }
        if my_start == other_start {
            return FastError {
                span: self.span,
                error: self.error,
                list: FastErrorList::Node(Box::new(other)),
            }
        }
        other
    }

    pub fn maybe_longest(self, initial: Option<FastError<'a, E>>) -> FastError<'a, E> {
        match initial {
            Some(e) => self.longest(e),
            None => self,
        }
    }
}
