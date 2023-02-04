use std::fmt::Debug;
use crate::span::{Span};

pub fn format_error<'a, E: Debug + Clone + Copy>(e: ErrorPicker<E>) -> String {
    format!("Syntax error at index {}. Expected one of the following:\n{}", e.get_span().start, e.get_messages().into_iter().map(|e| {
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

#[derive(Debug, Clone, Copy)]
pub struct ParseErrorSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct ErrorPicker<E: Debug + Clone + Copy> {
    longest_chain: ParseErrorSpan,
    // We separate my_message from other_messages as a perf optimization; this allows us to
    // allocate an empty vec for new errors. Allocating an empty vec is much faster than allocating
    // a vec with a single element (benchmarking shows nearly doubled perf on large files), and
    // since all errors start off with an empty list, and most stay empty, this is worth a bit of
    // extra work on the error formatting side which should be relatively rare.
    my_message: ParseError<E>,
    other_messages: Vec<ParseError<E>>,
}

impl<E: Debug + Clone + Copy> ErrorPicker<E> {
    pub fn new<'a>(span: &Span<'a>, e: ParseError<E>) -> ErrorPicker<E> {
        ErrorPicker {
            my_message: e,
            other_messages: vec![],
            longest_chain: ParseErrorSpan {
                start: span.start,
                end: span.end,
            },
        }
    }

    pub fn get_span(&self) -> ParseErrorSpan {
        self.longest_chain
    }

    pub fn get_messages(self) -> Vec<ParseError<E>> {
        let mut cloned = self.other_messages.clone();
        cloned.reverse();
        cloned.push(self.my_message);
        cloned.reverse();
        cloned
    }

    pub fn longest(mut self, initial: ErrorPicker<E>) -> ErrorPicker<E> {
        if self.longest_chain.start > initial.longest_chain.start {
            return self;
        }
        if self.longest_chain.start == initial.longest_chain.start {
            self.other_messages.push(initial.my_message);
            self.other_messages.extend(initial.other_messages);
            return ErrorPicker {
                my_message: self.my_message,
                longest_chain: self.longest_chain,
                other_messages: self.other_messages,
            };
        }
        initial
    }

    pub fn maybe_longest(self, initial: Option<ErrorPicker<E>>) -> ErrorPicker<E> {
        match initial {
            Some(e) => self.longest(e),
            None => self,
        }
    }
}
