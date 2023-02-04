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
    message: ParseError<E>,
    messages: Vec<ParseError<E>>,
}

impl<E: Debug + Clone + Copy> ErrorPicker<E> {
    pub fn new<'a>(span: &Span<'a>, e: ParseError<E>) -> ErrorPicker<E> {
        ErrorPicker {
            message: e,
            messages: vec![],
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
        let mut cloned = self.messages.clone();
        cloned.push(self.message);
        cloned
    }

    pub fn longest(mut self, initial: ErrorPicker<E>) -> ErrorPicker<E> {
        if self.longest_chain.start > initial.longest_chain.start {
            return self;
        }
        if self.longest_chain.start == initial.longest_chain.start {
            self.messages.extend(initial.messages);
            return ErrorPicker {
                message: self.message,
                longest_chain: self.longest_chain,
                messages: self.messages,
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
