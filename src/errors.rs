use std::fmt::Debug;
use crate::span::{Span};

pub fn format_error<'a, E: Debug + Clone + Copy>(e: ErrorPicker<'a, E>) -> String {
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

#[derive(Debug, Clone)]
pub struct ErrorPicker<'a, E: Debug + Clone + Copy> {
    longest_chain: Span<'a>,
    messages: Vec<ParseError<E>>,
}

impl<'a, E: Debug + Clone + Copy> ErrorPicker<'a, E> {
    pub fn new(span: Span<'a>, e: ParseError<E>) -> ErrorPicker<'a, E> {
        ErrorPicker {
            messages: vec![ e ],
            longest_chain: span,
        }
    }

    pub fn get_span(&self) -> Span<'a> {
        self.longest_chain
    }

    pub fn get_messages(self) -> Vec<ParseError<E>> {
        self.messages
    }

    pub fn longest(mut self, initial: ErrorPicker<'a, E>) -> ErrorPicker<'a, E> {
        if self.longest_chain.start > initial.longest_chain.start {
            return self;
        }
        if self.longest_chain.start == initial.longest_chain.start {
            self.messages.extend(initial.messages);
            return ErrorPicker {
                longest_chain: self.longest_chain,
                messages: self.messages,
            };
        }
        initial
    }

    pub fn maybe_longest(self, initial: Option<ErrorPicker<'a, E>>) -> ErrorPicker<'a, E> {
        match initial {
            Some(e) => self.longest(e),
            None => self,
        }
    }
}
