use std::fmt::Debug;
use crate::span::{Span, Tracer};

pub fn format_error<'a, T: Tracer<'a>, E: Debug + Clone + Copy>(e: ErrorPicker<'a, T, E>) -> String {
    format!("Error at index {}:\n{}", e.get_span().start, e.get_messages().into_iter().map(|e| {
        format!("{:?}", e)
    }).collect::<Vec<String>>().join("\n"))
}

#[derive(Debug, Clone, Copy)]
pub enum ParseError<E: Debug + Clone + Copy> {
    Char(char),
    Byte(u8),
    Kind(E),
}

#[derive(Debug, Clone)]
pub struct ErrorPicker<'a, T: Tracer<'a>, E: Debug + Clone + Copy> {
    longest_chain: Span<'a, T>,
    messages: Vec<ParseError<E>>,
}

impl<'a, T: Tracer<'a>, E: Debug + Clone + Copy> ErrorPicker<'a, T, E> {
    pub fn new(span: Span<'a, T>, e: ParseError<E>) -> ErrorPicker<'a, T, E> {
        ErrorPicker {
            messages: vec![ e ],
            longest_chain: span,
        }
    }

    pub fn get_span(&self) -> Span<'a, T> {
        self.longest_chain
    }

    pub fn get_messages(self) -> Vec<ParseError<E>> {
        self.messages
    }

    pub fn longest(mut self, initial: ErrorPicker<'a, T, E>) -> ErrorPicker<'a, T, E> {
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

    pub fn maybe_longest(self, initial: Option<ErrorPicker<'a, T, E>>) -> ErrorPicker<'a, T, E> {
        match initial {
            Some(e) => self.longest(e),
            None => self,
        }
    }
}
