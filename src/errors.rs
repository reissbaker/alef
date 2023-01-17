use crate::span::{Span, Tracer};

#[derive(Debug, Clone)]
pub struct ErrorCollector<'a, T: Tracer<'a>> {
    longest_chain: Span<'a, T>,
    messages: Vec<String>,
}

impl<'a, T: Tracer<'a>> ErrorCollector<'a, T> {
    pub fn new(span: Span<'a, T>, msg: String) -> ErrorCollector<'a, T> {
        ErrorCollector {
            messages: vec![ msg ],
            longest_chain: span,
        }
    }

    pub fn get_span(&self) -> Span<'a, T> {
        self.longest_chain
    }

    pub fn get_messages(self) -> Vec<String> {
        self.messages
    }

    pub fn longest(self, initial: ErrorCollector<'a, T>) -> ErrorCollector<'a, T> {
        #[cfg(debug_assertions)]
        self.longest_chain.trace(
            &format!("Error found!\n1: {:?}\n2: {:?}", self, initial)
        );

        if self.longest_chain.start > initial.longest_chain.start {
            #[cfg(debug_assertions)]
            self.longest_chain.trace("Choosing 1\n");
            return self;
        }
        if self.longest_chain.start == initial.longest_chain.start {
            #[cfg(debug_assertions)]
            self.longest_chain.trace("Choosing both\n");
            let mut new_vec = self.messages.clone();
            new_vec.extend(initial.messages);
            return ErrorCollector {
                longest_chain: self.longest_chain,
                messages: new_vec,
            };
        }
        #[cfg(debug_assertions)]
        self.longest_chain.trace("Choosing 2\n");
        initial
    }

    pub fn maybe_longest(self, initial: Option<ErrorCollector<'a, T>>) -> ErrorCollector<'a, T> {
        match initial {
            Some(e) => self.longest(e),
            None => self,
        }
    }
}
