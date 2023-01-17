use std::marker::PhantomData;
use crate::span::{Span, Tracer, PrintlnTracer};

type ParseResult<'a, O, T> = Result<(Span<'a, T>, O, Option<ErrorCollector<'a, T>>), ErrorCollector<'a, T>>;

#[derive(Debug, Clone)]
pub struct ErrorCollector<'a, T: Tracer<'a>> {
    longest_chain: Span<'a, T>,
    messages: Vec<String>,
}
impl<'a, T: Tracer<'a>> ErrorCollector<'a, T> {
    fn new(span: Span<'a, T>, msg: String) -> ErrorCollector<'a, T> {
        ErrorCollector {
            messages: vec![ msg ],
            longest_chain: span,
        }
    }

    fn longest(self, initial: ErrorCollector<'a, T>) -> ErrorCollector<'a, T> {
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

    fn maybe_longest(self, initial: Option<ErrorCollector<'a, T>>) -> ErrorCollector<'a, T> {
        match initial {
            Some(e) => self.longest(e),
            None => self,
        }
    }
}

pub trait Parser<'a, O, T: Tracer<'a>> where Self: Sized {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T>;

    fn parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T> {
        #[cfg(debug_assertions)]
        span.trace("Starting parse...");
        // TODO DO WAY BETTER TRACING THIS IS SO SLOW
        let result = self.do_parse(span);
        match result {
            Err(e) => {
                span.trace(&format!("finished with error {:?}", e));
                Err(e)
            }
            Ok((remaining, data, e)) => {
                span.trace(&format!("trailing e: {:?}", e));
                Ok((remaining, data, e))
            }
        }
    }

    fn then<ONext, PNext>(self, next: PNext) -> Seq<'a, T, O, ONext, Self, PNext>
    where PNext: Parser<'a, ONext, T> {
        seq(self, next)
    }
}

impl<'a, O, T: Tracer<'a>, F: FnMut(&Span<'a, T>) -> ParseResult<'a, O, T>> Parser<'a, O, T> for F {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T> {
        self(span)
    }
}

fn byte<'a, T: Tracer<'a>>(b: u8) -> impl FnMut(&Span<'a, T>) -> ParseResult<'a, u8, T> {
    move |span: &Span<'a, T>| {
        let next_byte = span.as_bytes()[0];
        if next_byte == b {
            return Ok((span.consume(1), next_byte, None));
        }
        Err(ErrorCollector::new(*span, format!("Expected byte {}", b)))
    }
}

fn ascii<'a, T: Tracer<'a>>(c: char) -> impl FnMut(&Span<'a, T>) -> ParseResult<'a, char, T> {
    move |span: &Span<'a, T>| {
        let next_byte = span.as_bytes()[0];
        if next_byte == (c as u8) {
            return Ok((span.consume(1), next_byte as char, None));
        }
        Err(ErrorCollector::new(*span, format!("Expected char '{}'", c)))
    }
}

pub struct Seq<'a, T, OFirst, ONext, PFirst, PNext>
where T: Tracer<'a>, PFirst: Parser<'a, OFirst, T>, PNext: Parser<'a, ONext, T> {
    first: PFirst,
    next: PNext,
    _phantom: PhantomData<&'a (OFirst, ONext, T)>,
}
impl<'a, T, OFirst, ONext, PFirst, PNext> Parser<'a, (OFirst, ONext), T>
for Seq<'a, T, OFirst, ONext, PFirst, PNext>
where T: Tracer<'a>, PFirst: Parser<'a, OFirst, T>, PNext: Parser<'a, ONext, T> {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, (OFirst, ONext), T> {
        let (remaining, output, trailing_e) = self.first.parse(span)?;
        match self.next.parse(&remaining) {
            Err(e) => {
                match trailing_e {
                    None => Err(e),
                    Some(trailing_e) => Err(e.longest(trailing_e)),
                }
            }
            Ok((remaining, next_output, next_trailing_e)) => {
                Ok((remaining, (output, next_output), match trailing_e {
                    None => next_trailing_e,
                    Some(e) => match next_trailing_e {
                        None => Some(e),
                        Some(e2) => Some(e.longest(e2)),
                    }
                }))
            }
        }
    }
}

fn seq<'a, T, OFirst, ONext, PFirst, PNext>(
    first: PFirst, next: PNext
) -> Seq<'a, T, OFirst, ONext, PFirst, PNext>
where T: Tracer<'a>, PFirst: Parser<'a, OFirst, T>, PNext: Parser<'a, ONext, T> {
    Seq {
        first, next,
        _phantom: PhantomData,
    }
}

pub struct Many<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> {
    target: P,
    _phantom: PhantomData<&'a (O, T)>,
}
impl<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> Parser<'a, Vec<O>, T> for Many<'a, T, O, P> {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, Vec<O>, T> {
        let mut acc = vec![];
        let mut current_span = *span;
        let mut trailing_e = None;

        while current_span.as_bytes().len() > 0 {
            match self.target.parse(&current_span) {
                Err(e) => {
                    let combo_e = e.maybe_longest(trailing_e);
                    if acc.len() == 0 {
                        return Err(combo_e);
                    }
                    return Ok((current_span, acc, Some(combo_e)));
                }
                Ok((remaining, output, e)) => {
                    acc.push(output);
                    current_span = remaining;
                    trailing_e = e;
                }
            }
        }

        Ok((current_span, acc, None))
    }
}
fn many<'a, T, O, P>(target: P) -> Many<'a, T, O, P>
where T: Tracer<'a>, P: Parser<'a, O, T> {
    Many {
        target,
        _phantom: PhantomData,
    }
}

pub struct Opt<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> {
    target: P,
    _phantom: PhantomData<&'a (O, T)>,
}
impl<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> Parser<'a, Option<O>, T> for Opt<'a, T, O, P> {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, Option<O>, T> {
        match self.target.parse(span) {
            Err(e) => Ok((*span, None, Some(e))),
            Ok((remaining, output, trailing_e)) => Ok((remaining, Some(output), trailing_e)),
        }
    }
}
fn opt<'a, T, O, P>(target: P) -> Opt<'a, T, O, P>
where T: Tracer<'a>, P: Parser<'a, O, T> {
    Opt {
        target,
        _phantom: PhantomData,
    }
}

pub struct Any<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> {
    target: P,
    _phantom: PhantomData<&'a (O, T)>,
}
impl<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> Parser<'a, Option<Vec<O>>, T> for Any<'a, T, O, P> {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, Option<Vec<O>>, T> {
        let mut acc = vec![];
        let mut current_span = *span;
        let mut trailing_e = None;

        while current_span.as_bytes().len() > 0 {
            match self.target.parse(&current_span) {
                Err(e) => {
                    let combo = e.maybe_longest(trailing_e);
                    if acc.len() == 0 {
                        return Ok((current_span, None, Some(combo)));
                    }
                    return Ok((current_span, Some(acc), Some(combo)));
                }
                Ok((remaining, output, e)) => {
                    acc.push(output);
                    current_span = remaining;
                    trailing_e = e;
                }
            }
        }
        Ok((current_span, Some(acc), None))
    }
}
fn any<'a, T, O, P>(target: P) -> Any<'a, T, O, P>
where T: Tracer<'a>, P: Parser<'a, O, T> {
    Any {
        target,
        _phantom: PhantomData,
    }
}

pub struct Count<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> {
    target: P,
    _phantom: PhantomData<&'a (O, T)>,
}
impl<'a, T: Tracer<'a>, O, P: Parser<'a, O, T>> Parser<'a, usize, T> for Count<'a, T, O, P> {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, usize, T> {
        let mut acc = 0;
        let mut current_span = *span;
        let mut trailing_e = None;

        while current_span.as_bytes().len() > 0 {
            match self.target.parse(&current_span) {
                Err(e) => {
                    return Ok((current_span, acc, Some(e.maybe_longest(trailing_e))));
                }
                Ok((remaining, _, e)) => {
                    acc += 1;
                    current_span = remaining;
                    trailing_e = e;
                }
            }
        }
        Ok((current_span, acc, None))
    }
}
fn count<'a, T, O, P>(target: P) -> Count<'a, T, O, P>
where T: Tracer<'a>, P: Parser<'a, O, T> {
    Count {
        target,
        _phantom: PhantomData,
    }
}

pub struct Choose<'a, T, O, L, R>
where T: Tracer<'a>, L: Parser<'a, O, T>, R: Parser<'a, O, T> {
    left: L,
    right: R,
    _phantom: PhantomData<&'a (O, T)>,
}
impl<'a, T, O, L, R> Parser<'a, O, T> for Choose<'a, T, O, L, R>
where T: Tracer<'a>, L: Parser<'a, O, T>, R: Parser<'a, O, T> {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T> {
        match self.left.parse(span) {
            Err(left_err) => {
                match self.right.parse(span) {
                    Err(right_err) => {
                        Err(left_err.longest(right_err))
                    },
                    default => default,
                }
            }
            default => default,
        }
    }
}
fn choose<'a, T, O, L, R>(left: L, right: R) -> Choose<'a, T, O, L, R>
where T: Tracer<'a>, L: Parser<'a, O, T>, R: Parser<'a, O, T> {
    Choose {
        left, right,
        _phantom: PhantomData,
    }
}

pub struct Map<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(I) -> O {
    target: P,
    cb: F,
    _phantom: PhantomData<&'a (T, I, O)>,
}
impl<'a, T, I, O, P, F> Parser<'a, O, T> for Map<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(I) -> O {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T> {
        let (remaining, output, e) = self.target.parse(span)?;
        Ok((remaining, (self.cb)(output), e))
    }
}
fn map<'a, T, I, O, P, F>(target: P, cb: F) -> Map<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(I) -> O {
    Map {
        target, cb,
        _phantom: PhantomData,
    }
}

pub struct SpanMap<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(&Span<'a, T>) -> O {
    target: P,
    cb: F,
    _phantom: PhantomData<&'a (T, I, O)>,
}
impl<'a, T, I, O, P, F> Parser<'a, O, T> for SpanMap<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(&Span<'a, T>) -> O {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T> {
        let start = span.start;
        let (remaining, _, e) = self.target.parse(span)?;
        let consumed = Span { start, end: remaining.start, input: span.input, tracer: span.tracer };
        Ok((remaining, (self.cb)(&consumed), e))
    }
}
fn span<'a, T, I, O, P, F>(target: P, cb: F) -> SpanMap<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(&Span<'a, T>) -> O {
    SpanMap {
        target, cb,
        _phantom: PhantomData,
    }
}

fn expect_byte<'a, T: Tracer<'a>, F>(msg: &'a str, f: F) -> impl Parser<'a, u8, T>
where F: Fn(u8) -> bool {
    move |span: &Span<'a, T>| {
        let current_byte = span.as_bytes()[0];
        if f(current_byte) {
            return Ok((span.consume(1), current_byte, None));
        }
        Err(ErrorCollector::new(*span, msg.to_string()))
    }
}

fn alphanumeric<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    map(expect_byte("alphanumeric", |byte| {
        byte.is_ascii_alphanumeric()
    }), |byte| {
        byte as char
    })
}

fn alphabetic<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    map(expect_byte("alphabetic", |byte| {
        byte.is_ascii_alphabetic()
    }), |byte| {
        byte as char
    })
}

fn id<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, Ast<'a>, T> {
    span(alphabetic().then(count(alphanumeric())), |span| {
        Ast::Identifier(span.as_str())
    })
}

fn whitespace<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    choose(ascii(' '), ascii('\n'))
}

fn call<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, Ast<'a>, T> {
    map(
        ascii('(').then(opt(ascii(' '))).then(any(id().then(any(ascii(' '))))).then(ascii(')')),
        |output| {
            let (((_, _), ids), _) = output;
            match ids {
                None => Ast::Nil,
                Some(ids) => Ast::Call(ids.into_iter().map(|(id, _)| {
                    id
                }).collect()),
            }
        }
    )
}

#[derive(Debug)]
pub enum Ast<'a> {
    Call(Vec<Ast<'a>>),
    Identifier(&'a str),
    Nil,
}

pub fn parse<'a>(input: &'a str) -> Result<Ast<'a>, String> {
    if input.len() == 0 {
        return Ok(Ast::Nil);
    }

    let span = Span::new(input, ());
    match call().parse(&span) {
        Ok((_, output, _)) => {
            Ok(output)
        }
        Err(e) => {
            Err(format!("Error at index {}: {:?}", e.longest_chain.start, e.messages))
        }
    }
}
