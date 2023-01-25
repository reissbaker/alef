use std::marker::PhantomData;
use std::fmt::Debug;
use crate::span::{Span, Tracer, Trace, PrintlnTracer};
use crate::errors::{format_error, ErrorPicker, ParseError};

#[derive(Debug, Clone, Copy)]
pub enum ErrorKinds {
    Alphanumeric,
    Alphabetic,
    Digit,
}

type ErrorCollector<'a, T> = ErrorPicker<'a, T, ErrorKinds>;
type OkReturn<'a, O, T> = (Span<'a, T>, O, Option<ErrorCollector<'a, T>>);
type ParseResult<'a, O, T> = Result<OkReturn<'a, O, T>, ErrorCollector<'a, T>>;

pub trait Parser<'a, O, T: Tracer<'a>> where Self: Sized {
    fn do_parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T>;

    fn parse(&mut self, span: &Span<'a, T>) -> ParseResult<'a, O, T> {
        #[cfg(debug_assertions)]
        span.trace(Trace::StartParse);
        let result = self.do_parse(span);

        // Skip tracing if we're not in debug mode
        #[cfg(not(debug_assertions))]
        return result;

        // Trace if we're in debug mode
        #[cfg(debug_assertions)]
        match result {
            Err(e) => {
                // TODO: trace should accept error kinds
                span.trace(Trace::Err);
                Err(e)
            }
            Ok((remaining, data, e)) => {
                // TODO: ditto
                span.trace(Trace::Ok(e.as_ref().map(|_| ())));
                Ok((remaining, data, e))
            }
        }
    }

    fn then<ONext, PNext>(self, next: PNext) -> Seq<'a, T, O, ONext, Self, PNext>
    where PNext: Parser<'a, ONext, T> {
        seq(self, next)
    }

    fn or<P: Parser<'a, O, T>>(self, target: P) -> Choose<'a, T, O, Self, P> {
        choose(self, target)
    }

    fn opt(self) -> Opt<'a, T, O, Self> {
        opt(self)
    }

    fn any(self) -> Any<'a, T, O, Self> {
        any(self)
    }

    fn many(self) -> Many<'a, T, O, Self> {
        many(self)
    }

    fn map<O2, F: Fn(O) -> O2>(self, cb: F) -> Map<'a, T, O, O2, Self, F> {
        map(self, cb)
    }

    fn map_span<O2, F: Fn(&Span<'a, T>) -> O2>(self, cb: F) -> SpanMap<'a, T, O, O2, Self, F> {
        map_span(self, cb)
    }

    fn count(self) -> Count<'a, T, O, Self> {
        count(self)
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
        Err(ErrorCollector::new(*span, ParseError::Byte(b)))
    }
}

fn ascii<'a, T: Tracer<'a>>(c: char) -> impl FnMut(&Span<'a, T>) -> ParseResult<'a, char, T> {
    move |span: &Span<'a, T>| {
        let next_byte = span.as_bytes()[0];
        if next_byte == (c as u8) {
            return Ok((span.consume(1), next_byte as char, None));
        }
        Err(ErrorCollector::new(*span, ParseError::Char(c)))
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
        let consumed = span.get_consumed(remaining.start);
        Ok((remaining, (self.cb)(&consumed), e))
    }
}
fn map_span<'a, T, I, O, P, F>(target: P, cb: F) -> SpanMap<'a, T, I, O, P, F>
where T: Tracer<'a>, P: Parser<'a, I, T>, F: Fn(&Span<'a, T>) -> O {
    SpanMap {
        target, cb,
        _phantom: PhantomData,
    }
}

fn take_while<'a, T: Tracer<'a>, F: Fn(u8) -> bool>(e: ParseError<ErrorKinds>, f: F) -> impl FnMut(&Span<'a, T>) -> ParseResult<'a, &'a str, T> {
    move |span: &Span<'a, T>| {
        let mut count = 0;
        let mut did_err = false;
        for index in 0..span.len() {
            let current_byte = span.as_bytes()[index];
            if !f(current_byte) {
                did_err = true;
                break;
            }
            count += 1;
        }
        if count == 0 {
            return Err(ErrorCollector::new(*span, e));
        }
        let new_span = span.consume(count);
        if did_err {
            return Ok((new_span, span.as_str(), Some(ErrorCollector::new(*span, e))));
        }
        Ok((new_span, span.as_str(), None))
    }
}

fn expect_byte<'a, T: Tracer<'a>, F>(e: ParseError<ErrorKinds>, f: F) -> impl FnMut(&Span<'a, T>) -> ParseResult<'a, u8, T>
where F: Fn(u8) -> bool {
    move |span: &Span<'a, T>| {
        let current_byte = span.as_bytes()[0];
        if f(current_byte) {
            return Ok((span.consume(1), current_byte, None));
        }
        Err(ErrorCollector::new(*span, e))
    }
}

fn digit<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    expect_byte(ParseError::Kind(ErrorKinds::Digit), |byte| {
        byte.is_ascii_digit()
    }).map(|byte| {
        byte as char
    })
}

fn digit_str<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, &'a str, T> {
    take_while(ParseError::Kind(ErrorKinds::Digit), |byte| {
        byte.is_ascii_digit()
    })
}

fn alphanumeric_str<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, &'a str, T> {
    take_while(ParseError::Kind(ErrorKinds::Alphanumeric), |byte| {
        byte.is_ascii_alphanumeric()
    })
}

fn alphanumeric<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    expect_byte(ParseError::Kind(ErrorKinds::Alphanumeric), |byte| {
        byte.is_ascii_alphanumeric()
    }).map(|byte| {
        byte as char
    })
}

fn alphabetic<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    expect_byte(ParseError::Kind(ErrorKinds::Alphabetic), |byte| {
        byte.is_ascii_alphabetic()
    }).map(|byte| {
        byte as char
    })
}

fn int<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, Ast<'a>, T> {
    digit_str().map_span(|span| {
        Ast::Int(span.as_str().parse::<i64>().unwrap())
    })
}

fn float<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, Ast<'a>, T> {
    digit_str().then(ascii('.')).then(digit_str()).map_span(|span| {
        Ast::Float(span.as_str().parse::<f64>().unwrap())
    })
}

fn number<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, Ast<'a>, T> {
    float().or(int())
}

fn id_str<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, &'a str, T> {
    alphabetic().then(alphanumeric_str().opt()).map_span(|span| {
        span.as_str()
    })
}
fn id<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, Ast<'a>, T> {
    id_str().map(|output| {
        Ast::Identifier(output)
    })
}

fn space<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    ascii(' ')
}

fn newline<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    ascii('\n')
}

fn whitespace<'a, T: 'a + Tracer<'a>>() -> impl Parser<'a, char, T> {
    choose(space(), newline())
}

fn call<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Ast<'a>, T> {
    ascii('(')
        .then(whitespace().any())
        .then(
            any(
                seq(expr, whitespace().any())
            )
        )
        .then(ascii(')'))
        .map(|output| {
            let (((_, _), exprs), _) = output;
            match exprs {
                None => Ast::Nil,
                Some(exprs) => Ast::Call(exprs.into_iter().map(|(expr, _)| {
                    expr
                }).collect()),
            }
        }).parse(input)
}

fn list<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Ast<'a>, T> {
    ascii('[')
        .then(whitespace().any())
        .then(
            any(
                seq(expr, whitespace().any())
            )
        )
        .then(ascii(']'))
        .map(|output| {
            let (((_, _), exprs), _) = output;
            match exprs {
                None => Ast::Nil,
                Some(exprs) => Ast::List(exprs.into_iter().map(|(expr, _)| {
                    expr
                }).collect()),
            }
        }).parse(input)
}

fn dict<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Ast<'a>, T> {
    ascii('{')
        .then(whitespace().count())
        .then(choose(pairs_oneline, pairs_multiline).or(no_pairs))
        .then(whitespace().count())
        .then(ascii('}'))
        .map(|output| {
            let ((((_, _), pairs), _), _) = output;
            Ast::Dict(pairs)
        }).parse(input)
}

fn no_pairs<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Vec<(&'a str, Ast<'a>)>, T> {
    whitespace().count().map_span(|_| {
        vec![]
    }).parse(input)
}


fn pairs_oneline<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Vec<(&'a str, Ast<'a>)>, T> {
    any(seq(pair, whitespace().count()).then(ascii(',')).then(whitespace().count()))
        .then(pair)
        .then(ascii(',').opt())
        .map(|output| {
            let ((initial, final_pair), _) = output;
            let mut new_vec = vec![];
            match initial {
                None => {
                    new_vec.push(final_pair);
                    return new_vec;
                }
                Some(pairs) => {
                    new_vec.extend(pairs.into_iter().map(|data| {
                        let (((pair, _), _), _) = data;
                        pair
                    }).collect::<Vec<(&'a str, Ast<'a>)>>());
                    new_vec.push(final_pair);
                    return new_vec;
                }
            }
        }).parse(input)
}

fn pairs_multiline<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Vec<(&'a str, Ast<'a>)>, T> {
    any(
        seq(pair, space().count())
            .then(ascii(',').opt())
            .then(space().count())
            .then(newline())
            .then(whitespace().count())
    )
        .map(|output| {
            match output {
                None => vec![],
                Some(output) => {
                    output.into_iter().map(|item| {
                        let (((((pair, _), _), _), _), _) = item;
                        pair
                    }).collect()
                }
            }
        }).parse(input)
}

fn pair<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, (&'a str, Ast<'a>), T> {
    id_str()
        .then(whitespace().count())
        .then(ascii(':'))
        .then(whitespace().count())
        .then(expr)
        .map(|output| {
            let ((((id, _), _), _), ast) = output;
            (id, ast)
        }).parse(input)
}

fn expr<'a, T: 'a + Tracer<'a>>(input: &Span<'a, T>) -> ParseResult<'a, Ast<'a>, T> {
    number().or(id()).or(call).or(dict).or(list).parse(input)
}

#[derive(Debug)]
pub enum Ast<'a> {
    Call(Vec<Ast<'a>>),
    Dict(Vec<(&'a str, Ast<'a>)>),
    List(Vec<Ast<'a>>),
    Identifier(&'a str),
    Int(i64),
    Float(f64),
    Nil,
}

pub fn parse<'a>(input: &'a str) -> Result<Vec<Ast<'a>>, String> {
    if input.len() == 0 {
        return Ok(vec![]);
    }

    let span = Span::new(input, ());
    let parsed = many(
        whitespace().any()
        .then(call)
        .then(whitespace().any())
        .map(|output| {
            let ((_, calls), _) = output;
            calls
        })
    ).parse(&span);

    match parsed {
        Ok((remaining, output, e)) => {
            if remaining.as_bytes().len() != 0 {
                return Err(format_error(e.unwrap()));
            }
            Ok(output)
        }
        Err(e) => {
            Err(format_error(e))
        }
    }
}
