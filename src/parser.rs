use std::marker::PhantomData;
use std::fmt::Debug;
use crate::span::{Span, Trace, Tracers};
use crate::errors::{ErrorPicker, ParseError};
use crate::ast::{Ast, AstSpan};

#[derive(Debug, Clone, Copy)]
pub enum ErrorKinds {
    Alphanumeric,
    Alphabetic,
    Digit,
}

type ErrorCollector<'a> = ErrorPicker<'a, ErrorKinds>;
type OkReturn<'a, O> = (Span<'a>, O, Option<ErrorCollector<'a>>);
type ParseResult<'a, O> = Result<OkReturn<'a, O>, ErrorCollector<'a>>;

pub trait Parser<'a, O> where Self: Sized {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O>;

    fn parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
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

    fn then<ONext, PNext>(self, next: PNext) -> Seq<'a, O, ONext, Self, PNext>
    where PNext: Parser<'a, ONext> {
        seq(self, next)
    }

    fn or<P: Parser<'a, O>>(self, target: P) -> Choose<'a, O, Self, P> {
        choose(self, target)
    }

    fn opt(self) -> Opt<'a, O, Self> {
        opt(self)
    }

    fn any(self) -> Any<'a, O, Self> {
        any(self)
    }

    fn many(self) -> Many<'a, O, Self> {
        many(self)
    }

    fn map<O2, F: Fn(O, &Span<'a>) -> O2>(self, cb: F) -> Map<'a, O, O2, Self, F> {
        map(self, cb)
    }

    fn map_span<O2, F: Fn(&Span<'a>) -> O2>(self, cb: F) -> SpanMap<'a, O, O2, Self, F> {
        map_span(self, cb)
    }

    fn count(self) -> Count<'a, O, Self> {
        count(self)
    }

    fn peek<O2, P2: Parser<'a, O2>>(self, target: P2) -> Peek<'a, O, O2, Self, P2> {
        peek(self, target)
    }

    fn debug(self, msg: &'a str) -> DebugParser<'a, O, Self> {
        debug(msg, self)
    }
}

impl<'a, O, F: FnMut(&Span<'a>) -> ParseResult<'a, O>> Parser<'a, O> for F {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
        self(span)
    }
}

fn eoi<'a>() -> impl FnMut(&Span<'a>) -> ParseResult<'a, ()> {
    move |span: &Span<'a>| {
        if span.len() == 0 {
            return Ok((*span, (), None));
        }
        Err(ErrorCollector::new(*span, ParseError::EOI))
    }
}

fn byte<'a>(b: u8) -> impl FnMut(&Span<'a>) -> ParseResult<'a, u8> {
    move |span: &Span<'a>| {
        let next_byte = span.as_bytes()[0];
        if next_byte == b {
            return Ok((span.consume(1), next_byte, None));
        }
        Err(ErrorCollector::new(*span, ParseError::Byte(b)))
    }
}

fn ascii<'a>(c: char) -> impl FnMut(&Span<'a>) -> ParseResult<'a, char> {
    move |span: &Span<'a>| {
        let next_byte = span.as_bytes()[0];
        if next_byte == (c as u8) {
            return Ok((span.consume(1), next_byte as char, None));
        }
        Err(ErrorCollector::new(*span, ParseError::Char(c)))
    }
}

pub struct Peek<'a, O, O2, P: Parser<'a, O>, P2: Parser<'a, O2>> {
    target: P,
    peek_target: P2,
    _phantom: PhantomData<&'a (O, O2)>,
}
impl<'a, O, O2, P: Parser<'a, O>, P2: Parser<'a, O2>> Parser<'a, O> for Peek<'a, O, O2, P, P2> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
        let (remaining, data, trailing_e) = self.target.parse(span)?;
        self.peek_target.parse(&remaining)?;
        Ok((remaining, data, trailing_e))
    }
}

fn peek<'a, O, O2, P: Parser<'a, O>, P2: Parser<'a, O2>>(target: P, peek_target: P2) -> Peek<'a, O, O2, P, P2> {
    Peek {
        target, peek_target,
        _phantom: PhantomData,
    }
}

pub struct DebugParser<'a, O, P: Parser<'a, O>> {
    msg: &'a str,
    target: P,
    _phantom: PhantomData<O>,
}
impl<'a, O, P: Parser<'a, O>> Parser<'a, O> for DebugParser<'a, O, P> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
        println!("{}", self.msg);
        self.target.parse(span)
    }
}
pub fn debug<'a, O, P: Parser<'a, O>>(msg: &'a str, target: P) -> DebugParser<'a, O, P> {
    DebugParser {
        msg, target,
        _phantom: PhantomData,
    }
}

pub struct StrMatch<'a> {
    target: &'a str
}
impl<'a> Parser<'a, &'a str> for StrMatch<'a> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, &'a str> {
        let target_bytes = self.target.as_bytes();
        let source_bytes = span.as_bytes();
        for i in 0..self.target.len() {
            if target_bytes[i] != source_bytes[i] {
                return Err(ErrorCollector::new(
                    span.err_consume(i),
                    ParseError::Char(target_bytes[i] as char)
                ));
            }
        }
        let count = self.target.len();
        Ok((span.consume(count), self.target, None))
    }
}
fn ascii_str<'a>(target: &'a str) -> StrMatch<'a> {
    StrMatch { target }
}

pub struct Seq<'a, OFirst, ONext, PFirst, PNext>
where PFirst: Parser<'a, OFirst>, PNext: Parser<'a, ONext> {
    first: PFirst,
    next: PNext,
    _phantom: PhantomData<&'a (OFirst, ONext)>,
}
impl<'a, OFirst, ONext, PFirst, PNext> Parser<'a, (OFirst, ONext)>
for Seq<'a, OFirst, ONext, PFirst, PNext>
where PFirst: Parser<'a, OFirst>, PNext: Parser<'a, ONext> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, (OFirst, ONext)> {
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

fn seq<'a, OFirst, ONext, PFirst, PNext>(
    first: PFirst, next: PNext
) -> Seq<'a, OFirst, ONext, PFirst, PNext>
where PFirst: Parser<'a, OFirst>, PNext: Parser<'a, ONext> {
    Seq {
        first, next,
        _phantom: PhantomData,
    }
}

pub struct Many<'a, O, P: Parser<'a, O>> {
    target: P,
    _phantom: PhantomData<&'a O>,
}
impl<'a, O, P: Parser<'a, O>> Parser<'a, Vec<O>> for Many<'a, O, P> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, Vec<O>> {
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
fn many<'a, O, P>(target: P) -> Many<'a, O, P>
where P: Parser<'a, O> {
    Many {
        target,
        _phantom: PhantomData,
    }
}

pub struct Opt<'a, O, P: Parser<'a, O>> {
    target: P,
    _phantom: PhantomData<&'a O>,
}
impl<'a, O, P: Parser<'a, O>> Parser<'a, Option<O>> for Opt<'a, O, P> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, Option<O>> {
        match self.target.parse(span) {
            Err(e) => Ok((*span, None, Some(e))),
            Ok((remaining, output, trailing_e)) => Ok((remaining, Some(output), trailing_e)),
        }
    }
}
fn opt<'a, O, P>(target: P) -> Opt<'a, O, P>
where P: Parser<'a, O> {
    Opt {
        target,
        _phantom: PhantomData,
    }
}

pub struct Any<'a, O, P: Parser<'a, O>> {
    target: P,
    _phantom: PhantomData<&'a O>,
}
impl<'a, O, P: Parser<'a, O>> Parser<'a, Option<Vec<O>>> for Any<'a, O, P> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, Option<Vec<O>>> {
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
fn any<'a, O, P>(target: P) -> Any<'a, O, P>
where P: Parser<'a, O> {
    Any {
        target,
        _phantom: PhantomData,
    }
}

pub struct Count<'a, O, P: Parser<'a, O>> {
    target: P,
    _phantom: PhantomData<&'a O>,
}
impl<'a, O, P: Parser<'a, O>> Parser<'a, usize> for Count<'a, O, P> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, usize> {
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
fn count<'a, O, P>(target: P) -> Count<'a, O, P>
where P: Parser<'a, O> {
    Count {
        target,
        _phantom: PhantomData,
    }
}

pub struct Choose<'a, O, L, R>
where L: Parser<'a, O>, R: Parser<'a, O> {
    left: L,
    right: R,
    _phantom: PhantomData<&'a O>,
}
impl<'a, O, L, R> Parser<'a, O> for Choose<'a, O, L, R>
where L: Parser<'a, O>, R: Parser<'a, O> {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
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
fn choose<'a, O, L, R>(left: L, right: R) -> Choose<'a, O, L, R>
where L: Parser<'a, O>, R: Parser<'a, O> {
    Choose {
        left, right,
        _phantom: PhantomData,
    }
}

pub struct Map<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(I, &Span<'a>) -> O {
    target: P,
    cb: F,
    _phantom: PhantomData<&'a (I, O)>,
}
impl<'a, I, O, P, F> Parser<'a, O> for Map<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(I, &Span<'a>) -> O {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
        let (remaining, output, e) = self.target.parse(span)?;
        let consumed = span.get_consumed(remaining.start);
        Ok((remaining, (self.cb)(output, &consumed), e))
    }
}
fn map<'a, I, O, P, F>(target: P, cb: F) -> Map<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(I, &Span<'a>) -> O {
    Map {
        target, cb,
        _phantom: PhantomData,
    }
}

pub struct SpanMap<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(&Span<'a>) -> O {
    target: P,
    cb: F,
    _phantom: PhantomData<&'a (I, O)>,
}
impl<'a, I, O, P, F> Parser<'a, O> for SpanMap<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(&Span<'a>) -> O {
    fn do_parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
        let (remaining, _, e) = self.target.parse(span)?;
        let consumed = span.get_consumed(remaining.start);
        Ok((remaining, (self.cb)(&consumed), e))
    }
}
fn map_span<'a, I, O, P, F>(target: P, cb: F) -> SpanMap<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(&Span<'a>) -> O {
    SpanMap {
        target, cb,
        _phantom: PhantomData,
    }
}

fn take_while<'a, F: Fn(u8) -> bool>(e: ParseError<ErrorKinds>, f: F) -> impl FnMut(&Span<'a>) -> ParseResult<'a, &'a str> {
    move |span: &Span<'a>| {
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

fn expect_byte<'a, F>(e: ParseError<ErrorKinds>, f: F) -> impl FnMut(&Span<'a>) -> ParseResult<'a, u8>
where F: Fn(u8) -> bool {
    move |span: &Span<'a>| {
        let current_byte = span.as_bytes()[0];
        if f(current_byte) {
            return Ok((span.consume(1), current_byte, None));
        }
        Err(ErrorCollector::new(*span, e))
    }
}

fn digit<'a>() -> impl Parser<'a, char> {
    expect_byte(ParseError::Kind(ErrorKinds::Digit), |byte| {
        byte.is_ascii_digit()
    }).map(|byte, _| {
        byte as char
    })
}

fn digit_str<'a>() -> impl Parser<'a, &'a str> {
    take_while(ParseError::Kind(ErrorKinds::Digit), |byte| {
        byte.is_ascii_digit()
    })
}

fn alphanumeric_str<'a>() -> impl Parser<'a, &'a str> {
    take_while(ParseError::Kind(ErrorKinds::Alphanumeric), |byte| {
        byte.is_ascii_alphanumeric()
    })
}

fn alphanumeric<'a>() -> impl Parser<'a, char> {
    expect_byte(ParseError::Kind(ErrorKinds::Alphanumeric), |byte| {
        byte.is_ascii_alphanumeric()
    }).map(|byte, _| {
        byte as char
    })
}

fn alphanumeric_or_underscore_str<'a>() -> impl Parser<'a, &'a str> {
    take_while(ParseError::Kind(ErrorKinds::Alphanumeric), |byte| {
        byte.is_ascii_alphanumeric() || byte == 95
    })
}

fn alphabetic<'a>() -> impl Parser<'a, char> {
    expect_byte(ParseError::Kind(ErrorKinds::Alphabetic), |byte| {
        byte.is_ascii_alphabetic()
    }).map(|byte, _| {
        byte as char
    })
}

fn int<'a>() -> impl Parser<'a, Ast<'a>> {
    digit_str().map_span(|span| {
        Ast::Int(span.into(), span.as_str().parse::<i64>().unwrap())
    })
}

fn float<'a>() -> impl Parser<'a, Ast<'a>> {
    digit_str().then(ascii('.')).then(digit_str()).map_span(|span| {
        Ast::Float(span.into(), span.as_str().parse::<f64>().unwrap())
    })
}

fn number<'a>() -> impl Parser<'a, Ast<'a>> {
    float().or(int()).peek(trailing_values())
}

fn id_str<'a>() -> impl Parser<'a, &'a str> {
    alphabetic()
        .then(alphanumeric_or_underscore_str().opt())
        .then(ascii('?').opt())
        .peek(trailing_values())
        .map_span(|span| {
            span.as_str()
        })
}
fn id<'a>() -> impl Parser<'a, Ast<'a>> {
    id_str().map(|output, span| {
        Ast::Identifier(span.into(), output)
    })
}

fn field<'a>() -> impl Parser<'a, Ast<'a>> {
    ascii('.').then(id_str().or(operator_str())).map_span(|span| {
        let dotless = span.slice(1, span.len());
        Ast::Field(span.into(), dotless.as_str())
    })
}

fn space<'a>() -> impl Parser<'a, char> {
    ascii(' ')
}

fn newline<'a>() -> impl Parser<'a, char> {
    ascii('\n')
}

fn whitespace<'a>() -> impl Parser<'a, char> {
    choose(space(), newline())
}

fn trailing_values<'a>() -> impl Parser<'a, char> {
    whitespace()
        .or(ascii(')'))
        .or(ascii('}'))
        .or(ascii(']'))
        .or(ascii('>'))
        .or(ascii(':'))
        .or(ascii('.'))
        .or(eoi().map(|_, _| {
            // TODO: Make an either() that returns a left vs right value, so you can appropriately
            // model nulls and varying return types, while keeping the default of coalescing values
            // of the same type for or() since that's usually more convenient
            // But whatever null byte here is fine I guess
            0 as char
        }))
}

fn surrounded<'a, O, FO, LO, F, L, P>(first: F, last: L, parser: P) -> impl Parser<'a, O>
where O: 'a,
FO: 'a,
LO: 'a,
F: Parser<'a, FO>,
L: Parser<'a, LO>,
P: Parser<'a, O> {
    first
        .then(whitespace().count())
        .then(parser)
        .then(whitespace().count())
        .then(last)
        .map(|output, _| {
            let ((((_, _), exprs), _), _) = output;
            exprs
        })
}

fn typelist<'a>(input: &Span<'a>) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('<'), ascii('>'),
        seq(expr, whitespace().count()).then(id_str())
    ).map(|output, span| {
        let ((ast, _), id_str) = output;
        Ast::TypeAssert(span.into(), Box::new(ast), id_str)
    }).parse(input)
}

fn macro_call<'a>(input: &Span<'a>) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('{'), ascii('}'),
        many(seq(expr, whitespace().any()))
    )
    .map(|exprs, span| {
        Ast::Macro(span.into(), exprs.into_iter().map(|(expr, _)| {
            expr
        }).collect())
    }).parse(input)
}

fn call<'a>(input: &Span<'a>) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('('), ascii(')'),
        any(seq(expr, whitespace().any()))
    )
    .map(|exprs, span| {
        match exprs {
            None => Ast::Call(span.into(), vec![]),
            Some(exprs) => Ast::Call(span.into(), exprs.into_iter().map(|(expr, _)| {
                expr
            }).collect()),
        }
    }).parse(input)
}

fn list<'a>(input: &Span<'a>) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('['), ascii(']'),
        any(seq(expr, whitespace().any()))
    )
    .map(|exprs, span| {
        match exprs {
            None => Ast::List(span.into(), vec![]),
            Some(exprs) => Ast::List(span.into(), exprs.into_iter().map(|(expr, _)| {
                expr
            }).collect()),
        }
    }).parse(input)
}

fn dict<'a>(input: &Span<'a>) -> ParseResult<'a, Ast<'a>> {
    ascii('{')
        .then(whitespace().count())
        .then(choose(pairs_multiline, pairs_oneline).or(no_pairs))
        .then(whitespace().count())
        .then(ascii('}'))
        .map(|output, span| {
            let ((((_, _), pairs), _), _) = output;
            Ast::Dict(span.into(), pairs)
        }).parse(input)
}

fn no_pairs<'a>(input: &Span<'a>) -> ParseResult<'a, Vec<((AstSpan, &'a str), Ast<'a>)>> {
    whitespace().count().map_span(|_| {
        vec![]
    }).parse(input)
}

fn pairs_oneline<'a>(input: &Span<'a>) -> ParseResult<'a, Vec<((AstSpan, &'a str), Ast<'a>)>> {
    any(seq(pair, whitespace().count()).then(ascii(',')).then(whitespace().count()))
        .then(pair)
        .map(|output, _| {
            let (initial, final_pair) = output;
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
                    }).collect::<Vec<((AstSpan, &'a str), Ast<'a>)>>());
                    new_vec.push(final_pair);
                    return new_vec;
                }
            }
        }).parse(input)
}

fn pairs_multiline<'a>(input: &Span<'a>) -> ParseResult<'a, Vec<((AstSpan, &'a str), Ast<'a>)>> {
    any(
        seq(pair, space().count())
            .then(ascii(',').opt())
            .then(space().count())
            .then(newline())
            .then(whitespace().count())
    )
        .map(|output, _| {
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

fn pair<'a>(input: &Span<'a>) -> ParseResult<'a, ((AstSpan, &'a str), Ast<'a>)> {
    id_str()
        .map(|output, span| {
            (span.into(), output)
        })
        .then(whitespace().count())
        .then(ascii(':'))
        .then(whitespace().count())
        .then(expr)
        .map(|output, _| {
            let ((((id, _), _), _), ast) = output;
            (id, ast)
        }).parse(input)
}

fn operator_str<'a>() -> impl Parser<'a, &'a str> {
    ascii_str("&&")
        .or(ascii_str("&"))
        .or(ascii_str("||"))
        .or(ascii_str("|"))
        .or(ascii_str("++"))
        .or(ascii_str("+"))
        .or(ascii_str("->"))
        .or(ascii_str("--"))
        .or(ascii_str("-="))
        .or(ascii_str("-"))
        .or(ascii_str("**"))
        .or(ascii_str("*"))
        .or(ascii_str("/"))
        .or(ascii_str("%"))
        .or(ascii_str("=="))
        .or(ascii_str("!="))
        .or(ascii_str("!"))
        .or(ascii_str(">="))
        .or(ascii_str(">"))
        .or(ascii_str("<="))
        .or(ascii_str("<"))
        .peek(trailing_values())
}

fn operator<'a>() -> impl Parser<'a, Ast<'a>> {
    operator_str()
        .map(|string, span| {
            Ast::Identifier(span.into(), string)
        })
}


fn expr<'a>(input: &Span<'a>) -> ParseResult<'a, Ast<'a>> {
    number()
        .or(id())
        .or(macro_call)
        .or(call)
        .or(dict)
        .or(list)
        .or(typelist)
        .or(operator())
        .or(field())
        .parse(input)
}

pub fn parse<'a>(input: &'a str) -> Result<Vec<Ast<'a>>, ErrorCollector<'a>> {
    if input.len() == 0 {
        return Ok(vec![]);
    }

    let span = Span::new(input, Tracers::Nil);
    let parsed = many(
        whitespace().any()
        .then(expr)
        .then(whitespace().any())
        .map(|output, _| {
            let ((_, exprs), _) = output;
            exprs
        })
    ).parse(&span);

    match parsed {
        Ok((remaining, output, e)) => {
            if remaining.as_bytes().len() != 0 {
                return Err(e.unwrap())
            }
            Ok(output)
        }
        Err(e) => {
            Err(e)
        }
    }
}
