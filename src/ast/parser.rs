use std::marker::PhantomData;
use std::fmt::Debug;
use crate::ast::span::Span;
use crate::ast::trace::{Trace, Tracers};
use crate::ast::errors::{ErrorPicker, ParseError};
use crate::ast::ast::{Ast, AstSpan};
use crate::ast::parse_context::ParseContext;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKinds {
    Alphanumeric,
    Alphabetic,
    Digit,
}

type ErrorCollector = ErrorPicker<ErrorKinds>;
type OkReturn<'a, O> = (Span<'a>, O, Option<ErrorCollector>);
type ParseResult<'a, O> = Result<OkReturn<'a, O>, ErrorCollector>;

pub trait Parser<'a, O> {
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O>;

    fn parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O> {
        #[cfg(debug_assertions)]
        ctx.trace(Trace::StartParse, span);
        let result = self.do_parse(span, ctx);

        // Skip tracing if we're not in debug mode
        #[cfg(not(debug_assertions))]
        return result;

        // Trace if we're in debug mode
        #[cfg(debug_assertions)]
        match result {
            Err(e) => {
                // TODO: trace should accept error kinds
                ctx.trace(Trace::Err, span);
                Err(e)
            }
            Ok((remaining, data, e)) => {
                // TODO: ditto
                ctx.trace(Trace::Ok(e.as_ref().map(|_| ())), span);
                Ok((remaining, data, e))
            }
        }
    }

    fn opt(self) -> Opt<'a, O, Self>
    where Self: Sized {
        opt(self)
    }

    fn any(self) -> Any<'a, O, Self>
    where Self: Sized {
        any(self)
    }

    fn many(self) -> Many<'a, O, Self>
    where Self: Sized {
        many(self)
    }

    fn map<O2, F: Fn(O, &Span<'a>) -> O2>(self, cb: F) -> Map<'a, O, O2, Self, F>
    where Self: Sized {
        map(self, cb)
    }

    fn map_span<O2, F: Fn(&Span<'a>) -> O2>(self, cb: F) -> SpanMap<'a, O, O2, Self, F>
    where Self: Sized {
        map_span(self, cb)
    }

    fn count(self) -> Count<'a, O, Self>
    where Self: Sized {
        count(self)
    }

    fn peek<O2, P2: Parser<'a, O2>>(self, target: P2) -> Peek<'a, O, O2, Self, P2>
    where Self: Sized {
        peek(self, target)
    }

    fn debug(self, msg: &'a str) -> DebugParser<'a, O, Self>
    where Self: Sized {
        debug(msg, self)
    }
}

impl<'a, O, F: FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, O>> Parser<'a, O> for F {
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O> {
        self(span, ctx)
    }
}

fn eoi<'a>() -> impl FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, ()> {
    move |span: &Span<'a>, ctx: &ParseContext| {
        if span.len() == 0 {
            return Ok((*span, (), None));
        }
        Err(ErrorCollector::new(span, ctx, ParseError::EOI))
    }
}

fn byte<'a>(b: u8) -> impl FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, u8> {
    move |span: &Span<'a>, ctx: &ParseContext| {
        let next_byte = span.as_bytes()[0];
        if next_byte == b {
            return Ok((span.consume(ctx, 1), next_byte, None));
        }
        Err(ErrorCollector::new(span, ctx, ParseError::Byte(b)))
    }
}

fn ascii<'a>(c: char) -> impl FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, char> {
    move |span: &Span<'a>, ctx: &ParseContext| {
        let next_byte = span.as_bytes()[0];
        if next_byte == (c as u8) {
            return Ok((span.consume(ctx, 1), next_byte as char, None));
        }
        Err(ErrorCollector::new(span, ctx, ParseError::Char(c)))
    }
}

pub struct Peek<'a, O, O2, P: Parser<'a, O>, P2: Parser<'a, O2>> {
    target: P,
    peek_target: P2,
    _phantom: PhantomData<&'a (O, O2)>,
}
impl<'a, O, O2, P: Parser<'a, O>, P2: Parser<'a, O2>> Parser<'a, O> for Peek<'a, O, O2, P, P2> {
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O> {
        let (remaining, data, trailing_e) = self.target.parse(span, ctx)?;
        self.peek_target.parse(&remaining, ctx)?;
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O> {
        println!(
            "{}\n{}\n{}",
            "=====================================================================================",
            self.msg,
            "=====================================================================================",
        );
        self.target.parse(span, ctx)
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, &'a str> {
        let target_bytes = self.target.as_bytes();
        let source_bytes = span.as_bytes();
        for i in 0..self.target.len() {
            if target_bytes[i] != source_bytes[i] {
                return Err(ErrorCollector::new(
                    &span.err_consume(i),
                    ctx,
                    ParseError::Char(target_bytes[i] as char)
                ));
            }
        }
        let count = self.target.len();
        Ok((span.consume(ctx, count), self.target, None))
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, (OFirst, ONext)> {
        let (remaining, output, trailing_e) = self.first.parse(span, ctx)?;
        match self.next.parse(&remaining, ctx) {
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

#[macro_export]
macro_rules! seq {
    ($l:expr, $r:expr)  => ( Seq { first: $l, next: $r, _phantom: PhantomData } );
    ($l:expr, $r:expr,) => ( Seq { first: $l, next: $r, _phantom: PhantomData } );
    ($o:expr, $($e:expr),*)  => ( Seq { first: $o, next: $crate::seq!($($e),*), _phantom: PhantomData });
    ($o:expr, $($e:expr),*,) => ( Seq { first: $o, next: $crate::seq!($($e),*), _phantom: PhantomData } );
}

pub struct Many<'a, O, P: Parser<'a, O>> {
    target: P,
    _phantom: PhantomData<&'a O>,
}
impl<'a, O, P: Parser<'a, O>> Parser<'a, Vec<O>> for Many<'a, O, P> {
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Vec<O>> {
        let mut acc = vec![];
        let mut current_span = *span;
        let mut trailing_e = None;

        while current_span.as_bytes().len() > 0 {
            match self.target.parse(&current_span, ctx) {
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Option<O>> {
        match self.target.parse(span, ctx) {
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Option<Vec<O>>> {
        let mut acc = vec![];
        let mut current_span = *span;
        let mut trailing_e = None;

        while current_span.as_bytes().len() > 0 {
            match self.target.parse(&current_span, ctx) {
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, usize> {
        let mut acc = 0;
        let mut current_span = *span;
        let mut trailing_e = None;

        while current_span.as_bytes().len() > 0 {
            match self.target.parse(&current_span, ctx) {
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

/*
 * A macro that generates a parsing closure by pattern-matching the given parsers responses,
 * returning the first one that is successful, or a compilation of their errors if none are.
 * For example, given parsers a, b, and c, it generates something like the following closure:
 *
 *     (|span: &Span<'a>, ctx: &ParseContext| {
 *         match a.parse(span, ctx) {
 *             Ok(data) => Ok(data),
 *             Err(e_a) => {
 *                 match b.parse(span, ctx) {
 *                     Ok(data) => Ok(data),
 *                     Err(e_b) => {
 *                         match c.parse(span, ctx) {
 *                             Ok(data) => Ok(data),
 *                             Err(e_c) => {
 *                                 if !ctx.collect_errors {
 *                                   e_c
 *                                 }
 *                                 else {
 *                                     Err(e_a.longest(
 *                                         e_b.longest(
 *                                             e_c
 *                                         )
 *                                     ))
 *                                 }
 *                             }
 *                         }
 *                     }
 *                 }
 *             }
 *         }
 *     })
 *
 * This is obviously a huge pain to write by hand, hence the closure-generating macro. Previously
 * we used a homogenous list that recursively called .parse on itself, similar to seq!, but since
 * choose! has to combine all errors and can't early-exit on first error like seq!, there was a
 * performance penalty on all of the recursive calls and the repeated forward-propagation of any
 * previous error in the list. While this macro is more complicated than seq!, it generates
 * significantly faster runtime code than the old homogenous list implementation.
 */
#[macro_export]
macro_rules! choose {
    /*
     * Pattern matches for external calls
     */
    ($l:expr, $r:expr) => (
        (|span: &Span<'a>, ctx: &ParseContext| {
            $crate::choose!(@inner span, ctx, @sep, @sep, $l, $r)
        })
    );
    ($l:expr, $r:expr, $($rest:expr),*) => (
        (|span: &Span<'a>, ctx: &ParseContext| {
            $crate::choose!(@inner span, ctx, @sep, @sep, $l, $r, $($rest),*)
        })
    );
    ($l:expr, $r:expr, $($rest:expr),*,) => (
        (|span: &Span<'a>, ctx: &ParseContext| {
            $crate::choose!(@inner span, ctx, @sep, @sep, $l, $r, $($rest),*)
        })
    );

    /*
     * Pattern matches for internal match generation
     */
    // Base case: we are at the last parser expression
    (@inner $span:ident, $ctx:ident, @sep, $($e:expr),*, @sep, $last: expr) => (
        match $last.parse($span, $ctx) {
            Ok(data) => Ok(data),
            Err(err) => {
                if !$ctx.collect_errors {
                    Err(err)
                }
                else {
                    Err(
                        $crate::choose!(@unroll_errors err, $($e),*)
                    )
                }
            }
        }
    );

    // Base case: we are at the first parser expression, and no errors have been found yet
    (@inner $span:ident, $ctx:ident, @sep, @sep, $l: expr, $($rest:expr),+) => (
        match $l.parse($span, $ctx) {
            Ok(data) => Ok(data),
            Err(err) => {
                $crate::choose!(@inner $span, $ctx, @sep, err, @sep, $($rest),*)
            }
        }
    );

    // Recursive case: we have collected some errors, and more parsing exprs remain to be processed
    (@inner $span:ident, $ctx:ident, @sep, $($e:expr),*, @sep, $l: expr, $($rest:expr),+) => (
        match $l.parse($span, $ctx) {
            Ok(data) => Ok(data),
            Err(err) => {
                $crate::choose!(@inner $span, $ctx, @sep, err, $($e),*, @sep, $($rest),*)
            }
        }
    );

    /*
     * Pattern matches for error unrolling
     */
    // Base case: we are on the last error
    (@unroll_errors $le:expr) => (
        $le
    );

    // Recursive case: we have more than one errors remaining to be processed
    (@unroll_errors $le:expr, $($e:expr),+ ) => (
        $le.longest(
            $crate::choose!(@unroll_errors $($e),*)
        )
    );
}

pub struct Map<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(I, &Span<'a>) -> O {
    target: P,
    cb: F,
    _phantom: PhantomData<&'a (I, O)>,
}
impl<'a, I, O, P, F> Parser<'a, O> for Map<'a, I, O, P, F>
where P: Parser<'a, I>, F: Fn(I, &Span<'a>) -> O {
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O> {
        let (remaining, output, e) = self.target.parse(span, ctx)?;
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
    fn do_parse(&mut self, span: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, O> {
        let (remaining, _, e) = self.target.parse(span, ctx)?;
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

fn take_while<'a, F: Fn(u8) -> bool>(e: ParseError<ErrorKinds>, f: F) -> impl FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, &'a str> {
    move |span: &Span<'a>, ctx: &ParseContext| {
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
            return Err(ErrorCollector::new(span, ctx, e));
        }
        let new_span = span.consume(ctx, count);
        if did_err {
            return Ok((new_span, span.as_str(), Some(ErrorCollector::new(span, ctx, e))));
        }
        Ok((new_span, span.as_str(), None))
    }
}

fn ignore_whitespace<'a>() -> impl FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, ()> {
    move |span: &Span<'a>, ctx: &ParseContext| {
        let mut count = 0;
        for index in 0..span.len() {
            let current_char = span.as_bytes()[index] as char;
            if !(current_char == ' ' || current_char == '\n') {
                break;
            }
            count += 1;
        }
        let new_span = span.consume(ctx, count);
        Ok((new_span, (), Some(ErrorCollector::new(span, ctx, ParseError::Char(' ')))))
    }
}

fn expect_byte<'a, F>(e: ParseError<ErrorKinds>, f: F) -> impl FnMut(&Span<'a>, &ParseContext) -> ParseResult<'a, u8>
where F: Fn(u8) -> bool {
    move |span: &Span<'a>, ctx: &ParseContext| {
        let current_byte = span.as_bytes()[0];
        if f(current_byte) {
            return Ok((span.consume(ctx, 1), current_byte, None));
        }
        Err(ErrorCollector::new(span, ctx, e))
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

// Allows alphanumeric characters, underscores, and dashes
fn id_rest_str<'a>() -> impl Parser<'a, &'a str> {
    take_while(ParseError::Kind(ErrorKinds::Alphanumeric), |byte| {
        byte.is_ascii_alphanumeric() || byte == 95 || byte == 45
    })
}

fn alphabetic<'a>() -> impl Parser<'a, char> {
    expect_byte(ParseError::Kind(ErrorKinds::Alphabetic), |byte| {
        byte.is_ascii_alphabetic()
    }).map(|byte, _| {
        byte as char
    })
}

fn int<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    digit_str().map_span(|span| {
        Ast::Int(span.into(), span.as_str().parse::<i64>().unwrap())
    }).parse(input, ctx)
}

fn float<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    seq!(
        digit_str(),
        ascii('.'),
        digit_str(),
    ).map_span(|span| {
        Ast::Float(span.into(), span.as_str().parse::<f64>().unwrap())
    }).parse(input, ctx)
}

fn number<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    choose!(float, int).peek(trailing_values).parse(input, ctx)
}

fn id_str<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, &'a str> {
    seq!(
        alphabetic(),
        id_rest_str().opt(),
        ascii('?').opt(),
    ).peek(trailing_values).map_span(|span| {
        span.as_str()
    }).parse(input, ctx)
}
fn id<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    map(id_str, |output, span| {
        Ast::Identifier(span.into(), output)
    }).parse(input, ctx)
}

fn field<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    seq!(
        ascii('.'),
        choose!(id_str, operator_str),
    ).map_span(|span| {
        let dotless = span.slice(1, span.len());
        Ast::Field(span.into(), dotless.as_str())
    }).parse(input, ctx)
}

fn trait_ref<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    seq!(
        ascii(':'),
        choose!(id_str, operator_str),
    ).map_span(|span| {
        let colonless = span.slice(1, span.len());
        Ast::TraitRef(span.into(), colonless.as_str())
    }).parse(input, ctx)
}

fn space<'a>() -> impl Parser<'a, char> {
    ascii(' ')
}

fn newline<'a>() -> impl Parser<'a, char> {
    ascii('\n')
}

fn whitespace<'a>() -> impl Parser<'a, char> {
    choose!(space(), newline())
}

fn trailing_values<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, char> {
    choose!(
        whitespace(),
        ascii(')'),
        ascii('}'),
        ascii(']'),
        ascii('>'),
        ascii(':'),
        ascii('.'),
        eoi().map(|_, _| {
            // TODO: Make an either() that returns a left vs right value, so you can appropriately
            // model nulls and varying return types, while keeping the default of coalescing values
            // of the same type for or() since that's usually more convenient
            // But whatever null byte here is fine I guess
            0 as char
        }),
    ).parse(input, ctx)
}

fn surrounded<'a, O, FO, LO, F, L, P>(first: F, last: L, parser: P) -> impl Parser<'a, O>
where O: 'a,
FO: 'a,
LO: 'a,
F: Parser<'a, FO>,
L: Parser<'a, LO>,
P: Parser<'a, O> {
    seq!(
        first,
        ignore_whitespace(),
        parser,
        ignore_whitespace(),
        last,
    ).map(|output, _| {
        let (_, (_, (exprs, (_, _)))) = output;
        exprs
    })
}

fn typelist<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('<'), ascii('>'),
        seq!(expr, ignore_whitespace(), id_str)
    ).map(|output, span| {
        let (ast, (_, id_str)) = output;
        Ast::TypeAssert(span.into(), Box::new(ast), id_str)
    }).parse(input, ctx)
}

fn macro_call<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('{'), ascii('}'),
        many(seq!(expr, ignore_whitespace()))
    )
    .map(|exprs, span| {
        Ast::Macro(span.into(), exprs.into_iter().map(|(expr, _)| {
            expr
        }).collect())
    }).parse(input, ctx)
}

fn call<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('('), ascii(')'),
        any(seq!(expr, ignore_whitespace()))
    )
    .map(|exprs, span| {
        match exprs {
            None => Ast::Call(span.into(), vec![]),
            Some(exprs) => Ast::Call(span.into(), exprs.into_iter().map(|(expr, _)| {
                expr
            }).collect()),
        }
    }).parse(input, ctx)
}

fn list<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    surrounded(ascii('['), ascii(']'),
        any(seq!(expr, ignore_whitespace()))
    )
    .map(|exprs, span| {
        match exprs {
            None => Ast::List(span.into(), vec![]),
            Some(exprs) => Ast::List(span.into(), exprs.into_iter().map(|(expr, _)| {
                expr
            }).collect()),
        }
    }).parse(input, ctx)
}

fn no_pairs<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Vec<((AstSpan, &'a str), Ast<'a>)>> {
    ignore_whitespace().map_span(|_| {
        vec![]
    }).parse(input, ctx)
}

fn pairs_oneline<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Vec<((AstSpan, &'a str), Ast<'a>)>> {
    seq!(
        any(seq!(pair, ignore_whitespace(), ascii(','), ignore_whitespace())),
        pair,
    ).map(|output, _| {
        let (initial, final_pair) = output;
        let mut new_vec = vec![];
        match initial {
            None => {
                new_vec.push(final_pair);
                return new_vec;
            }
            Some(pairs) => {
                new_vec.extend(pairs.into_iter().map(|data| {
                    let (pair, _) = data;
                    pair
                }).collect::<Vec<((AstSpan, &'a str), Ast<'a>)>>());
                new_vec.push(final_pair);
                return new_vec;
            }
        }
    }).parse(input, ctx)
}

fn pairs_multiline<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Vec<((AstSpan, &'a str), Ast<'a>)>> {
    any(
        seq!(
            pair,
            space().count(),
            ascii(',').opt(),
            space().count(),
            newline(),
            ignore_whitespace(),
        )
    ).map(|output, _| {
        match output {
            None => vec![],
            Some(output) => {
                output.into_iter().map(|item| {
                    let (pair, (_, (_, (_, (_, _))))) = item;
                    pair
                }).collect()
            }
        }
    }).parse(input, ctx)
}

fn pair<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, ((AstSpan, &'a str), Ast<'a>)> {
    seq!(
        map(id_str, |output, span| {
            (span.into(), output)
        }),
        ignore_whitespace(),
        ascii_str(":="),
        ignore_whitespace(),
        expr,
    ).map(|output, _| {
        let (id, (_, (_, (_, ast)))) = output;
        (id, ast)
    }).parse(input, ctx)
}

fn operator_str<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, &'a str> {
    choose!(
        ascii_str("&&"),
        ascii_str("&"),
        ascii_str("||"),
        ascii_str("|"),
        ascii_str("++"),
        ascii_str("+"),
        ascii_str("=>"),
        ascii_str("--"),
        ascii_str("-="),
        ascii_str("-"),
        ascii_str("**"),
        ascii_str("*"),
        ascii_str("/"),
        ascii_str("%"),
        ascii_str("=="),
        ascii_str("!="),
        ascii_str("="),
        ascii_str("!"),
        ascii_str(">="),
        ascii_str(">"),
        ascii_str("<="),
        ascii_str("<"),
    ).peek(trailing_values).parse(input, ctx)
}

fn operator<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    map(operator_str, |string, span| {
        Ast::Identifier(span.into(), string)
    }).parse(input, ctx)
}

fn expr<'a>(input: &Span<'a>, ctx: &ParseContext) -> ParseResult<'a, Ast<'a>> {
    choose!(
        number,
        call,
        macro_call,
        id,
        list,
        typelist,
        operator,
        field,
        trait_ref,
    ).parse(input, ctx)
}

pub fn parse<'a>(input: &'a str) -> Result<Vec<Ast<'a>>, ErrorCollector> {
    if input.len() == 0 {
        return Ok(vec![]);
    }

    let span = Span::new(input);
    let ctx = ParseContext {
        collect_errors: false,
        tracer: Tracers::Nil,
    };

    match parse_with_ctx(&span, &ctx) {
        Ok(output) => Ok(output),
        Err(_) => {
            let collection_ctx = ParseContext {
                collect_errors: true,
                tracer: Tracers::Nil,
            };
            parse_with_ctx(&span, &collection_ctx)
        }
    }
}

fn parse_with_ctx<'a>(span: &Span<'a>, ctx: &ParseContext) -> Result<Vec<Ast<'a>>, ErrorCollector> {
    let parsed = many(
        seq!(
            ignore_whitespace(),
            expr,
            ignore_whitespace(),
        ).map(|output, _| {
            let (_, (exprs, _)) = output;
            exprs
        })
    ).parse(span, ctx);

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
