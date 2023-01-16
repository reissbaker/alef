use std::marker::PhantomData;

type ParseResult<'a, O> = Result<(Span<'a>, O), String>;

#[derive(Debug, Clone)]
pub struct Span<'a> {
    start: usize,
    end: usize,
    input: &'a str,
}

impl<'a> Span<'a> {
    fn new(input: &'a str) -> Span<'a> {
        Span {
            input,
            start: 0,
            end: input.len(),
        }
    }

    fn consume(&self, count: usize) -> Span<'a> {
        Span {
            start: self.start + count,
            end: self.end,
            input: self.input,
        }
    }
    fn as_bytes(&self) -> &[u8] {
        &self.input.as_bytes()[self.start..self.end]
    }
    fn as_str(&self) -> &'a str {
        &self.input[self.start..self.end]
    }
}

pub trait Parser<'a, O> where Self: Sized {
    fn parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O>;

    fn then<ONext, PNext>(self, next: PNext) -> Seq<'a, O, ONext, Self, PNext>
    where PNext: Parser<'a, ONext> {
        seq(self, next)
    }
}

impl<'a, O, F: FnMut(&Span<'a>) -> ParseResult<'a, O>> Parser<'a, O> for F {
    fn parse(&mut self, span: &Span<'a>) -> ParseResult<'a, O> {
        self(span)
    }
}

fn byte<'a>(b: u8) -> impl FnMut(&Span<'a>) -> ParseResult<'a, u8> {
    move |span: &Span<'a>| {
        let next_byte = span.as_bytes()[0];
        if next_byte == b {
            return Ok((span.consume(1), next_byte));
        }
        Err(format!("Expected byte {}", b))
    }
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
    fn parse(&mut self, span: &Span<'a>) -> ParseResult<'a, (OFirst, ONext)> {
        let (remaining, output) = self.first.parse(span)?;
        let (remaining, next_output) = self.next.parse(&remaining)?;
        Ok((remaining, (output, next_output)))
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

fn many<'a, O, P>(mut target: P) -> impl FnMut(&Span<'a>) -> ParseResult<'a, Vec<O>>
where P: Parser<'a, O> {
    move |span: &Span<'a>| {
        let mut acc = vec![];
        let mut current_span = span.clone();
        while current_span.as_bytes().len() > 0 {
            match target.parse(&current_span) {
                Err(_) => {
                    if acc.len() == 0 {
                        return Err("Nothing matched".to_string());
                    }
                    return Ok((current_span, acc));
                }
                Ok((remaining, output)) => {
                    acc.push(output);
                    current_span = remaining;
                }
            }
        }
        Ok((current_span, acc))
    }
}

fn opt<'a, O, P>(mut target: P) -> impl FnMut(&Span<'a>) -> ParseResult<'a, Option<O>>
where P: Parser<'a, O> {
    move |span: &Span<'a>| {
        match target.parse(span) {
            Err(_) => Ok((span.clone(), None)),
            Ok((remaining, output)) => Ok((remaining, Some(output))),
        }
    }
}

fn any<'a, O, P>(mut target: P) -> impl FnMut(&Span<'a>) -> ParseResult<'a, Option<Vec<O>>>
where P: Parser<'a, O> {
    move |span: &Span<'a>| {
        let mut acc = vec![];
        let mut current_span = span.clone();
        while current_span.as_bytes().len() > 0 {
            match target.parse(&current_span) {
                Err(_) => {
                    if acc.len() == 0 {
                        return Ok((current_span, None));
                    }
                    return Ok((current_span, Some(acc)));
                }
                Ok((remaining, output)) => {
                    acc.push(output);
                    current_span = remaining;
                }
            }
        }
        Ok((current_span, Some(acc)))
    }
}
fn count<'a, O, P>(mut target: P) -> impl FnMut(&Span<'a>) -> ParseResult<'a, usize>
where P: Parser<'a, O> {
    move |span: &Span<'a>| {
        let mut acc = 0;
        let mut current_span = span.clone();
        while current_span.as_bytes().len() > 0 {
            match target.parse(&current_span) {
                Err(_) => {
                    return Ok((current_span, acc));
                }
                Ok((remaining, output)) => {
                    acc += 1;
                    current_span = remaining;
                }
            }
        }
        Ok((current_span, acc))
    }
}

fn choose<'a, O, L, R>(mut left: L, mut right: R) -> impl FnMut(&Span<'a>) -> ParseResult<'a, O>
where L: Parser<'a, O>, R: Parser<'a, O> {
    move |span: &Span| {
        match left.parse(span) {
            Err(_) => right.parse(span),
            default => default,
        }
    }
}

fn ascii<'a>(character: char) -> impl FnMut(&Span<'a>) -> ParseResult<'a, char> {
    map(byte(character as u8), |_, b| { (b as char) })
}

fn map<'a, I, O, P, F>(mut target: P, f: F) -> impl FnMut(&Span<'a>) -> ParseResult<'a, O>
where P: Parser<'a, I>, F: Fn(&Span<'a>, I) -> O {
    move |span: &Span| {
        let start = span.start;
        let (remaining, output) = target.parse(span)?;
        let consumed = Span { start, end: remaining.start, input: span.input };
        Ok((remaining, f(&consumed, output)))
    }
}

fn expect_byte<'a, F>(msg: &'a str, f: F) -> impl FnMut(&Span<'a>) -> ParseResult<'a, u8>
where F: Fn(u8) -> bool {
    move |span: &Span<'a>| {
        let current_byte = span.as_bytes()[0];
        if f(current_byte) {
            return Ok((span.consume(1), current_byte));
        }
        Err(msg.to_string())
    }
}

fn alphanumeric<'a>() -> impl FnMut(&Span<'a>) -> ParseResult<'a, char> {
    map(expect_byte("alphanumeric", |byte| {
        byte.is_ascii_alphanumeric()
    }), |_, byte| {
        byte as char
    })
}
fn alphabetic<'a>() -> impl FnMut(&Span<'a>) -> ParseResult<'a, char> {
    map(expect_byte("alphabetic", |byte| {
        byte.is_ascii_alphabetic()
    }), |_, byte| {
        byte as char
    })
}

fn id<'a>() -> impl FnMut(&Span<'a>) -> ParseResult<'a, Ast<'a>> {
    map(seq(alphabetic(), count(alphanumeric())), |span, _| {
        Ast::Identifier(span.as_str())
    })
}

fn call<'a>() -> impl FnMut(&Span<'a>) -> ParseResult<'a, Ast<'a>> {
    map(
        ascii('(').then(any(id().then(any(ascii(' '))))).then(ascii(')')),
        |_, output| {
            let ((_, ids), _) = output;
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

    let span = Span::new(input);
    let (_, output) = call().parse(&span)?;
    Ok(output)
}
