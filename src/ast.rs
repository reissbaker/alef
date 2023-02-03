use crate::span::Span;

#[derive(Debug, Copy, Clone)]
pub struct AstSpan {
    pub start: usize,
    pub end: usize,
}

impl<'a> From<&Span<'a>> for AstSpan {
    fn from(value: &Span<'a>) -> Self {
        AstSpan {
            start: value.start,
            end: value.end,
        }
    }
}
impl<'a> From<Span<'a>> for AstSpan {
    fn from(value: Span<'a>) -> Self {
        AstSpan {
            start: value.start,
            end: value.end,
        }
    }
}

pub type DictPairs<'a> = Vec<((AstSpan, &'a str), Ast<'a>)>;

pub enum Ast<'a> {
    Macro(AstSpan, Vec<Ast<'a>>),
    Call(AstSpan, Vec<Ast<'a>>),
    Dict(AstSpan, DictPairs<'a>),
    List(AstSpan, Vec<Ast<'a>>),
    TypeAssert(AstSpan, Box<Ast<'a>>, &'a str),
    Identifier(AstSpan, &'a str),
    Field(AstSpan, &'a str),
    Int(AstSpan, i64),
    Float(AstSpan, f64),
}

impl<'a> std::fmt::Debug for Ast<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Macro(span, vec) => {
                write!(f, "Macro({}-{}, {:?})", span.start, span.end, vec)?;
            }
            Ast::Call(span, vec) => {
                write!(f, "Call({}-{}, {:?})", span.start, span.end, vec)?;
            }
            Ast::Dict(span, pairs) => {
                let pair_str = pairs.into_iter().map(|pair| {
                    let (key_span, key) = pair.0;
                    format!("(Key({}-{}, {:?}), {:?})", key_span.start, key_span.end, key, pair.1)
                }).collect::<Vec<String>>().join(",");

                write!(f, "Dict({}-{}, [{:?}])", span.start, span.end, pair_str)?;
            }
            Ast::List(span, vec) => {
                write!(f, "List({}-{}, {:?})", span.start, span.end, vec)?;
            }
            Ast::TypeAssert(span, type_ast, id) => {
                write!(f, "TypeAssert({}-{}, {:?}, {:?})", span.start, span.end, type_ast, id)?;
            }
            Ast::Identifier(span, name) => {
                write!(f, "Identifier({}-{}, {:?})", span.start, span.end, name)?;
            }
            Ast::Int(span, val) => {
                write!(f, "Int({}-{}, {:?})", span.start, span.end, val)?;
            }
            Ast::Float(span, val) => {
                write!(f, "Float({}-{}, {:?})", span.start, span.end, val)?;
            }
            Ast::Field(span, name) => {
                write!(f, "Field({}-{}, {:?})", span.start, span.end, name)?;
            }
        }
        Ok(())
    }
}
