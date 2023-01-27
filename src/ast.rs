use crate::span::Span;

pub type DictPairs<'a> = Vec<((Span<'a>, &'a str), Ast<'a>)>;

pub enum Ast<'a> {
    Macro(Span<'a>, Vec<Ast<'a>>),
    Call(Span<'a>, Vec<Ast<'a>>),
    Dict(Span<'a>, DictPairs<'a>),
    List(Span<'a>, Vec<Ast<'a>>),
    TypeAssert(Span<'a>, Box<Ast<'a>>, &'a str),
    Identifier(Span<'a>, &'a str),
    Int(Span<'a>, i64),
    Float(Span<'a>, f64),
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
        }
        Ok(())
    }
}
