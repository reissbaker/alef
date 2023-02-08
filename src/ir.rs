use core::slice::Iter;
use crate::ast;
use crate::ast::{Ast, AstSpan};

pub type Id<'a> = (IrSpan<'a>, String);
pub type DictPairs<'a> = Vec<(Id<'a>, Ir<'a>)>;
pub type IrResult<'a, T> = Result<T, IrError<'a>>;

pub enum IrError<'a> {
    ArgumentError(IrSpan<'a>, &'static str),
    SyntaxError(IrSpan<'a>, &'static str),
    Unimplemented(IrSpan<'a>),
}
impl<'a> IrError<'a> {
    pub fn get_span(&self) -> IrSpan<'a> {
        match self {
            IrError::ArgumentError(span, _) => *span,
            IrError::SyntaxError(span, _) => *span,
            IrError::Unimplemented(span) => *span,
        }
    }
    pub fn message(&self) -> &'static str {
        match self {
            IrError::ArgumentError(_, msg) => *msg,
            IrError::SyntaxError(_, msg) => *msg,
            IrError::Unimplemented(_) => "Unimplemented",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IrSpan<'a> {
    pub source_path: &'a str,
    pub start: usize,
    pub end: usize,
}

impl<'a> IrSpan<'a> {
    fn from_ast_span<'b>(source_path: &'a str, span: &AstSpan) -> IrSpan<'a> {
        IrSpan {
            source_path,
            start: span.start,
            end: span.end,
        }
    }
}

#[derive(Debug)]
pub struct TypeAssert<'a> {
    typename: Id<'a>,
    id: Id<'a>,
}

#[derive(Debug)]
pub enum LambdaArg<'a> {
    Typed(TypeAssert<'a>),
    Untyped(Id<'a>),
}

pub type ArgList<'a> = Vec<LambdaArg<'a>>;

#[derive(Debug)]
pub enum Ir<'a> {
    Let(IrSpan<'a>, Id<'a>, Box<Ir<'a>>),
    Set(IrSpan<'a>, Id<'a>, Box<Ir<'a>>),
    Lambda(IrSpan<'a>, ArgList<'a>, Vec<Ir<'a>>),
    Call(IrSpan<'a>, Box<Ir<'a>>, Vec<Ir<'a>>),
    Dict(IrSpan<'a>, DictPairs<'a>),
    List(IrSpan<'a>, Vec<Ir<'a>>),
    TypeAssert(IrSpan<'a>, TypeAssert<'a>),
    Id(Id<'a>),
    FieldAccess(Box<Ir<'a>>, Id<'a>),
    Int(IrSpan<'a>, i64),
    Float(IrSpan<'a>, f64),
    Nil(IrSpan<'a>),
}

pub fn to_ir_vec<'a, 'b>(source_path: &'a str, ast_vec: &Vec<Ast<'b>>) -> IrResult<'a, Vec<Ir<'a>>> {
    iter_to_ir_vec(source_path, ast_vec.iter())
}

fn iter_to_ir_vec<'a, 'b>(source_path: &'a str, ast_iter: std::slice::Iter<'b, Ast<'b>>) -> IrResult<'a, Vec<Ir<'a>>> {
    let mut coalesced = vec![];

    for ast in ast_iter {
        match ast {
            Ast::Field(span, id_str) => {
                let last = coalesced.pop();
                match last {
                    None => {
                        return Err(unexpected_field_access(source_path, span));
                    }
                    Some(last) => {
                        coalesced.push(Ir::FieldAccess(
                            Box::new(last),
                            (IrSpan::from_ast_span(source_path, &span), (*id_str).into())
                        ));
                    }
                }
            }
            default => {
                coalesced.push(ast_to_ir(source_path, &default)?);
            }
        }
    }

    Ok(coalesced)
}

fn ast_to_ir<'a, 'b>(source_path: &'a str, ast: &Ast<'b>) -> IrResult<'a, Ir<'a>> {
    match ast {
        Ast::Macro(span, args) => {
            // Empty macro = actually an empty dict
            if args.len() == 0 {
                return Ok(Ir::Dict(IrSpan::from_ast_span(source_path, span), vec![]));
            }
            let mut args_iter = args.iter();

            match args_iter.next().unwrap() {
                Ast::Identifier(_, macro_name) => {
                    match *macro_name {
                        "let" => parse_let(source_path, span, args_iter),
                        "set" => parse_set(source_path, span, args_iter),
                        "def" => {
                            match args_iter.next().unwrap() {
                                Ast::Identifier(id_span, id_str) => {
                                    match args_iter.next().unwrap() {
                                        Ast::List(_, items) => {
                                            let arglist = to_ir_arglist(source_path, items);
                                            Ok(Ir::Let(
                                                IrSpan::from_ast_span(source_path, span),
                                                parse_id(source_path, id_span, id_str),
                                                Box::new(
                                                    Ir::Lambda(
                                                        IrSpan::from_ast_span(source_path, span),
                                                        arglist,
                                                        iter_to_ir_vec(source_path, args_iter)?
                                                    )
                                                )
                                            ))
                                        }
                                        _ => {
                                            panic!("Third arg of def must be a list");
                                        }
                                    }
                                }
                                _ => {
                                    panic!("Second arg of let must be an ID");
                                }
                            }
                        }

                        "->" => {
                            match args_iter.next().unwrap() {
                                Ast::List(_, items) => {
                                    let arglist = to_ir_arglist(source_path, items);
                                    Ok(Ir::Lambda(
                                        IrSpan::from_ast_span(source_path, span),
                                        arglist,
                                        iter_to_ir_vec(source_path, args_iter)?
                                    ))
                                }
                                _ => {
                                    panic!("Next arg of a lambda must be a list");
                                }
                            }
                        }

                        default => {
                            panic!("unknown macro {}", default);
                        }
                    }
                }
                _ => {
                    panic!("unexpanded macro");
                }
            }
        }
        Ast::Call(span, args) => {
            if args.len() == 0 {
                return Ok(Ir::Nil(IrSpan::from_ast_span(source_path, span)));
            }
            let mut ir_args = to_ir_vec(source_path, args)?;
            let head = ir_args.remove(0);

            Ok(Ir::Call(IrSpan::from_ast_span(source_path, span), Box::new(head), ir_args))
        }
        Ast::Dict(span, dict_pairs) => {
            Ok(Ir::Dict(
                IrSpan::from_ast_span(source_path, span),
                to_ir_pairs(source_path, dict_pairs)?
            ))
        }
        Ast::List(span, _items) => {
            Err(IrError::Unimplemented(IrSpan::from_ast_span(source_path, span)))
        }
        Ast::TypeAssert(span, _type_ast, _id_str) => {
            Err(IrError::Unimplemented(IrSpan::from_ast_span(source_path, span)))
        }
        Ast::Identifier(span, id_str) => {
            Ok(Ir::Id(parse_id(source_path, span, id_str)))
        }
        Ast::Field(span, _) => {
            Err(unexpected_field_access(source_path, span))
        }
        Ast::Int(span, val) => {
            Ok(Ir::Int(IrSpan::from_ast_span(source_path, span), *val))
        }
        Ast::Float(span, val) => {
            Ok(Ir::Float(IrSpan::from_ast_span(source_path, span), *val))
        }
    }
}

fn unexpected_field_access<'a>(source_path: &'a str, span: &AstSpan) -> IrError<'a> {
    IrError::SyntaxError(
        IrSpan::from_ast_span(source_path, span),
        "Unexpected field access: no preceding expression"
    )
}

fn to_ir_arglist<'a, 'b>(source_path: &'a str, list: &Vec<Ast<'b>>) -> ArgList<'a> {
    list.iter().map(|item| {
        match item {
            // TODO FIXME this span is not the right span for the id string
            Ast::TypeAssert(todo_fixme_span, type_ast, id) => {
                match **type_ast {
                    Ast::Identifier(type_span, type_id) => {
                        LambdaArg::Typed(TypeAssert {
                            typename: (
                                IrSpan::from_ast_span(source_path, &type_span),
                                (*type_id).into()
                            ),
                            id: (
                                IrSpan::from_ast_span(source_path, todo_fixme_span),
                                (*id).into(),
                            )
                        })
                    }
                    _ => {
                        panic!("Expected a valid type identifier");
                    }
                }
            }
            Ast::Identifier(span, id) => {
                LambdaArg::Untyped((IrSpan::from_ast_span(source_path, span), (*id).into()))
            }
            _ => {
                panic!("Expected a type assertion or an ID");
            }
        }
    }).collect()
}
fn to_ir_pairs<'a, 'b>(source_path: &'a str, pairs: &ast::DictPairs<'b>) -> IrResult<'a, DictPairs<'a>> {
    let mut vec = vec![];
    for ((span, id_str), ast) in pairs.iter() {
        vec.push((
            (IrSpan::from_ast_span(source_path, &span), String::from(*id_str)),
            ast_to_ir(source_path, ast)?
        ))
    }
    Ok(vec)
}

fn parse_id<'a, 'b>(source_path: &'a str, span: &AstSpan, id: &'b str) -> Id<'a> {
    (IrSpan::from_ast_span(source_path, &span), (*id).into())
}

fn parse_let<'a, 'b>(source_path: &'a str, span: &AstSpan, args_iter: Iter<Ast<'b>>) -> IrResult<'a, Ir<'a>> {
    let (id, val) = parse_let_set_args(source_path, args_iter)?;
    Ok(Ir::Let(
        IrSpan::from_ast_span(source_path, span),
        id,
        val
    ))
}

fn parse_set<'a, 'b>(source_path: &'a str, span: &AstSpan, args_iter: Iter<Ast<'b>>) -> IrResult<'a, Ir<'a>> {
    let (id, val) = parse_let_set_args(source_path, args_iter)?;
    Ok(Ir::Set(
        IrSpan::from_ast_span(source_path, span),
        id,
        val
    ))
}

fn parse_let_set_args<'a, 'b>(source_path: &'a str, mut args_iter: Iter<Ast<'b>>) -> IrResult<'a, (Id<'a>, Box<Ir<'a>>)> {
    match args_iter.next().unwrap() {
        Ast::Identifier(id_span, id_str) => {
            let value = args_iter.next().ok_or_else(|| {
                // Make sure the error actually points to after the second argument! The second arg
                // isn't the problem, it's the fact that there isn't a third arg
                let end_span = AstSpan {
                    start: id_span.end,
                    // We're guaranteed that this isn't an index-out-of-bounds exception, since if
                    // it parsed this far, by definition there's a closing brace for the let
                    // expression
                    end: id_span.end + 1,
                };
                IrError::ArgumentError(
                    IrSpan::from_ast_span(source_path, &end_span),
                    "Expected a third argument, but only two were passed"
                )
            })?;

            let mut peekable = args_iter.peekable();
            match peekable.peek() {
                Some(ast) => {
                    return Err(IrError::ArgumentError(
                        IrSpan::from_ast_span(source_path, ast.get_span()),
                        "Unexpected argument"
                    ))
                }
                None => {},
            }

            Ok((
                parse_id(source_path, id_span, id_str),
                Box::new(ast_to_ir(source_path, value)?)
            ))
        }
        ast => {
            Err(IrError::ArgumentError(
                IrSpan::from_ast_span(source_path, &ast.get_span()),
                "Expected an ID",
            ))
        }
    }
}
