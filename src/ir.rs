use crate::ast;
use crate::ast::{Ast, AstSpan};

pub type Id<'a> = (IrSpan<'a>, String);
pub type DictPairs<'a> = Vec<(Id<'a>, Ir<'a>)>;

#[derive(Debug)]
pub struct IrSpan<'a> {
    source_path: &'a str,
    start: usize,
    end: usize,
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

pub fn to_ir_vec<'a, 'b>(source_path: &'a str, ast_vec: &Vec<Ast<'b>>) -> Vec<Ir<'a>> {
    iter_to_ir_vec(source_path, ast_vec.into_iter())
}

fn iter_to_ir_vec<'a, 'b>(source_path: &'a str, ast_iter: std::slice::Iter<'b, Ast<'b>>) -> Vec<Ir<'a>> {
    let mut coalesced = vec![];

    for ast in ast_iter {
        match ast {
            Ast::Field(span, id_str) => {
                let last = coalesced.pop();
                match last {
                    None => {
                        panic!("some error here; this is field access with nothing preceding");
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
                coalesced.push(ast_to_ir(source_path, &default));
            }
        }
    }

    coalesced
}

fn ast_to_ir<'a, 'b>(source_path: &'a str, ast: &Ast<'b>) -> Ir<'a> {
    match ast {
        Ast::Macro(span, args) => {
            // Empty macro = actually an empty dict
            if args.len() == 0 {
                return Ir::Dict(IrSpan::from_ast_span(source_path, span), vec![]);
            }
            if args.len() <= 2 {
                panic!("not enough macro args");
            }
            let mut args_iter = args.into_iter();

            match args_iter.next().unwrap() {
                Ast::Identifier(_, macro_name) => {
                    match *macro_name {
                        "let" => {
                            match args_iter.next().unwrap() {
                                Ast::Identifier(id_span, id_str) => {
                                    Ir::Let(
                                        IrSpan::from_ast_span(source_path, span),
                                        (
                                            IrSpan::from_ast_span(source_path, &id_span),
                                            (*id_str).into()
                                        ),
                                        Box::new(ast_to_ir(source_path, args_iter.next().unwrap()))
                                    )
                                }
                                _ => {
                                    panic!("Second arg of let must be an ID");
                                }
                            }
                        }
                        "set" => {
                            if args.len() != 3 {
                                panic!("Too many arguments for set");
                            }
                            match args_iter.next().unwrap() {
                                Ast::Identifier(id_span, id_str) => {
                                    Ir::Set(
                                        IrSpan::from_ast_span(source_path, span),
                                        (
                                            IrSpan::from_ast_span(source_path, &id_span),
                                            (*id_str).into()
                                        ),
                                        Box::new(ast_to_ir(source_path, args_iter.next().unwrap()))
                                    )
                                }
                                _ => {
                                    panic!("Second arg of let must be an ID");
                                }
                            }
                        }
                        "def" => {
                            match args_iter.next().unwrap() {
                                Ast::Identifier(id_span, id_str) => {
                                    match args_iter.next().unwrap() {
                                        Ast::List(_, items) => {
                                            let arglist = to_ir_arglist(source_path, items);
                                            Ir::Let(
                                                IrSpan::from_ast_span(source_path, span),
                                                (
                                                    IrSpan::from_ast_span(source_path, id_span),
                                                    (*id_str).into()
                                                ),
                                                Box::new(
                                                    Ir::Lambda(
                                                        IrSpan::from_ast_span(source_path, span),
                                                        arglist,
                                                        iter_to_ir_vec(source_path, args_iter)
                                                    )
                                                )
                                            )
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
                                    Ir::Lambda(
                                        IrSpan::from_ast_span(source_path, span),
                                        arglist,
                                        iter_to_ir_vec(source_path, args_iter)
                                    )
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
                return Ir::Nil(IrSpan::from_ast_span(source_path, span));
            }
            let mut ir_args = to_ir_vec(source_path, args);
            let head = ir_args.remove(0);

            Ir::Call(IrSpan::from_ast_span(source_path, span), Box::new(head), ir_args)
        }
        Ast::Dict(span, dict_pairs) => {
            Ir::Dict(IrSpan::from_ast_span(source_path, span), to_ir_pairs(source_path, dict_pairs))
        }
        Ast::List(span, items) => {
            Ir::Nil(IrSpan::from_ast_span(source_path, span))
        }
        Ast::TypeAssert(span, type_ast, id_str) => {
            Ir::Nil(IrSpan::from_ast_span(source_path, span))
        }
        Ast::Identifier(span, id_str) => {
            Ir::Id((IrSpan::from_ast_span(source_path, &span), (*id_str).into()))
        }
        Ast::Field(_, _) => {
            panic!("actually throw an error here; this is a field without a prior expr")
        }
        Ast::Int(span, val) => {
            Ir::Int(IrSpan::from_ast_span(source_path, span), *val)
        }
        Ast::Float(span, val) => {
            Ir::Float(IrSpan::from_ast_span(source_path, span), *val)
        }
    }
}

fn to_ir_arglist<'a, 'b>(source_path: &'a str, list: &Vec<Ast<'b>>) -> ArgList<'a> {
    list.into_iter().map(|item| {
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
fn to_ir_pairs<'a, 'b>(source_path: &'a str, pairs: &ast::DictPairs<'b>) -> DictPairs<'a> {
    pairs.into_iter().map(|((span, id_str), ast)| {
        (
            (IrSpan::from_ast_span(source_path, &span), String::from(*id_str)),
            ast_to_ir(source_path, ast)
        )
    }).collect()
}
