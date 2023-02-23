use std::vec::IntoIter;
use crate::ast;
use crate::ast::ast::{Ast, AstSpan};

pub type Id<'a> = (IrSpan<'a>, String);
pub type DictPair<'a> = (Id<'a>, Ir<'a>);
pub type DictPairs<'a> = Vec<Box<DictPair<'a>>>;
pub type IrResult<'a, T> = Result<T, IrError<'a>>;

pub enum IrError<'a> {
    ArgumentError(IrSpan<'a>, &'static str),
    SyntaxError(IrSpan<'a>, &'static str),
    ReferenceError(IrSpan<'a>, &'static str),
    Unimplemented(IrSpan<'a>),
}
impl<'a> IrError<'a> {
    pub fn get_span(&self) -> IrSpan<'a> {
        match self {
            IrError::ArgumentError(span, _) => *span,
            IrError::SyntaxError(span, _) => *span,
            IrError::Unimplemented(span) => *span,
            IrError::ReferenceError(span, _) => *span,
        }
    }
    pub fn message(&self) -> &'static str {
        match self {
            IrError::ArgumentError(_, msg) => *msg,
            IrError::SyntaxError(_, msg) => *msg,
            IrError::ReferenceError(_, msg) => *msg,
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

impl<'a> Into<AstSpan> for IrSpan<'a> {
    fn into(self) -> AstSpan {
        return AstSpan {
            start: self.start,
            end: self.end,
        }
    }
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
pub struct When<'a> {
    pub span: IrSpan<'a>,
    pub condition: Ir<'a>,
    pub body: Vec<Ir<'a>>,
}

#[derive(Debug)]
pub struct Else<'a> {
    pub span: IrSpan<'a>,
    pub body: Vec<Ir<'a>>,
}

#[derive(Debug)]
pub enum Ir<'a> {
    Let(IrSpan<'a>, Id<'a>, Box<Ir<'a>>),
    Set(IrSpan<'a>, Id<'a>, Box<Ir<'a>>),
    Lambda(IrSpan<'a>, ArgList<'a>, Vec<Ir<'a>>),
    Call(IrSpan<'a>, Box<Ir<'a>>, Vec<Ir<'a>>),
    Case(IrSpan<'a>, Vec<When<'a>>, Option<Box<Else<'a>>>),
    When(Box<When<'a>>),
    Else(IrSpan<'a>, Vec<Ir<'a>>),
    Dict(IrSpan<'a>, DictPairs<'a>),
    DictPair(IrSpan<'a>, Box<DictPair<'a>>),
    List(IrSpan<'a>, Vec<Ir<'a>>),
    TypeAssert(IrSpan<'a>, TypeAssert<'a>),
    Id(Id<'a>),
    FieldAccess(IrSpan<'a>, Box<Ir<'a>>, Id<'a>),
    TraitRef(IrSpan<'a>, Box<Ir<'a>>, Id<'a>),
    Int(IrSpan<'a>, i64),
    Float(IrSpan<'a>, f64),
    Nil(IrSpan<'a>),
}

impl<'a> Ir<'a> {
    fn get_span(&self) -> &IrSpan<'a> {
        match self {
            Ir::Let(span, _, _) => span,
            Ir::Set(span, _, _) => span,
            Ir::Lambda(span, _, _) => span,
            Ir::Call(span, _, _) => span,
            Ir::Case(span, _, _) => span,
            Ir::When(data) => &data.span,
            Ir::Else(span, _) => span,
            Ir::Dict(span, _) => span,
            Ir::DictPair(span, _) => span,
            Ir::List(span, _) => span,
            Ir::TypeAssert(span, _) => span,
            Ir::Id((span, _)) => span,
            Ir::FieldAccess(span, _, _) => span,
            Ir::TraitRef(span, _, _) => span,
            Ir::Int(span, _) => span,
            Ir::Float(span, _) => span,
            Ir::Nil(span) => span,
        }
    }
}

pub fn to_ir_vec<'a, 'b>(source_path: &'a str, ast_vec: &Vec<Ast<'b>>) -> IrResult<'a, Vec<Ir<'a>>> {
    iter_to_ir_vec(source_path, ast_vec.iter())
}

fn iter_to_ir_vec<'a, 'b>(source_path: &'a str, ast_iter: std::slice::Iter<'b, Ast<'b>>) -> IrResult<'a, Vec<Ir<'a>>> {
    let mut coalesced = vec![];
    let mut last_ast_span;

    for ast in ast_iter {
        last_ast_span = ast.get_span();
        match ast {
            // TODO: these are essentially the same, and where they differ, Ir::FieldAccess is
            // worse, so you should just combine these into a parsing fn that works for either
            Ast::Field(span, id_str) => {
                let last = coalesced.pop();
                match last {
                    None => {
                        return Err(unexpected_field_access(source_path, span));
                    }
                    Some(last) => {
                        let full_span = IrSpan {
                            source_path,
                            start: last_ast_span.start,
                            end: span.end,
                        };
                        coalesced.push(Ir::FieldAccess(
                            full_span,
                            Box::new(last),
                            (IrSpan::from_ast_span(source_path, &span), (*id_str).into())
                        ));
                    }
                }
            }
            Ast::TraitRef(span, id_str) => {
                let last = coalesced.pop();
                match last {
                    None => {
                        return Err(unexpected_trait_ref(source_path, span));
                    }
                    Some(last) => {
                        let full_span = IrSpan {
                            source_path,
                            start: last_ast_span.start,
                            end: span.end,
                        };
                        coalesced.push(Ir::TraitRef(
                            full_span,
                            Box::new(last),
                            (IrSpan::from_ast_span(source_path, &span), (*id_str).into()),
                        ))
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
            // Macro-expand everything before continuing.
            let expanded = to_ir_vec(source_path, args)?;

            // For now, just unswap the head if it's a trait function reference in : form. In the
            // future you should be actually tracking context, types, etc so you can look up the
            // correct macro function for the object if it's a trait function call.
            // TODO: Implement as per above
            let mut args_iter = unswap_head_trait(expanded).into_iter();

            // At this point we've macro-expanded all the args, so the first arg needs to be an ID
            // (since we've unswapped the head). In the future we should make this some sort of
            // reference lookup as per above
            let (macro_name_span, macro_name) = expect_id(
                source_path,
                args_iter.next(),
                &IrSpan::from_ast_span(source_path, span),
            )?;

            // We need to do this conversion for most of the arms of the match, so w/e, let's just
            // do it here for all of them for simplicity
            let ir_span = IrSpan::from_ast_span(source_path, span);

            match macro_name.as_str() {
                "let" => parse_let(source_path, &ir_span, args_iter),
                "set" => parse_set(source_path, &ir_span, args_iter),
                "def" => parse_def(source_path, &ir_span, &macro_name_span, args_iter),
                "=>" => parse_lambda(source_path, &ir_span, &macro_name_span, args_iter),
                "=" => parse_pair(source_path, &ir_span, &macro_name_span, args_iter),
                "when" => parse_when(source_path, &ir_span, &macro_name_span, args_iter),
                "else" => parse_else(&ir_span, args_iter),
                "case" => parse_case(source_path, &ir_span, &macro_name_span, args_iter),
                "dict" => parse_dict_macro(source_path, &ir_span, &macro_name_span, args_iter),
                _ => Err(IrError::ReferenceError(macro_name_span, "Unknown macro name")),
            }
        }
        Ast::Call(span, args) => {
            // Empty calls syntactically refer to nil, like Rust and Scheme
            if args.len() == 0 {
                return Ok(Ir::Nil(IrSpan::from_ast_span(source_path, span)));
            }
            // Otherwise, parse the call!
            let mut ir_args = to_ir_vec(source_path, args)?;
            let head = ir_args.remove(0);

            Ok(Ir::Call(IrSpan::from_ast_span(source_path, span), Box::new(head), ir_args))
        }
        Ast::List(span, items) => {
            Ok(Ir::List(
                IrSpan::from_ast_span(source_path, span),
                to_ir_vec(source_path, items)?
            ))
        }
        Ast::TypeAssert(span, type_ast, id_str) => {
            let type_ir = ast_to_ir(source_path, type_ast)?;
            let ir_span = IrSpan::from_ast_span(source_path, span);
            Ok(Ir::TypeAssert(
                ir_span,
                TypeAssert {
                    typename: expect_id(source_path, Some(type_ir), &ir_span)?,
                    // TODO this span is wrong; the AST needs to encode the real span and not just
                    // the string id
                    id: (IrSpan::from_ast_span(source_path, span), id_str.to_string())
                }
            ))
        }
        Ast::TraitRef(span, _id_str) => {
            // If we got a trait reference here, there was no preceding expr to combine it with (or
            // else the vec version of this call would've stripped it out). Error.
            Err(unexpected_trait_ref(source_path, span))
        }
        Ast::Identifier(span, id_str) => {
            Ok(Ir::Id(parse_id(source_path, span, id_str)))
        }
        Ast::Field(span, _) => {
            // If we got a field access here, there was no preceding expr to combine it with (or
            // else the vec version of this call would've stripped it out). Error.
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

fn unswap_head_trait<'a>(args: Vec<Ir<'a>>) -> Vec<Ir<'a>> {
    let mut args_iter = args.into_iter();
    match args_iter.next() {
        None => {
            vec![]
        }
        Some(Ir::TraitRef(_, ir_box, id)) => {
            let mut unswapped = vec![
                Ir::Id(id),
                *ir_box,
            ];
            unswapped.extend(args_iter);
            unswapped
        }
        Some(item) => {
            let mut identical = vec![ item ];
            identical.extend(args_iter);
            identical
        }
    }
}

fn expect_ir<'a>(
    source_path: &'a str,
    maybe_ir: Option<Ir<'a>>,
    prev_span: &IrSpan<'a>,
    err_msg: &'static str,
) -> IrResult<'a, Ir<'a>> {
    maybe_ir.ok_or_else(|| {
        // Make sure the error actually points to after the prev parsed expr!
        let end_span = IrSpan {
            source_path,
            start: prev_span.end,
            // We're guaranteed that this isn't an index-out-of-bounds exception, since if
            // it parsed this far, by definition there's a closing token for the group
            // expression
            end: prev_span.end + 1,
        };
        IrError::ArgumentError(end_span, err_msg)
    })
}

fn expect_list<'a>(
    source_path: &'a str,
    maybe_ir: Option<Ir<'a>>,
    prev_span: &IrSpan<'a>
) -> IrResult<'a, (IrSpan<'a>, Vec<Ir<'a>>)> {
    let ir = expect_ir(
        source_path,
        maybe_ir,
        prev_span,
        "Expected a list"
    )?;
    match ir {
        Ir::List(span, items) => Ok((span, items)),
        ir => {
            Err(IrError::ArgumentError(*ir.get_span(), "Expected a list"))
        }
    }
}
fn expect_id<'a>(
    source_path: &'a str,
    maybe_ir: Option<Ir<'a>>,
    prev_span: &IrSpan<'a>,
) -> IrResult<'a, (IrSpan<'a>, String)> {
    let ir = expect_ir(
        source_path,
        maybe_ir,
        prev_span,
        "Expected an ID"
    )?;
    match ir {
        Ir::Id((span, name)) => Ok((span, name)),
        ir => {
            Err(IrError::ArgumentError(*ir.get_span(), "Expected an ID"))
        }
    }
}

fn expect_when<'a>(
    source_path: &'a str,
    maybe_ir: Option<Ir<'a>>,
    prev_span: &IrSpan<'a>,
) -> IrResult<'a, Box<When<'a>>> {
    let msg = "Expected a when clause";
    let ir = expect_ir(
        source_path,
        maybe_ir,
        prev_span,
        msg,
    )?;
    match ir {
        Ir::When(data) => Ok(data),
        ir => {
            Err(IrError::ArgumentError(*ir.get_span(), msg))
        }
    }
}
fn attempt_when<'a>(
    ir: Ir<'a>,
) -> Result<Box<When<'a>>, Ir<'a>> {
    match ir {
        Ir::When(data) => Ok(data),
        ir => Err(ir),
    }
}

fn expect_else<'a>(
    source_path: &'a str,
    maybe_ir: Option<Ir<'a>>,
    prev_span: &IrSpan<'a>,
) -> IrResult<'a, Else<'a>> {
    let msg = "Expected an else clause";
    let ir = expect_ir(
        source_path,
        maybe_ir,
        prev_span,
        msg,
    )?;
    match ir {
        Ir::Else(span, data) => {
            Ok(Else {
                span,
                body: data,
            })
        }
        ir => {
            Err(IrError::ArgumentError(*ir.get_span(), msg))
        }
    }
}

fn expect_dict_pair<'a>(
    source_path: &'a str,
    maybe_ir: Option<Ir<'a>>,
    prev_span: &IrSpan<'a>,
) -> IrResult<'a, (IrSpan<'a>, Box<DictPair<'a>>)> {
    let msg = "Expect a dictionary pair";
    let ir = expect_ir(
        source_path,
        maybe_ir,
        prev_span,
        msg,
    )?;
    match ir {
        Ir::DictPair(span, pair_box) => Ok((span, pair_box)),
        ir => Err(IrError::ArgumentError(*ir.get_span(), msg)),
    }
}

fn unexpected_field_access<'a>(source_path: &'a str, span: &AstSpan) -> IrError<'a> {
    IrError::SyntaxError(
        IrSpan::from_ast_span(source_path, span),
        "Unexpected field access: no preceding expression"
    )
}
fn unexpected_trait_ref<'a>(source_path: &'a str, span: &AstSpan) -> IrError<'a> {
    IrError::SyntaxError(
        IrSpan::from_ast_span(source_path, span),
        "Unexpected trait reference: no preceding expression"
    )
}

fn expect_arglist<'a>(source_path: &'a str, list: Vec<Ir<'a>>) -> IrResult<'a, ArgList<'a>> {
    let mut arglist = vec![];

    for item in list.into_iter() {
        match item {
            Ir::TypeAssert(_, type_assert) => {
                arglist.push(LambdaArg::Typed(type_assert));
            }
            Ir::Id(id) => {
                arglist.push(LambdaArg::Untyped(id));
            }
            ir => {
                return Err(IrError::ArgumentError(*ir.get_span(), "Expected a type assert or an ID"));
            }
        }
    }
    return Ok(arglist);
}

fn parse_id<'a, 'b>(source_path: &'a str, span: &AstSpan, id: &'b str) -> Id<'a> {
    (IrSpan::from_ast_span(source_path, &span), (*id).into())
}

fn parse_pair<'a>(source_path: &'a str, span: &IrSpan<'a>, macro_name_span: &IrSpan<'a>, mut args_iter: IntoIter<Ir<'a>>) -> IrResult<'a, Ir<'a>> {
    let (id_span, id_str) = expect_id(source_path, args_iter.next(), macro_name_span)?;
    let ir = expect_ir(source_path, args_iter.next(), &id_span, "Expected a value")?;
    Ok(Ir::DictPair(
        *span,
        Box::new(((id_span, id_str), ir))
    ))
}

fn parse_let<'a>(source_path: &'a str, span: &IrSpan<'a>, args_iter: IntoIter<Ir<'a>>) -> IrResult<'a, Ir<'a>> {
    let (id, val) = parse_let_set_args(source_path, args_iter)?;
    Ok(Ir::Let(*span, id, val))
}

fn parse_set<'a>(source_path: &'a str, span: &IrSpan<'a>, args_iter: IntoIter<Ir<'a>>) -> IrResult<'a, Ir<'a>> {
    let (id, val) = parse_let_set_args(source_path, args_iter)?;
    Ok(Ir::Set(*span, id, val))
}

fn parse_let_set_args<'a>(source_path: &'a str, mut args_iter: IntoIter<Ir<'a>>) -> IrResult<'a, (Id<'a>, Box<Ir<'a>>)> {
    match args_iter.next().unwrap() {
        Ir::Id((id_span, id_str)) => {
            let value = expect_ir(
                source_path,
                args_iter.next(),
                &id_span,
                "Expected a third argument, but only two were passed"
            )?;

            let mut peekable = args_iter.peekable();
            match peekable.peek() {
                Some(ir) => {
                    return Err(IrError::ArgumentError(*ir.get_span(), "Unexpected argument"))
                }
                None => {},
            }

            Ok(((id_span, id_str), Box::new(value)))
        }
        ir => {
            Err(IrError::ArgumentError(*ir.get_span(), "Expected an ID"))
        }
    }
}
fn parse_when<'a>(source_path: &'a str, span: &IrSpan<'a>, macro_name_span: &IrSpan<'a>, mut args_iter: IntoIter<Ir<'a>>) -> IrResult<'a, Ir<'a>> {
    let condition = expect_ir(
        source_path,
        args_iter.next(),
        macro_name_span,
        "Expected a condition for the when clause"
    )?;
    let body = args_iter.collect();
    Ok(Ir::When(Box::new(When {
        condition, body,
        span: *span,
    })))
}

fn parse_else<'a>(span: &IrSpan<'a>, args_iter: IntoIter<Ir<'a>>) -> IrResult<'a, Ir<'a>> {
    let body = args_iter.collect();
    Ok(Ir::Else(*span, body))
}

fn parse_def<'a>(
    source_path: &'a str,
    macro_span: &IrSpan<'a>,
    macro_name_span: &IrSpan<'a>,
    mut args_iter: IntoIter<Ir<'a>>
) -> IrResult<'a, Ir<'a>> {
    let (name_span, name_str) = expect_id(
        source_path,
        args_iter.next(),
        macro_name_span,
    )?;
    let (_, items) = expect_list(
        source_path,
        args_iter.next(),
        &name_span,
    )?;
    let arglist = expect_arglist(source_path, items)?;
    Ok(Ir::Let(
        *macro_span,
        (*macro_name_span, name_str),
        Box::new(
            Ir::Lambda(*macro_span, arglist, args_iter.collect())
        )
    ))
}

fn parse_lambda<'a, 'b>(
    source_path: &'a str,
    macro_span: &IrSpan<'a>,
    macro_name_span: &IrSpan<'a>,
    mut args_iter: IntoIter<Ir<'a>>
) -> IrResult<'a, Ir<'a>> {
    let (_, items) = expect_list(
        source_path,
        args_iter.next(),
        macro_name_span,
    )?;
    let arglist = expect_arglist(source_path, items)?;

    Ok(Ir::Lambda(*macro_span, arglist, args_iter.collect()))
}

fn parse_case<'a>(
    source_path: &'a str,
    macro_span: &IrSpan<'a>,
    macro_name_span: &IrSpan<'a>,
    mut args_iter: IntoIter<Ir<'a>>
) -> IrResult<'a, Ir<'a>> {
    let clause = expect_when(
        source_path,
        args_iter.next(),
        macro_name_span,
    )?;
    let mut last_span = clause.span;

    let mut whens = vec![ *clause ];
    let mut else_clause = None;

    while let Some(ir) = args_iter.next() {
        if else_clause.is_some() {
            return Err(IrError::ArgumentError(
                *ir.get_span(),
                "This `case` statement already has an else; additional arguments after an `else` are unsupported"
            ));
        }

        match attempt_when(ir) {
            Ok(when) => {
                last_span = when.span;
                whens.push(*when);
            }
            Err(ir) => {
                let parsed_else = expect_else(source_path, Some(ir), &last_span)?;
                last_span = parsed_else.span;
                else_clause = Some(Box::new(parsed_else));
            }
        }
    }

    Ok(Ir::Case(
        *macro_span,
        whens,
        else_clause,
    ))
}

fn parse_dict_macro<'a>(
    source_path: &'a str,
    macro_span: &IrSpan<'a>,
    macro_name_span: &IrSpan<'a>,
    args_iter: IntoIter<Ir<'a>>
) -> IrResult<'a, Ir<'a>> {
    let mut prev_span = *macro_name_span;
    Ok(Ir::Dict(
        *macro_span,
        args_iter.map(|ir| -> IrResult<'a, Box<DictPair<'a>>> {
            let (span, pair_box) = expect_dict_pair(source_path, Some(ir), &prev_span)?;
            prev_span = span;
            Ok(pair_box)
        }).collect::<IrResult<'a, Vec<Box<DictPair<'a>>>>>()?
    ))
}
