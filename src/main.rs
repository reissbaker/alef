mod ast;
mod macros;
mod ir;
mod symbols;
mod types;

use std::fs;
use miette::{NamedSource, Diagnostic, SourceSpan, ErrReport};
use ast::parser::parse;
use ast::errors::format_error;
use symbols::SymbolTable;
use ir::{to_ir_vec, DisplayFromSymbol};
use types::{Scope, Signature, Nominal, TypeComparison, TypeVal, Primitive, Algebraic, Static};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("parse error")]
#[diagnostic()]
pub struct PrettyParseError {
    #[source_code]
    src: NamedSource,

    #[label("Error location")]
    error_loc: SourceSpan,

    #[help]
    expected: String,
}

fn main() -> miette::Result<()> {
    let path = std::env::args().last().unwrap();

    match fs::read_to_string(path.clone()) {
        Err(e) => {
            panic!("{:?}", e);
        }
        Ok(file) => {
            let parsed = parse(&file).map_err(|e| {
                let parse_file = file.clone();
                ErrReport::from(PrettyParseError {
                    src: NamedSource::new(path.clone(), parse_file),
                    error_loc: (e.get_span().start..e.get_span().start + 1).into(),
                    expected: format_error(e),
                })
            })?;

            println!("Parse success! Converting to IR...");
            let mut table = SymbolTable::new();

            let ir = to_ir_vec(&path, &mut table, &parsed).map_err(|e| {
                let span = e.get_span();
                let file = fs::read_to_string(span.source_path).expect("file does not exist");
                ErrReport::from(PrettyParseError {
                    src: NamedSource::new(span.source_path, file),
                    error_loc: (span.start..span.start + 1).into(),
                    expected: e.message().into(),
                })
            })?;

            println!("IR convert success!\nIR:");
            for (pos, expr) in ir.iter().enumerate() {
                println!("\n{}: {:?}", pos + 1, expr.to_pretty_string(&table));
            }

            let i = TypeVal::Primitive(Primitive::I64);
            let i_9 = TypeVal::Static(Static::I64(9));
            let i9_or_i = TypeVal::Algebraic(Algebraic::Sum(
                    vec![i.clone(), i_9.clone()]
            ));
            let i9_and_i = TypeVal::Algebraic(Algebraic::Intersection(
                    vec![i.clone(), i_9.clone()]
            ));

            let scope = Scope::new();
            println!("\n\n9 is subtype of i64: {}", i_9.is_subtype(&scope, &i));
            println!("i64 is subtype of i9: {}", i.is_subtype(&scope, &i_9));

            println!("\n\n9 is subtype of sum(i64, 9): {}", i_9.is_subtype(&scope, &i9_or_i));
            println!("i64 is subtype of sum(i64, 9): {}", i.is_subtype(&scope, &i9_or_i));
            println!("sum(i64, 9) is subtype of i64: {}", i9_or_i.is_subtype(&scope, &i));
            println!("sum(i64, 9) is subtype of 9: {}", i9_or_i.is_subtype(&scope, &i_9));

            println!("\n\n9 is subtype of intersect(i64, 9): {}", i_9.is_subtype(&scope, &i9_and_i));
            println!("i64 is subtype of intersect(i64, 9): {}", i.is_subtype(&scope, &i9_and_i));
            println!("intersect(i64, 9) is subtype of i64: {}", i9_and_i.is_subtype(&scope, &i));
            println!("intersect(i64, 9) is subtype of 9: {}", i9_and_i.is_subtype(&scope, &i_9));

            let sig_i = TypeVal::Signature(Signature::new(
                vec![ i.clone() ],
                i.clone(),
            ));
            let sig_9 = TypeVal::Signature(Signature::new(
                vec![ i_9.clone() ],
                i_9.clone(),
            ));
            println!("\n\n(i64) -> i64 is subtype of (9) -> 9: {}", sig_i.is_subtype(&scope, &sig_9));
            println!("(9) -> 9 is subtype of (i64) -> i64: {}", sig_9.is_subtype(&scope, &sig_i));

            let bitmap_sym = table.symbol("Bitmap".into());
            let tex_sym = table.symbol("Texture".into());
            let Bitmap = TypeVal::Nominal(Nominal::new(bitmap_sym));
            let Texture = TypeVal::Nominal(Nominal::new(tex_sym));
            println!("\n\nBitmap nominal is subtype of Texture nominal: {}", Bitmap.is_subtype(&scope, &Texture));
            println!("Texture nominal is subtype of Bitmap nominal: {}", Texture.is_subtype(&scope, &Bitmap));
            println!("Texture nominal is subtype of Texture nominal: {}", Texture.is_subtype(&scope, &Texture));

            let mut ref_scope = Scope::new();
            let type_ref = table.symbol("T".into());
            ref_scope.insert(type_ref, Bitmap.clone());
            let BitmapRef = TypeVal::Reference(type_ref);
            println!("\n\nBitmap reference is subtype of Bitmap nominal: {}", BitmapRef.is_subtype(&ref_scope, &Bitmap));
            println!("Bitmap nominal is subtype of Bitmap reference: {}", Bitmap.is_subtype(&ref_scope, &BitmapRef));
            println!("Bitmap reference is a subtype of Texture: {}", BitmapRef.is_subtype(&ref_scope, &Texture));
            println!("Texture nominal is a subtype of BitmapRef: {}", Texture.is_subtype(&ref_scope, &BitmapRef));

            Ok(())
        }
    }
}
