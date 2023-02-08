mod span;
mod errors;
mod ast;
mod parser;
mod macros;
mod ir;
mod parse_context;
mod trace;

use std::fs;
use miette::{NamedSource, Diagnostic, SourceSpan, ErrReport};
use parser::parse;
use errors::format_error;
use ir::to_ir_vec;
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

            let ir = to_ir_vec(&path, &parsed).map_err(|e| {
                let parse_file = file.clone();
                ErrReport::from(PrettyParseError {
                    src: NamedSource::new(path.clone(), parse_file),
                    error_loc: (e.get_span().start..e.get_span().start + 1).into(),
                    expected: e.message().into(),
                })
            })?;

            println!("IR convert success!\nIR:");
            for (pos, expr) in ir.iter().enumerate() {
                println!("\n{}: {:?}", pos + 1, expr);
            }

            Ok(())
        }
    }
}
