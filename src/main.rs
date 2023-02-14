mod ast;
mod macros;
mod ir;

use std::fs;
use miette::{NamedSource, Diagnostic, SourceSpan, ErrReport};
use ast::parser::parse;
use ast::errors::format_error;
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
                println!("\n{}: {:?}", pos + 1, expr);
            }

            Ok(())
        }
    }
}
