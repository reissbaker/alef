mod span;
mod errors;
mod ast;
mod parser;
mod macros;

use std::fs;
use miette::{NamedSource, Diagnostic, SourceSpan, ErrReport};
use parser::parse;
use errors::format_error;
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
    let path = "./samples/test.con";
    match fs::read_to_string(path) {
        Err(e) => {
            panic!("{:?}", e);
        }
        Ok(file) => {
            match parse(&file.clone()) {
                Ok(parsed) => {
                    println!("Parse success!\nExpressions parsed:");
                    for (pos, expr) in parsed.iter().enumerate() {
                        println!("\n{}: {:?}", pos + 1, expr);
                    }
                }
                Err(e) => {
                    return Err(ErrReport::from(PrettyParseError {
                        src: NamedSource::new(path, file),
                        error_loc: (e.get_span().start..e.get_span().start + 1).into(),
                        expected: format_error(e),
                    }));
                }
            }
            Ok(())
        }
    }
}
