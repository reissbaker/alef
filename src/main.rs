mod span;
mod errors;
mod parser;
use parser::parse;
use std::{
    fs,
    error::Error,
};

fn main() -> Result<(), Box<dyn Error>> {
    let file = fs::read_to_string("./samples/test.con")?;
    match parse(&file) {
        Ok(parsed) => {
            println!("parse success: {:?}", parsed);
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    }

    Ok(())
}
