mod span;
mod errors;
mod parser;
use parser::parse;

fn main() {
    let parsed = parse("(hello world<>)");
    println!("test parse: {:?}", parsed);
}
