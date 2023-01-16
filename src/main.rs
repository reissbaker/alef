mod parser;
use parser::parse;

fn main() {
    let mut parsed = parse("(hello world)");
    for _ in 0..100000 {
        parsed = parse("(hello world)");
    }
    println!("test parse: {:?}", parsed);
}
