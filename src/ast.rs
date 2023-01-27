pub type DictPairs<'a> = Vec<(&'a str, Ast<'a>)>;

#[derive(Debug)]
pub enum Ast<'a> {
    Macro(Vec<Ast<'a>>),
    Call(Vec<Ast<'a>>),
    Dict(DictPairs<'a>),
    List(Vec<Ast<'a>>),
    TypeAssert(Box<Ast<'a>>, &'a str),
    Identifier(&'a str),
    Int(i64),
    Float(f64),
}
