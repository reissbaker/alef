pub type Id = (IrSpan, String);
pub type DictPairs = Vec<(Id, Ir)>;

pub struct IrSpan {
    source_path: String,
    start: usize,
    end: usize,
}

pub struct TypeAssert {
    typename: Id,
    id: Id,
}

pub enum LambdaArg {
    Typed(TypeAssert),
    Untyped(Id),
}

pub type ArgList = Vec<LambdaArg>;

#[derive(Debug)]
pub enum Ir {
    Let(Id, Ir),
    Set(Id, Ir),
    Lambda(ArgList, Vec<Ir>),
    Call(Id, Vec<Ir>),
    Dict(DictPairs),
    List(Vec<Ir>),
    TypeAssert(TypeAssert),
    Id(Id),
    Int(i64),
    Float(f64),
    Nil,
}
