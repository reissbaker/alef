use crate::ir::Ir;
use std::collections::HashMap;

pub struct TypeCondArm {
    condition: Type,
    body: Vec<Type>,
}

pub struct TypeElseArm {
    body: Vec<Type>,
}

pub struct WhereType {
    args: Vec<TypeArg>,
    body: Vec<Type>,
}

pub enum TypeArg {
    Unbound,
    Bound(Type),
}

pub enum Type {
    I64,
    IStatic(i64),
    U64,
    UStatic(u64),
    F64,
    FStatic(f64),
    Char,
    CharStatic(char),
    Str,
    StrStatic(String),
    True,
    False,
    Bool,
    Nil,
    Never,
    Sum(Vec<Type>),
    Intersection(Vec<Type>),
    Fn(Vec<Type>, Box<Type>),
    Structural(Vec<(String, Type)>),
    Cond(Vec<TypeCondArm>, Option<TypeElseArm>),
    Where(WhereType),
    Kind(Vec<TypeArg>),
    Type(Box<Type>),
}

pub struct Scope {
    parent: Box<Scope>,
    local_scope: HashMap<String, Type>,
}

impl Scope {
    fn get_type(&self, id: String) -> Option<&Type> {
        self.local_scope.get(&id)
    }
}

pub struct TypeError {
    msg: String,
}

impl Type {
    fn check<'a>(&self, scope: &Scope, ir: Ir<'a>) -> Result<(), Vec<TypeError>> {
        Err(vec![])
    }
}
