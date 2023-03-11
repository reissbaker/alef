use std::collections::HashMap;
use crate::ir::Ir;
use crate::symbols::{Symbol, SymbolTable};

#[derive(Clone, Debug)]
pub struct TypeCondArm {
    condition: Type,
    body: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeElseArm {
    body: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct WhereType {
    args: Vec<TypeArg>,
    body: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum TypeArg {
    Unbound,
    Bound(Type),
}

pub trait TypeComparison {
    fn is_subtype(&self, other: &TypeVal) -> bool;

    fn algebraic_subtype(&self, other: &Algebraic) -> bool {
        match other {
            Algebraic::Sum(types) => {
                types.iter().any(|t| self.is_subtype(t))
            }
            Algebraic::Intersection(types) => {
                types.iter().all(|t| self.is_subtype(t))
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Primitive {
    I64,
    U64,
    F64,
    Char,
    Str,
    Bool,
    Nil,
    Never
}

impl TypeComparison for Primitive {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        // If you're a primitive, you are a subtype of yourself
        // TODO: Conds + Type Aliases
        match other {
            TypeVal::Primitive(other_prim) => {
                other_prim == self
            }
            TypeVal::Static(_) => false,
            TypeVal::Algebraic(algebraic_type) => self.algebraic_subtype(algebraic_type),
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Static {
    I64(i64),
    U64(u64),
    F64(f64),
    Char(char),
    Str(String),
    True,
    False,
}

impl Static {
    fn primitive_supertype(&self) -> Primitive {
        match self {
            Static::I64(_) => Primitive::I64,
            Static::U64(_) => Primitive::U64,
            Static::F64(_) => Primitive::F64,
            Static::Char(_) => Primitive::Char,
            Static::Str(_) => Primitive::Str,
            Static::True => Primitive::Bool,
            Static::False => Primitive::Bool,
        }
    }
}

impl TypeComparison for Static {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        match other {
            TypeVal::Primitive(primitive) => {
                *primitive == self.primitive_supertype()
            },
            TypeVal::Static(other_static) => {
                self == other_static
            }
            TypeVal::Algebraic(algebraic_type) => self.algebraic_subtype(algebraic_type),
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Algebraic {
    Sum(Vec<TypeVal>),
    Intersection(Vec<TypeVal>),
}

impl TypeComparison for Algebraic {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        match self {
            Algebraic::Sum(types) => {
                types.iter().all(|t| t.is_subtype(other))
            }
            Algebraic::Intersection(types) => {
                types.iter().any(|t| t.is_subtype(other))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeFn {
    Cond(Vec<TypeCondArm>, Option<TypeElseArm>),
    Where(WhereType),
}

impl TypeFn {
    fn compute(&self, args: Vec<&Type>) -> &Type {
        // TODO: actually eval these
        match self {
            TypeFn::Cond(cond_arms, maybe_else) => {
                &cond_arms.iter().nth(0).unwrap().condition
            }
            TypeFn::Where(where_type) => {
                where_type.body.iter().nth(0).unwrap()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Signature {
    args: Vec<TypeVal>,
    result: Box<TypeVal>,
}

impl Signature {
    pub fn new(args: Vec<TypeVal>, ret: TypeVal) -> Signature {
        Signature {
            args,
            result: Box::new(ret),
        }
    }
}

impl TypeComparison for Signature {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        match other {
            TypeVal::Algebraic(alg_type) => self.algebraic_subtype(alg_type),
            TypeVal::Signature(other_sig) => {
                if self.args.len() != other_sig.args.len() {
                    return false;
                }
                for (i, arg) in self.args.iter().enumerate() {
                    if !arg.is_subtype(&other_sig.args[i]) {
                        return false;
                    }
                }
                self.result.is_subtype(&other_sig.result)
            }
            // All other types fail
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Nominal {
    name: Symbol,
}

impl Nominal {
    pub fn new(name: Symbol) -> Nominal {
        Nominal { name }
    }
}

impl TypeComparison for Nominal {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        match other {
            TypeVal::Algebraic(alg_type) => self.algebraic_subtype(alg_type),
            TypeVal::Nominal(nominal) => self.name == nominal.name,
            // All other types are not a match
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeVal {
    Primitive(Primitive),
    Static(Static),
    Algebraic(Algebraic),
    Signature(Signature),
    Nominal(Nominal),
}

#[derive(Clone, Debug)]
pub enum Type {
    Val(TypeVal),
    Fn(TypeFn),
    Kind(Vec<TypeArg>),
    TypeAlias(Box<TypeVal>),
}

pub struct Scope {
    parent: Box<Scope>,
    local_scope: HashMap<Symbol, Type>,
}

impl Scope {
    fn get_type(&self, id: Symbol) -> Option<&Type> {
        self.local_scope.get(&id)
    }
}

pub struct TypeError {
    msg: String,
}

impl TypeComparison for TypeVal {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        match self {
            TypeVal::Primitive(primitive) => primitive.is_subtype(other),
            TypeVal::Static(static_type) => static_type.is_subtype(other),
            TypeVal::Algebraic(alg_type) => alg_type.is_subtype(other),
            TypeVal::Signature(sig_type) => sig_type.is_subtype(other),
            TypeVal::Nominal(nominal) => nominal.is_subtype(other),
        }
    }
}

impl Type {
    fn check<'a>(&self, table: &SymbolTable, scope: &Scope, ir: Ir<'a>) -> Result<(), Vec<TypeError>> {
        match self {
            Type::Val(TypeVal::Primitive(Primitive::I64)) => {
                match ir {
                    Ir::Int(_, _) => {
                        Ok(())
                    }
                    Ir::Id(id) => {
                        if let Some(id_type) = scope.get_type(id.1) {
                            /*
                            if id_type.is_subtype(self) {
                                return Ok(())
                            }
                            */
                            return Err(vec![])
                        }
                        Err(vec![])
                    }
                    _ => {
                        Err(vec![])
                    }
                }
            }

            _ => {
                Err(vec![])
            }
        }
    }
}
