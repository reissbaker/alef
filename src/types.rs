use std::collections::HashMap;
use crate::ir::Ir;
use crate::symbols::{Symbol, SymbolTable};

#[derive(Clone, Debug)]
pub struct TypeCondArm {
    condition: TypeVal,
    body: Vec<TypeVal>,
}

#[derive(Clone, Debug)]
pub struct TypeElseArm {
    body: Vec<TypeVal>,
}

#[derive(Clone, Debug)]
pub struct WhereType {
    args: Vec<TypeArg>,
    body: Vec<TypeVal>,
}

#[derive(Clone, Debug)]
pub enum TypeArg {
    Unbound,
    Bound(Type),
}

pub trait TypeComparison {
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool;

    fn algebraic_subtype(&self, scope: &Scope<TypeVal>, other: &Algebraic) -> bool {
        match other {
            Algebraic::Sum(types) => {
                types.iter().any(|t| self.is_subtype(scope, t))
            }
            Algebraic::Intersection(types) => {
                types.iter().all(|t| self.is_subtype(scope, t))
            }
        }
    }

    fn reference_subtype(&self, scope: &Scope<TypeVal>, id: &Symbol) -> bool {
        match scope.lookup(*id) {
            None => panic!("unknown reference"),
            Some(val) => self.is_subtype(scope, val),
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
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool {
        // If you're a primitive, you are a subtype of yourself
        match other {
            TypeVal::Algebraic(algebraic_type) => self.algebraic_subtype(scope, algebraic_type),
            TypeVal::Reference(id) => self.reference_subtype(scope, id),
            TypeVal::Primitive(other_prim) => {
                other_prim == self
            }
            TypeVal::Static(_) => false,
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
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool {
        match other {
            TypeVal::Algebraic(algebraic_type) => self.algebraic_subtype(scope, algebraic_type),
            TypeVal::Reference(id) => self.reference_subtype(scope, id),
            TypeVal::Signature(_) => false,
            TypeVal::Nominal(_) => false,
            TypeVal::Primitive(primitive) => {
                *primitive == self.primitive_supertype()
            },
            TypeVal::Static(other_static) => {
                self == other_static
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Algebraic {
    Sum(Vec<TypeVal>),
    Intersection(Vec<TypeVal>),
}

impl TypeComparison for Algebraic {
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool {
        match self {
            Algebraic::Sum(types) => {
                types.iter().all(|t| t.is_subtype(scope, other))
            }
            Algebraic::Intersection(types) => {
                types.iter().any(|t| t.is_subtype(scope, other))
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
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool {
        match other {
            TypeVal::Algebraic(alg_type) => self.algebraic_subtype(scope, alg_type),
            TypeVal::Reference(id) => self.reference_subtype(scope, id),
            TypeVal::Signature(other_sig) => {
                if self.args.len() != other_sig.args.len() {
                    return false;
                }
                for (i, arg) in self.args.iter().enumerate() {
                    if !arg.is_subtype(scope, &other_sig.args[i]) {
                        return false;
                    }
                }
                self.result.is_subtype(scope, &other_sig.result)
            }
            // All other types fail
            TypeVal::Static(_) => false,
            TypeVal::Primitive(_) => false,
            TypeVal::Nominal(_) => false,
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
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool {
        match other {
            TypeVal::Algebraic(alg_type) => self.algebraic_subtype(scope, alg_type),
            TypeVal::Nominal(nominal) => self.name == nominal.name,
            TypeVal::Reference(id) => self.reference_subtype(scope, id),
            // All other types are not a match
            TypeVal::Signature(_) => false,
            TypeVal::Static(_) => false,
            TypeVal::Primitive(_) => false,
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
    Reference(Symbol),
}

#[derive(Clone, Debug)]
pub enum TypeFn {
    Cond(Vec<TypeCondArm>, Option<TypeElseArm>),
    Where(WhereType),
    Kind(Vec<TypeArg>),
    Interface(Vec<(Symbol, TypeVal)>),
}

impl TypeFn {
    fn compute(&self, scope: &mut Scope<TypeVal>, args: Vec<TypeVal>) -> TypeVal {
        // TODO: actually eval these
        match self {
            TypeFn::Cond(_, _) => {
                TypeVal::Primitive(Primitive::Never)
            }
            TypeFn::Where(_) => {
                TypeVal::Primitive(Primitive::Never)
            }
            TypeFn::Kind(_) => {
                TypeVal::Primitive(Primitive::Never)
            }
            TypeFn::Interface(_) => {
                TypeVal::Primitive(Primitive::Never)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Val(TypeVal),
    Fn(TypeFn),
    TypeAlias(Box<TypeVal>),
}

pub struct Scope<Value> {
    parent: Option<Box<Scope<Value>>>,
    local_scope: HashMap<Symbol, Value>,
}

impl<Value> Scope<Value> {
    pub fn new() -> Scope<Value> {
        Scope { parent: None, local_scope: HashMap::new() }
    }
    pub fn lookup(&self, id: Symbol) -> Option<&Value> {
        self.local_scope.get(&id).or_else(|| {
            self.parent.as_ref().and_then(|parent| {
                parent.lookup(id)
            })
        })
    }
    pub fn insert(&mut self, id: Symbol, val: Value) -> Option<Value> {
        self.local_scope.insert(id, val)
    }
}

pub struct TypeError {
    msg: String,
}

impl TypeComparison for TypeVal {
    fn is_subtype(&self, scope: &Scope<TypeVal>, other: &TypeVal) -> bool {
        match self {
            TypeVal::Primitive(primitive) => primitive.is_subtype(scope, other),
            TypeVal::Static(static_type) => static_type.is_subtype(scope, other),
            TypeVal::Algebraic(alg_type) => alg_type.is_subtype(scope, other),
            TypeVal::Signature(sig_type) => sig_type.is_subtype(scope, other),
            TypeVal::Nominal(nominal) => nominal.is_subtype(scope, other),
            TypeVal::Reference(id) => {
                match scope.lookup(*id) {
                    None => panic!("unknown symbol"),
                    Some(typeval) => typeval.is_subtype(scope, other),
                }
            }
        }
    }
}

impl Type {
    fn check<'a>(&self, table: &SymbolTable, scope: &Scope<TypeVal>, ir: Ir<'a>) -> Result<(), Vec<TypeError>> {
        match self {
            Type::Val(TypeVal::Primitive(Primitive::I64)) => {
                match ir {
                    Ir::Int(_, _) => {
                        Ok(())
                    }
                    Ir::Id(id) => {
                        if let Some(id_type) = scope.lookup(id.1) {
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
