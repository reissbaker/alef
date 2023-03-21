use crate::scope::Scope;
use crate::type_val::{TypeVal, TypeComparison};
use crate::symbols::Symbol;

pub trait ComputeType {
    fn compute<'a>(&self, scope: &Scope<'a, TypeVal>, args: Vec<&TypeVal>) -> TypeVal;
}

pub trait EvalBodyType {
    fn eval(&mut self, body: &Vec<WhereBody>) -> TypeVal;
}

impl<'a> EvalBodyType for Scope<'a, TypeVal> {
    fn eval(&mut self, body: &Vec<WhereBody>) -> TypeVal {
        // TODO: implement correctly
        reify(self, body.iter().reduce(|_, item| {
            match item {
                WhereBody::Alias(id, val) => {
                    self.insert(*id, reify(self, val));
                    item
                },
                _ => item,
            }
        }).unwrap())
    }
}

fn reify<'a>(scope: &mut Scope<'a, TypeVal>, expr: &WhereBody) -> TypeVal {
    match expr {
        WhereBody::Val(val) => val.clone(),
        WhereBody::Ref(reference) => scope.lookup(reference.id).unwrap().clone(),
        WhereBody::Alias(_, val) => reify(scope, val),
        WhereBody::Intersect(_) => panic!("Unimplemented"),
        WhereBody::Sum(_) => panic!("Unimplemented"),
    }
}

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
    arglist: Vec<(Symbol, TypeBound)>,
    body: Vec<WhereBody>,
}

// TODO: references must actually be TypeVals in order to allow for self-referential types. This
// means TypeVals actually can only be computed given a scope. Sad, but necessary. It will simplify
// where bodies significantly, though: you only need to handle vals and aliases, since everything
// else is a valid typeval.
#[derive(Clone, Debug)]
pub struct Ref {
    id: Symbol,
}

impl Ref {
    pub fn new(id: Symbol) -> Ref {
        Ref { id }
    }
    fn compute<'a>(&self, scope: &Scope<'a, TypeVal>) -> TypeVal {
        scope.lookup(self.id).unwrap().clone()
    }
}

#[derive(Clone, Debug)]
enum WhereBody {
    Val(TypeVal),
    Ref(Ref),
    Alias(Symbol, Box<WhereBody>),
    Intersect(Vec<Box<WhereBody>>),
    Sum(Vec<Box<WhereBody>>),
}

impl ComputeType for WhereType {
    fn compute<'a>(&self, scope: &Scope<'a, TypeVal>, args: Vec<&TypeVal>) -> TypeVal {
        let mut child_scope = scope.child();

        for (i, (id, bound)) in self.arglist.iter().enumerate() {
            let arg = args[i];
            if bound.is_subtype(&arg) {
                child_scope.insert(*id, arg.clone());
            }
            else {
                panic!("type mismatch");
            }
        }

        child_scope.eval(&self.body)
    }
}

#[derive(Clone, Debug)]
pub enum TypeBound {
    Unbound,
    Bound(TypeVal),
}

impl<'a> TypeComparison<'a> for TypeBound {
    fn is_subtype(&self, other: &TypeVal) -> bool {
        match self {
            TypeBound::Unbound => true,
            TypeBound::Bound(typeval) => typeval.is_subtype(other),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeFn {
    Cond(Vec<TypeCondArm>, Option<TypeElseArm>),
    Where(WhereType),
    Kind(Vec<TypeBound>),
    Interface(Vec<(Symbol, TypeVal)>),
    Ref(Ref),
}
