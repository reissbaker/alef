use crate::ir::Ir;
use crate::scope::Scope;
use crate::type_val::{TypeVal, Primitive};
use crate::type_fn::TypeFn;

#[derive(Clone, Debug)]
pub enum Type {
    Val(TypeVal),
    Fn(TypeFn),
    TypeAlias(Box<TypeVal>),
}

pub struct TypeError {
    msg: String,
}

impl Type {
    fn check<'a>(&self, scope: &Scope<TypeVal>, ir: Ir<'a>) -> Result<(), Vec<TypeError>> {
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
