use std::collections::HashMap;
use crate::symbols::Symbol;

pub struct Scope<'a, Value> {
    parent: Option<Box<&'a Scope<'a, Value>>>,
    local_scope: HashMap<Symbol, Value>,
}

impl<'a, Value> Scope<'a, Value> {
    pub fn new() -> Scope<'a, Value> {
        Scope { parent: None, local_scope: HashMap::new() }
    }

    pub fn child(&'a self) -> Scope<'a, Value> {
        Scope { parent: Some(Box::new(self)), local_scope: HashMap::new() }
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
