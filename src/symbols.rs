use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Symbol {
    id: usize,
}

pub struct SymbolTable {
    guid: usize,
    table: HashMap<String, Symbol>,
    reverse: HashMap<Symbol, String>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            guid: 0,
            table: HashMap::new(),
            reverse: HashMap::new(),
        }
    }

    pub fn symbol(&mut self, name: String) -> Symbol {
        if self.table.contains_key(&name) {
            return *self.table.get(&name).unwrap();
        }
        let sym = Symbol {
            id: self.guid,
        };
        self.guid += 1;
        self.table.insert(name.clone(), sym);
        self.reverse.insert(sym, name.clone());
        *self.table.get(&name).unwrap()
    }

    pub fn string(&self, sym: &Symbol) -> Option<&String> {
        self.reverse.get(sym)
    }
}
