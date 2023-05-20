#![feature(map_try_insert)]

use std::collections::{ HashMap, VecDeque };

pub struct IdentInfo {
    pub name: String,
}

pub enum Ident {
    Unqual(IdentInfo),
    Qual(Vec<IdentInfo>),
}

struct Scope {
    inner: HashMap<String, Symbol>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            inner: HashMap::new(),
        }
    }

    pub fn query(&self, name: &String) -> Option<&Symbol> {
        self.inner.get(name)
    }


    pub fn insert(&mut self, symbol: Symbol) -> Option<&Symbol> {
        let symbol_name = symbol.name().clone();
        match self.inner.try_insert(symbol_name.clone(), symbol) {
            Err(_) => self.query(&symbol_name),
            Ok(_) => None,
        }
    }
}

struct SymbolTable {
    global: Scope,
    nested: VecDeque<Scope>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            global: Scope::new(),
            nested: VecDeque::new(),
        }
    }

    fn current_scope(&self) -> &Scope {
        self.nested.front()
            .unwrap_or_else(|| &self.global)
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.nested.front_mut()
            .unwrap_or_else(|| &mut self.global)
    }

    pub fn resolve(&self, name: &String) -> Option<&Symbol> {
        self.current_scope()
            .query(name)
    }

    pub fn insert(&mut self, symbol: Symbol) -> Option<&Symbol> {
        self.current_scope_mut()
            .insert(symbol)
    }
}

struct Symbol {
    ident: Ident,
    kind: SymbolKind,
}

impl Symbol {
    pub fn enumeration(name: String, members: Vec<Symbol>) -> Symbol {
        Symbol {
            ident: Ident::Unqual(IdentInfo { name }),
            kind: SymbolKind::Enum(members),
        }
    }

    pub fn ident(name: String) -> Symbol {
        Symbol {
            ident: Ident::Unqual(IdentInfo { name }),
            kind: SymbolKind::Ident,
        }
    }

    pub fn qual_ident(name: String, qualifiers: Vec<IdentInfo>) -> Symbol {
        Symbol {
            ident: Ident::Qual(qualifiers),
            kind: SymbolKind::Ident,
        }
    }

    pub fn name(&self) -> &String {
        match self.ident {
            Ident::Unqual(ref info) => &info.name,
            Ident::Qual(ref info_vec) => &info_vec.last().unwrap().name,
        }
    }
}

enum SymbolKind {
    Enum(Vec<Symbol>),
    Struct(Vec<Symbol>),
    Ident,
}

fn main() {
    let mut st = SymbolTable::new();

    let enum_a = Symbol::enumeration(
        "Animal".to_string(),
        vec![
            Symbol::ident("Dog".to_string()),
            Symbol::ident("Cat".to_string()),
        ]
    );

    st.insert(enum_a);

    if st.resolve(&"Animals".to_string()).is_some() {
        println!("SOME");
    } else {
        println!("NONE");
    }
}
