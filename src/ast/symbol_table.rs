use std::collections::{ HashMap, VecDeque };
use crate::ast::{ Ident, IdentInfo };

// use crate::io::file::SourceLoc;

// #[derive(Clone)]
// pub struct IdentInfo {
//     name: String,
// }

// impl IdentInfo {
//     pub fn new(name: &str) -> IdentInfo {
//         IdentInfo { name: name.to_string() }
//     }
//
//     pub fn name(&self) -> &String {
//         &self.name
//     }
// }
//
// #[derive(Clone)]
// pub enum Ident {
//     Unqual(IdentInfo),
//     // Replace with Vec<IdentInfo> later.
//     Qual(IdentInfo, IdentInfo),
// }

#[derive(Clone)]
pub struct Scope {
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

    // pub fn query_mut(&mut self, name: &String) -> Option<&mut Symbol> {
    //     self.inner.get_mut(name)
    // }

    pub fn insert(&mut self, symbol: Symbol) -> Option<&Symbol> {
        let symbol_name = symbol.name().clone();
        match self.inner.try_insert(symbol_name.clone(), symbol) {
            Err(_) => self.query(&symbol_name),
            Ok(_) => None,
        }
    }
}

pub struct SymbolTable {
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

    // fn current_scope(&self) -> &Scope {
    //     self.nested.front()
    //         .unwrap_or_else(|| &self.global)
    // }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.nested.front_mut()
            .unwrap_or_else(|| &mut self.global)
    }

    pub fn query_raw(&self, name: &String) -> Option<&Symbol> {
        for scope in &self.nested {
            if let Some(symbol) = scope.query(&name) {
                return Some(symbol)
            }
        }
        self.global.query(&name)
    }

    pub fn query(&self, info: &IdentInfo) -> Option<&Symbol> {
        self.query_raw(&info.name)
    }

    // pub fn query(&self, info: &IdentInfo) -> Option<&Symbol> {
    //     self.current_scope().query(&info.name)
    // }

    // pub fn query_mut(&mut self, info: &IdentInfo) -> Option<&mut Symbol> {
    //     self.current_scope_mut().query_mut(&info.name)
    // }

    pub fn insert(&mut self, symbol: Symbol) -> Option<&Symbol> {
        self.current_scope_mut()
            .insert(symbol)
    }

    /// Pushes a scope to the front of the queue.
    ///
    /// This is called whenever a new scope is introduced.
    pub fn push_scope(&mut self) {
        self.nested.push_front(Scope::new());
    }

    /// Pops a scope from the front of the queue.
    ///
    /// This is called whenever a scope ends. If the parent scope is the global scope, the `scopes`
    /// vector will be empty. If this scope's parent scope is another nested scope, the current
    /// scope is popped to make the parent scope the current scope.
    pub fn pop_scope(&mut self) {
        self.nested.pop_front();
    }
}

#[derive(Clone)]
pub struct Symbol {
    ident: Ident,
    kind: SymbolKind,
}

impl Symbol {
    pub fn func(ident: Ident, params: Vec<Symbol>) -> Symbol {
        Symbol {
            ident,
            kind: SymbolKind::Func(params),
        }
    }

    pub fn enumeration(ident: Ident, members: Vec<Symbol>) -> Symbol {
        let mut member_map = Scope::new();
        for m in members {
            member_map.insert(m);
        }
        Symbol {
            ident,
            kind: SymbolKind::Enum(member_map),
        }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn new_unqual(ident: Ident) -> Symbol {
        Symbol {
            ident,
            kind: SymbolKind::Ident,
        }
    }

    pub fn kind(&self) -> &SymbolKind {
        &self.kind
    }

    // pub fn new_ident(name: String) -> Symbol {
    //     Symbol {
    //         ident: Ident::Unqual(IdentInfo { name }),
    //         kind: SymbolKind::Ident,
    //     }
    // }

    // pub fn qual_ident(name: String, qualifiers: Vec<IdentInfo>) -> Symbol {
    //     Symbol {
    //         ident: Ident::Qual(qualifiers),
    //         kind: SymbolKind::Ident,
    //     }
    // }

    pub fn name(&self) -> &String {
        match self.ident {
            Ident::Unqual(ref info) => &info.name,
            Ident::Qual(_, ref info) => &info.name,
        }
    }
}

#[derive(Clone)]
pub enum SymbolKind {
    Func(Vec<Symbol>),
    // Struct(Vec<Symbol>),
    Enum(Scope),
    Ident,
}

// fn resolve_ident(st: &mut SymbolTable, ident: &Ident) {
//     let status_string = match ident {
//         Ident::Unqual(ref info) =>
//             match st.query(info) {
//                 Some(ref found_symbol) => format!("RESOLVED:\n  {}", found_symbol.name()),
//                 None => format!("ERROR:\n  {} does not exist!", info.name()),
//             },
//         Ident::Qual(ref qualifier, ref info) => {
//             let parent = st.query(qualifier);
//             match parent {
//                 Some(ref parent) =>
//                     match parent.kind {
//                         SymbolKind::Enum(ref enum_scope) =>
//                             match enum_scope.query(info.name()) {
//                                 Some(ref found_symbol) => format!("RESOLVED:\n  {}::{}", qualifier.name(), found_symbol.name()),
//                                 None => format!("ERROR:\n  {}::{} does not exist", qualifier.name(), info.name()),
//                             },
//                         SymbolKind::Ident => format!("ERROR:\n  {} is not an enumeration!", parent.name()),
//                     }
//                 None => format!("ERROR:\n  {} does not exist in {}::{}!", qualifier.name(), qualifier.name(), info.name()),
//             }
//         }
//     };
//     println!("\n{}", status_string);
// }
