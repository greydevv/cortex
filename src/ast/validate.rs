//! The main AST validation interface.

use crate::symbols::{ TyKind, IntSize, BinOpKind, UnaryOpKind };
use crate::io::error::{ Result, CortexError };
use crate::io::file::{ FileSpan, FilePos, SourceLoc };
use crate::ast::{
    Module,
    Func,
    Enum,
    Expr,
    ExprKind,
    Compound,
    Stmt,
    StmtKind,
    LitKind,
    Ident,
    ScopeRes,
};
use crate::ast::symbol_table::{
    SymbolTable,
    Symbol,
    SymbolKind,
};
use crate::sess::SessCtx;

// #[derive(Clone)]
// struct IdentEntry {
//     pub ident: Ident,
//     pub kind: IdentEntryKind,
// }
//
// impl IdentEntry {
//     /// Constructs a new `IdentEntry`.
//     pub fn new(ident: Ident, kind: IdentEntryKind) -> IdentEntry {
//         IdentEntry {
//             kind,
//             ident,
//         }
//     }
// }
//
// #[derive(Clone)]
// enum IdentEntryKind {
//     Func(Vec<Ident>),
//     Enum,
//     Var,
// }

// /// A representation of a scope.
// ///
// /// Effectively just a wrapper around a `HashMap`.
// struct Scope {
//     inner: HashMap<String, IdentEntry>,
// }
//
// impl Scope {
//     /// Creates a new scope.
//     pub fn new() -> Scope {
//         Scope {
//             inner: HashMap::new()
//         }
//     }
//
//     /// Queries the scope for the given `Ident`.
//     ///
//     /// If the key is not present, `None` is returned. If the key is present, a reference to it is
//     /// returned.
//     pub fn query(&self, ident: &Ident) -> Option<&IdentEntry> {
//         self.inner.get(ident.get_raw())
//     }
//
//     /// Inserts an `IdentEntry` into the scope.
//     ///
//     /// If insertion was successful, `None` is returned. If insertion failed, i.e. the entry
//     /// already exists, a reference to the conflicting entry is returned.
//     pub fn insert(&mut self, entry: IdentEntry) -> Option<&IdentEntry> {
//         match self.inner.try_insert(entry.ident.get_raw().clone(), entry.clone()) {
//             Err(_) => self.query(&entry.ident), // return the conflicting entry
//             Ok(_) => None,
//         }
//     }
// }

/// The symbol table object.
// struct SymbolTable {
//     /// Global scope
//     global: Scope,
//     /// Nested scopes
//     nested: VecDeque<Scope>
// }

// impl SymbolTable {
//     /// Creates a new symbol table.
//     pub fn new() -> SymbolTable {
//         SymbolTable {
//             // global scope
//             global: Scope::new(),
//             // nested scopes
//             nested: VecDeque::new(),
//         }
//     }
//
//     /// Pushes a scope to the front of the queue.
//     ///
//     /// This is called whenever a new (nested) scope is introduced.
//     pub fn push_scope(&mut self) {
//         self.nested.push_front(Scope::new());
//     }
//
//     /// Pops a scope from the front of the queue.
//     ///
//     /// This is called whenever a scope ends. If the parent scope is the global scope, the `scopes`
//     /// vector will be empty. If this scope's parent scope is another nested scope, the current
//     /// scope is popped to make the parent scope the current scope.
//     pub fn pop_scope(&mut self) {
//         self.nested.pop_front();
//     }
//
//     /// Returns a mutable reference to the current scope.
//     fn current_scope(&mut self) -> &mut Scope {
//         self.nested.front_mut()
//             .unwrap_or_else(|| &mut self.global)
//     }
//
//     /// Queries the symbol table.
//     ///
//     /// If the entry does not exist, `None` is returned. Otherwise, `Some` is returned containing a
//     /// reference to the entry.
//     pub fn try_query(&self, ident: &Ident) -> Option<&IdentEntry> {
//         for scope in &self.nested {
//             if let Some(entry) = scope.query(ident) {
//                 return Some(entry)
//             }
//         }
//         self.global.query(ident)
//     }
//
//     /// Inserts an entry into the symbol table.
//     ///
//     /// If insertion fails, i.e., there exists a matching entry in the symbol table, `Some` is
//     /// returned containing that entry. Otherwise, `None` signifies a successful insertion.
//     pub fn try_insert(&mut self, entry: IdentEntry) -> Option<&IdentEntry> {
//         self.current_scope().insert(entry)
//     }
// }

/// The validator object.
pub struct Validator<'a> {
    /// The context of the current compilation session.
    ctx: &'a SessCtx,
    symb_tab: SymbolTable,
}

impl<'a> Validator<'_> {
    /// Creates a new validator object.
    pub fn new(ctx: &'a SessCtx) -> Validator<'a> {
        Validator {
            ctx,
            symb_tab: SymbolTable::new(),
        }
    }

    /// The main driver for the validator. This method runs over the AST and validates it, mainly
    /// by type checking.
    pub fn validate(&mut self, module: &mut Module) -> Result {
        module.stmts_mut().iter_mut()
            .try_for_each(|node| {
                self.validate_stmt(node)?;
                Ok(())
            })
    }

    /// Validates a function node.
    fn validate_func(&mut self, func: &mut Func) -> Result<TyKind> {
        // Put func ident in parent's symbol table (before the function's symbol table is
        // initialized)
        let param_symbols: Vec<Symbol> = func.params.iter()
            .map(|p: &Ident| Symbol::new_unqual(p.clone()))
            .collect();
        let func_symbol = Symbol::func(
            // TODO: More lazy cloning... optimize later.
            func.ident.clone(),
            param_symbols.clone(),
        );
        self.symb_tab_insert(func_symbol)?;
        // Initialize function's symbol table.
        self.symb_tab.push_scope();
        // Put function arguments inside its symbol table. 
        param_symbols.iter()
            .try_for_each(|p: &Symbol| -> Result {
                // TODO: This cloning is lazy. Optimize later!
                self.symb_tab_insert(p.clone())
            })?;
        let body_ret_ty = self.validate_compound(&mut func.body, true)?;
        // Check if the function's return type was satisfied.
        if func.ident.ty_kind().compat(&body_ret_ty).is_none() {
            let ret_span = match func.body.get_break_stmt() {
                // Underline the return statement.
                Some(ref stmt) => *stmt.loc().span(),
                // Underline the closing brace to show a missing return statement.
                None => func.body.span().end(),
            };
            let loc = SourceLoc::new(self.ctx.file_path(), ret_span);
            return Err(CortexError::incompat_types(
                &self.ctx,
                &func.ident.ty_kind(),
                &body_ret_ty,
                loc,
            ).into())
        }
        self.symb_tab.pop_scope();
        Ok(func.ident.ty_kind().clone())
    }

    fn symb_tab_insert(&mut self, symbol: Symbol) -> Result {
        match self.symb_tab.insert(symbol.clone()) {
            Some(conflict) => Err(CortexError::illegal_ident(&self.ctx, symbol.ident().info(), Some(conflict.ident())).into()),
            None => Ok(()),
        }
    }

    fn symb_tab_query(&mut self, ident: &mut Ident) -> Result<&Symbol> {
        match ident {
            Ident::Unqual(ref info) =>
                // Here we have the case 'foo'.
                match self.symb_tab.query(info) {
                    Some(found_symbol) => {
                        ident.set_ty_kind(found_symbol.ident().ty_kind().clone());
                        Ok(found_symbol)
                    },
                    // 'foo' does not exist.
                    None => Err(CortexError::illegal_ident(&self.ctx, ident.info(), None).into()),
                },
            Ident::Qual(ref qual_info, ref info) => {
                // Here we have the case 'foo::bar'. Need to resolve both 'foo' and 'bar'.
                let qual_symbol = self.symb_tab.query(qual_info);
                match qual_symbol {
                    Some(ref qual_symbol) =>
                        match qual_symbol.kind() {
                            // Resolve 'bar' in scope of 'foo'.
                            SymbolKind::Enum(ref enum_scope) =>
                                match enum_scope.query(&info.name) {
                                    Some(target_symbol) => {
                                        // Resolved 'bar'.
                                        ident.set_ty_kind(target_symbol.ident().ty_kind().clone());
                                        Ok(target_symbol)
                                    },
                                    // 'bar' does not exist in 'foo'.
                                    None => Err(CortexError::nonexistent_enum_member(&self.ctx, qual_info, info).into()),
                                },
                            // 'foo' is not an enum.
                            _ => Err(CortexError::not_enum(&self.ctx, qual_info).into()),
                        }
                    None => Err(CortexError::illegal_ident(&self.ctx, qual_info, None).into()),
                }
            }
        }
    }

    /// Validates a generic expression node.
    fn validate_expr(&mut self, expr: &mut Expr) -> Result<TyKind> {
        let mut loc = expr.loc().clone();
        match expr.kind {
            ExprKind::Id(ref mut ident) => self.symb_tab_query(ident).and_then(|symbol| Ok(symbol.ident().ty_kind().clone())),
            ExprKind::Lit(ref lit_kind) => Ok(self.validate_lit(lit_kind)),
            ExprKind::Binary(ref op_kind, ref mut lhs, ref mut rhs) => self.validate_bin_op(op_kind, lhs, rhs),
            ExprKind::Unary(ref unary_op_kind, ref mut expr) => self.validate_unary_op(unary_op_kind, expr),
            ExprKind::Call(ref mut ident, ref mut args) => {
                let func_symbol = self.symb_tab_query(ident)?.clone();
                let func_ty_kind = func_symbol.ident().ty_kind().clone();
                match func_symbol.kind() {
                    SymbolKind::Func(expected_args) =>
                        // Check if correct amount of params are passed
                        if args.len() == expected_args.len() {
                            // Check types of params
                            args.iter_mut()
                                .zip(expected_args)
                                .try_for_each(|(arg, expected_arg)| -> Result {
                                    let arg_ty_kind = self.validate_expr(arg)?;
                                    let type_ok = match expected_arg.ident().ty_kind() {
                                        TyKind::UserDef(ty_string) => {
                                            if let TyKind::UserDef(ref arg_ty_string, ..) = arg_ty_kind {
                                                // Both types are user-defined, check if they
                                                // match.
                                                arg_ty_string == ty_string
                                            } else {
                                                // If one type isn't user defined, we know they
                                                // don't match.
                                                false
                                            }
                                        },
                                        _ => &arg_ty_kind == expected_arg.ident().ty_kind(),
                                    };
                                    if !type_ok {
                                        println!("Before error: {}", arg.loc().span());
                                        Err(CortexError::incompat_types(&self.ctx, &expected_arg.ident().ty_kind(), &arg_ty_kind, arg.loc().clone()).into())
                                    } else {
                                        Ok(())
                                    }
                                })
                        } else {
                            // Capture certain arguments in underline depending on
                            // missing/excessive arguments
                            if expected_args.is_empty() {
                                // Capture every argument in underline
                                // Unwrapping safe, args is not empty
                                let beg_span = args.first().unwrap().loc().span();
                                let end_span = args.last().unwrap().loc().span();
                                loc.set_span(beg_span.to(end_span))
                            } else {
                                if args.len() < expected_args.len() {
                                    let pos = match args.last() {
                                        // Point after last parameter of function call
                                        Some(arg) => arg.loc().span().end,
                                        // Point after opening parenthesis of function call
                                        None => FilePos::new(loc.span().beg.line, loc.span().beg.col+1),
                                    };
                                    loc.set_span(FileSpan::one(pos))
                                } else {
                                    // Unwrapping safe, args.len() > expected_args.len()
                                    let beg_span = args.get(expected_args.len()).unwrap().loc().span();
                                    // Unwrapping safe, args not empty
                                    let end_span = args.last().unwrap().loc().span();
                                    loc.set_span(beg_span.to(end_span))
                                }
                            }
                            Err(CortexError::args_n_mismatch(&self.ctx, expected_args.len(), args.len(), loc).into())
                        },
                    _ => return Err(CortexError::illegal_ident_call(&self.ctx, loc, &func_symbol.ident()).into()), 
                }?;
                Ok(func_ty_kind)
            },
            ExprKind::ScopeRes(ref mut scope_res) => self.validate_scope_res(scope_res),
        }
    }

    fn validate_scope_res(&mut self, _scope_res: &mut ScopeRes) -> Result<TyKind> {
        todo!("validation for ExprKind::ScopeRes")
    }

    /// Validates a block.
    fn validate_compound(&mut self, compound: &mut Compound, created_scope: bool) -> Result<TyKind> {
        if !created_scope {
            self.symb_tab.push_scope();
        }
        let mut ret_kind = TyKind::Void;
        let break_idx = compound.get_break_idx();
        compound.stmts_mut().iter_mut()
            .enumerate()
            .try_for_each(|(i, child)| -> Result {
                let ty_kind = self.validate_stmt(child)?;
                if let Some(break_idx) = break_idx {
                    if break_idx == i{
                        ret_kind = ty_kind;
                    }
                }
                Ok(())
            })?;
        self.symb_tab.pop_scope();
        // TODO: Warn user if there is code after breaking statement by using this condition:
        // `if break_idx < (children.len() as u32) - 1`
        Ok(ret_kind)
    }

    /// Validates a statement.
    fn validate_stmt(&mut self, stmt: &mut Stmt) -> Result<TyKind> {
        match stmt.kind {
            StmtKind::Let(ref mut ident, ref mut expr) =>
                // make sure the variable being defined isn't used in its own definition by
                // validating the right-hand side of the equals sign first
                self.validate_expr(expr)
                    .and_then(|rhs_ty_kind| {
                        match ident.ty_kind() {
                            TyKind::Infer => ident.set_ty_kind(rhs_ty_kind.clone()),
                            TyKind::UserDef(ty_string) => {
                                match self.symb_tab.query_raw(ty_string) {
                                    Some(symbol) =>
                                        if symbol.ident().ctx().is_typedef() {
                                            ident.set_ty_kind(symbol.ident().ty_kind().clone());
                                        } else {
                                            return Err(CortexError::not_typedef(&self.ctx, ty_string, ident.loc()).into());
                                        },
                                    None => return Err(CortexError::unknown_typedef(&self.ctx, ty_string, ident.loc()).into()),
                                }
                            },
                            _ => {
                                if ident.ty_kind().compat(&rhs_ty_kind).is_none() {
                                    return Err(CortexError::incompat_types(self.ctx, &ident.ty_kind(), &rhs_ty_kind, expr.loc().clone()).into());
                                }
                            }
                        }
                        let symbol = Symbol::new_unqual(ident.clone());
                        self.symb_tab_insert(symbol)?;
                        Ok(rhs_ty_kind)
                    }),
            StmtKind::Ret(ref mut expr) =>
                match expr {
                    Some(expr) => self.validate_expr(expr),
                    None => Ok(TyKind::Void),
                },
            StmtKind::Func(ref mut func) => self.validate_func(func),
            StmtKind::Expr(ref mut expr) => self.validate_expr(expr),
            StmtKind::If(ref mut expr, ref mut body, ref mut other) => self.validate_if(expr, body, other),
            StmtKind::Else(ref mut body) => self.validate_compound(body, false),
            StmtKind::While(ref mut expr, ref mut body) => self.validate_while(expr, body),
            StmtKind::Compound(ref mut compound) => self.validate_compound(compound, false),
            StmtKind::Incl(ref mut module) => self.validate(module).and_then(|_| Ok(TyKind::Void)),
            StmtKind::Enum(ref mut enumer) => self.validate_enum(enumer).and_then(|_| Ok(TyKind::Void)),
        }
    }

    /// Validates an enumeration definition.
    fn validate_enum(&mut self, enumer: &mut Enum) -> Result {
        let enum_member_symbols = enumer.members.iter()
            .map(|m: &Ident| -> Symbol {
                Symbol::new_unqual(m.clone())
            })
            .collect();
        let symbol = Symbol::enumeration(enumer.ident().clone(), enum_member_symbols);
        self.symb_tab_insert(symbol)
    }

    /// Validates a while loop.
    fn validate_while(&mut self, expr: &mut Option<Expr>, body: &mut Compound) -> Result<TyKind> {
        // Loop condition is optional. If not supplied, loop will run forever until broken.
        if let Some(expr) = expr {
            self.expect_bool_from(expr)?;
        }
        self.validate_compound(body, false)
    }

    /// Validates an if (or else-if) statement.
    fn validate_if(&mut self, expr: &mut Expr, body: &mut Compound, other: &mut Option<Box<Stmt>>) -> Result<TyKind> {
        self.expect_bool_from(expr)?;
        let ret_ty_kind = self.validate_compound(body, false)?;
        if let Some(else_if_or_else) = other {
            self.validate_stmt(else_if_or_else)?;
        }
        Ok(ret_ty_kind)
    }

    /// Determines the type of a literal.
    fn validate_lit(&self, lit: &LitKind) -> TyKind {
        match lit {
            // TODO: Use 'n' in 'Num(n)' to determine size of type
            LitKind::Num(_) => TyKind::Int(IntSize::N32),
            LitKind::Str(_) => TyKind::Str,
            LitKind::Bool(_) => TyKind::Bool,
        }
    }

    /// Expects a given expression to produce the boolean type.
    fn expect_bool_from(&mut self, expr: &mut Expr) -> Result {
        let expr_ty_kind = self.validate_expr(expr)?;
        TyKind::Bool.compat(&expr_ty_kind)
            .ok_or_else(|| CortexError::incompat_types(self.ctx, &TyKind::Bool, &expr_ty_kind, expr.loc().clone()).into())
            .and_then(|_| Ok(()))
    }

    /// Helper method for validating a unary operation.
    fn validate_unary_op(&mut self, unary_op_kind: &UnaryOpKind, expr: &mut Expr) -> Result<TyKind> {
        let expr_ty_kind = self.validate_expr(expr)?;
        let op_ty_kind = match unary_op_kind {
            UnaryOpKind::Not => TyKind::Bool,
            UnaryOpKind::Neg => TyKind::Int(IntSize::N32),
        };
        op_ty_kind.compat(&expr_ty_kind)
            .ok_or_else(|| CortexError::incompat_types(self.ctx, &op_ty_kind, &expr_ty_kind, expr.loc().clone()).into())
            .and_then(|_| Ok(op_ty_kind.clone()))
    }

    /// Helper method for validating a binary operation.
    fn validate_bin_op(&mut self, bin_op_kind: &BinOpKind, lhs: &mut Expr, rhs: &mut Expr) -> Result<TyKind> {
        let lhs_ty = self.validate_expr(lhs)?;
        let rhs_ty = self.validate_expr(rhs)?;
        if lhs_ty.compat(&rhs_ty).is_none() {
            return Err(CortexError::incompat_types(self.ctx, &lhs_ty, &rhs_ty, rhs.loc().clone()).into());
        }
        // TODO: Need to check which operators can be applied to what type! And if an operator can
        // be applied to a specific type, what type does that operation yield?
        let op_ty = match bin_op_kind {
            BinOpKind::Eql => TyKind::Void,
            BinOpKind::Gr
                | BinOpKind::GrEql
                | BinOpKind::Lt
                | BinOpKind::LtEql
                | BinOpKind::EqlBool => TyKind::Bool,
            BinOpKind::Add
                | BinOpKind::Sub
                | BinOpKind::Mul
                | BinOpKind::Div => TyKind::Int(IntSize::N32),
        };

        if lhs_ty != op_ty {
            return Err(CortexError::incompat_types_in_bin_op(
                &self.ctx,
                bin_op_kind, 
                &lhs_ty,
                lhs.loc().clone(),
            ).into());
        }

        if rhs_ty != op_ty {
            return Err(CortexError::incompat_types_in_bin_op(
                &self.ctx,
                bin_op_kind, 
                &rhs_ty,
                rhs.loc().clone(),
            ).into());
        }

        Ok(op_ty)
    }
}
