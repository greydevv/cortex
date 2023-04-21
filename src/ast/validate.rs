//! The main AST validation interface.

use std::collections::{
    HashMap,
    VecDeque
};

use crate::symbols::{ TyKind, IntSize, BinOpKind, UnaryOpKind };
use crate::io::error::{ Result, CortexError };
use crate::io::file::{ FileSpan, FilePos };
use crate::ast::{
    AstNode,
    Func,
    Expr,
    ExprKind,
    StmtKind,
    LitKind,
    CondKind,
    LoopKind,
    Ident,
};
use crate::sess::SessCtx;

#[derive(Clone)]
struct IdentEntry {
    pub ident: Ident,
    pub kind: IdentEntryKind,
}

impl IdentEntry {
    pub fn new(ident: Ident, kind: IdentEntryKind) -> IdentEntry {
        IdentEntry {
            kind,
            ident,
        }
    }
}

#[derive(Clone)]
enum IdentEntryKind {
    Func(Vec<Ident>),
    Var,
}

/// The symbol table object.
struct SymbolTable {
    /// Global scope
    global: HashMap<String, IdentEntry>,
    /// Nested scopes
    scopes: VecDeque<HashMap<String, IdentEntry>>,
}

impl SymbolTable {
    /// Creates a new symbol table.
    pub fn new() -> SymbolTable {
        SymbolTable {
            // global scope
            global: HashMap::new(),
            // nested scopes
            scopes: VecDeque::new()
        }
    }

    /// Pushes a scope to the front of the queue.
    ///
    /// This is called whenever a new (nested) scope is introduced.
    pub fn push_scope(&mut self) {
        self.scopes.push_front(HashMap::new());
    }

    /// Pops a scope from the front of the queue.
    ///
    /// This is called whenever a scope ends. If the parent scope is the global scope, the `scopes`
    /// vector will be empty. If this scope's parent scope is another nested scope, the current
    /// scope is popped to make the parent scope the current scope.
    pub fn pop_scope(&mut self) {
        self.scopes.pop_front();
    }

    /// Returns a mutable reference to the current scope.
    fn current_scope(&mut self) -> &mut HashMap<String, IdentEntry> {
        self.scopes.front_mut()
            .unwrap_or_else(|| &mut self.global)
    }

    /// Queries the symbol table.
    ///
    /// If the entry does not exist, `None` is returned. Otherwise, `Some` is returned containing a
    /// reference to the entry.
    pub fn try_query(&self, ident: &Ident) -> Option<&IdentEntry> {
        for scope in &self.scopes {
            if scope.contains_key(ident.raw()) {
                return scope.get(ident.raw());
            }
        }
        self.global.get(ident.raw())
    }

    /// Inserts an entry into the symbol table.
    ///
    /// If insertion fails, i.e., there exists a matching entry in the symbol table, `Some` is
    /// returned containing that entry. Otherwise, `None` signifies a successful insertion.
    pub fn try_insert(&mut self, entry: IdentEntry) -> Option<IdentEntry> {
        if let Some(conflict) = self.try_query(&entry.ident) {
            return Some(conflict.clone());
        }
        if self.current_scope().try_insert(entry.ident.raw().clone(), entry.clone()).is_err() {
            // unwrapping is safe here (insert found a conflict, so we know it exists)
            Some(self.try_query(&entry.ident).unwrap().clone())
        } else {
            None
        }
    }
}

/// The validator object.
pub struct Validator<'a> {
    /// The context of the current compilation session.
    ctx: &'a SessCtx,
    symb_tab: SymbolTable,
}

impl<'a> Validator<'_> {
    /// Creates a new validator object.
    pub fn new(ctx: &'a SessCtx) -> Validator<'a> {
        // let symb_tab = SymbolTableList::new();
        // initialize global symbol table
        // symb_tab.push();
        Validator {
            ctx,
            symb_tab: SymbolTable::new(),
            // glob_symb_tab: SymbolTable::new(),
            // symb_tab,
        }
    }

    /// The main driver for the validator. This method runs over the AST and validates it, mainly
    /// by type checking.
    pub fn validate(&mut self, tree: &mut Vec<Box<AstNode>>) -> Result {
        tree.iter_mut()
            .try_for_each(|node| {
                self.validate_node(node)?;
                Ok(())
            })
    }

    /// Validates a generic AST node.
    fn validate_node(&mut self, node: &mut Box<AstNode>) -> Result<TyKind> {
        match **node {
            AstNode::Func(ref mut func) => self.validate_func(func),
            AstNode::Expr(ref mut expr) => self.validate_expr(expr),
        }
    }

    /// Validates a function node.
    fn validate_func(&mut self, func: &mut Func) -> Result<TyKind> {
        // put func ident in parent's symbol table (before the function's symbol table is
        // initialized)
        let entry = IdentEntry::new(
            func.ident.clone(),
            IdentEntryKind::Func(func.params.clone())
        );
        self.symb_tab_insert(entry)?;
            // return 
        // self.symb_tab
        // initialize function's symbol table
        self.symb_tab.push_scope();
        func.params.iter()
            .try_for_each(|p| -> Result {
                let entry = IdentEntry::new(
                    p.clone(),
                    IdentEntryKind::Var,
                );
                self.symb_tab_insert(entry)
            })?;
        self.validate_expr(&mut func.body)?;
        self.symb_tab.pop_scope();
        Ok(*func.ident.ty_kind())
    }

    fn symb_tab_insert(&mut self, entry: IdentEntry) -> Result {
        match self.symb_tab.try_insert(entry.clone()) {
            Some(conflict) => Err(CortexError::illegal_ident(&self.ctx, &entry.ident, Some(&conflict.ident)).into()),
            None => Ok(()),
        }
    }

    fn symb_tab_query(&mut self, ident: &mut Ident) -> Result<&IdentEntry> {
        match self.symb_tab.try_query(ident) {
            Some(def_entry) => {
                ident.update_ty(def_entry.ident.ty_kind);
                Ok(def_entry)
            },
            None => Err(CortexError::illegal_ident(&self.ctx, &ident, None).into()),
        }
    }

    /// Validates a generic expression node.
    fn validate_expr(&mut self, expr: &mut Expr) -> Result<TyKind> {
        let mut span = *expr.span();
        match expr.kind {
            ExprKind::Id(ref mut ident) => self.symb_tab_query(ident).and_then(|entry| Ok(*entry.ident.ty_kind())),
            ExprKind::Lit(ref lit_kind) => Ok(self.validate_lit(lit_kind)),
            ExprKind::Compound(ref mut children, break_idx) => self.validate_compound(children, break_idx),
            ExprKind::Binary(ref op_kind, ref mut lhs, ref mut rhs) => self.validate_bin_op(op_kind, lhs, rhs),
            ExprKind::Stmt(ref mut stmt_kind) => self.validate_stmt(stmt_kind),
            ExprKind::Cond(ref mut cond_kind) => self.validate_cond(cond_kind),
            ExprKind::Loop(ref mut loop_kind) => self.validate_loop(loop_kind),
            ExprKind::Unary(ref unary_op_kind, ref mut expr) => self.validate_unary_op(unary_op_kind, expr),
            ExprKind::Call(ref mut ident, ref mut args) => {
                let func_entry = self.symb_tab_query(ident)?.clone();
                let func_ty_kind = *func_entry.ident.ty_kind();
                match func_entry.kind {
                    IdentEntryKind::Var =>
                        return Err(CortexError::illegal_ident_call(&self.ctx, *expr.span(), &func_entry.ident).into()),
                    IdentEntryKind::Func(expected_args) =>
                        // Check if correct amount of params are passed
                        if args.len() == expected_args.len() {
                            // Check types of params
                            args.iter_mut().zip(expected_args)
                                .try_for_each(|(arg, expected_arg)| -> Result {
                                    let arg_ty_kind = self.validate_expr(arg)?;
                                    if arg_ty_kind != expected_arg.ty_kind {
                                        Err(CortexError::incompat_types(&self.ctx, &expected_arg.ty_kind, &arg_ty_kind, arg.span()).into())
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
                                let beg_span = args.first().unwrap().span();
                                let end_span = args.last().unwrap().span();
                                span = beg_span.to(end_span);
                            }  else {
                                if args.len() < expected_args.len() {
                                    let pos = match args.last() {
                                        // Point after last parameter of function call
                                        Some(arg) => arg.span().end,
                                        // Point after opening parenthesis of function call
                                        None => FilePos::new(span.beg.line, span.beg.col+1),
                                    };
                                    span = FileSpan::one(pos);
                                } else {
                                    // Unwrapping safe, args.len() > expected_args.len()
                                    let beg_span = args.get(expected_args.len()).unwrap().span();
                                    // Unwrapping safe, args not empty
                                    let end_span = args.last().unwrap().span();
                                    span = beg_span.to(end_span);
                                }
                            }
                            Err(CortexError::args_n_mismatch(&self.ctx, expected_args.len(), args.len(), span).into())
                        },
                }?;
                Ok(func_ty_kind)
            }
        }
    }

    /// Validates a block.
    fn validate_compound(&mut self, children: &mut Vec<Box<AstNode>>, break_idx: Option<u32>) -> Result<TyKind> {
        self.symb_tab.push_scope();
        let mut ret_kind = TyKind::Void;
        children.iter_mut()
            .enumerate()
            .try_for_each(|(i, child)| -> Result {
                let ty_kind = self.validate_node(child)?;
                if let Some(break_idx) = break_idx {
                    if break_idx == i as u32 {
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

    /// Validates a conditional expression.
    fn validate_cond(&mut self, cond_kind: &mut CondKind) -> Result<TyKind> {
        match cond_kind {
            CondKind::If(ref mut expr, ref mut body, ref mut other) => {
                let if_ty_kind = self.expect_bool_from(expr)
                    .and_then(|_| self.validate_expr(body))?;
                match other {
                    Some(other) => self.validate_expr(other),
                    None => Ok(if_ty_kind),
                }
            },
            CondKind::Else(ref mut body) => self.validate_expr(body),
        }
    }

    /// Validates a loop.
    fn validate_loop(&mut self, loop_kind: &mut LoopKind) -> Result<TyKind> {
        match loop_kind {
            LoopKind::While(ref mut expr, ref mut body) =>
                match expr {
                    Some(expr) => self.expect_bool_from(expr),
                    None => Ok(())
                }
                .and_then(|_| self.validate_expr(body))
        }
    }

    /// Validates a statement.
    fn validate_stmt(&mut self, stmt_kind: &mut StmtKind) -> Result<TyKind> {
        match stmt_kind {
            StmtKind::Let(ref mut ident, ref mut expr) =>
                // make sure the variable being defined isn't used in its own definition by
                // validating the right-hand side of the equals sign first
                self.validate_expr(expr)
                    .and_then(|rhs_ty_kind| {
                        if ident.ty_kind == TyKind::Infer {
                            ident.update_ty(rhs_ty_kind);
                        } else {
                            if ident.ty_kind.compat(&rhs_ty_kind).is_none() {
                                return Err(CortexError::incompat_types(self.ctx, &ident.ty_kind, &rhs_ty_kind, expr.span()).into());
                            }
                        }
                        let entry = IdentEntry::new(
                            ident.clone(),
                            IdentEntryKind::Var,
                        );
                        self.symb_tab_insert(entry)?;
                        Ok(rhs_ty_kind)
                    }),
            StmtKind::Ret(ref mut expr) =>
                match expr {
                    Some(expr) => self.validate_expr(expr),
                    None => Ok(TyKind::Void),
                },
            _ => unimplemented!("validation for {}", stmt_kind),
        }
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
            .ok_or_else(|| CortexError::incompat_types(self.ctx, &TyKind::Bool, &expr_ty_kind, expr.span()).into())
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
            .ok_or_else(|| CortexError::incompat_types(self.ctx, &op_ty_kind, &expr_ty_kind, expr.span()).into())
            .and_then(|_| Ok(op_ty_kind))
    }

    /// Helper method for validating a binary operation.
    fn validate_bin_op(&mut self, bin_op_kind: &BinOpKind, lhs: &mut Expr, rhs: &mut Expr) -> Result<TyKind> {
        let lhs_ty = self.validate_expr(lhs)?;
        let rhs_ty = self.validate_expr(rhs)?;
        if lhs_ty.compat(&rhs_ty).is_none() {
            return Err(CortexError::incompat_types(self.ctx, &lhs_ty, &rhs_ty, rhs.span()).into());
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
        Ok(op_ty)
    }
}
