//! The main AST validation interface.

use std::collections::{
    HashMap,
    VecDeque
};

use crate::symbols::{ TyKind, IntSize, BinOpKind, UnaryOpKind };
use crate::io::error::Result;
use crate::ast::{
    AstNode,
    Func,
    Expr,
    ExprKind,
    StmtKind,
    LitKind,
    CondKind,
    Ident,
    IdentCtx,
};
use crate::io::error::CortexError;
use crate::sess::SessCtx;

/// The symbol table object.
struct SymbolTable {
    // Global scope
    global: HashMap<String, Ident>,
    // Nested scopes
    scopes: VecDeque<HashMap<String, Ident>>,
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
    fn current_scope(&mut self) -> &mut HashMap<String, Ident> {
        self.scopes.front_mut()
            .unwrap_or_else(|| &mut self.global)
    }

    /// Queries the symbol table.
    ///
    /// If the entry does not exist, `None` is returned. Otherwise, `Some` is returned containing a
    /// reference to the entry.
    pub fn try_query(&self, ident: &Ident) -> Option<&Ident> {
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
    pub fn try_insert(&mut self, ident: &Ident) -> Option<Ident> {
        if let Some(conflict) = self.try_query(ident) {
            return Some(conflict.clone());
        }
        if self.current_scope().try_insert(ident.raw().clone(), ident.clone()).is_err() {
            Some(self.try_query(ident).unwrap().clone())
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
        let ret_ty_kind = self.validate_ident(&mut func.ident)?;
        // initialize function's symbol table
        self.symb_tab.push_scope();
        func.params.iter_mut()
            .try_for_each(|p| -> Result {
                self.validate_ident(p)?;
                Ok(())
            })?;
        self.validate_expr(&mut func.body)?;
        self.symb_tab.pop_scope();
        Ok(ret_ty_kind)
    }

    /// Validates a generic expression node.
    fn validate_expr(&mut self, expr: &mut Expr) -> Result<TyKind> {
        match expr.kind {
            ExprKind::Id(ref mut ident) => self.validate_ident(ident),
            ExprKind::Lit(ref lit_kind) => Ok(self.validate_lit(lit_kind)),
            ExprKind::Compound(ref mut children) => {
                self.symb_tab.push_scope();
                children.iter_mut()
                    .try_for_each(|c| -> Result {
                        self.validate_node(c)?;
                        Ok(())
                    })?;
                self.symb_tab.pop_scope();
                Ok(TyKind::Void)
            },
            ExprKind::Binary(ref op_kind, ref mut lhs, ref mut rhs) => {
                let lhs_ty = self.validate_expr(lhs)?;
                let rhs_ty = self.validate_expr(rhs)?;
                self.validate_bin_op(op_kind, &lhs_ty, &rhs_ty)
            }
            ExprKind::Stmt(ref mut stmt_kind) => self.validate_stmt(stmt_kind),
            ExprKind::Cond(ref mut cond_kind) => self.validate_cond(cond_kind),
            ExprKind::Unary(ref unary_op_kind, ref mut expr) => {
                let expr_ty = self.validate_expr(expr)?;
                self.validate_unary_op(unary_op_kind, &expr_ty)
            },
            ExprKind::Call(ref mut ident, ref mut args) => {
                let func_ret_ty = self.validate_ident(ident)?;
                args.iter_mut()
                    .try_for_each(|a| -> Result {
                        self.validate_expr(a)?;
                        Ok(())
                    })?;
                Ok(func_ret_ty)
            }
        }
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
            CondKind::While(ref mut expr, ref mut body) =>
                self.expect_bool_from(expr)
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
                            ident.ty_kind.compat(&rhs_ty_kind)?;
                        }
                        self.validate_ident(ident)?;
                        Ok(rhs_ty_kind)
                    }),
            _ => unimplemented!()
        }
    }

    /// Determines, based on its context, if the identifier is valid.
    ///
    /// If the identifier is `x` and the context is [`IdentCtx::Ref`], the validator needs to make
    /// sure `x` is an already defined symbol. If instead the context of `x` is [`IdentCtx::Def`],
    /// for example, the symbol table needs updated with `x`.
    fn validate_ident(&mut self, ident: &mut Ident) -> Result<TyKind> {
        match ident.ctx() {
            IdentCtx::Def
                | IdentCtx::Param
                | IdentCtx::FuncDef =>
                    // ident.ty_kind should NEVER be TyKind::Infer here (handled in let statement
                    // validation)
                    match self.symb_tab.try_insert(ident) {
                        Some(ref conflict) =>
                            Err(CortexError::illegal_ident(&self.ctx, &ident, Some(conflict)).into()),
                        None => Ok(ident.ty_kind),
                    },
            IdentCtx::Ref
                | IdentCtx::FuncCall =>
                    match self.symb_tab.try_query(ident) {
                        Some(def_ident) => {
                            ident.update_ty(def_ident.ty_kind);
                            Ok(ident.ty_kind)
                        },
                        None => Err(CortexError::illegal_ident(&self.ctx, &ident, None).into()),
                    },
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
        self.validate_expr(expr)
            .and_then(|ref ty_kind| TyKind::Bool.compat(ty_kind))
    }

    /// Helper method for validating a unary operation.
    fn validate_unary_op(&self, unary_op_kind: &UnaryOpKind, expr_ty: &TyKind) -> Result<TyKind> {
        let op_ty = match unary_op_kind {
            UnaryOpKind::Not => TyKind::Bool,
            UnaryOpKind::Neg => TyKind::Int(IntSize::N32),
        };
        op_ty.compat(expr_ty)
            .and_then(|_| Ok(op_ty))
    }

    /// Helper method for validating a binary operation.
    fn validate_bin_op(&self, bin_op_kind: &BinOpKind, lhs_ty: &TyKind, rhs_ty: &TyKind) -> Result<TyKind> {
        lhs_ty.compat(rhs_ty)?;
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
