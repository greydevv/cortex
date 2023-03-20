//! The main AST validation interface.

use std::collections::{ HashMap, VecDeque };
use std::convert::From;

use crate::symbols::{ TyKind, BinOpKind };
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

/// Describes an identifier in the symbol table.
struct IdentInfo {
    /// The type of the identifier.
    ty_kind: TyKind,
    #[allow(dead_code)]
    /// The context of the identifier.
    ident_ctx: IdentCtx,
    /// How many times this identifier has been referenced.
    ref_count: u32,
}

impl IdentInfo {
    /// Creates a new identifier information object.
    pub fn new(ty_kind: TyKind, ident_ctx: IdentCtx) -> IdentInfo {
        IdentInfo {
            ty_kind,
            ident_ctx,
            ref_count: 0,
        }
    }

    /// Updates the number of times this particular identifier has been referenced.
    pub fn update_ref_count(&mut self) {
        self.ref_count += 1;
    }
}

impl From<&Ident> for IdentInfo {
    fn from(ident: &Ident) -> IdentInfo {
        IdentInfo::new(
            *ident.ty_kind(),
            *ident.ctx(),
        )
    }
}

/// A wrapper around `Vec` to manage symbol tables belonging to different scopes.
///
/// Each scope is associated with its own [`SymbolTable`]. When a scope is being validated, a new
/// symbol table is constructed and placed in the front of the queue. Subsequent symbol table
/// entries in the queue belong to ancestors of the scope currently being validated. When scopes
/// are finished being validated, its associated symbol table is popped from the queue to prohibit
/// other scopes from illegalay referencing symbols which are no longer in scope.
struct SymbolTableList {
    /// The wrapped list of symbol tables.
    inner: VecDeque<SymbolTable>,
}

impl SymbolTableList {
    /// Creates a new symbol table list.
    pub fn new() -> SymbolTableList {
        SymbolTableList {
            inner: VecDeque::new(),
        }
    }

    /// Queries the symbol table with the given identifier.
    ///
    /// This method iterates over each symbol table attempting to find the queried symbol. If the
    /// symbol is not found `None` is returned.
    pub fn query(&mut self, ident: &Ident) -> Option<&IdentInfo> {
        for symb_tab in &mut self.inner {
            if let Some(q_ident) = symb_tab.query(ident) {
                return Some(q_ident);
            }
        }
        None
    }

    /// Attempts to insert an identifier into the symbol table.
    ///
    /// This method iterates over each symbol table attempting to find a conflicting entry. If none
    /// is found, `None` is returned. Otherwise, the result of [`SymbolTable::try_insert`] is
    /// returned.
    pub fn try_insert<'a>(&'a mut self, ident: &'a Ident) -> Option<&Ident> {
        // TODO: Return the occupying IdentInfo instead. This could be useful for underlining (in
        // the error) the definition of the already defined identifier.
        for symb_tab in &mut self.inner {
            if symb_tab.query(ident).is_some() {
                return Some(ident);
            }
        }
        self.inner.front_mut()
            .and_then(|curr_symb_tab| curr_symb_tab.try_insert(ident))
    }

    /// Pushes a new symbol table to the front of the queue.
    pub fn push(&mut self) {
        self.inner.push_front(SymbolTable::new());
    }

    /// Pops a symbol table from the front of the queue.
    pub fn pop(&mut self) {
        self.inner.pop_front();
    }
}

/// The symbol table object.
struct SymbolTable {
    /// The wrapped symbol table.
    inner: HashMap<String, IdentInfo>
}

impl SymbolTable {
    /// Creates a new (empty) symbol table.
    pub fn new() -> SymbolTable {
        SymbolTable {
            inner: HashMap::new()
        }
    }

    /// Queries the symbol table with the given identifier.
    ///
    /// If the identifier is found, its reference count is then updated.
    pub fn query(&mut self, ident: &Ident) -> Option<&IdentInfo> {
        // TODO: Ref count isn't used right now, but implication of calling this method without
        // intent of updating ref count could cause unintended bugs. Investigate!
        self.inner.get_mut(ident.raw())
            .and_then(|info| {
                info.update_ref_count();
                Some(&*info)
            })
    }

    /// Attempts to insert an identifier into the symbol table.
    ///
    /// If insertion is successful, `None` is returned. Otherwise, the identifier that couldn't be
    /// inserted is returned.
    pub fn try_insert<'a>(&'a mut self, ident: &'a Ident) -> Option<&Ident> {
        // TODO: Return the occupying IdentInfo instead. This could be useful for underlining (in
        // the error) the definition of the already defined identifier.
        if let Some(_) = self.query(ident) {
            Some(ident)
        } else {
            self.inner.insert(ident.raw().clone(), IdentInfo::from(ident));
            None
        }
    }
}

/// The validator object.
pub struct Validator {
    /// Global symbol table.
    // TODO: Currently, only one global symbol table for testing purposes and initial
    // implementation. This needs changed to support anything other than one function.
    symb_tab: SymbolTableList,
}

impl Validator {
    /// Creates a new validator object.
    pub fn new() -> Validator {
        let mut symb_tab = SymbolTableList::new();
        // initialize global symbol table
        symb_tab.push();
        Validator {
            symb_tab
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
        self.symb_tab.push();
        func.params.iter_mut()
            .try_for_each(|p| -> Result {
                self.validate_ident(p)?;
                Ok(())
            })?;
        self.validate_expr(&mut func.body)?;
        self.symb_tab.pop();
        Ok(ret_ty_kind)
    }

    /// Validates a generic expression node.
    fn validate_expr(&mut self, expr: &mut Expr) -> Result<TyKind> {
        match expr.kind {
            ExprKind::Id(ref mut ident) => self.validate_ident(ident),
            ExprKind::Lit(ref lit_kind) => Ok(self.validate_lit(lit_kind)),
            ExprKind::Compound(ref mut children) => {
                self.symb_tab.push();
                children.iter_mut()
                    .try_for_each(|c| -> Result {
                        self.validate_node(c)?;
                        Ok(())
                    })?;
                self.symb_tab.pop();
                Ok(TyKind::Void)
            },
            ExprKind::Binary(ref op_kind, ref mut lhs, ref mut rhs) => {
                let lhs_ty = self.validate_expr(lhs)?;
                let rhs_ty = self.validate_expr(rhs)?;
                // TODO: Need to do anything with operator associativity?
                self.validate_bin_op(op_kind, &lhs_ty, &rhs_ty)
            }
            ExprKind::Stmt(ref mut stmt_kind) => self.validate_stmt(stmt_kind),
            ExprKind::Cond(ref mut cond_kind) => self.validate_cond(cond_kind),
            _ => unimplemented!("{}", expr.kind),
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
                // validate the variable being defined isn't used in its own definition by
                // validating the right-hand side of the equals sign first
                self.validate_expr(expr)
                    .and_then(|_| self.validate_ident(ident)),
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
                    // format error based on context (param, func, variable)
                    match self.symb_tab.try_insert(ident) {
                        Some(_) => todo!("PROPER ERROR: defined symbol '{}' already exists!", ident.raw()),
                        None => Ok(*ident.ty_kind())
                    }
            IdentCtx::Ref =>
                self.symb_tab.query(ident)
                    // format error based on context (param, func, variable)
                    .ok_or_else(|| todo!("PROPER ERROR: referenced symbol '{}' doesn't exist!", ident.raw()))
                    .and_then(|info| {
                        ident.update_ty(info.ty_kind);
                        Ok(info.ty_kind)
                    })
                        
        }
    } 

    /// Determines the type of a literal.
    fn validate_lit(&self, lit: &LitKind) -> TyKind {
        match lit {
            // TODO: Use 'n' in 'Num(n)' to determine size of type
            LitKind::Num(_) => TyKind::Int(32),
            LitKind::Str(_) => TyKind::Str,
        }
    }

    /// Expects a given expression to produce the boolean type.
    fn expect_bool_from(&mut self, expr: &mut Expr) -> Result {
        self.validate_expr(expr)
            .and_then(|ref ty_kind| TyKind::Bool.compat(ty_kind))
    }

    /// Helper method for validating a binary operator.
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
                | BinOpKind::Div => TyKind::Int(32),
        };
        Ok(op_ty)
    }
}
