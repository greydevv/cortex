//! The main AST validation interface.

use std::collections::HashMap;
use std::convert::From;

use crate::symbols::{ TyKind, BinOpKind };
use crate::io::error::Result;
use crate::ast::{
    AstNodeNew,
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
    glob: SymbolTable,
}

impl Validator {
    /// Creates a new validator object.
    pub fn new() -> Validator {
        Validator {
            glob: SymbolTable::new(),
        }
    }

    /// The main driver for the validator. This method runs over the AST and validates it, mainly
    /// by type checking.
    pub fn validate(&mut self, tree: &mut Vec<Box<AstNodeNew>>) -> Result {
        tree.iter_mut()
            .try_for_each(|node| {
                self.validate_node(node)?;
                Ok(())
            })
    }

    /// Validates a generic AST node.
    fn validate_node(&mut self, node: &mut Box<AstNodeNew>) -> Result<TyKind> {
        match **node {
            AstNodeNew::Func(ref mut func) => self.validate_func(func),
            AstNodeNew::Expr(ref mut expr) => self.validate_expr(expr),
        }
    }

    /// Validates a function node.
    fn validate_func(&mut self, func: &mut Func) -> Result<TyKind> {
        let ret_ty_kind = self.validate_ident(&mut func.ident)?;
        func.params.iter_mut()
            .try_for_each(|p| -> Result {
                self.validate_ident(p)?;
                Ok(())
            })?;
        self.validate_expr(&mut func.body)?;
        Ok(ret_ty_kind)
    }

    /// Validates a generic expression node.
    fn validate_expr(&mut self, expr: &mut Expr) -> Result<TyKind> {
        match expr.kind {
            ExprKind::Id(ref mut ident) => self.validate_ident(ident),
            ExprKind::Lit(ref lit_kind) => self.validate_lit(lit_kind),
            ExprKind::Compound(ref mut children) => {
                children.iter_mut()
                    .try_for_each(|c| -> Result {
                        self.validate_node(c)?;
                        Ok(())
                    })?;
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
            // TODO: Need to evaluate rhs of let first otherwise `let y = y` compiles fine (BAD)
            StmtKind::Let(ref mut ident, ref mut expr) =>
                // validate the variable being defined isn't used in its own definition by
                // validating the right-hand side of the equals sign first
                self.validate_expr(expr)
                    .and_then(|_| self.validate_ident(ident)),
            _ => unimplemented!()
        }
    }

    /// Determined.
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
                    match self.glob.try_insert(ident) {
                        Some(_) => todo!("PROPER ERROR: defined symbol '{}' already exists!", ident.raw()),
                        None => Ok(*ident.ty_kind())
                    }
            IdentCtx::Ref =>
                self.glob.query(ident)
                    // format error based on context (param, func, variable)
                    .ok_or_else(|| todo!("PROPER ERROR: referenced symbol '{}' doesn't exist!", ident.raw()))
                    .and_then(|info| {
                        ident.update_ty(info.ty_kind);
                        Ok(info.ty_kind)
                    })
                        
        }
    } 

    /// Determines the type of a literal.
    fn validate_lit(&self, lit: &LitKind) -> Result<TyKind> {
        // TODO: Does this need to return a Result? It won't fail, so it should probably just
        // return a TyKind, not Result<TyKind>.
        let ty_kind = match lit {
            // TODO: Use 'n' in 'Num(n)' to determine size of type
            LitKind::Num(_) => TyKind::Int(32),
            LitKind::Str(_) => TyKind::Str,
        };
        Ok(ty_kind)
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
