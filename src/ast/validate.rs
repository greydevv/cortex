use std::collections::HashMap;
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

struct IdentInfo {
    ty_kind: TyKind,
    #[allow(dead_code)]
    ident_ctx: IdentCtx,
    ref_count: u32,
}

impl IdentInfo {
    pub fn new(ty_kind: TyKind, ident_ctx: IdentCtx) -> IdentInfo {
        IdentInfo {
            ty_kind,
            ident_ctx,
            ref_count: 0,
        }
    }

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

struct SymbolTable {
    inner: HashMap<String, IdentInfo>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            inner: HashMap::new()
        }
    }

    pub fn query(&mut self, ident: &Ident) -> Option<&IdentInfo> {
        self.inner.get_mut(ident.raw())
            .and_then(|info| {
                info.update_ref_count();
                Some(&*info)
            })
    }

    pub fn try_insert<'a>(&'a mut self, ident: &'a Ident) -> Option<&Ident> {
        if let Some(_) = self.query(ident) {
            Some(ident)
        } else {
            self.inner.insert(ident.raw().clone(), IdentInfo::from(ident));
            None
        }
    }
}

pub struct Validator {
    glob: SymbolTable,
}

impl Validator {
    pub fn new() -> Validator {
        Validator {
            glob: SymbolTable::new(),
        }
    }

    pub fn validate(&mut self, tree: &mut Vec<Box<AstNode>>) -> Result {
        tree.iter_mut()
            .try_for_each(|node| {
                self.validate_node(node)?;
                Ok(())
            })
    }

    fn validate_node(&mut self, node: &mut Box<AstNode>) -> Result<TyKind> {
        match **node {
            AstNode::Func(ref mut func) => self.validate_func(func),
            AstNode::Expr(ref mut expr) => self.validate_expr(expr),
        }
    }

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

    fn validate_lit(&self, lit: &LitKind) -> Result<TyKind> {
        let ty_kind = match lit {
            // TODO: Use 'n' in 'Num(n)' to determine size of type
            LitKind::Num(_) => TyKind::Int(32),
            LitKind::Str(_) => TyKind::Str,
        };
        Ok(ty_kind)
    }

    fn expect_bool_from(&mut self, expr: &mut Expr) -> Result {
        self.validate_expr(expr)
            .and_then(|ref ty_kind| TyKind::Bool.compat(ty_kind))
    }

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
