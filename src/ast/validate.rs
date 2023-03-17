use std::collections::HashMap;
use std::convert::From;

use crate::lexer::token::{ TyKind, BinOpKind };
use crate::io::error::{ CortexError, Result };
use crate::ast::{
    AstNodeNew,
    Func,
    Expr,
    ExprKind,
    StmtKind,
    LitKind,
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

    pub fn validate(&mut self, tree: &mut Vec<Box<AstNodeNew>>) -> Result {
        tree.iter_mut().try_for_each(|node| self.validate_node(node))
    }

    fn validate_node(&mut self, node: &mut Box<AstNodeNew>) -> Result {
        match **node {
            AstNodeNew::Func(ref mut func) => self.validate_func(func),
            AstNodeNew::Expr(ref mut expr) => self.validate_expr(expr),
        }
    }

    fn validate_func(&mut self, func: &mut Func) -> Result {
        self.validate_ident(&mut func.ident)?;
        func.params.iter_mut()
            .try_for_each(|p| {
                self.validate_ident(p)
            })?;
        self.validate_expr(&mut func.body)?;
        Ok(())
    }

    fn validate_expr(&mut self, expr: &mut Expr) -> Result {
        match expr.kind {
            ExprKind::Id(ref mut ident) => self.validate_ident(ident),
            ExprKind::Lit(ref lit_kind) => self.validate_lit(lit_kind),
            ExprKind::Compound(ref mut children) =>
                children.iter_mut()
                    .try_for_each(|c| self.validate_node(c)),
            ExprKind::Binary(ref _op_kind, ref mut lhs, ref mut rhs) =>
                self.validate_expr(lhs)
                    .and_then(|_| self.validate_expr(rhs)),
            ExprKind::Unary(..) => Ok(()),
            ExprKind::Stmt(ref mut stmt_kind) => self.validate_stmt(stmt_kind),
            _ => unimplemented!(),
        }
    }

    fn validate_stmt(&mut self, stmt_kind: &mut StmtKind) -> Result {
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

    fn validate_ident(&mut self, ident: &mut Ident) -> Result {
        match ident.ctx() {
            IdentCtx::Def
                | IdentCtx::Param
                | IdentCtx::FuncDef =>
                    // format error based on context (param, func, variable)
                    match self.glob.try_insert(ident) {
                        Some(_) => todo!("PROPER ERROR: defined symbol '{}' already exists!", ident.raw()),
                        None => Ok(())
                    }
            IdentCtx::Ref =>
                self.glob.query(ident)
                    // format error based on context (param, func, variable)
                    .ok_or_else(|| todo!("PROPER ERROR: referenced symbol '{}' doesn't exist!", ident.raw()))
                    .and_then(|info| {
                        ident.update_ty(info.ty_kind);
                        Ok(())
                    })
                        
        }
    } 

    fn validate_lit(&self, lit: &LitKind) -> Result {
        match lit {
            LitKind::Num(_n) => Ok(()),
            LitKind::Str(_str) => Ok(()),
        }
    }

    // fn validate_bin_op(&self, bin_op_kind: BinOpKind, lhs_ty: &TyKind, rhs_ty: &TyKind) {
    //     todo!()
    // }
}
