use std::collections::HashMap;

use crate::lexer::token::{ TyKind, KwdKind, BinOpKind };
use crate::io::error::{ CortexError, Result };
use crate::ast::{ AstNode, IdentCtx };

struct IdentInfo {
    ty_kind: TyKind,
    ref_count: u32,
}

impl IdentInfo {
    pub fn new(ty_kind: TyKind) -> IdentInfo {
        IdentInfo {
            ty_kind,
            ref_count: 0,
        }
    }

    pub fn update_ref_count(&mut self) {
        self.ref_count += 1;
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

    pub fn query(&mut self, id: &String) -> Option<&mut IdentInfo> {
        self.inner.get_mut(id)
    }

    pub fn insert(&mut self, id: &String, ident_info: IdentInfo) -> Option<IdentInfo> {
        self.inner.insert(id.clone(), ident_info)
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
        for node in tree.iter() {
            self.validate_node(node)?;
        }
        Ok(())
    }

    fn validate_node(&mut self, node: &Box<AstNode>) -> Result<TyKind> {
        let ty_kind = match **node {
            // AstNode::Num(..) => (),
            AstNode::Id(ref id, ref ty_kind, ref ident_ctx) => self.validate_id(id, ty_kind, ident_ctx)?,
            AstNode::BinExpr { ref op, ref lhs, ref rhs } => self.validate_bin_expr(op, lhs, rhs)?,
            // AstNode::UnaryExpr {..} => (),
            AstNode::Stmt { ref kind, ref expr } => self.validate_stmt(kind, expr)?,
            AstNode::Func { ref body, ..} => self.validate_node(body)?,
            AstNode::Compound(ref children) => self.validate_compound(children)?,
            // AstNode::Cond {..} => (),
            // AstNode::Include(..) => (),
            _ => TyKind::Void,
        };
        Ok(ty_kind)
    }

    fn validate_compound(&mut self, children: &Vec<Box<AstNode>>) -> Result<TyKind> {
        for node in children {
            self.validate_node(node)?;
        }
        Ok(TyKind::Void)
    }

    fn validate_bin_expr(&mut self, _op: &BinOpKind, lhs: &Box<AstNode>, rhs: &Box<AstNode>) -> Result<TyKind> {
        let left_ty = self.validate_node(lhs)?;
        let _right_ty = self.validate_node(rhs)?;
        Ok(left_ty)
    } 

    pub fn validate_stmt(&mut self, kwd_kind: &KwdKind, expr: &Box<AstNode>) -> Result<TyKind> {
        match kwd_kind {
            &KwdKind::Let => self.validate_node(expr)?,
            _ => unimplemented!(),
        };
        Ok(TyKind::Void)
    }

    fn validate_id(&mut self, id: &String, ty_kind: &TyKind, ident_ctx: &IdentCtx) -> Result<TyKind> {
        match self.glob.query(id) {
            Some(ref mut info) =>
                // TODO: Decl & Param technically the same?
                match ident_ctx {
                    IdentCtx::Decl | IdentCtx::Param => 
                        Err(CortexError::ArgError(format!("'{}' was already defined!", id)).into()),
                    IdentCtx::Ref => {
                        info.update_ref_count();
                        Ok(info.ty_kind.clone())
                    },
                },
            None =>
                match ident_ctx {
                    IdentCtx::Decl | IdentCtx::Param => {
                        let info = IdentInfo::new(ty_kind.clone());
                        // already know it hasn't been defined (matching None on symbol table
                        // query) so we don't need to handle the Option returned from the call to
                        // isnert.
                        self.glob.insert(id, info);
                        Ok(ty_kind.clone())
                    }
                    IdentCtx::Ref =>
                        Err(CortexError::ArgError(format!("'{}' is an unknown identifier", id)).into())
                }
        }
    }
}
