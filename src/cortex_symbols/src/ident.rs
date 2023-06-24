use std::fmt;
use std::rc::Rc;

use crate::{
    types::TyKind,
    span::FileSpan,
    file::{
        SourceLoc,
        FilePath,
    },
};

/// The context of an identifier.
#[derive(PartialEq, Clone, Copy, strum_macros::Display)]
pub enum IdentCtx {
    /// Variable definition.
    Def,
    /// Variable reference.
    Ref,
    /// Function parameter.
    Param,
    /// Invocation (function call).
    FuncCall,
    /// Function definition.
    FuncDef,
    /// Enum definition.
    EnumDef,
    /// Struct definition.
    StructDef,
}

impl IdentCtx {
    pub fn is_typedef(&self) -> bool {
        match self {
            IdentCtx::EnumDef => true,
            _ => false,
        }
    }
}

/// The identifier object.
#[derive(Clone)]
pub struct IdentInfo {
    name: String,
    ty_kind: TyKind,
    ctx: IdentCtx,
    // If this is `Some(..)`, then it should be treated as a method call.
    args: Option<Vec<i32>>,
    loc: SourceLoc,
}

impl IdentInfo {
    /// Creates an identifier.
    pub fn new(name: String, ty_kind: TyKind, ctx: IdentCtx, loc: SourceLoc) -> IdentInfo {
        IdentInfo {
            name,
            ty_kind,
            ctx,
            args: None,
            loc,
        }
    }

    pub fn new_call(name: String, ret_ty: TyKind, args: Vec<i32>, loc: SourceLoc) -> IdentInfo {
        IdentInfo {
            name,
            ty_kind: ret_ty,
            ctx: IdentCtx::FuncCall,
            args: Some(args),
            loc,
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    /// Gets the identifier's type.
    pub fn ty_kind(&self) -> &TyKind {
        &self.ty_kind
    }

    /// Sets the identifier's type.
    pub fn update_ty(&mut self, ty_kind: TyKind) {
        self.ty_kind = ty_kind;
    }

    pub fn set_ctx(&mut self, ctx: IdentCtx) {
        self.ctx = ctx;
    }

    pub fn set_ty_kind(&mut self, ty_kind: TyKind) {
        self.ty_kind = ty_kind;
    }

    /// Gets the identifier's context.
    pub fn ctx(&self) -> &IdentCtx {
        &self.ctx
    }

    /// Obtain a reference to the identifier's span.
    pub fn span(&self) -> &FileSpan {
        self.loc.span()
    }

    /// Obtain a reference to the file the identifier was defined in.
    pub fn file_path(&self) -> &Rc<FilePath> {
        self.loc.file_path()
    }

    pub fn set_args(&mut self, args: Vec<i32>) {
        self.args = Some(args);
    }

    pub fn args_mut(&mut self) -> &mut Option<Vec<i32>> {
        &mut self.args
    }

    pub fn args(&self) -> &Option<Vec<i32>> {
        &self.args
    }

    /// Obtain a reference to the location of the identifier in the source code.
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }

    /// Converts the identifier's context into a string for use in error output.
    pub fn pretty_ctx(&self) -> String {
        match self.ctx {
            IdentCtx::Def | IdentCtx::Ref => "variable",
            IdentCtx::Param => "parameter",
            IdentCtx::FuncCall => "function call",
            IdentCtx::FuncDef => "function",
            IdentCtx::EnumDef => "enum",
            IdentCtx::StructDef => "struct",
        }.to_string()
    }
}

impl fmt::Display for IdentInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone)]
pub enum Ident {
    Unqual(IdentInfo),
    Qual(IdentInfo, IdentInfo),
}

impl Ident {
    pub fn span(&self) -> FileSpan {
        match self {
            Ident::Unqual(ref info) => info.span().clone(),
            Ident::Qual(ref qual_info, ref info) => qual_info.span().to(info.span()),
        }
    }

    pub fn new_unqual(info: IdentInfo) -> Ident {
        Ident::Unqual(info)
    }

    pub fn new_qual(qual_info: IdentInfo, info: IdentInfo) -> Ident {
        Ident::Qual(qual_info, info)
    }

    pub fn info(&self) -> &IdentInfo {
        match self {
            Ident::Unqual(ref info) => &info,
            Ident::Qual(_, ref info) => &info,
        }
    }

    fn info_mut(&mut self) -> &mut IdentInfo {
        match self {
            Ident::Unqual(ref mut info) => info,
            Ident::Qual(_, ref mut info) => info,
        }
    }

    pub fn name(&self) -> &String {
        &self.info().name
    }

    pub fn ctx(&self) -> &IdentCtx {
        &self.info().ctx
    }

    pub fn ty_kind(&self) -> &TyKind {
        &self.info().ty_kind
    }

    pub fn set_ty_kind(&mut self, ty_kind: TyKind) {
        self.info_mut().set_ty_kind(ty_kind);
    }

    pub fn loc(&self) -> &SourceLoc {
        &self.info().loc
    }

    pub fn set_ctx(&mut self, ctx: IdentCtx) {
        self.info_mut().set_ctx(ctx);
    }

    pub fn args_mut(&mut self) -> &mut Option<Vec<i32>> {
        self.info_mut().args_mut()
    }

    pub fn args(&self) -> &Option<Vec<i32>> {
        self.info().args()
    }

    pub fn set_args(&mut self, args: Vec<i32>) {
        self.info_mut().set_args(args);
    }

    pub fn pretty_ctx(&self) -> String {
        self.info().pretty_ctx()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ident::Unqual(ref info) => write!(f, "{}", info.name),
            Ident::Qual(ref qual_info, ref info) => write!(f, "{}::{}", qual_info.name, info.name),
        }
    }
}
