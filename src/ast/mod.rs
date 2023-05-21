//! The AST objects and validator.

use std::rc::Rc;
use std::fmt;

use crate::symbols::{
    TyKind,
    LitKind,
    BinOpKind,
    UnaryOpKind
};
use crate::io::file::{
    FilePath,
    FileSpan,
    SourceLoc
};

pub mod validate;
mod symbol_table;

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
}

/// The identifier object.
#[derive(PartialEq, Clone)]
pub struct IdentInfo {
    name: String,
    ty_kind: TyKind,
    ctx: IdentCtx,
    loc: SourceLoc,
}

impl IdentInfo {
    /// Creates an identifier.
    pub fn new(name: String, ty_kind: TyKind, ctx: IdentCtx, loc: SourceLoc) -> IdentInfo {
        IdentInfo {
            name,
            ty_kind,
            ctx,
            loc,
        }
    }

    /// Gets the raw identifier.
    // pub fn get_raw(&self) -> &String {
    //     &self.raw
    // }

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

    /// Obtain a reference to the location of the identifier in the source code.
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }

    /// Converts the identifier's context into a string for use in error output.
    pub fn pretty_ctx(&self) -> String {
        match self.ctx {
            IdentCtx::Def => "variable",
            IdentCtx::FuncDef => "function",
            IdentCtx::Param => "parameter",
            // TODO: This is bad :). Need another way of describing context (probably a
            // wrapper enum, one for definitions and another for references).
            _ => unimplemented!(),
        }.to_string()
    }
}

impl fmt::Display for IdentInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(PartialEq, Clone)]
pub enum Ident {
    Unqual(IdentInfo),
    // TODO: Need to capture first_info.span().to(second_info.span()) in errors.
    Qual(IdentInfo, IdentInfo),
}

impl Ident {
    pub fn new_unqual(info: IdentInfo) -> Ident {
        Ident::Unqual(info)
    }

    pub fn new_qual(qual_info: IdentInfo, info: IdentInfo) -> Ident {
        Ident::Qual(qual_info, info)
    }

    fn info(&self) -> &IdentInfo {
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
        self.info_mut().set_ty_kind(ty_kind)
    }

    pub fn loc(&self) -> &SourceLoc {
        &self.info().loc
    }

    pub fn set_ctx(&mut self, ctx: IdentCtx) {
        self.info_mut().set_ctx(ctx)
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

/// The AST function variant.
#[derive(Clone)]
pub struct Func {
    /// Function identifier (name) with type as return type.
    ident: Ident,
    /// Function parameters.
    params: Vec<Ident>,
    /// Function body.
    body: Compound,
}

impl Func {
    /// Creates a function node.
    pub fn new(ident: Ident, params: Vec<Ident>, body: Compound) -> Func {
        Func {
            ident,
            params,
            body,
        }
    }
}

/// The AST expression variant.
#[derive(Clone)]
pub struct Expr {
    /// The kind of expression.
    kind: ExprKind,
    /// The location of the entire expression.
    loc: SourceLoc,
}

impl Expr {
    /// Creates an expression node.
    pub fn new(kind: ExprKind, loc: SourceLoc) -> Expr {
        Expr { kind, loc }
    }

    /// Obtain a reference to the expression's span.
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }
}

/// The various kinds of expression components.
#[derive(Clone, strum_macros::Display)]
pub enum ExprKind {
    /// Binary expressions.
    Binary(BinOpKind, Box<Expr>, Box<Expr>),
    /// Unary expressions.
    Unary(UnaryOpKind, Box<Expr>),
    /// Variable names.
    Id(Ident),
    /// Numeric literals.
    Lit(LitKind),
    /// Invocations (function calls).
    Call(Ident, Vec<Box<Expr>>),
    /// Scope resolution (e.g., `Animal::Dog`)
    ScopeRes(ScopeRes),
}

#[derive(Clone)]
pub struct ScopeRes {
    idents: Vec<Ident>,
}

impl ScopeRes {
    /// Construct a new scope resolution node.
    pub fn new() -> ScopeRes {
        ScopeRes {
            idents: Vec::new(),
        }
    }
}

/// Object representing a translation unit. 
#[derive(Clone)]
pub struct Module {
    /// File path of module.
    name: FilePath,
    /// List of top-level statements.
    stmts: Vec<Stmt>,
}

impl Module {
    /// Creates a new module.
    pub fn new(name: FilePath) -> Module {
        Module {
            name,
            stmts: Vec::new(),
        }
    }

    /// Add a statement to the tree.
    pub fn add_node(&mut self, node: Stmt) {
        self.stmts.push(node)
    }

    /// Obtain a mutable reference to the list of top-level statements.
    pub fn stmts_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.stmts
    }

    /// Obtain the string representation of the module.
    pub fn debug_string(&self) -> String {
        self.debug(Indent::new())
    }
}

/// Object representing statements.
#[derive(Clone)]
pub struct Stmt {
    kind: StmtKind,
    loc: SourceLoc,
}

impl Stmt {
    /// Creates a new statement.
    pub fn new(kind: StmtKind, loc: SourceLoc) -> Stmt {
        Stmt {
            kind,
            loc,
        }
    }

    /// Obtain a reference to the statement's location.
    pub fn loc(&self) -> &SourceLoc {
        &self.loc
    }
}

#[derive(Clone, strum_macros::Display)]
pub enum StmtKind {
    /// Include statement.
    Incl(Module),
    /// Return statement.
    Ret(Option<Expr>),
    /// Let statement.
    Let(Ident, Expr),
    /// While loop statement.
    ///
    /// If the expression is omitted from a while loop, the loop will run forever. This is
    /// semantically the same as `while (true)` or just `loop` in Rust.
    While(Option<Expr>, Compound),
    /// If or else-if statement.
    If(Expr, Compound, Option<Box<Stmt>>),
    /// Else statement.
    Else(Compound),
    /// Collection of statements.
    Compound(Compound),
    /// Function definition.
    Func(Func),
    /// Standalone expression followed by a semicolon.
    Expr(Expr),
    Enum(Enum),
}

#[derive(Clone)]
pub struct Enum {
    ident: Ident,
    members: Vec<Ident>,
}

impl AstDebug for Enum {
    fn debug(&self, indent: Indent) -> String {
        format!("({})\n{}",
            self.ident,
            self.members.iter()
                .enumerate()
                .map(|(i, ident)| -> String {
                    if i == self.members.len() - 1 {
                        format!("{}{}",
                            indent,
                            ident,
                        )
                    } else {
                        format!("{}{}\n",
                            indent,
                            ident,
                        )
                    }
                })
                .collect::<String>()
        )
    }
}

impl Enum {
    pub fn new(ident: Ident) -> Enum {
        Enum {
            ident,
            members: Vec::new(),
        }
    }

    pub fn get_member(&self, member: &Ident) -> Option<&Ident> {
        for m in &self.members {
            if m.name() == member.name() {
                return Some(m);
            }
        }
        None
    }

    pub fn add_member(&mut self, member: Ident) {
        self.members.push(member)
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}

#[derive(Clone)]
pub struct Compound {
    stmts: Vec<Stmt>,
    break_idx: Option<usize>,
    // TODO: Should this be a SourceLoc?
    span: FileSpan,
}

impl Compound {
    /// Creates a new compound object.
    pub fn new() -> Compound {
        Compound {
            stmts: Vec::new(),
            break_idx: None,
            span: FileSpan::dummy(),
        }
    }

    /// Set the span.
    pub fn set_span(&mut self, span: FileSpan) {
        self.span = span;
    }

    /// Obtain the span.
    pub fn span(&self) -> &FileSpan {
        &self.span
    }

    /// Add a statement.
    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }

    /// Set the index at which the breaking statement exists in the list of statements.
    pub fn set_break_idx(&mut self, idx: usize) {
        self.break_idx = Some(idx);
    }

    /// Obtain the index at which the breaking statement exists in the list of statements.
    pub fn get_break_idx(&self) -> Option<usize> {
        self.break_idx
    }

    /// Obtain a reference to the breaking statement, if it exists.
    pub fn get_break_stmt(&self) -> Option<&Stmt> {
        self.break_idx.and_then(|i| self.stmts.get(i))
    }

    /// Obtain an immutable reference to the list of statements.
    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }

    /// Obtain a mutable reference to the list of statements.
    pub fn stmts_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.stmts
    }
}

/// The various kinds of loops.
#[derive(Clone, strum_macros::Display)]
pub enum LoopKind {
    While(Option<Box<Expr>>, Box<Expr>),
}

/// The various kinds of conditional expressions.
#[derive(Clone, strum_macros::Display)]
pub enum CondKind {
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Else(Box<Expr>),
}

/// A helper struct representing an indentation in the string representation of the AST.
#[derive(Clone)]
struct Indent(usize);

impl std::fmt::Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "  ".repeat(self.0))
    }
}

impl Indent {
    /// Creates a new indent object.
    pub fn new() -> Indent {
        Indent(0)
    }

    /// Obtain an indent with an extra level of indentation.
    pub fn plus(&self) -> Indent {
        Indent(self.0 + 1)
    }
}

/// Provides a way to debug the AST by obtaining a string representation.
trait AstDebug {
    fn debug(&self, indent: Indent) -> String;
}

fn debug_vec<T: AstDebug + Sized>(v: &Vec<T>, indent: Indent) -> String {
    if v.is_empty() {
        return String::new();
    }
    format!("\n{}",
        v.iter()
            .enumerate()
            .map(|(i, e)| -> String {
                if i == v.len() - 1 {
                    e.debug(indent.clone())
                } else {
                    format!("{}\n", e.debug(indent.clone()))
                }
            })
            .collect::<String>()
    )
}

impl AstDebug for Module {
    fn debug(&self, indent: Indent) -> String {
        format!("{}Module({}){}",
            indent,
            self.name,
            debug_vec(&self.stmts, indent.clone()),
        )
    }
}

impl AstDebug for Stmt {
    fn debug(&self, indent: Indent) -> String {
        match &self.kind {
            StmtKind::Incl(ref module) => 
                format!("{}{}\n{}",
                    indent,
                    self.kind,
                    module.debug(indent.plus()),
                ),
            StmtKind::Func(ref func) =>
                format!("{}{}{}",
                    indent,
                    self.kind,
                    func.debug(indent.clone())
                ),
            StmtKind::Let(ref ident, ref expr) =>
                format!("{}{}({}: {})\n{}",
                    indent,
                    self.kind,
                    ident,
                    ident.ty_kind(),
                    expr.debug(indent.plus()),
                ),
            StmtKind::Ret(ref expr) =>
                match expr {
                    Some(expr) =>
                        format!("{}{}\n{}",
                            indent,
                            self.kind,
                            expr.debug(indent.plus()),
                        ),
                    None => format!("{}(void)", self.kind),
                },
            StmtKind::Expr(ref expr) => expr.debug(indent),
            StmtKind::Compound(ref c) =>
                format!("{}\n{}",
                    self.kind,
                    c.debug(indent.plus())
                ),
            StmtKind::While(ref expr, ref body) =>
                match expr {
                    Some(expr) =>
                        format!("{}{}\n{}{}",
                            indent,
                            self.kind,
                            expr.debug(indent.plus()),
                            body.debug(indent.plus())
                        ),
                    None =>
                        format!("{}{}(forever){}",
                            indent,
                            self.kind,
                            body.debug(indent.plus())
                        ),
                },
            StmtKind::If(ref expr, ref body, ref other) => {
                let if_string = format!("{}{}\n{}{}",
                    indent,
                    self.kind,
                    expr.debug(indent.plus()),
                    body.debug(indent.plus())
                );
                match other {
                    Some(other) =>
                        format!("{}\n{}",
                            if_string,
                            other.debug(indent.plus())
                        ),
                    None => if_string,
                }
            },
            StmtKind::Else(ref body) =>
                format!("{}{}{}",
                    indent,
                    self.kind,
                    body.debug(indent.plus())
                ),
            StmtKind::Enum(ref enum_def) =>
                format!("{}{}{}",
                    indent,
                    self.kind,
                    enum_def.debug(indent.plus()),
                )
        }
    }
}

impl AstDebug for Expr {
    fn debug(&self, indent: Indent) -> String {
        match &self.kind {
            ExprKind::Binary(bin_op_kind, lhs, rhs) =>
                format!(
                    "{}{}({})\n{}\n{}",
                    indent,
                    self.kind,
                    bin_op_kind,
                    lhs.debug(indent.plus()),
                    rhs.debug(indent.plus())
                ),
            ExprKind::Unary(unary_op_kind, expr) =>
                format!(
                    "{}{}({})\n{}",
                    indent,
                    self.kind,
                    unary_op_kind,
                    expr.debug(indent.plus()),
                ),
            ExprKind::Lit(lit_kind) => {
                let lit_string = match lit_kind {
                    LitKind::Num(n) => n.to_string(),
                    LitKind::Str(str) => str.to_owned(),
                    LitKind::Bool(val) => val.to_string(),
                };
                format!(
                    "{}{}({})",
                    indent,
                    lit_kind,
                    lit_string,
                )
            },
            ExprKind::Id(ident) =>
                format!("{}{}({}, {}, {})",
                    indent,
                    self.kind,
                    ident,
                    ident.ty_kind(),
                    ident.ctx(),
                ),
            ExprKind::Call(ident, args) if args.is_empty() =>
                // No arguments
                format!("{}{}({})",
                    indent,
                    self.kind,
                    ident,
                ),
            ExprKind::Call(ident, args) =>
                format!("{}{}({})\n{}",
                    indent,
                    self.kind,
                    ident,
                    args.iter()
                        .enumerate()
                        .map(|(i, arg)| -> String {
                            if i == args.len() - 1 { 
                                arg.debug(indent.plus())
                            } else { 
                                arg.debug(indent.plus()) + "\n"
                            }
                        })
                        .collect::<String>()
                ),
            ExprKind::ScopeRes(scope_res) =>
                format!("{}{}\n{}",
                    indent,
                    self.kind,
                    scope_res.debug(indent.plus()),
                ),
        }
    }
}

impl AstDebug for ScopeRes {
    fn debug(&self, indent: Indent) -> String {
        self.idents.iter()
            .enumerate()
            .map(|(i, ident)| -> String {
                if i == self.idents.len() - 1 { 
                    format!("{}{}", indent, ident)
                } else { 
                    format!("{}{}::", indent, ident)
                }
            })
            .collect::<String>()
    }
}

impl AstDebug for Compound {
    fn debug(&self, indent: Indent) -> String {
        debug_vec(&self.stmts, indent)
    }
}

impl AstDebug for Func {
    fn debug(&self, indent: Indent) -> String {
        format!(
            "({ident}) -> {ret_ty}{body}",
            ident = self.ident,
            ret_ty = self.ident.ty_kind(),
            body = self.body.debug(indent.plus())
        )
    }
}
