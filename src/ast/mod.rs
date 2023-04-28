//! The AST objects and validator.

use crate::symbols::{
    TyKind,
    LitKind,
    BinOpKind,
    UnaryOpKind
};
use crate::io::file::FileSpan;

pub mod validate;

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
}

/// The identifier object.
#[derive(Clone)]
pub struct Ident {
    raw: String,
    ty_kind: TyKind,
    ctx: IdentCtx,
    span: FileSpan,
}

impl Ident {
    /// Creates an identifier.
    pub fn new(ident: &String, ty_kind: TyKind, ctx: IdentCtx, span: FileSpan) -> Ident {
        Ident {
            raw: ident.clone(),
            ty_kind,
            ctx,
            span,
        }
    }

    /// Gets the raw identifier.
    pub fn raw(&self) -> &String {
        &self.raw
    }

    /// Gets the identifier's type.
    pub fn ty_kind(&self) -> &TyKind {
        &self.ty_kind
    }

    /// Sets the identifier's type.
    pub fn update_ty(&mut self, ty_kind: TyKind) {
        self.ty_kind = ty_kind;
    }

    /// Gets the identifier's context.
    pub fn ctx(&self) -> &IdentCtx {
        &self.ctx
    }

    /// Obtain a reference to the identifier's span.
    pub fn span(&self) -> &FileSpan {
        &self.span
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

/// The AST node object.
#[derive(Clone, strum_macros::Display)]
pub enum AstNode {
    Func(Func),
    Expr(Expr),
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
    /// The span of the entire expression.
    span: FileSpan,
}

impl Expr {
    /// Creates an expression node.
    pub fn new(kind: ExprKind, span: FileSpan) -> Expr {
        Expr { kind, span }
    }

    /// Obtain a reference to the expression's span.
    pub fn span(&self) -> &FileSpan {
        &self.span
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
}

/// Object representing a translation unit. 
pub struct Module {
    /// File path of module.
    name: String,
    /// List of top-level statements.
    stmts: Vec<Stmt>,
}

impl Module {
    /// Creates a new module.
    pub fn new(name: String) -> Module {
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
    span: FileSpan,
}

impl Stmt {
    /// Creates a new statement.
    pub fn new(kind: StmtKind, span: FileSpan) -> Stmt {
        Stmt {
            kind,
            span,
        }
    }

    /// Obtain a reference to the statement's span.
    pub fn span(&self) -> &FileSpan {
        &self.span
    }
}

#[derive(Clone, strum_macros::Display)]
pub enum StmtKind {
    /// Include statement.
    Incl(String),
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
}

// TODO: Do I need to add a span to compound for underlining entire blocks of code?
#[derive(Clone)]
pub struct Compound {
    stmts: Vec<Stmt>,
    break_idx: Option<usize>,
}

impl Compound {
    /// Creates a new compound object.
    pub fn new() -> Compound {
        Compound {
            stmts: Vec::new(),
            break_idx: None,
        }
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

impl AstDebug for AstNode {
    fn debug(&self, indent: Indent) -> String {
        match self {
            AstNode::Func(func) => func.debug(indent),
            AstNode::Expr(expr) => expr.debug(indent),
        }
    }
}

impl AstDebug for Module {
    fn debug(&self, indent: Indent) -> String {
        format!("{}Module({})\n{}",
            indent,
            self.name,
            self.stmts.iter()
                .enumerate()
                .map(|(i, child)| -> String {
                    if i == self.stmts.len() - 1 { 
                        child.debug(indent.plus())
                    } else { 
                        format!("{}\n", child.debug(indent.plus()))
                    }
                })
                .collect::<String>()
        )
    }
}

impl AstDebug for Stmt {
    fn debug(&self, indent: Indent) -> String {
        match &self.kind {
            StmtKind::Incl(ref file_path) => format!("{}({})", self.kind, file_path),
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
                    ident.raw(),
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
            StmtKind::Compound(ref compound) =>
                format!("{}\n{}",
                    self.kind,
                    compound.debug(indent.plus())
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
                        format!("{}(forever){}",
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
                    ident.raw(),
                    ident.ty_kind(),
                    ident.ctx(),
                ),
            ExprKind::Call(ident, args) if args.len() == 0 =>
                // no args
                format!("{}{}({})",
                    indent,
                    self.kind,
                    ident.raw()
                ),
            ExprKind::Call(ident, args) =>
                format!("{}{}({})\n{}",
                    indent,
                    self.kind,
                    ident.raw(),
                    args.iter()
                    // need to check if child is not last
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
        }
    }
}

impl AstDebug for Compound {
    fn debug(&self, indent: Indent) -> String {
        format!("\n{}",
            self.stmts.iter()
            // need to check if child is not last
                .enumerate()
                .map(|(i, child)| -> String {
                    if i == self.stmts.len() - 1 { 
                        child.debug(indent.clone())
                    } else { 
                        format!("{}\n", child.debug(indent.clone()))
                    }
                })
                .collect::<String>()
        )
    }
}

impl AstDebug for Func {
    fn debug(&self, indent: Indent) -> String {
        format!(
            "({ident}) -> {ret_ty}{body}",
            ident = self.ident.raw(),
            ret_ty = self.ident.ty_kind(),
            body = self.body.debug(indent.plus())
        )
    }
}
