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

    pub fn span(&self) -> &FileSpan {
        &self.span
    }

    pub fn pretty_ctx(&self) -> String {
        match self.ctx {
            IdentCtx::Def => "variable",
            IdentCtx::FuncDef => "function",
            IdentCtx::Param => "parameter",
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
    body: Box<Expr>,
}

impl Func {
    /// Creates a function node.
    pub fn new(ident: Ident, params: Vec<Ident>, body: Box<Expr>) -> Func {
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
    kind: ExprKind,
    span: FileSpan,
}

impl Expr {
    /// Creates an expression node.
    pub fn new(kind: ExprKind, span: FileSpan) -> Expr {
        Expr { kind, span }
    }

    pub fn fold(&mut self) -> Option<Expr> {
        match &self.kind {
            // ExprKind::Binary(bin_op_kind, lhs, rhs) => ExprKind::fold_binary(bin_op_kind, lhs, rhs),
            _ => None,
        }
    }

    pub fn span(&self) -> &FileSpan {
        &self.span
    }

    // fn fold_binary(bin_op_kind: &BinOpKind, lhs: &Box<Expr>, rhs: &Box<Expr>) -> Option<Expr> {
    //     match (&lhs.kind, &rhs.kind) {
    //         (ExprKind::Lit(LitKind::Num(a)), ExprKind::Lit(LitKind::Num(b))) =>
    //             Some(Expr::new(ExprKind::Lit(LitKind::Num(a + b)))),
    //         _ => unimplemented!(),
    //     }
    // }
}

/// The various kinds of expression components.
#[derive(Clone, strum_macros::Display)]
pub enum ExprKind {
    /// Binary expressions.
    Binary(BinOpKind, Box<Expr>, Box<Expr>),
    /// Unary expressions.
    Unary(UnaryOpKind, Box<Expr>),
    /// Statements associated with expressions.
    Stmt(StmtKind),
    /// Variable names.
    Id(Ident),
    /// Numeric literals.
    Lit(LitKind),
    /// Invocations (function calls).
    Call(Ident, Vec<Box<Expr>>),
    /// Collection of expressions (block).
    Compound(Vec<Box<AstNode>>, Option<u32>),
    /// Conditionals (if, else if, else).
    Cond(CondKind),
    /// Loops (while, for).
    Loop(LoopKind),
}

#[allow(dead_code)]
struct Module {
    name: String,
    stmts: Vec<Stmt>,
}

impl Module {
    #[allow(dead_code)]
    pub fn new(name: String) -> Module {
        Module {
            name,
            stmts: Vec::new(),
        }
    }
}

#[allow(dead_code)]
enum Stmt {
    /// Include statement.
    Incl(Module),
    /// Return statement.
    Ret(Expr),
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
    /// A collection of statements.
    Compound(Compound),
    /// Function definition.
    Func(Func),
}

#[allow(dead_code)]
struct Compound {
    stmts: Vec<Stmt>,
    break_idx: Option<usize>,
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
    // While(Box<Expr>, Box<Expr>),
}

/// The various kinds of statements.
#[derive(Clone, strum_macros::Display)]
pub enum StmtKind {
    /// Let statements, e.g. `let x = 13;`.
    Let(Ident, Box<Expr>),
    /// Return statements, e.g. `ret x + y;`.
    Ret(Option<Box<Expr>>),
    /// Include statements.
    Incl(String),
    /// While loop.
    While(Box<Expr>, Box<Expr>)
}

impl AstNode {
    /// A helper for the [`AstDebug`] trait to print without supplying the number of indents.
    pub fn debug_string(&self) -> String {
        self.debug(0)
    }

    // pub fn fold(&self) -> Option<AstNode> {
    //     match &self {
    //         AstNode::BinaryExpr { op, lhs, rhs } => {
    //             match (&**rhs, &**lhs) {
    //                 (AstNode::Num(lhs_n), AstNode::Num(rhs_n)) => Some(AstNode::Num(AstNode::fold_const_i32(op.clone(), *lhs_n, *rhs_n))),
    //                 _ => None
    //             }
    //         },
    //         _ => None
    //     }
    // }
    //
    // pub fn fold_const_i32(op_kind: OpKind, a: i32, b: i32) -> i32 {
    //     match op_kind {
    //         OpKind::Add => a + b,
    //         OpKind::Sub => a - b,
    //         OpKind::Mul => a * b,
    //         OpKind::Div => a / b,
    //         _ => panic!("Unhandled OpKind in constant folding of i32: {}", op_kind)
    //     }
    // }
}

/// A trait providing a way to print the AST.
trait AstDebug {
    fn debug(&self, indents: usize) -> String;
}

impl AstDebug for AstNode {
    fn debug(&self, indents: usize) -> String {
        match self {
            AstNode::Func(func) => func.debug(indents),
            AstNode::Expr(expr) => expr.debug(indents),
        }
    }
}

impl AstDebug for Expr {
    fn debug(&self, indents: usize) -> String {
        let out_string = match &self.kind {
            ExprKind::Compound(children, ..) if children.len() == 0 =>
                // block is empty
                String::new(),
            ExprKind::Compound(children, ..) =>
                format!("\n{}",
                    children.iter()
                    // need to check if child is not last
                        .enumerate()
                        .map(|(i, child)| -> String {
                            if i == children.len() - 1 { 
                                child.debug(indents+1)
                            } else { 
                                child.debug(indents+1) + "\n"
                            }
                        })
                        .collect::<String>()
                ),
            ExprKind::Stmt(stmt_kind) => {
                let stmt_string = match stmt_kind {
                    StmtKind::Let(ident, expr) =>
                        format!(
                            "({}: {})\n{}",
                            ident.raw(),
                            ident.ty_kind(),
                            expr.debug(indents+2),
                        ),
                    StmtKind::Ret(expr) =>
                        match expr {
                            Some(expr) => format!("\n{}", expr.debug(indents+2)),
                            None => TyKind::Void.to_string(),
                        },
                    _ => unimplemented!()
                };
                format!("\n{}{}{}", "  ".repeat(indents+1), stmt_kind, stmt_string)
            },
            ExprKind::Binary(bin_op_kind, lhs, rhs) =>
                format!(
                    "({})\n{}\n{}",
                    bin_op_kind,
                    lhs.debug(indents+1),
                    rhs.debug(indents+1)
                ),
            ExprKind::Unary(unary_op_kind, expr) =>
                format!(
                    "({})\n{}",
                    unary_op_kind,
                    expr.debug(indents+1),
                ),
            ExprKind::Lit(lit_kind) => {
                let lit_string = match lit_kind {
                    LitKind::Num(n) => n.to_string(),
                    LitKind::Str(str) => str.to_owned(),
                    LitKind::Bool(val) => val.to_string(),
                };
                format!(
                    "\n{}{}({})",
                    "  ".repeat(indents+1),
                    lit_kind,
                    lit_string,
                )
            },
            ExprKind::Id(ident) =>
                format!("({}, {}, {})",
                    ident.raw(),
                    ident.ty_kind(),
                    ident.ctx(),
                ),
            ExprKind::Call(ident, args) if args.len() == 0 =>
                // no args
                format!("({})", ident.raw()),
            ExprKind::Call(ident, args) =>
                format!("({})\n{}Args\n{}",
                    ident.raw(),
                    "  ".repeat(indents+1),
                    args.iter()
                    // need to check if child is not last
                        .enumerate()
                        .map(|(i, arg)| -> String {
                            if i == args.len() - 1 { 
                                arg.debug(indents+2)
                            } else { 
                                arg.debug(indents+2) + "\n"
                            }
                        })
                        .collect::<String>()
                ),
            ExprKind::Cond(cond_kind) =>
                match cond_kind {
                    CondKind::If(expr, body, ref other) =>
                        match other {
                            Some(other) =>
                                format!(
                                    "({})\n{}\n{}\n{}",
                                    cond_kind,
                                    expr.debug(indents+1),
                                    body.debug(indents+1),
                                    other.debug(indents+1),
                                ),
                            None => 
                                format!(
                                    "({})\n{}\n{}",
                                    cond_kind,
                                    expr.debug(indents+1),
                                    body.debug(indents+1),
                                ),
                        },
                    CondKind::Else(body) =>
                        format!(
                            "({})\n{}",
                            cond_kind,
                            body.debug(indents+1),
                        ),
                }
            ExprKind::Loop(loop_kind) =>
                match loop_kind {
                    LoopKind::While(expr, body) =>
                        match expr {
                            Some(expr) =>
                                format!(
                                    "({})\n{}\n{}",
                                    loop_kind,
                                    expr.debug(indents+1),
                                    body.debug(indents+1),
                                ),
                            None =>
                                format!(
                                    "({} (forever))\n{}",
                                    loop_kind,
                                    body.debug(indents+1),
                                )
                    }
                }
        };
        format!("{}{}{}", "  ".repeat(indents), self.kind, out_string)
    }
}

impl AstDebug for Func {
    fn debug(&self, indents: usize) -> String {
        let out_string = format!(
            "Func({ident}) -> {ret_ty}\n{body}",
            ident = self.ident.raw(),
            ret_ty = self.ident.ty_kind(),
            body = self.body.debug(indents+1)
        );
        format!("{}{}", "  ".repeat(indents), out_string)
    }
}
