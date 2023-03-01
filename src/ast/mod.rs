use crate::lexer::token::{ TyKind, KwdKind, BinOpKind, UnaryOpKind };

#[derive(Clone, strum_macros::Display)]
pub enum AstConditionalKind {
    SoloIf {
        expr: Box<AstNode>
    },
    // used for both if and else if
    If {
        expr: Box<AstNode>,
        other: Box<AstNode>,
    },
    Else
}

#[derive(Clone, strum_macros::Display)]
pub enum AstNode {
    Num(i32),
    Id(String),
    BinExpr {
        op: BinOpKind,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    UnaryExpr {
        op: UnaryOpKind,
        rhs: Box<AstNode>,
    },
    Stmt {
        kind: KwdKind,
        expr: Box<AstNode>,
    },
    Func {
        id: String,
        ret_ty: TyKind,
        params: Vec<Box<AstNode>>,
        body: Box<AstNode>,
    },
    Compound(Vec<Box<AstNode>>),
    Cond {
        kind: AstConditionalKind,
        body: Box<AstNode>,
    },
    Include(String),
}

impl AstNode {
    pub fn debug_format(&self) -> String {
        self.debug(0)
    }

    fn debug(&self, indents: usize) -> String {
        let ast_string = match self {
            AstNode::Num(n) => format!("({})", n),
            AstNode::Id(s) => format!("({})", s),
            AstNode::BinExpr { op, lhs, rhs } =>
                format!(
                    "({})\n{}\n{}",
                    op,
                    lhs.debug(indents+1),
                    rhs.debug(indents+1),
                ),
            AstNode::UnaryExpr { op, rhs } =>
                format!(
                    "({})\n{}",
                    op,
                    rhs.debug(indents+1),
                ),
            AstNode::Stmt { kind, expr } => format!("({})\n{}", kind, expr.debug(indents+1)),
            AstNode::Func { id, ret_ty, params, body } if params.is_empty() => format!("({}) -> {}\n{}", id, ret_ty, body.debug(indents+1)),
            AstNode::Func { id, ret_ty, params, body } => {
                let mut params_str = String::new();
                for (i, param) in params.iter().enumerate() {
                    let param_str = if i == params.len()-1 {
                        format!("{}", param.debug(0))
                    } else {
                        format!("{}, ", param.debug(0))
                    };
                    params_str = params_str + &param_str;
                }
                format!("({}: {}) -> {}\n{}", id, params_str, ret_ty, body.debug(indents+1))
            },
            AstNode::Compound(children) => {
                let mut children_str = String::new();
                for (i, child) in children.iter().enumerate() {
                    let child_str = if i == 0 {
                        format!("{}", child.debug(indents))
                    } else {
                        format!("\n{}", child.debug(indents))
                    };
                    children_str = children_str + &child_str;
                }
                // return early because we only want to include contents of compound and not any
                // additional information
                return format!("{}", children_str);
            },
            AstNode::Cond { kind, body } =>
                match kind {
                    AstConditionalKind::SoloIf { expr } => format!("({})\n{}\n{}", kind, expr.debug(indents+1), body.debug(indents+1)),
                    AstConditionalKind::If { expr, other } => format!("({})\n{}\n{}\n{}", kind, expr.debug(indents+1), body.debug(indents+1), other.debug(indents+1)),
                    AstConditionalKind::Else => format!("({})\n{}", kind, body.debug(indents+1)),
                },
            AstNode::Include(incl_path) => format!("({})", incl_path),
        };

        format!("{}{}{}", "  ".repeat(indents), self, ast_string)
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
