use crate::lexer::token::OpKind;

#[derive(Clone, strum_macros::Display)]
pub enum AstNode {
    Num(i32),
    Id(String),
    UnaryExpr {
        op: OpKind,
        child: Box<AstNode>
    },
    BinaryExpr {
        op: OpKind,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>
    }
}

impl AstNode {
    pub fn debug(&self, indents: usize) -> String {
        let ast_string = match self {
            AstNode::Num(n) => format!("({})", n),
            AstNode::Id(s) => format!("({})", s),
            AstNode::BinaryExpr { op, lhs, rhs } =>
                format!(
                    "({})\n{}\n{}",
                    op,
                    lhs.debug(indents+1),
                    rhs.debug(indents+1),
                ),
            _ => unimplemented!()
        };

        format!("{}{}{}", "  ".repeat(indents).to_string(), self, ast_string)
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
