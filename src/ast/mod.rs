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
}
