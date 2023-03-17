use crate::symbols::{ Literal, MaybeFrom };
use crate::symbols::TokenKind;

#[derive(PartialEq, Debug, Clone)]
pub enum DelimKind {
    Period,
    Comma,
    Scolon,
    Colon,
}

impl Literal for DelimKind {
    fn literal(&self) -> String {
        match &self {
            DelimKind::Period => String::from("."),
            DelimKind::Comma => String::from(","),
            DelimKind::Scolon => String::from(";"),
            DelimKind::Colon => String::from(":"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum BraceKind {
    Paren,
    Curly,
    Square,
}

#[derive(PartialEq)]
pub enum OpAssoc {
    Left,
    Right
}

#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum BinOpKind {
    // other operators
    Eql,

    // arithmetic operators
    Add,
    Sub,
    Mul,
    Div,

    // boolean operators
    EqlBool,
    Gr,
    Lt,
    GrEql,
    LtEql,
}

impl BinOpKind {
    /// Returns the precedence of the `BinOpKind`
    pub fn prec(&self) -> i32 {
        match *self {
            BinOpKind::Mul | BinOpKind::Div => 5,
            BinOpKind::Add | BinOpKind::Sub => 4,
            BinOpKind::Gr | BinOpKind::Lt | BinOpKind::GrEql | BinOpKind::LtEql => 3,
            BinOpKind::EqlBool => 2,
            BinOpKind::Eql => 1,
        }
    }

    /// Returns the associativity of the `BinOpKind`
    pub fn assoc(&self) -> OpAssoc {
        match *self {
            BinOpKind::Eql => OpAssoc::Right,
            BinOpKind::Add
                | BinOpKind::Sub
                | BinOpKind::Mul
                | BinOpKind::Div
                | BinOpKind::EqlBool
                | BinOpKind::Gr
                | BinOpKind::Lt
                | BinOpKind::GrEql
                | BinOpKind::LtEql => OpAssoc::Left,
        }
    }
}

impl Literal for BinOpKind {
    fn literal(&self) -> String {
        match &self {
            BinOpKind::Eql => String::from("="),
            BinOpKind::Add => String::from("+"),
            BinOpKind::Sub => String::from("-"),
            BinOpKind::Mul => String::from("*"),
            BinOpKind::Div => String::from("/"),
            BinOpKind::EqlBool => String::from("=="),
            BinOpKind::Gr => String::from(">"),
            BinOpKind::Lt => String::from("<"),
            BinOpKind::GrEql => String::from(">="),
            BinOpKind::LtEql => String::from("<="),

        }
    }
}

impl MaybeFrom<TokenKind> for BinOpKind {
    /// Attempts to convert a generic [`TokenKind`] into a `BinOpKind`.
    ///
    /// It is possible for a unary negation operation to be converted into a binary subtraction
    /// operator. Take the following example.
    /// 
    /// ```
    /// 16 -3
    /// 16 + -3
    /// ```
    /// Above, the two equations are mathematically equivalent. But they are not syntactically
    /// equivalent. The first equation yields three tokens whereas the second equation yields four
    /// tokens.
    ///
    /// Furthermore, the first equation's '-' will be seen as a unary negation since it is not
    /// followed by whitespace. However, it should be converted into a binary subtraction. This is
    /// where `BinOpKind::maybe_from(possible_unary_op)` can be used.
    fn maybe_from(tok_kind: &TokenKind) -> Option<BinOpKind> {
        match tok_kind {
            TokenKind::BinOp(op_kind) => Some(op_kind.clone()),
            TokenKind::UnaryOp(UnaryOpKind::Neg) => Some(BinOpKind::Sub),
            _ => None,
        }
    }
}


#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum UnaryOpKind {
    /// arithmetic negation (e.g., negated `x` is `-x`)
    Neg,
    /// logical not
    Not,
}

impl Literal for UnaryOpKind {
    fn literal(&self) -> String {
        match &self {
            UnaryOpKind::Neg => String::from("-"),
            UnaryOpKind::Not => String::from("!"),
        }
    }
}


#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum KwdKind {
    Func,
    Include,
    For,
    Let,
    Ret,
    If,
    Else, // don't need else if, parser just peeks for If if on Else
    While,
}

impl Literal for KwdKind {
    fn literal(&self) -> String {
        match &self {
            KwdKind::For => String::from("for"),
            KwdKind::Func => String::from("func"),
            KwdKind::Include => String::from("include"),
            KwdKind::Let => String::from("let"),
            KwdKind::Ret => String::from("ret"),
            KwdKind::If => String::from("if"),
            KwdKind::Else => String::from("else"),
            KwdKind::While => String::from("while"),
        }
    }
}

impl MaybeFrom<String> for KwdKind {
    fn maybe_from(value: &String) -> Option<KwdKind> {
        match value.as_str() {
            "func" => Some(KwdKind::Func),
            "include" => Some(KwdKind::Include),
            "for" => Some(KwdKind::For),
            "let" => Some(KwdKind::Let),
            "ret" => Some(KwdKind::Ret),
            "if" => Some(KwdKind::If),
            "else" => Some(KwdKind::Else),
            "while" => Some(KwdKind::While),
            _ => None,
        }
    }
}
