use crate::symbols::{ Literal, MaybeFrom };

/// The various kinds of delimiters.
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

/// The various kinds of braces.
#[derive(PartialEq, Debug, Clone)]
pub enum BraceKind {
    Paren,
    Curly,
    Square,
}

/// Describes left or right operator associativity.
#[derive(PartialEq)]
pub enum OpAssoc {
    Left,
    Right
}

/// The various kinds of binary operators.
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
    Else, // An else-if variant is not required, parser just peeks for If if on Else
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
