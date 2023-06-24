use crate::traits::{
    MaybeFrom,
    Literal,
};

/// The various kinds of literals.
#[derive(PartialEq, Clone, Debug, strum_macros::Display)]
pub enum LitKind {
    /// Numeric literals, e.g., `4` and `0.14`.
    Num(i32), // just i32 for now
    /// String literals, e.g., `"Hello, Cortex!"`.
    Str(String),
    /// Boolean literals, i.e., `true` and `false`.
    Bool(bool),
}

impl Literal for LitKind {
    /// # Examples
    /// ```
    /// let lit_kind = LitKind::Num(13);
    /// lit_kind.literal() // "13"
    /// ```
    fn literal(&self) -> String {
        match self {
            LitKind::Num(n) => n.to_string(),
            LitKind::Str(s) => s.clone(),
            LitKind::Bool(v) => v.to_string(),
        }
    }
}

/// The various kinds of delimiters.
#[derive(PartialEq, Clone, Debug)]
pub enum DelimKind {
    Comma,
    Scolon,
    Colon,
    /// A scope resolution separator (e.g., `::` in `Foo::Bar`)
    ScopeSep,
    /// A dot resolution separator (e.g., `.` in `foo.bar`)
    DotSep,
}

impl Literal for DelimKind {
    fn literal(&self) -> String {
        match &self {
            DelimKind::Comma => String::from(","),
            DelimKind::Scolon => String::from(";"),
            DelimKind::Colon => String::from(":"),
            DelimKind::ScopeSep => String::from("::"),
            DelimKind::DotSep => String::from("."),
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
#[derive(PartialEq, Clone, Debug, strum_macros::Display)]
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

#[derive(PartialEq, Clone, Debug, strum_macros::Display)]
pub enum UnaryOpKind {
    /// Arithmetic negation (e.g., `-x`).
    Neg,
    /// Logical not (e.g., `!x`).
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

#[derive(PartialEq, Clone, Debug, strum_macros::Display)]
pub enum KwdKind {
    Func,
    Include,
    For,
    Let,
    Ret,
    If,
    Else, // An else-if variant is not required, parser just peeks for If if on Else
    While,
    Enum,
    Struct,
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
            "enum" => Some(KwdKind::Enum),
            "struct" => Some(KwdKind::Struct),
            _ => None,
        }
    }
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
            KwdKind::Enum => String::from("enum"),
            KwdKind::Struct => String::from("struct"),
        }
    }
}
