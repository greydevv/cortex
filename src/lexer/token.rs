use std::fmt;

use crate::io::file::{ FilePos, FileSpan };

// pub enum Count {
//     Optional,
//     One,
//     Many(u32),
// }

// we can create a grammar from structures like this
// in parser, create a Vec<Token> and ExpectedToken.tok will reference one of those tokens
// each time an AST obj is returned, clear the Vec and move to the next structure
// pub struct ExpectedToken<'a> {
//     tok: &'a Token,
//     count: Count,
// }

pub trait Len {
    fn len(&self) -> usize;
}

/// Safe type conversions by way of [`Option`] when a type conversion may not be possible.
///
/// This method is different from [`TryFrom`] as it returns an [`Option`] instead of a [`Result`].
/// Therefore, this method should be used in cases in which the error information is not important.
/// Instead of using a placeholder when destructuring an error from [`TryFrom`], use `MaybeFrom`
/// to handle [`None`] to be more explicit in these cases.
///
/// ```rust
/// // TryFrom
/// match SomeType::try_from(other_type) {
///     Ok(converted_value) => ...,
///     Err(_) => ...
/// }
///
/// // MaybeFrom
/// match SomeType::maybe_from(&other_type) {
///     Some(converted_value) => ...,
///     None => ...
/// }
/// ```
pub trait MaybeFrom<T>: Sized {
    fn maybe_from(value: &T) -> Option<Self>;
}

pub trait Literal {
    fn literal(&self) -> String;
}

/// A token resulting from source code tokenization.
#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: FileSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: FileSpan) -> Token {
        Token {
            kind,
            span,
        }
    }

    /// Returns a [`Token`] representing end-of-file (EOF).
    pub fn eof(pos: FilePos) -> Token {
        Token::new(
            TokenKind::EOF,
            FileSpan::new(pos, FilePos::new(pos.line, pos.col+1))
        )
    }

    /// Returns a dummy [`Token`] for use in initializers when the default token is unknown.
    pub fn dummy() -> Token {
        Token::new(
            TokenKind::Dummy,
            FileSpan::new(FilePos::new(0, 0), FilePos::new(0, 0)),
        )
    }

    /// Returns `true` if `self.kind` is a closing brace and 'other' is its matching closing brace.
    pub fn closes(&self, other: &Token) -> bool {
        match &self.kind {
            TokenKind::BraceClosed(open_brace_kind) =>
                match &other.kind {
                    TokenKind::BraceOpen(closed_brace_kind) => *open_brace_kind == *closed_brace_kind,
                    _ => false,
                },
            _ => false
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == TokenKind::EOF
    }

    pub fn value(&self) -> String {
        self.kind.literal()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{ {} {} }}", self.kind, self.kind.literal(), self.span)
    }
}

/// Different possible kinds of [`Token`].
#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum TokenKind {
    Num(i32),
    Id(String),
    String(String),
    Arrow,
    Delim(DelimKind),
    BraceOpen(BraceKind),
    BraceClosed(BraceKind),
    BinOp(BinOpKind),
    UnaryOp(UnaryOpKind),
    Ty(TyKind),
    Kwd(KwdKind),
    EOF,
    Unknown(char),
    Dummy,
}

impl Literal for TokenKind {
    fn literal(&self) -> String {
        match self {
            TokenKind::Num(n) => n.to_string(),
            TokenKind::Id(s) => s.clone(),
            TokenKind::String(s) => s.clone(),
            TokenKind::Arrow => String::from("->"),
            TokenKind::Delim(delim_kind) => delim_kind.literal(),
            TokenKind::BraceOpen(brace_kind) =>
                match brace_kind {
                    BraceKind::Paren => String::from("("),
                    BraceKind::Curly => String::from("{"),
                    BraceKind::Square => String::from("["),
                },
            TokenKind::BraceClosed(brace_kind) =>
                match brace_kind {
                    BraceKind::Paren => String::from(")"),
                    BraceKind::Curly => String::from("}"),
                    BraceKind::Square => String::from("]"),
                },
            TokenKind::BinOp(op_kind) => op_kind.literal(),
            TokenKind::UnaryOp(op_kind) => op_kind.literal(),
            TokenKind::Ty(ty_kind) => ty_kind.literal(),
            TokenKind::Kwd(kwd_kind) => kwd_kind.literal(),
            TokenKind::EOF => String::from("\\0"),
            TokenKind::Unknown(c) => c.to_string(),
            TokenKind::Dummy => String::from("dummy"),
        }
    }
}

impl Len for TokenKind {
    fn len(&self) -> usize {
        self.literal().len()
    }
}

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
    pub fn prec(&self) -> i32 {
        match *self {
            BinOpKind::Mul | BinOpKind::Div => 5,
            BinOpKind::Add | BinOpKind::Sub => 4,
            BinOpKind::Gr | BinOpKind::Lt | BinOpKind::GrEql | BinOpKind::LtEql => 3,
            BinOpKind::EqlBool => 2,
            BinOpKind::Eql => 1,
        }
    }

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
    /// Attempts to convert a generic [`TokenKind`] into a [`BinOpKind`].
    ///
    /// It is possible for a unary negation operation to be converted into a binary subtraction
    /// operator. Take the following example.
    /// 
    /// ```
    /// 16 -3
    /// 16 + -3
    /// ```
    /// Above, the two arithmetic equations are equivalent.
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
    // arithmetic operators
    Neg,

    // boolean operators
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

#[derive(PartialEq)]
pub enum OpAssoc {
    Left,
    Right
}

#[derive(PartialEq, Clone, Debug)]
pub enum TyKind {
    Int(usize),
    Void,
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Int(size) => write!(f, "i{}", size),
            TyKind::Void => write!(f, "void"),
        }
    }
}

impl MaybeFrom<String> for TyKind {
    fn maybe_from(value: &String) -> Option<TyKind> {
        match value.as_str() {
            "i32" => Some(TyKind::Int(32)),
            _ => None,
        }
    }
}

impl Literal for TyKind {
    fn literal(&self) -> String {
        match &self {
            TyKind::Int(size) => format!("i{}", size),
            TyKind::Void => String::from("void"),
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closes() {
        let compat_toks = vec![
            (
                Token::new(TokenKind::BraceOpen(BraceKind::Paren), FileSpan::one(FilePos::new(1,1))),
                Token::new(TokenKind::BraceClosed(BraceKind::Paren), FileSpan::one(FilePos::new(1,1))),
            ),
            (
                Token::new(TokenKind::BraceOpen(BraceKind::Curly), FileSpan::one(FilePos::new(1,1))),
                Token::new(TokenKind::BraceClosed(BraceKind::Curly), FileSpan::one(FilePos::new(1,1))),
            ),
            (
                Token::new(TokenKind::BraceOpen(BraceKind::Square), FileSpan::one(FilePos::new(1,1))),
                Token::new(TokenKind::BraceClosed(BraceKind::Square), FileSpan::one(FilePos::new(1,1))),
            ),
        ];

        for (open_tok, close_tok) in compat_toks {
            assert!(close_tok.closes(&open_tok));
        }

        let incompat_toks = vec![
            (
                Token::new(TokenKind::BraceOpen(BraceKind::Paren), FileSpan::one(FilePos::new(1,1))),
                Token::new(TokenKind::BraceOpen(BraceKind::Paren), FileSpan::one(FilePos::new(1,1))),
            ),
            (
                Token::new(TokenKind::BraceOpen(BraceKind::Curly), FileSpan::one(FilePos::new(1,1))),
                Token::new(TokenKind::BraceClosed(BraceKind::Paren), FileSpan::one(FilePos::new(1,1))),
            ),
            (
                Token::new(TokenKind::BraceOpen(BraceKind::Square), FileSpan::one(FilePos::new(1,1))),
                Token::new(TokenKind::BraceClosed(BraceKind::Curly), FileSpan::one(FilePos::new(1,1))),
            ),
        ];

        for (open_tok, close_tok) in incompat_toks {
            assert!(!close_tok.closes(&open_tok));
        }
    }
}
