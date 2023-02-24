use std::fmt;
use std::convert::From;

use crate::io::file::{ FilePos, FileSpan };

pub trait Len {
    fn len(&self) -> usize;
}

pub trait MaybeFrom<T>: Sized {
    fn maybe_from(value: T) -> Option<Self>;
}

pub trait Literal {
    fn literal(&self) -> String;
}

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

    pub fn eof(pos: FilePos) -> Token {
        Token::new(
            TokenKind::EOF,
            FileSpan::new(pos, FilePos::new(pos.line, pos.col+1))
        )
    }

    pub fn dummy() -> Token {
        Token::new(
            TokenKind::Dummy,
            FileSpan::new(FilePos::new(0, 0), FilePos::new(0, 0)),
        )
    }

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

#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum TokenKind {
    Num(i32),
    Id(String),
    String(String),
    Arrow,
    Delim(DelimKind),
    BraceOpen(BraceKind),
    BraceClosed(BraceKind),
    Op(OpKind),
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
            TokenKind::Op(op_kind) => op_kind.literal(),
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
pub enum OpKind {
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

impl OpKind {
    pub fn from(tok_kind: &TokenKind) -> Option<OpKind> {
        match tok_kind {
            TokenKind::Op(op_kind) => Some(op_kind.clone()),
            _ => None,
        }
    }

    pub fn prec(&self) -> i32 {
        match *self {
            OpKind::Mul | OpKind::Div => 5,
            OpKind::Add | OpKind::Sub => 4,
            OpKind::Gr | OpKind::Lt | OpKind::GrEql | OpKind::LtEql => 3,
            OpKind::EqlBool => 2,
            OpKind::Eql => 1,
        }
    }

    pub fn assoc(&self) -> OpAssoc {
        match *self {
            OpKind::Eql => OpAssoc::Right,
            OpKind::Add
                | OpKind::Sub
                | OpKind::Mul
                | OpKind::Div
                | OpKind::EqlBool
                | OpKind::Gr
                | OpKind::Lt
                | OpKind::GrEql
                | OpKind::LtEql => OpAssoc::Left,
        }
    }
}

impl Literal for OpKind {
    fn literal(&self) -> String {
        match &self {
            OpKind::Eql => String::from("="),
            OpKind::Add => String::from("+"),
            OpKind::Sub => String::from("-"),
            OpKind::Mul => String::from("*"),
            OpKind::Div => String::from("/"),
            OpKind::EqlBool => String::from("=="),
            OpKind::Gr => String::from(">"),
            OpKind::Lt => String::from("<"),
            OpKind::GrEql => String::from(">="),
            OpKind::LtEql => String::from("<="),

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

impl MaybeFrom<&String> for TyKind {
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

impl MaybeFrom<&String> for KwdKind {
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
