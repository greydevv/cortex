use std::fmt;

use crate::io::file::SourceLocation;

pub struct Token {
    pub kind: TokenKind,
    pub val: String,
    pub loc: SourceLocation,
}

impl Token {
    pub fn new(kind: TokenKind, val: String, loc: SourceLocation) -> Token {
        Token {
            kind,
            val,
            loc,
        }
    }

    pub fn eof(loc: SourceLocation) -> Token {
        Token {
            kind: TokenKind::EOF,
            val: String::from("\0"),
            loc,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.kind == TokenKind::EOF {
            write!(f, "{} {{ {} }}", self.kind, self.loc)
        } else {
            write!(f, "{} {{ {} {} }}", self.kind, self.val, self.loc)
        }
    }
}

#[derive(PartialEq, Debug, strum_macros::Display)]
pub enum TokenKind {
    Num,
    Id,
    String,
    Scolon,
    Oparen,
    Cparen,
    Op(OpKind),
    EOF,
    Kwd(KwdKind),
    Unknown,
}

#[derive(PartialEq, Debug)]
pub enum OpKind {
    Plus,
}

#[derive(PartialEq, Debug)]
pub enum KwdKind {
    Func,
    Include,
    For,
}

impl KwdKind {
    pub fn from_string(value: &String) -> Option<KwdKind> {
        match value.as_str() {
            "func" => Some(KwdKind::Func),
            "include" => Some(KwdKind::Include),
            "for" => Some(KwdKind::For),
            _ => None,
        }
    }
}
