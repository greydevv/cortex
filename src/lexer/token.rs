use std::fmt;

use crate::io::file::SourceLocation;

pub struct Token {
    pub kind: TokenKind,
    val: String,
    loc: SourceLocation,
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

#[derive(PartialEq, strum_macros::Display)]
pub enum TokenKind {
    Num,
    Scolon,
    Op(OpKind),
    EOF,
    Unknown,
}

#[derive(PartialEq)]
pub enum OpKind {
    Plus,
}
