use std::fmt;

use crate::io::file::SourceLocation;

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}

impl Token {
    pub fn new(kind: TokenKind, loc: SourceLocation) -> Token {
        Token {
            kind,
            loc,
        }
    }

    pub fn eof(loc: SourceLocation) -> Token {
        Token {
            kind: TokenKind::EOF,
            loc,
        }
    }

    pub fn closes(&self, other: &Token) -> bool {
        match &self.kind {
            TokenKind::Brace(brace_kind, brace_face) if *brace_face == BraceFace::Closed =>
                match &other.kind {
                    TokenKind::Brace(other_brace_kind, _) => *brace_kind == *other_brace_kind,
                    _ => false,
                },
            _ => false
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{ {} {} }}", self.kind, self.kind.literal(), self.loc)
    }
}

#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum TokenKind {
    Num(i32),
    Id(String),
    String(String),
    Arrow,
    Delim(DelimKind),
    Brace(BraceKind, BraceFace),
    Op(OpKind),
    Kwd(KwdKind),
    EOF,
    Unknown(char),
}

#[derive(PartialEq, Debug, Clone)]
pub enum DelimKind {
    Period,
    Comma,
    Scolon,
    Colon,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BraceKind {
    Paren,
    Curly,
    Square,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BraceFace {
    Open,
    Closed,
}

impl BraceFace {
    pub fn is_closed(&self) -> bool {
        *self == BraceFace::Closed
    } 

    pub fn is_open(&self) -> bool {
        *self == BraceFace::Open
    }
}

#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(PartialEq, Debug, Clone)]
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

pub trait Literal {
    fn literal(&self) -> String;

    fn len(&self) -> usize {
        self.literal().len()
    }
}

impl Literal for TokenKind {
    fn literal(&self) -> String {
        match &self {
            TokenKind::Num(n) => n.to_string(),
            TokenKind::Id(s) => s.clone(),
            TokenKind::String(s) => s.clone(),
            TokenKind::Arrow => String::from("->"),
            TokenKind::Delim(delim_kind) => delim_kind.literal(),
            TokenKind::Brace(brace_kind, brace_face) => 
                match brace_kind {
                    BraceKind::Paren =>
                        match brace_face {
                            BraceFace::Open => String::from("("),
                            BraceFace::Closed => String::from(")"),
                        },
                    BraceKind::Curly =>
                        match brace_face {
                            BraceFace::Open => String::from("{"),
                            BraceFace::Closed => String::from("}"),
                        },
                    BraceKind::Square =>
                        match brace_face {
                            BraceFace::Open => String::from("["),
                            BraceFace::Closed => String::from("]"),
                        },
                },
            TokenKind::Op(op_kind) => op_kind.literal(),
            TokenKind::Kwd(kwd_kind) => kwd_kind.literal(),
            TokenKind::EOF => String::from("\\0"),
            TokenKind::Unknown(c) => c.to_string(),
        }
    }
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

impl Literal for OpKind {
    fn literal(&self) -> String {
        match &self {
            OpKind::Add => String::from("+"),
            OpKind::Sub => String::from("-"),
            OpKind::Mul => String::from("*"),
            OpKind::Div => String::from("/"),
        }
    }
}

impl Literal for KwdKind {
    fn literal(&self) -> String {
        match &self {
            KwdKind::For => String::from("for"),
            KwdKind::Func => String::from("func"),
            KwdKind::Include => String::from("include"),
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
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Open), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Closed), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Curly, BraceFace::Open), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Curly, BraceFace::Closed), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Square, BraceFace::Open), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Square, BraceFace::Closed), SourceLocation::default()),
            ),
        ];

        for (open_tok, close_tok) in compat_toks {
            assert!(close_tok.closes(&open_tok));
        }

        let incompat_toks = vec![
            (
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Open), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Open), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Curly, BraceFace::Open), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Closed), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Square, BraceFace::Open), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Curly, BraceFace::Closed), SourceLocation::default()),
            ),
        ];

        for (open_tok, close_tok) in incompat_toks {
            assert!(!close_tok.closes(&open_tok));
        }
    }
}
