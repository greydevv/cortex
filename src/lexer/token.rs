use std::fmt;

use crate::io::file::SourceLocation;

#[derive(Clone)]
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
        if self.kind == TokenKind::EOF {
            write!(f, "{} {{ {} }}", self.kind, self.loc)
        } else {
            write!(f, "{} {{ {} {} }}", self.kind, self.val, self.loc)
        }
    }
}

#[derive(PartialEq, Debug, Clone, strum_macros::Display)]
pub enum TokenKind {
    Num,
    Id,
    String,
    Scolon,
    Brace(BraceKind, BraceFace),
    Op(OpKind),
    EOF,
    Kwd(KwdKind),
    Unknown,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BraceKind {
    Paren,
    Brace,
    Square,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BraceFace {
    Open,
    Closed,
}

#[derive(PartialEq, Debug, Clone)]
pub enum OpKind {
    Plus,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closes() {
        let compat_toks = vec![
            (
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Open), String::from("("), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Closed), String::from(")"), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Brace, BraceFace::Open), String::from("{"), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Brace, BraceFace::Closed), String::from("}"), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Square, BraceFace::Open), String::from("["), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Square, BraceFace::Closed), String::from("]"), SourceLocation::default()),
            ),
        ];

        for (open_tok, close_tok) in compat_toks {
            assert!(close_tok.closes(&open_tok));
        }

        let incompat_toks = vec![
            (
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Open), String::from("("), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Open), String::from("("), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Brace, BraceFace::Open), String::from("{"), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Paren, BraceFace::Closed), String::from(")"), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brace(BraceKind::Square, BraceFace::Open), String::from("["), SourceLocation::default()),
                Token::new(TokenKind::Brace(BraceKind::Brace, BraceFace::Closed), String::from("}"), SourceLocation::default()),
            ),
        ];

        for (open_tok, close_tok) in incompat_toks {
            assert!(!close_tok.closes(&open_tok));
        }
    }
}
