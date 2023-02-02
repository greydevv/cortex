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
            TokenKind::Brack(brack_kind, brack_state) if *brack_state == BracketFace::Closed =>
                match &other.kind {
                    TokenKind::Brack(other_brack_kind, _) => *brack_kind == *other_brack_kind,
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
    Brack(BracketKind, BracketFace),
    Op(OpKind),
    EOF,
    Kwd(KwdKind),
    Unknown,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BracketKind {
    Paren,
    Brace,
    Square,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BracketFace {
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
                Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Open), String::from("("), SourceLocation::default()),
                Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Closed), String::from(")"), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{"), SourceLocation::default()),
                Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}"), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Open), String::from("["), SourceLocation::default()),
                Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Closed), String::from("]"), SourceLocation::default()),
            ),
        ];

        for (open_tok, close_tok) in compat_toks {
            assert!(close_tok.closes(&open_tok));
        }

        let incompat_toks = vec![
            (
                Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Open), String::from("("), SourceLocation::default()),
                Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Open), String::from("("), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Open), String::from("{"), SourceLocation::default()),
                Token::new(TokenKind::Brack(BracketKind::Paren, BracketFace::Closed), String::from(")"), SourceLocation::default()),
            ),
            (
                Token::new(TokenKind::Brack(BracketKind::Square, BracketFace::Open), String::from("["), SourceLocation::default()),
                Token::new(TokenKind::Brack(BracketKind::Brace, BracketFace::Closed), String::from("}"), SourceLocation::default()),
            ),
        ];

        for (open_tok, close_tok) in incompat_toks {
            assert!(!close_tok.closes(&open_tok));
        }
    }
}
