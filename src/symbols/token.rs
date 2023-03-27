//! Structures relevant to tokenization.

use std::convert::From;
use std::fmt;

use crate::io::file::{ FilePos, FileSpan };
use crate::symbols::{ Literal, Len };
use crate::symbols::types::TyKind;
use crate::symbols::kinds::{
    DelimKind,
    BraceKind,
    BinOpKind,
    UnaryOpKind,
    KwdKind,
};

/// A token resulting from source code tokenization.
#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: FileSpan,
}

impl Token {
    /// Creates a new token.
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

    /// Returns `true` if the token represented EOF. Otherwise, `false`.
    pub fn is_eof(&self) -> bool {
        self.kind == TokenKind::EOF
    }

    /// Gets the raw value of the token.
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
    /// Literals (numeric, string, boolean, etc.)
    Lit(LitKind),
    /// Numeric literals
    // Num(i32),
    /// Identifiers
    Id(String),
    /// String literals
    // String(String),
    /// Right-facing arrow (denoting return types)
    Arrow,
    /// Delimiters (punctuation excluding braces)
    Delim(DelimKind),
    /// Opening braces
    BraceOpen(BraceKind),
    /// Closing braces
    BraceClosed(BraceKind),
    /// Binary operators
    BinOp(BinOpKind),
    /// Unary operators
    UnaryOp(UnaryOpKind),
    /// Built-in type literals
    Ty(TyKind),
    /// Built-in keywords
    Kwd(KwdKind),
    /// End-of-file
    EOF,
    Unknown(char),
    Dummy,
}

impl Literal for TokenKind {
    /// # Examples
    /// ```
    /// let tok_kind = TokenKind::Id("foo".into());
    /// tok_kind.literal() // "foo"
    /// ```
    fn literal(&self) -> String {
        match self {
            TokenKind::Lit(lit_kind) => lit_kind.literal(),
            TokenKind::Id(s) => s.clone(),
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
