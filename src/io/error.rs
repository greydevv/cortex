use std::fmt;
use std::error;
use std::convert::From;

use colored::Colorize;

use crate::lexer::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum CortexError {
    SyntaxError(String),
    FileIOError(String),
}

impl CortexError {
    pub fn syntax_err(msg: &str) -> CortexError {
        CortexError::SyntaxError(String::from(msg))
    }

    pub fn file_io_err(msg: &str) -> CortexError {
        CortexError::FileIOError(String::from(msg))
    }

    pub fn unclosed_brace(tok: &Token) -> CortexError {
        let msg = format!("unclosed '{}'", tok.val);
        CortexError::SyntaxError(msg)
    }

    pub fn unopened_brace(tok: &Token) -> CortexError {
        let msg = format!("unopened '{}'", tok.val);
        CortexError::SyntaxError(msg)
    }
}

impl error::Error for CortexError {}

impl fmt::Display for CortexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (err_kind, err_msg) = match self {
            CortexError::SyntaxError(msg) => ("SyntaxError", msg),
            CortexError::FileIOError(msg) => ("FileIOError", msg),
        };
        write!(f, "{}\n  {}", err_kind.red().bold(), err_msg)
    }
}
