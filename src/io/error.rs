use std::fmt;
use std::error;

use colored::Colorize;

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    SyntaxError(String),
}

impl LexicalError {
    pub fn syntax_err(msg: &str) -> LexicalError {
        LexicalError::SyntaxError(String::from(msg))
    }
}

impl error::Error for LexicalError {}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (err_kind, err_msg) = match self {
            LexicalError::SyntaxError(msg) => ("SyntaxError", msg)
        };
        write!(f, "{}\n  {}", err_kind.red().bold(), err_msg)
    }
}
