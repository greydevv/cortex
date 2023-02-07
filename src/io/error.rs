use std::fmt;
use std::error;
use std::convert::From;

use colored::Colorize;

use crate::io::file::{ FileHandler, SourceLocation };
use crate::lexer::token::{ Token, Literal };

#[derive(Debug, Clone, PartialEq)]
pub enum CortexError {
    SyntaxError(String, SourceLocation, Option<String>),
    FileIOError(String),
}

impl CortexError {
    pub fn syntax_err(msg: &str, loc: SourceLocation) -> CortexError {
        CortexError::SyntaxError(String::from(msg), loc, None)
    }

    pub fn file_io_err(msg: &str) -> CortexError {
        CortexError::FileIOError(String::from(msg))
    }

    pub fn invalid_integer_literal(literal: &String, loc: SourceLocation) -> CortexError {
        CortexError::SyntaxError(
            format!("'{}' is not a valid integer literal", literal),
            loc,
            Some(String::from("Integer literals are expressed as a sequence of digits, e.g., \"23\" or \"921\"."))
        )
    }

    pub fn unclosed_brace(tok: &Token) -> CortexError {
        let msg = format!("unclosed '{}'", tok.kind.literal());
        CortexError::SyntaxError(msg, tok.loc, None)
    }

    pub fn unopened_brace(tok: &Token) -> CortexError {
        let msg = format!("unopened '{}'", tok.kind.literal());
        CortexError::SyntaxError(msg, tok.loc, None)
    }
}

fn underline_err(loc: &SourceLocation, fh: &FileHandler) -> String {
    let mut lines = fh.contents().lines();
    let line_nr = loc.line.to_string();
    let indent = " ".repeat(line_nr.len() + 1);
    format!(
        "{}|\n{} | {}\n{}| {}{}",
        indent,
        line_nr,
        lines.nth(loc.line - 1).unwrap(),
        indent,
        " ".repeat(loc.col - 1),
        "^".repeat(1).red().bold(),
    )
}

impl CortexError {
    pub fn display(&self, fh: &FileHandler) {
        let (err_kind, err_msg, err_loc, err_info) = match self {
            CortexError::SyntaxError(msg, loc, info) => ("SyntaxError", msg, Some(loc), info.clone()),
            CortexError::FileIOError(msg) => ("FileIOError", msg, None, None)
        };
        
        eprintln!("{}: {}", err_kind.red().bold(), err_msg);
        match err_loc {
            Some(loc) => eprintln!("{}", underline_err(&loc, fh)),
            None => ()
        }
        match err_info {
            Some(info) => eprintln!("{}", info),
            None => ()
        }
    }
}

impl error::Error for CortexError {}

impl fmt::Display for CortexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (err_kind, err_msg) = match self {
            CortexError::SyntaxError(msg, _, _) => ("SyntaxError", msg),
            CortexError::FileIOError(msg) => ("FileIOError", msg),
        };

        write!(f, "{}: {}\n", err_kind.red().bold(), err_msg)
    }
}
