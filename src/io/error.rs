use std::fmt;
use std::error;
use std::convert::From;

use colored::Colorize;

use crate::io::file::{ FileHandler, FileSpan };
use crate::lexer::token::{ Token, Len };

pub type Result<T = (), E = CortexError> = std::result::Result<T, E>;

#[derive(PartialEq, Debug, Clone)]
pub enum CortexError {
    ArgError(String),
    SyntaxError(String, FileSpan, Option<String>),
    FileIOError(String),
}

impl CortexError {
    pub fn syntax_err(msg: &str, span: FileSpan) -> CortexError {
        CortexError::SyntaxError(String::from(msg), span, None)
    }

    pub fn file_io_err(msg: &str) -> CortexError {
        CortexError::FileIOError(String::from(msg))
    }

    pub fn invalid_integer_literal(literal: &String, span: FileSpan) -> CortexError {
        CortexError::SyntaxError(
            format!("'{}' is not a valid integer literal", literal),
            span,
            Some(String::from("Integer literals are expressed as a sequence of digits, e.g., \"23\" or \"921\"."))
        )
    }

    pub fn expected_bin_op(literal: &String, span: FileSpan) -> CortexError {
        CortexError::SyntaxError(
            format!("'{}' is not a binary operator", literal),
            span,
            None,
        )
    }

    pub fn unclosed_brace(tok: &Token) -> CortexError {
        let msg = format!("unclosed '{}'", tok.value());
        CortexError::SyntaxError(msg, tok.span, None)
    }

    pub fn unopened_brace(tok: &Token) -> CortexError {
        let msg = format!("unopened '{}'", tok.value());
        CortexError::SyntaxError(msg, tok.span, None)
    }
}

impl CortexError {
    pub fn underline(span: &FileSpan, fh: &FileHandler) -> String {
        let mut lines = fh.contents().lines();
        let line_nr = span.beg.line.to_string();
        let indent = " ".repeat(line_nr.len() + 1);
        format!(
            "{}|\n{} | {}\n{}| {}{}",
            indent,
            line_nr,
            lines.nth(span.beg.line - 1).unwrap_or_else(|| ""),
            indent,
            " ".repeat(span.beg.col - 1),
            "^".repeat(span.len()).red().bold(),
        )
    }

    pub fn display_msg(&self) {
        let err_msg = match self {
            CortexError::ArgError(msg) => msg,
            CortexError::SyntaxError(msg, _, _) => msg,
            CortexError::FileIOError(msg) => msg,
        };
        eprintln!("{}: {}", "error".red().bold(), err_msg);
    }

    pub fn display_with_source(&self, fh: &FileHandler) {
        self.display_msg();
        let (err_span, err_info) = match self {
            CortexError::ArgError(_) => return,
            CortexError::SyntaxError(_, span, info) => (Some(span), info.clone()),
            CortexError::FileIOError(_) => return,
        };
        
        match err_span {
            Some(span) => eprintln!("{}", CortexError::underline(&span, fh)),
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
            CortexError::ArgError(msg) => ("ArgError", msg),
            CortexError::SyntaxError(msg, _, _) => ("SyntaxError", msg),
            CortexError::FileIOError(msg) => ("FileIOError", msg),
        };

        write!(f, "{}: {}\n", err_kind.red().bold(), err_msg)
    }
}
