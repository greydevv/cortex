//! The main interface for generating and errors.

use std::fmt;
use std::error::Error;
use std::convert::From;

use colored::Colorize;
use clap::error::ContextKind;
use clap::error::Error as ClapError;

use crate::ast::{ Ident, IdentCtx };
use crate::io::file::{ FileHandler, FileSpan };
use crate::symbols::{ Token, Len };
use crate::sess::SessCtx;

/// Result alias used throughout compilation stages.
pub type Result<T = (), E = Box<CortexError>> = std::result::Result<T, E>;

/// The main error object.
#[derive(PartialEq, Debug)]
pub enum CortexError {
    ArgError(String),
    SyntaxError {
        file_path: String,
        msg: String,
        span: FileSpan,
        help: Option<String>,
    },
    FileIOError(String),
    TypeError(String),
}

/// Converts an error raised from [`clap`](https://docs.rs/clap/latest/clap/) to a standard Cortex
/// error.
impl From<ClapError> for CortexError {
    fn from(value: ClapError) -> CortexError {
        let (kind, val) = value.context().nth(0).unwrap();
        match kind {
            ContextKind::InvalidArg => {
                CortexError::ArgError(format!("invalid argument '{}'", val))
            }
            _ => unimplemented!()
        }
    }
}

impl CortexError {
    /// Creates a syntax error.
    pub fn syntax_err(ctx: &SessCtx, msg: &str, span: FileSpan, help: Option<String>) -> CortexError {
        CortexError::SyntaxError{
            file_path: ctx.file_path(),
            msg: String::from(msg),
            span,
            help,
        }
    }

    /// Creates a file I/O error.
    pub fn file_io_err(msg: &str) -> CortexError {
        CortexError::FileIOError(String::from(msg))
    }

    /// Creates an error from an invalid integer literal.
    pub fn invalid_integer_literal(ctx: &SessCtx, lit: &String, span: FileSpan) -> CortexError {
        CortexError::SyntaxError { 
            file_path: ctx.file_path(),
            msg: format!("'{}' is not a valid integer literal", lit),
            span,
            help: Some(String::from("Integer literals are expressed as a sequence of digits, e.g., \"23\" or \"921\"."))
        }
    }

    /// Creates an error when a binary operator was expected but one was not received.
    pub fn expected_bin_op(ctx: &SessCtx, lit: &String, span: FileSpan) -> CortexError {
        CortexError::SyntaxError { 
            file_path: ctx.file_path(),
            msg: format!("'{}' is not a binary operator", lit),
            span,
            help: None,
        }
    }

    /// Creates an error describing an unclosed brace.
    pub fn unclosed_brace(ctx: &SessCtx, tok: &Token) -> CortexError {
        CortexError::SyntaxError {
            file_path: ctx.file_path(),
            msg: format!("unclosed '{}'", tok.value()),
            span: tok.span,
            help: None
        }
    }

    /// Creates an error describing an unopened brace.
    pub fn unopened_brace(ctx: &SessCtx, tok: &Token) -> CortexError {
        CortexError::SyntaxError{
            file_path: ctx.file_path(),
            msg: format!("unopened '{}'", tok.value()),
            span: tok.span,
            help: None
        }
    }

    /// Creates an error describing an illegal reference or definition of an identifier.
    pub fn illegal_ident(ctx: &SessCtx, ident: &Ident) -> CortexError {
        let msg = match ident.ctx() {
            IdentCtx::Def
                | IdentCtx::Param
                | IdentCtx::FuncDef =>
                    format!(
                        "'{}' was already defined",
                        ident.raw(),
                    ),
            IdentCtx::Ref =>
                    format!(
                        "variable '{}' does not exist",
                        ident.raw(),
                    ),
            IdentCtx::FuncCall =>
                    format!(
                        "function '{}' does not exist",
                        ident.raw(),
                    ),
        };
        CortexError::SyntaxError {
            file_path: ctx.file_path(),
            msg,
            span: *ident.span(),
            // TODO: Accept conflicting ident (if any) as a parameter and put it in the help. For
            // example, if the variable 'sum' cannot be defined because it's a function defined
            // elsewhere, the help should note the sum was previously defined as a function and
            // underline it.
            help: None,
        }
    }
}

impl CortexError {
    /// Generates a `String` with related info regarding the offending token(s) causing the error.
    ///
    /// The following snippet shows an example output generated from an error due to a reference to
    /// an unknown variable.
    ///
    /// ```text
    ///   at [5:1] in path/to/some_file.cx
    ///   |
    /// 5 | unknown_var = 10;
    ///   | ^^^^^^^^^^^
    /// ```
    ///
    /// The offending token is `unknown_var` and is underlined using multiple instances of the `'^'`
    /// character. Pertinent information is included above the offending source code, such as the
    /// filename (in this case, `path/to/some_file.cx`) and the location at which the error
    /// happend, formatted as `[line:col]` (in this case, `[5:1]`).
    ///
    /// Note that the error message itself is not included in this output as some errors are not
    /// associated with a piece of source code, e.g., an error raised for invalid compilation
    /// arguments via the CLI.
    ///
    pub fn underline(span: &FileSpan, file_path: &String) -> Result<String> {
        let fh = FileHandler::new(file_path.clone())?;
        let mut lines = fh.contents().lines();
        let line_nr = span.beg.line.to_string();
        let indent = " ".repeat(line_nr.len() + 1);
        // TODO: Put identifiers in below format to make it easier to read.
        Ok(format!(
            "{}{} [{}:{}] {} {}\n{}|\n{} | {}\n{}| {}{}",
            indent,
            "at".bold(),
            span.beg.line,
            span.beg.col,
            "in".bold(),
            file_path,
            indent,
            line_nr.bold(),
            lines.nth(span.beg.line - 1).unwrap_or_else(|| ""),
            indent,
            " ".repeat(span.beg.col - 1),
            "^".repeat(span.len()).red().bold(),
        ))
    }
}

impl Error for CortexError {}

impl fmt::Display for CortexError {
    /// Generates the error output.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = match self {
            CortexError::ArgError(msg) => msg,
            CortexError::SyntaxError { msg, .. } => msg,
            CortexError::FileIOError(msg) => msg,
            CortexError::TypeError(msg) => msg,
        };

        write!(f, "{}: {}\n", "error".red().bold(), err_msg)?;
        match self {
            CortexError::SyntaxError { ref file_path, ref span, .. } => {
                let src = CortexError::underline(span, file_path);
                match src {
                    Ok(src) => write!(f, "{}", src),
                    Err(_) => Ok(()),
                }
            },
            _ => Ok(()),
        }
    }
}
