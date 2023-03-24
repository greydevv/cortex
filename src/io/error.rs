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
#[derive(Debug)]
pub struct CortexError(pub Vec<Diagnostic>);
// pub enum CortexError {
//     ArgError(String),
//     SyntaxError {
//         file_path: String,
//         msg: String,
//         span: FileSpan,
//         help: Option<String>,
//     },
//     FileIOError(String),
//     TypeError(String),
// }

/// Converts an error raised from [`clap`](https://docs.rs/clap/latest/clap/) to a standard Cortex
/// error.
impl From<ClapError> for CortexError {
    fn from(value: ClapError) -> CortexError {
        let (kind, val) = value.context().nth(0).unwrap();
        match kind {
            // ContextKind::InvalidArg => {
            //     CortexError::ArgError(format!("invalid argument '{}'", val))
            // }
            ContextKind::InvalidArg =>
                Diagnostic::new(
                    format!("invalid argument '{}'", val),
                    DiagnosticKind::Error,
                ).into(),
            _ => unimplemented!()
        }
    }
}

impl From<Diagnostic> for CortexError {
    fn from(value: Diagnostic) -> CortexError {
        CortexError(vec![value])
    }
}

impl From<Vec<Diagnostic>> for CortexError {
    fn from(value: Vec<Diagnostic>) -> CortexError {
        CortexError(value)
    }
}

impl CortexError {
    /// Creates a syntax error.
    pub fn syntax_err(ctx: &SessCtx, msg: &str, span: FileSpan, _help: Option<String>) -> CortexError {
        Diagnostic::new_with_spans(
            msg.to_string(),
            DiagnosticKind::Error,
            &ctx.fh,
            vec![(ctx.file_path().clone(), span)],
        ).into()
        // CortexError::SyntaxError{
        //     file_path: ctx.file_path(),
        //     msg: String::from(msg),
        //     span,
        //     help,
        // }
    }

    /// Creates a file I/O error.
    pub fn file_io_err(msg: &str) -> CortexError {
        Diagnostic::new(
            msg.to_string(),
            DiagnosticKind::Error,
        ).into()
        // CortexError::FileIOError(String::from(msg))
    }

    /// Creates an error from an invalid integer literal.
    pub fn invalid_integer_literal(ctx: &SessCtx, lit: &String, span: FileSpan) -> CortexError {
        Diagnostic::new_with_spans(
            format!("'{}' is not a valid integer literal", lit),
            DiagnosticKind::Error,
            &ctx.fh,
            vec![(ctx.file_path().clone(), span)],
        ).into()
        // CortexError::SyntaxError { 
        //     file_path: ctx.file_path(),
        //     msg: format!("'{}' is not a valid integer literal", lit),
        //     span,
        //     help: Some(String::from("Integer literals are expressed as a sequence of digits, e.g., \"23\" or \"921\"."))
        // }
    }

    /// Creates an error when a binary operator was expected but one was not received.
    pub fn expected_bin_op(ctx: &SessCtx, lit: &String, span: FileSpan) -> CortexError {
        Diagnostic::new_with_spans(
            format!("'{}' is not a binary operator", lit),
            DiagnosticKind::Error,
            &ctx.fh,
            vec![(ctx.file_path().clone(), span)],
        ).into()
        // CortexError::SyntaxError { 
        //     file_path: ctx.file_path(),
        //     msg: format!("'{}' is not a binary operator", lit),
        //     span,
        //     help: None,
        // }
    }

    /// Creates an error describing an unclosed brace.
    pub fn unclosed_brace(ctx: &SessCtx, tok: &Token) -> CortexError {
        Diagnostic::new_with_spans(
            format!("unclosed '{}'", tok.value()),
            DiagnosticKind::Error,
            &ctx.fh,
            vec![(ctx.file_path().clone(), tok.span)],
        ).into()
        // CortexError::SyntaxError {
        //     file_path: ctx.file_path(),
        //     msg: format!("unclosed '{}'", tok.value()),
        //     span: tok.span,
        //     help: None
        // }
    }

    /// Creates an error describing an unopened brace.
    pub fn unopened_brace(ctx: &SessCtx, tok: &Token) -> CortexError {
        Diagnostic::new_with_spans(
            format!("unopened '{}'", tok.value()),
            DiagnosticKind::Error,
            &ctx.fh,
            vec![(ctx.file_path().clone(), tok.span)],
        ).into()
        // CortexError::SyntaxError{
        //     file_path: ctx.file_path(),
        //     msg: format!("unopened '{}'", tok.value()),
        //     span: tok.span,
        //     help: None
        // }
    }

    pub fn expected_but_got(ctx: &SessCtx, expected: &str, tok: &Token) -> CortexError {
        let msg = format!(
            "expected {} but got '{}'",
            expected,
            tok.value(),
        );
        Diagnostic::new_with_spans(
            msg,
            DiagnosticKind::Error,
            &ctx.fh,
            vec![(ctx.file_path().clone(), tok.span)],
        ).into()
    }

    /// Creates an error describing an illegal reference or definition of an identifier.
    pub fn illegal_ident(ctx: &SessCtx, ident: &Ident, conflict: Option<&Ident>) -> CortexError {
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
        let mut diags = vec![
            Diagnostic::new_with_spans(
                msg,
                DiagnosticKind::Error,
                &ctx.fh,
                vec![(ctx.file_path().clone(), *ident.span())],
            )
        ];
        if let Some(conflict) = conflict {
            let help_msg = format!(
                "'{}' was defined earlier as a {}",
                conflict.raw(),
                conflict.pretty_ctx(),
            );
            diags.push(Diagnostic::new_with_spans(
                help_msg,
                DiagnosticKind::Help,
                &ctx.fh,
                vec![(ctx.file_path().clone(), *conflict.span())],
            ))
        }

        CortexError(diags)
        // CortexError::SyntaxError {
        //     file_path: ctx.file_path(),
        //     msg,
        //     span: *ident.span(),
        //     // TODO: Accept conflicting ident (if any) as a parameter and put it in the help. For
        //     // example, if the variable 'sum' cannot be defined because it's a function defined
        //     // elsewhere, the help should note the sum was previously defined as a function and
        //     // underline it.
        //     help,
        // }
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
        write!(f, "{}", ErrorFormatter::format(&self.0))
        // let err_msg = match self {
        //     CortexError::ArgError(msg) => msg,
        //     CortexError::SyntaxError { msg, .. } => msg,
        //     CortexError::FileIOError(msg) => msg,
        //     CortexError::TypeError(msg) => msg,
        // };
        //
        // write!(f, "{}: {}\n", "error".red().bold(), err_msg)?;
        // match self {
        //     CortexError::SyntaxError { ref file_path, ref span, ref help, .. } => {
        //         let src = CortexError::underline(span, file_path);
        //         match src {
        //             Ok(src) =>
        //                 match help {
        //                     Some(help_msg) => write!(f, "{}\n{}", src, help_msg),
        //                     None => write!(f, "{}", src),
        //                 }
        //             Err(_) => Ok(()),
        //         }
        //     },
        //     _ => Ok(()),
        // }
    }
}

/// The main error formatter.
pub struct ErrorFormatter;

impl ErrorFormatter {
    /// Formats a vector of `Diagnostic` objects into a singular string ready for output to
    /// console.
    pub fn format(diagnostics: &Vec<Diagnostic>) -> String {
        let mut diag_string = String::new();
        for (i, diag) in diagnostics.iter().enumerate() {
            diag_string = diag_string + &diag.to_string();
            if i < diagnostics.len() - 1 {
                diag_string = diag_string + "\n";
            }
        }
        diag_string
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f, "{}{} {}",
            self.kind.colored(),
            ":".to_string().bold(),
            self.msg,
        )?;
        if let Some(source_string) = self.get_source_string() {
            write!(f, "\n{}", source_string)
        } else {
            Ok(())
        }
    }
}

/// The various kinds of diagnostics.
#[derive(Debug, strum_macros::Display)]
pub enum DiagnosticKind {
    #[strum(serialize="error")]
    Error,
    #[strum(serialize="help")]
    Help,
    #[strum(serialize="warning")]
    Warn,
}

#[derive(Debug)]
pub struct Diagnostic {
    /// The diagnostic's message.
    msg: String,
    /// The kind of diagnostic.
    kind: DiagnosticKind,
    /// Optional positions in source code.
    spans: Option<Vec<(String, String, FileSpan)>>,
}


impl Diagnostic {
    pub fn new(msg: String, kind: DiagnosticKind) -> Diagnostic {
        Diagnostic {
            msg,
            kind,
            spans: None,
        }
    }

    pub fn new_with_spans(msg: String, kind: DiagnosticKind, fh: &FileHandler, spans: Vec<(String, FileSpan)>) -> Diagnostic {
        let mut spans_assoc = Vec::new();
        for (file_path, span) in spans {
            let line = fh.contents().lines().nth(span.beg.line-1).unwrap_or_else(|| "");
            spans_assoc.push((file_path, line.to_string(), span));
        }
        Diagnostic {
            msg,
            kind,
            spans: Some(spans_assoc),
        }
    }

    fn get_source_string(&self) -> Option<String> {
        match self.spans {
            Some(ref spans) => {
                // TODO: Need to be careful here. Use of lines.nth in the format macro will consume
                // this iterator. This means that if the same line of source code is included in
                // another diagnostic, then it will show up as a blank line. Eventually, will need
                // to rethink this and find an alternative solution for the unwrap_or_else(|| "")
                // call.
                // let mut lines = fh.contents().lines();
                let mut sources = Vec::new();
                for (file_path, line, span) in spans {
                    let line_nr = span.beg.line.to_string();
                    let indent = " ".repeat(line_nr.len() + 1);
                    let underline = self.kind.underline_char()
                        .repeat(span.len())
                        .color(self.kind.color())
                        .bold();
                    sources.push(format!(
                        "{}{} [{}:{}] {} {}\n{}|\n{} | {}\n{}| {}{}",
                        indent,
                        "at".bold(),
                        span.beg.line,
                        span.beg.col,
                        "in".bold(),
                        file_path,
                        // fh.file_path(),
                        indent,
                        line_nr.bold(),
                        line,
                        // lines.nth(span.beg.line - 1).unwrap_or_else(|| ""),
                        indent,
                        " ".repeat(span.beg.col - 1),
                        underline,
                    ));
                }
                Some(sources.join("\n"))
            },
            None => None,
        }
    }

    // pub fn is_fatal(&self) -> bool {
    //     self.kind.is_fatal()
    // }
}

impl DiagnosticKind {
    /// Returns the colored output from the display method. 
    pub fn colored(&self) -> colored::ColoredString {
        self.to_string()
            .color(self.color())
            .bold()
    }

    /// Returns the primary color used for each kind of diagnostic.
    pub fn color(&self) -> colored::Color {
        match self {
            DiagnosticKind::Error => colored::Color::Red,
            DiagnosticKind::Help => colored::Color::Blue,
            DiagnosticKind::Warn => colored::Color::Yellow,
        }
    }

    /// Returns the character that composes the physical underline in diagnostic outputs.
    pub fn underline_char(&self) -> String {
        // all same characters for now
        match self {
            DiagnosticKind::Error
                | DiagnosticKind::Help
                | DiagnosticKind::Warn => "^",
        }.to_string()
    }

    // pub fn _is_fatal(&self) -> bool {
    //     match self {
    //         DiagnosticKind::Error => true,
    //         DiagnosticKind::Help
    //             | DiagnosticKind::Warn => false,
    //     }
    // }
}

// impl From<&CortexError> for Diagnostic {
//     fn from(value: &CortexError) -> Diagnostic {
        // match value {
        //     CortexError::SyntaxError { file_path, msg, span, .. } => {
        //         let spans = (
        //             // TODO: fix unwrap
        //             FileHandler::new(file_path.clone()).unwrap(),
        //             vec![span.clone()],
        //         );
        //         Diagnostic::new_with_spans(
        //             msg.clone(),
        //             DiagnosticKind::Error,
        //             spans,
        //         )
        //     },
        //     _ => todo!(),
        // }
//     }
// }











