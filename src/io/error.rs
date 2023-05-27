//! The main interface for generating and errors.

use std::fmt;
use std::error::Error;
use std::ops::Deref;
use std::io::ErrorKind;
use std::convert::From;

use colored::Colorize;
#[allow(unused_imports)]
use clap::error::ErrorKind as ClapErrorKind;
use clap::error::Error as ClapError;

use crate::ast::{ Ident, IdentInfo, IdentCtx };
use crate::io::file::{
    FileHandler,
    FilePath,
    FileSpan,
    SourceLoc
};
use crate::symbols::{
    Token,
    Len,
    TyKind,
    BinOpKind,
    Literal,
};
use crate::sess::SessCtx;

/// Result alias used throughout compilation stages.
pub type Result<T = (), E = Box<CortexError>> = std::result::Result<T, E>;

/// The main error object.
#[derive(Debug)]
pub struct CortexError(pub Vec<Diagnostic>);

/// Converts an error raised from [`clap`](https://docs.rs/clap/latest/clap/) to a standard Cortex
/// error.
impl From<ClapError> for CortexError {
    fn from(e: ClapError) -> CortexError {
        match e.kind() {
            _ => unimplemented!("clap-rs error to CortexError: {}", e.kind())
        }
    }
}

/// A wrapper around [`std::io::Error`] meant to associate the error with a file path.
pub(in crate::io) struct IOErrorWrapper<'a>(pub std::io::Error, pub &'a FilePath);

impl From<IOErrorWrapper<'_>> for CortexError {
    /// Effectively converts a [`std::io::Error`] associated with a file path into a `CortexError`.
    fn from (value: IOErrorWrapper) -> CortexError {
        let IOErrorWrapper(error, file_path) = value;
        let err_msg = match error.kind() {
            ErrorKind::NotFound => format!("'{file_path}' does not exist"),
            ErrorKind::PermissionDenied => format!("cannot open '{file_path}': permission denied"),
            ErrorKind::InvalidData => format!("contents of '{file_path}' are not valid UTF-8"),
            _ => format!("could not read '{file_path}'"),
        };
        Diagnostic::new(
            err_msg,
            DiagnosticKind::Error,
        ).into()
    }
}

impl From<Diagnostic> for CortexError {
    /// Creates an error from a single diagnostic.
    fn from(value: Diagnostic) -> CortexError {
        CortexError(vec![value])
    }
}

impl From<Vec<Diagnostic>> for CortexError {
    /// Creates an error from multiple diagnostics.
    fn from(value: Vec<Diagnostic>) -> CortexError {
        CortexError(value)
    }
}

impl CortexError {
    /// Creates a syntax error.
    pub fn syntax_err(ctx: &SessCtx, msg: &str, span: FileSpan, _help: Option<String>) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            msg.to_string(),
            DiagnosticKind::Error,
            vec![SourceLoc::new(ctx.file_path(), span.clone())],
        ).into()
    }

    /// Creates a file I/O error.
    pub fn file_io_err(msg: &str) -> CortexError {
        Diagnostic::new(
            msg.to_string(),
            DiagnosticKind::Error,
        ).into()
    }

    /// Creates an error from an invalid integer literal.
    pub fn invalid_integer_literal(ctx: &SessCtx, lit: &String, span: FileSpan) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("'{}' is not a valid integer literal", lit),
            DiagnosticKind::Error,
            vec![SourceLoc::new(ctx.file_path(), span.clone())],
        ).into()
    }

    /// Creates an error when a binary operator was expected but one was not received.
    pub fn expected_bin_op(ctx: &SessCtx, lit: &String, span: FileSpan) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("'{}' is not a binary operator", lit),
            DiagnosticKind::Error,
            vec![SourceLoc::new(ctx.file_path(), span.clone())],
        ).into()
    }

    /// Creates an error describing an unclosed brace.
    pub fn unclosed_brace(ctx: &SessCtx, tok: &Token) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("unclosed '{}'", tok.value()),
            DiagnosticKind::Error,
            vec![SourceLoc::new(ctx.file_path(), tok.span.clone())],
        ).into()
    }

    /// Creates an error describing an unopened brace.
    pub fn unopened_brace(ctx: &SessCtx, tok: &Token) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("unopened '{}'", tok.value()),
            DiagnosticKind::Error,
            vec![SourceLoc::new(ctx.file_path(), tok.span.clone())],
        ).into()
    }

    /// Creates an error describing a type mismatch.
    pub fn incompat_types(ctx: &SessCtx, expected_ty: &TyKind, received_ty: &TyKind, expr_loc: SourceLoc) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("expected type '{}' but got type '{}'", *expected_ty, *received_ty),
            DiagnosticKind::Error,
            vec![expr_loc]
        ).into()
    }

    /// Creates an error describing an invalid application of a binary operator to a certain type.
    pub fn incompat_types_in_bin_op(ctx: &SessCtx, bin_op_kind: &BinOpKind, received_ty: &TyKind, expr_loc: SourceLoc) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("binary operator '{}' cannot be applied to operand of type '{}'", bin_op_kind.literal(), received_ty),
            DiagnosticKind::Error,
            vec![expr_loc],
        ).into()
    }

    /// Creates an error describing the case when an expectation of a token was not satisfied.
    pub fn expected_but_got(ctx: &SessCtx, expected: &str, tok: &Token) -> CortexError {
        let msg = format!(
            "expected {} but got '{}'",
            expected,
            tok.value(),
        );
        Diagnostic::new_with_spans(
            ctx,
            msg,
            DiagnosticKind::Error,
            vec![SourceLoc::new(ctx.file_path(), tok.span.clone())],
        ).into()
    }

    /// Creates an error describing a mismatch between the expected number of arguments passed into
    /// a function call and the actual number of arguments passed.
    pub fn args_n_mismatch(ctx: &SessCtx, expected_n: usize, received_n: usize, loc: SourceLoc) -> CortexError {
        let arg_str = match expected_n {
            1 => "argument",
            _ => "arguments"
        };
        Diagnostic::new_with_spans(
            ctx,
            format!("expected {} {} but received {}", expected_n, arg_str, received_n),
            DiagnosticKind::Error,
            vec![loc]
        ).into()
    }

    fn dupe_ident_helper(ctx: &SessCtx, ident: &Ident, original_ident: &Ident, structure_kind: &str) -> CortexError {
        let diags = vec![
            Diagnostic::new_with_spans(
                ctx,
                format!("duplicate {} member '{}'", structure_kind, ident),
                DiagnosticKind::Error,
                vec![ident.loc().clone()],
            ),
            Diagnostic::new_with_spans(
                ctx,
                format!("'{}' was defined here", ident),
                DiagnosticKind::Help,
                vec![original_ident.loc().clone()],
            ),
        ];
        CortexError::from(diags)
    }

    /// Creates an error describing a duplicate enum member.
    pub fn dupe_enum_member(ctx: &SessCtx, ident: &Ident, original_ident: &Ident) -> CortexError {
        CortexError::dupe_ident_helper(ctx, ident, original_ident, "enum")
    }

    /// Creates an error describing a duplicate struct member.
    pub fn dupe_struct_member(ctx: &SessCtx, ident: &Ident, original_ident: &Ident) -> CortexError {
        CortexError::dupe_ident_helper(ctx, ident, original_ident, "struct")
    }

    pub fn illegal_member_access(ctx: &SessCtx, loc: SourceLoc, ty_kind: TyKind) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("type '{}' does not have members", ty_kind),
            DiagnosticKind::Error,
            vec![loc]
        ).into()
    }

    // Creates an error describing a nonexistent member (e.g., `bar` does not exist in `foo.bar`).
    pub fn nonexistent_member(ctx: &SessCtx, parent_ty: TyKind, member: &Ident) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("member '{}' does not exist on type '{}'", member.name(), parent_ty),
            DiagnosticKind::Error,
            vec![member.loc().clone()]
        ).into()
    }

    /// Creates an error describing an include statement with a file path that doesn't exist.
    pub fn nonexistent_include(ctx: &SessCtx, file_path: &FilePath, loc: SourceLoc) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("could not locate file '{}'", file_path),
            DiagnosticKind::Error,
            vec![loc]
        ).into()
    }

    /// Creates an error describing an invalid invocation of an identifier, e.g. trying to call a
    /// variable instead of a function.
    pub fn illegal_ident_call(ctx: &SessCtx, loc: SourceLoc, conflict: &Ident) -> CortexError {
        let msg = match conflict.ctx() {
            IdentCtx::EnumDef => String::from("cannot invoke an enum"),
            _ => format!("cannot invoke type {}", conflict.ty_kind()),
        };
        Diagnostic::new_with_spans(
            ctx,
            msg,
            DiagnosticKind::Error,
            vec![loc]
        ).into()
    }

    /// Creates an error describing the case of `let foo: bar = ...` where `bar` not a typedef.
    pub fn not_typedef(ctx: &SessCtx, name: &String, loc: &SourceLoc) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("'{}' is not a type", name),
            DiagnosticKind::Error,
            vec![loc.clone()],
        ).into()
    }

    /// Creates an error describing the case of `let foo: Bar = ...` where `Bar` is an unknown
    /// typedef.
    ///
    /// This method is different from `CortexError::not_typedef` because `Bar` may be a typedef; it
    /// just isn't in scope.
    pub fn unknown_typedef(ctx: &SessCtx, name: &String, loc: &SourceLoc) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("unknown type '{}'", name),
            DiagnosticKind::Error,
            vec![loc.clone()],
        ).into()
    }

    /// Creates an error describing the case of `Foo::Bar` where `Foo` is an enum that does not
    /// define the member 'Bar'.
    pub fn nonexistent_enum_member(ctx: &SessCtx, qual_info: &IdentInfo, info: &IdentInfo) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("{} is not a member of {}", info, qual_info),
            DiagnosticKind::Error,
            vec![info.loc().clone()],
        ).into()
    }

    /// Creates an error describing the case of `Foo::Bar` where `Foo` is not an enum.
    pub fn not_enum(ctx: &SessCtx, qual_info: &IdentInfo) -> CortexError {
        Diagnostic::new_with_spans(
            ctx,
            format!("{} is not an enumeration", qual_info),
            DiagnosticKind::Error,
            vec![qual_info.loc().clone()],
        ).into()
    }

    /// Creates an error describing an illegal reference or definition of an identifier.
    pub fn illegal_ident(ctx: &SessCtx, info: &IdentInfo, conflict: Option<&Ident>) -> CortexError {
        let msg = match info.ctx() {
            IdentCtx::Def
                | IdentCtx::Param
                | IdentCtx::FuncDef
                | IdentCtx::EnumDef
                | IdentCtx::StructDef =>
                    format!(
                        "'{}' was already defined",
                        info,
                    ),
            IdentCtx::Ref =>
                    format!(
                        "variable '{}' does not exist",
                        info,
                    ),
            IdentCtx::FuncCall =>
                    format!(
                        "function '{}' does not exist",
                        info,
                    ),
        };
        let mut diags = vec![
            Diagnostic::new_with_spans(
                ctx,
                msg,
                DiagnosticKind::Error,
                vec![info.loc().clone()],
            )
        ];
        if let Some(conflict) = conflict {
            let help_msg = format!(
                "'{}' was defined earlier as a {}",
                conflict,
                conflict.pretty_ctx(),
            );
            diags.push(Diagnostic::new_with_spans(
                ctx,
                help_msg,
                DiagnosticKind::Help,
                vec![conflict.loc().clone()],
            ))
        }
        CortexError::from(diags)
    }
}

impl Error for CortexError {}

impl fmt::Display for CortexError {
    /// Generates the error output.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ErrorFormatter::format(&self.0))
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
    /// Formats the diagnostic into a string ready for output to console.
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
#[derive(PartialEq, Debug, strum_macros::Display)]
pub enum DiagnosticKind {
    /// An error.
    #[strum(serialize="error")]
    Error,
    /// A help message, such as a suggestion.
    #[strum(serialize="help")]
    Help,
    /// A general warning.
    #[strum(serialize="warning")]
    Warn,
}

#[derive(PartialEq, Debug)]
pub struct Diagnostic {
    /// The diagnostic's message.
    msg: String,
    /// The kind of diagnostic.
    kind: DiagnosticKind,
    /// Optional positions in source code.
    spans: Option<Vec<(SourceLoc, String)>>,
}


impl Diagnostic {
    /// Creates a new diagnostic with no associated source code.
    pub fn new(msg: String, kind: DiagnosticKind) -> Diagnostic {
        Diagnostic {
            msg,
            kind,
            spans: None,
        }
    }

    /// Creates a new diagnostic with associated source code.
    pub fn new_with_spans(ctx: &SessCtx, msg: String, kind: DiagnosticKind, locs: Vec<SourceLoc>) -> Diagnostic {
        let mut spans_assoc = Vec::new();
        for loc in locs {
            // TODO: Need to be careful here. Use of lines.nth in the format macro will consume
            // this iterator. This means that if the same line of source code is included in
            // another diagnostic, then it will show up as a blank line. Eventually, will need
            // to rethink this and find an alternative solution for the unwrap_or_else(|| "")
            // call. This seems inefficient to call contents().lines() every loop. In either case,
            // need to take a look at this.
            let line_no = loc.span().beg.line-1;
            let line = if ctx.fh.file_path() == loc.file_path() {
                // Don't read the file again if it's already open.
                ctx.fh.contents()
                    .lines()
                    .nth(line_no)
                    .unwrap_or_else(|| "")
                    .to_string()
            } else {
                // Unwrapping should be safe here because no error with a span should be
                // thrown before that file's existence is verified (by the parser).
                FileHandler::new(loc.file_path().deref().clone())
                    .unwrap()
                    .contents()
                    .lines()
                    .nth(line_no)
                    .unwrap_or_else(|| "")
                    .to_string()
            };
            spans_assoc.push((loc, line));
        }
        Diagnostic {
            msg,
            kind,
            spans: Some(spans_assoc),
        }
    }

    /// Generates a `String` with related info regarding the offending token(s) causing the error.
    ///
    /// The following snippet shows an example output generated from an error due to a reference to
    /// an unknown variable.
    ///
    /// ```text
    ///   at [5:1] in path/to/some_file.cx
    ///   |
    /// 5 | lucky_num = 13;
    ///   | ^^^^^^^^^
    /// ```
    ///
    /// The offending token is `lucky_num` and is underlined using multiple instances of the `'^'`
    /// character. Pertinent information is included above the offending source code, such as the
    /// filename (in this case, `path/to/some_file.cx`) and the location at which the error
    /// happend, formatted as `[line:col]` (in this case, `[5:1]`).
    ///
    /// Note that the error message itself is not included in this output as some errors are not
    /// associated with a piece of source code, e.g., an error resulting from invalid CLI
    /// arguments.
    fn get_source_string(&self) -> Option<String> {
        match self.spans {
            Some(ref spans) => {
                let mut sources = Vec::new();
                for (loc, line) in spans {
                    let span = loc.span();
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
                        loc.file_path(),
                        indent,
                        line_nr.bold(),
                        line,
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

    /// Returns the character that composes the underline(s) in diagnostic outputs.
    ///
    /// # Examples
    ///
    /// If the character is "~", and we wish to underline `some_string`, the output looks something
    /// like:
    /// ```text
    /// some_string
    /// ~~~~~~~~~~~
    /// ```
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

#[cfg(test)]
pub mod tests {
    use super::*;

    pub fn assert_diags<T>(result: Result<T>, expected_diags: Vec<Diagnostic>) {
        assert!(result.is_err());
        let CortexError(diags) = *result.err().unwrap();
        assert!(diags.len() == expected_diags.len());
        diags.iter()
            .zip(&expected_diags)
            .for_each(|(diag, expected_diag)| assert!(diag == expected_diag))
    }
}
