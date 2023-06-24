use std::fmt;
use std::ops::Deref;

use colored::Colorize;

use cortex_symbols::{
    SessCtx,
    traits::Len,
    file::{
        FileHandler,
        SourceLoc,
    },
};

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

/// A single diagnostic (hint, error, warning, etc.).
#[derive(Debug)]
pub struct Diagnostic {
    msg: String,
    /// The kind of diagnostic.
    kind: DiagnosticKind,
    /// Optional elements of the source code to underline.
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
            let line_no = loc.span().beg().line()-1;
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
                    let line_nr = span.beg().line().to_string();
                    let indent = " ".repeat(line_nr.len() + 1);
                    let underline = self.kind.underline_char()
                        .repeat(span.len())
                        .color(self.kind.color())
                        .bold();
                    sources.push(format!(
                        "{}{} [{}:{}] {} {}\n{}|\n{} | {}\n{}| {}{}",
                        indent,
                        "at".bold(),
                        span.beg().line(),
                        span.beg().col(),
                        "in".bold(),
                        loc.file_path(),
                        indent,
                        line_nr.bold(),
                        line,
                        indent,
                        " ".repeat(span.beg().col() - 1),
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
