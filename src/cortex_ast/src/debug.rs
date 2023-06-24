use std::fmt;

/// A helper struct representing an indentation in the string representation of the AST.
#[derive(Clone)]
pub(crate) struct Indent(usize);

impl Indent {
    /// Creates a new indent object.
    pub fn new() -> Indent {
        Indent(0)
    }

    /// Obtain an indent with an extra level of indentation.
    pub fn plus(&self) -> Indent {
        Indent(self.0 + 1)
    }
}

impl fmt::Display for Indent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", "  ".repeat(self.0))
    }
}

/// Provides a way to debug the AST by obtaining a string representation.
pub(crate) trait AstDebug {
    fn debug(&self, indent: Indent) -> String;
}

