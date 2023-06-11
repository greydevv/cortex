//! Helpers for dealing with files.

use std::fs::File;
use std::fmt;
use std::io::Read;
use std::rc::Rc;

use crate::symbols::Len;
use crate::io::error::{ Result, CortexError, IOErrorWrapper };

#[derive(PartialEq, Debug, Clone)]
pub struct SourceLoc {
    file_path: Rc<FilePath>,
    span: FileSpan,
}

impl SourceLoc {
    /// Create a new source location object.
    pub fn new(file_path: &Rc<FilePath>, span: FileSpan) -> SourceLoc {
        SourceLoc {
            file_path: file_path.clone(),
            span,
        }
    }

    /// Obtain a reference to the location's file path.
    pub fn file_path(&self) -> &Rc<FilePath> {
        &self.file_path
    }

    /// Obtain the span of the location.
    pub fn span(&self) -> &FileSpan {
        &self.span
    }

    /// Set the span of the location.
    pub fn set_span(&mut self, span: FileSpan) {
        self.span = span
    }
}

/// The object representing a file path.
#[derive(PartialEq, Debug, Clone)]
pub struct FilePath(String);


impl FilePath {
    /// Creates a new file path object.
    pub fn new(file_path: &str) -> FilePath {
        FilePath(file_path.to_string())
    }

    /// Obtain a reference to the file path.
    pub fn raw(&self) -> &String {
        &self.0
    }
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The object representing a source file.
#[derive(Debug, Clone)]
pub struct FileHandler {
    file_path: Rc<FilePath>,
    src: String,
}

impl FileHandler {
    /// Creates a new file handler given the file path.
    pub fn new(file_path: FilePath) -> Result<FileHandler> {
        let mut f = File::open(&file_path.to_string()).map_err(|e| CortexError::from(IOErrorWrapper(e, &file_path)))?;
        let mut src = String::new();
        f.read_to_string(&mut src).map_err(|e| CortexError::from(IOErrorWrapper(e, &file_path)))?;
        Ok(FileHandler {
            file_path: file_path.into(),
            src
        })
    }

    /// Gets the source contents.
    pub fn contents(&self) -> &String {
        &self.src
    }

    /// Gets the file path.
    pub fn file_path(&self) -> &Rc<FilePath> {
        &self.file_path
    }
}

/// Compares file handlers based on their file path.
impl PartialEq for FileHandler {
    /// Compares the object's file paths for equality.
    fn eq(&self, other: &FileHandler) -> bool {
        self.file_path == other.file_path
    }

    /// Compares the object's file paths for inequality.
    fn ne(&self, other: &FileHandler) -> bool {
        self.file_path != other.file_path
    }
}

/// The object describing a span of source contents.
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct FileSpan {
    /// Beginning position.
    pub beg: FilePos,
    /// Ending position.
    pub end: FilePos,
}

impl FileSpan {
    /// Creates a new file span.
    pub fn new(beg: FilePos, end: FilePos) -> FileSpan {
        FileSpan {
            beg,
            end,
        }
    }

    /// Creates a new file span of length one.
    pub fn one(beg: FilePos) -> FileSpan {
        let end = FilePos::new(beg.line, beg.col+1);
        FileSpan::new(
            beg,
            end,
        )

    }

    /// Creates a new file span by using the beginning of the current file span and the end of the
    /// provided file span.
    pub fn to(&self, other: &FileSpan) -> FileSpan {
        // TODO: Do I need to check if the other file span is further along in the source code
        // than the current file span?
        FileSpan {
            beg: self.beg.clone(),
            end: other.end.clone(),
        }
    }

    /// Creates a new file span of length one beginning at the end of the current file span.
    pub fn end(&self) -> FileSpan {
        FileSpan {
            beg: FilePos { line: self.end.line, col: self.end.col-1 },
            end: self.end.clone(),
        }
    }

    /// Create a default file span with no important information.
    // TODO: Is there a better alternative default value for a file span?
    pub fn dummy() -> FileSpan {
        FileSpan {
            beg: FilePos { line: 1, col: 1 },
            end: FilePos { line: 1, col: 1 },
        }
    }
}

impl Len for FileSpan {
    fn len(&self) -> usize {
        // TODO: this will not work for multiline spans
        self.end.col - self.beg.col
    }
}

impl fmt::Display for FileSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.beg, self.end)
    }
}

/// The object describing a position in a file.
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct FilePos {
    pub line: usize,
    pub col: usize,
}

impl FilePos {
    /// Creates a new file position.
    pub fn new(line: usize, col: usize) -> FilePos {
        FilePos {
            line,
            col,
        }
    }

    /// Creates a default file position, starting at the first line and column (first character of
    /// any text-based file).
    pub fn default() -> FilePos {
        FilePos::new(1, 1)
    }
}

impl fmt::Display for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "(line {}, col {})", self.line, self.col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::error::{
        Diagnostic,
        DiagnosticKind,
        tests::assert_diags
    };

    impl FilePath {
        pub fn pseudo_fp() -> Rc<FilePath> {
            Rc::new(FilePath(String::new()))
        }
    }

    impl FileHandler {
        pub fn pseudo_fh(src: &str) -> FileHandler {
            FileHandler {
                file_path: FilePath::pseudo_fp(),
                src: src.to_string(),
            }
        }
    }

    #[test]
    fn open_existing_file() -> Result {
        let _result = FileHandler::new(FilePath::new("samples/debug.cx"))?;
        Ok(())
    }

    #[test]
    fn open_nonexistent_file() {
        let expected_diags = vec![
            Diagnostic::new(
                "'iamsomenonexistentfile' does not exist".to_string(),
                DiagnosticKind::Error,
            )
        ];
        let result = FileHandler::new(FilePath::new("iamsomenonexistentfile"));
        
        assert_diags(result, expected_diags);
    }
}
