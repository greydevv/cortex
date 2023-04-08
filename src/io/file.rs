//! Helpers for dealing with files.

use std::fs::File;
use std::fmt;
use std::io::Read;

use crate::symbols::Len;
use crate::io::error::{ Result, CortexError, IOErrorWrapper };

/// The object representing a source file.
#[derive(Debug, Clone)]
pub struct FileHandler {
    file_path: String,
    src: String,
}

impl FileHandler {
    /// Creates a new file handler given the file path.
    pub fn new(file_path: String) -> Result<FileHandler> {
        let mut f = File::open(&file_path).map_err(|e| CortexError::from(IOErrorWrapper(e, &file_path)))?;
        let mut src = String::new();
        f.read_to_string(&mut src).map_err(|e| CortexError::from(IOErrorWrapper(e, &file_path)))?;
        Ok(FileHandler {
            file_path,
            src
        })
    }

    /// Gets the source contents.
    pub fn contents(&self) -> &String {
        &self.src
    }

    /// Gets the file path.
    pub fn file_path(&self) -> &String {
        &self.file_path
    }
}

/// Compares file handlers based on their file path.
impl PartialEq for FileHandler {
    fn eq(&self, other: &FileHandler) -> bool {
        self.file_path == other.file_path
    }

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

    impl FileHandler {
        pub fn pseudo_fh(src: &str) -> FileHandler {
            FileHandler {
                file_path: String::new(),
                src: src.to_string(),
            }
        }
    }

    #[test]
    fn open_existing_file() -> Result {
        let _result = FileHandler::new(String::from("samples/debug.cx"))?;
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
        let result = FileHandler::new(String::from("iamsomenonexistentfile"));
        
        assert_diags(result, expected_diags);
    }
}
