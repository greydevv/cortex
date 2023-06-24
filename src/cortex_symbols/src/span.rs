use std::fmt;

use crate::traits::Len;

/// A 2D point in a file.
#[derive(Clone, Debug, Copy)]
pub struct FilePos {
    line: usize,
    col: usize,
}

impl FilePos {
    /// Create a new file position at a certain line and column.
    pub fn new(line: usize, col: usize) -> FilePos {
        FilePos {
            line,
            col,
        }
    }

    /// Create a default file position, starting at the first line and column (first character of
    /// any text-based file).
    pub fn default() -> FilePos {
        FilePos::new(1, 1)
    }
    /// Get the line, or y-coordinate.
    pub fn line(&self) -> usize {
        self.line
    }

    /// Get the column, or x-coordinate.
    pub fn col(&self) -> usize {
        self.col
    }

    /// Increment the column by one.
    pub fn inc_col(&mut self) {
        self.col += 1;
    }

    /// Set the column to one (start of new line).
    pub fn reset_col(&mut self) {
        self.col = 1;
    }

    /// Increment the line by one.
    pub fn inc_line(&mut self) {
        self.line += 1;
    }
}

impl fmt::Display for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "(line {}, col {})", self.line, self.col)
    }
}

/// A span between two 2D points in a file.
#[derive(Clone, Copy, Debug)]
pub struct FileSpan {
    beg: FilePos,
    end: FilePos,
}

impl FileSpan {
    /// Create a new file span.
    pub fn new(beg: FilePos, end: FilePos) -> FileSpan {
        FileSpan {
            beg,
            end,
        }
    }

    /// Create a new file span of length one.
    pub fn one(beg: FilePos) -> FileSpan {
        let end = FilePos::new(beg.line, beg.col+1);
        FileSpan::new(
            beg,
            end,
        )

    }

    /// Create a new file span from the beginning of the current file span to the end of the
    /// provided file span.
    pub fn to(&self, other: &FileSpan) -> FileSpan {
        // TODO: Do I need to check if the other file span is further along in the source code
        // than the current file span?
        FileSpan {
            beg: self.beg.clone(),
            end: other.end.clone(),
        }
    }

    /// Create a new file span of length one beginning at the end of the current file span.
    pub fn end_span(&self) -> FileSpan {
        FileSpan {
            beg: FilePos { line: self.end.line, col: self.end.col-1 },
            end: self.end.clone(),
        }
    }

    /// Get the beginning position.
    pub fn beg(&self) -> &FilePos {
        &self.beg
    }

    /// Get the ending position.
    pub fn end(&self) -> &FilePos {
        &self.end
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
