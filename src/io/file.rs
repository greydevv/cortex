use std::fs::File;
use std::io::{ Read, ErrorKind };
use std::fmt;

use crate::symbols::Len;
use crate::io::error::{ CortexError, Result };

#[derive(Debug)]
pub struct FileHandler {
    file_path: String,
    src: String,
}

impl FileHandler {
    pub fn new(file_path: String) -> Result<FileHandler> {
        let mut f = File::open(&file_path).map_err(|e| FileHandler::translate_err(e, &file_path))?;
        let mut src = String::new();
        f.read_to_string(&mut src).map_err(|e| FileHandler::translate_err(e, &file_path))?;
        Ok(FileHandler {
            file_path,
            src
        })
    }

    pub fn contents(&self) -> &String {
        &self.src
    }

    fn translate_err(error: std::io::Error, file_path: &String) -> CortexError {
        let err_msg = match error.kind() {
            ErrorKind::NotFound => format!("'{file_path}' does not exist"),
            ErrorKind::PermissionDenied => format!("cannot open '{file_path}': permission denied"),
            ErrorKind::InvalidData => format!("contents of '{file_path}' are not valid UTF-8"),
            _ => format!("could not read '{file_path}'"),
        };
        CortexError::FileIOError(err_msg)
    }

    pub fn file_path(&self) -> &String {
        &self.file_path
    }
}

impl PartialEq for FileHandler {
    fn eq(&self, other: &FileHandler) -> bool {
        self.file_path == other.file_path
    }

    fn ne(&self, other: &FileHandler) -> bool {
        self.file_path != other.file_path
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct FileSpan {
    pub beg: FilePos,
    pub end: FilePos,
}

impl FileSpan {
    pub fn new(beg: FilePos, end: FilePos) -> FileSpan {
        FileSpan {
            beg,
            end,
        }
    }

    pub fn one(beg: FilePos) -> FileSpan {
        let end = FilePos::new(beg.line, beg.col+1);
        FileSpan::new(
            beg,
            end,
        )

    }

    pub fn to(&self, other: FileSpan) -> FileSpan {
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

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct FilePos {
    pub line: usize,
    pub col: usize,
}

impl FilePos {
    pub fn new(line: usize, col: usize) -> FilePos {
        FilePos {
            line,
            col,
        }
    }

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
impl FileHandler {
    pub fn pseudo_fh(src: String) -> FileHandler {
        FileHandler {
            file_path: String::new(),
            src,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn open_file() -> Result {
        let _result = FileHandler::new(String::from("samples/debug.cx"))?;
        Ok(())
    }

    #[test]
    fn open_nonexistent_file() {
        let result = FileHandler::new(String::from("iamsomenonexistentfile"));
        let expected = CortexError::FileIOError(format!("'iamsomenonexistentfile' does not exist"));

        assert!(result.is_err());
        assert_eq!(*(result.err()).unwrap(), expected);
    }
}
