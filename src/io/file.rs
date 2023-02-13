use std::fs::File;
use std::io::{ Read, ErrorKind };
use std::fmt;

use crate::io::error::CortexError;

pub struct FileHandler {
    src: String,
}

impl FileHandler {
    pub fn new(file_path: String) -> Result<FileHandler, CortexError> {
        let mut f = File::open(&file_path).map_err(|e| FileHandler::translate_err(e, &file_path))?;
        let mut src = String::new();
        f.read_to_string(&mut src).map_err(|e| FileHandler::translate_err(e, &file_path))?;
        Ok(FileHandler {
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
}

#[derive(PartialEq, Debug, Clone)]
pub struct Span {
    beg: FilePos,
    end: FilePos,
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
mod tests {
    use super::*;

    #[test]
    fn open_file() -> Result<(), CortexError> {
        let _result = FileHandler::new(String::from("samples/tokens.cx"))?;
        Ok(())
    }

    #[test]
    fn open_nonexistent_file() {
        let result = FileHandler::new(String::from("iamsomenonexistentfile"));
        let expected = CortexError::FileIOError(format!("'iamsomenonexistentfile' does not exist"));

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }
}
