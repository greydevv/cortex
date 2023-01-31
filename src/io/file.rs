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

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct SourceLocation {
    pub line: u32,
    pub col: u32,
}

impl SourceLocation {
    pub fn new(line: u32, col: u32) -> SourceLocation {
        SourceLocation {
            line,
            col,
        }
    }

    pub fn default() -> SourceLocation {
        SourceLocation::new(1, 1)
    }
}

impl fmt::Display for SourceLocation {
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
