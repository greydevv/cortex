use std::fmt;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use crate::span::FileSpan;

/// A wrapper around a string representing a file path.
#[derive(PartialEq, Clone, Debug)]
pub struct FilePath(String);

impl FilePath {
    /// Create a new file path object.
    pub fn new(file_path: &str) -> FilePath {
        FilePath(file_path.to_string())
    }
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A span of a file associated with its file path.
#[derive(Debug, Clone)]
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

/// A wrapper around [`std::io::Error`], associating it with a file path.
#[derive(Debug)]
pub struct IOErrorWrapper(pub std::io::Error, pub FilePath);

/// A handler for file IO.
#[derive(Clone)]
pub struct FileHandler {
    file_path: Rc<FilePath>,
    src: String,
}

impl FileHandler {
    /// Create a new file handler by reading in a file given its provided file path.
    pub fn new(file_path: FilePath) -> Result<FileHandler, IOErrorWrapper> {
        let mut f = File::open(&file_path.to_string()).map_err(|e| IOErrorWrapper(e, file_path.clone()))?;
        let mut src = String::new();
        f.read_to_string(&mut src).map_err(|e| IOErrorWrapper(e, file_path.clone()))?;
        Ok(FileHandler {
            file_path: file_path.into(),
            src
        })
    }

    /// Get the source contents.
    pub fn contents(&self) -> &String {
        &self.src
    }

    /// Get the file path.
    pub fn file_path(&self) -> &Rc<FilePath> {
        &self.file_path
    }
}
