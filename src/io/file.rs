use std::fs::File;
use std::io::Read;
use std::error::Error;
use std::fmt;

pub struct FileHandler {
    src: String,
}

impl FileHandler {
    pub fn new(file_path: String) -> Result<FileHandler, Box<dyn Error>> {
        let mut f = File::open(&file_path)?;
        let mut src: String  = String::new();
        f.read_to_string(&mut src)?;
        Ok(FileHandler {
            src
        })
    }

    pub fn contents(&self) -> &String {
        &self.src
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct SourceLocation {
    pub line: u32,
    pub col: u32,
}

impl SourceLocation {
    pub fn default() -> SourceLocation {
        SourceLocation {
            line: 1,
            col: 1,
        }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "(line {}, col {})", self.line, self.col)
    }
}
