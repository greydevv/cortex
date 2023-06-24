pub mod traits;
pub mod token;
pub mod span;
pub mod file;
pub mod types;
pub mod ident;

use std::rc::Rc;

use crate::file::{ FileHandler, FilePath };

pub struct SessCtx {
    /// The file being compiled.
    pub fh: FileHandler,
}

impl SessCtx {
    /// Creates a new compilation session context.
    pub fn new(fh: FileHandler) -> SessCtx {
        SessCtx {
            fh,
        }
    }

    /// Gets the file path of the file being compiled.
    pub fn file_path(&self) -> &Rc<FilePath> {
        self.fh.file_path()
    }
}
