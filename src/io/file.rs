use std::fs::File;
use std::io::Read;
use std::error::Error;

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
