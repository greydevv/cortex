use crate::io::error::Result;
use crate::io::file::FileHandler;
use crate::parser::Parser;
use crate::args::parse_args;

pub struct SessCtx {
    pub fh: FileHandler,
}

impl SessCtx {
    pub fn new(fh: FileHandler) -> SessCtx {
        SessCtx {
            fh,
        }
    }

    pub fn file_path(&self) -> String {
        self.fh.file_path().clone()
    }
}

pub struct Session;

impl Session {
    pub fn initiate() -> Result {
        Session::compile()
    }

    // Only pass context to those who update it
    // Can pass context to error in teh catch method if needed
    fn compile() -> Result {
        let _args = parse_args()?;
        let ctx = SessCtx::new(
            FileHandler::new(String::from("samples/debug.cx"))?
        );
        let mut parser = Parser::new(&ctx)?;
        parser.parse().and_then(|tree| {
            for node in tree {
                println!("{}", node.debug_format());
            }
            Ok(())
        })?;
        Ok(())
    }
}
