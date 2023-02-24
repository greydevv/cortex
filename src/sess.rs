use crate::io::error::Result;
use crate::io::file::FileHandler;
use crate::parser::Parser;
use crate::args::parse_args;

pub struct Session;

impl Session {
    pub fn initiate() -> Result {
        Session::compile()
    }

    fn compile() -> Result {
        let _args = parse_args()?;
        let fh = FileHandler::new(String::from("samples/debug.cx"))?;
        let mut parser = Parser::new(&fh)?;
        parser.parse().and_then(|tree| {
            for node in tree {
                println!("{}", node.debug_format());
            }
            Ok(())
        })?;
        Ok(())
    }
}
