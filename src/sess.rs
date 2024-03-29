//! Compilation driver.

use std::rc::Rc;

use crate::io::error::Result;
use crate::io::file::{ FileHandler, FilePath };
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::args::parse_args;
use crate::ast::validate::Validator;

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

/// The compilation session object.
pub struct Session;

impl Session {
    pub fn initiate(file_path: FilePath) -> Result {
        Session::compile(file_path)
    }

    /// The main driver of the compilation process.
    fn compile(file_path: FilePath) -> Result {
        let args = parse_args()?;
        let ctx = SessCtx::new(
            FileHandler::new(file_path)?
        );
        if args.get_flag("only-display-tokens") {
            return Session::only_lexer_tokens(&ctx);
        }
        let mut parser = Parser::new(&ctx)?;
        parser.parse()
            .and_then(|ref mut module| {
                let mut vd = Validator::new(&ctx);
                vd.validate(module)?;
                println!("{}", module.debug_string());
                Ok(())
            })?;
        Ok(())
    }

    /// Prints the tokens resulting from tokenizing the source file.
    fn only_lexer_tokens(ctx: &SessCtx) -> Result {
        let mut lexer = Lexer::new(ctx);
        loop {
            let tok = lexer.next_token()?;
            println!("{}", tok);
            if lexer.eof() {
                break;
            }
        }
        Ok(())
    }
}
