use crate::io::error::Result;
use crate::io::file::FileHandler;
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::args::parse_args;
use crate::ast::validate::Validator;

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
    // Can pass context to error in the catch method if needed
    fn compile() -> Result {
        let args = parse_args()?;
        let ctx = SessCtx::new(
            FileHandler::new(String::from("samples/debug.cx"))?
        );
        if args.get_flag("only-display-tokens") {
            return Session::only_lexer_tokens(&ctx);
        }
        let mut parser = Parser::new(&ctx)?;
        parser.parse()
            .and_then(|ref mut tree| {
                let mut vd = Validator::new();
                vd.validate(tree)?;
                for node in &mut *tree {
                    println!("{}", node.debug_string());
                }
                Ok(())
            })?;
        Ok(())
    }

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
