mod args;

use crate::args::parse_args;

use cortex_lex::Lexer;
use cortex_parse::Parser;
use cortex_type_check::Validator;
use cortex_errors::{ Result, CortexError };
use cortex_symbols::{
    SessCtx,
    file::{
        FilePath,
        FileHandler,
    },
};

pub fn compile(file_path: FilePath) -> Result {
    let args = parse_args()?;
    let ctx = SessCtx::new(
        FileHandler::new(file_path)
            .map_err(|e| Box::new(CortexError::from(e)))?
    );
    if args.get_flag("only-display-tokens") {
        return only_lexer_tokens(&ctx);
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
