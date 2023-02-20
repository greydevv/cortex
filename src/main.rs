use std::env;
use std::time::Instant;

use colored::Colorize;

use crate::io::file::FileHandler;
use crate::io::error::CortexError;
use crate::parser::Parser;

pub mod io;
pub mod lexer;
pub mod parser;
pub mod ast;

fn main() {
    let mut args = env::args();
    args.next(); // skip executable name
    let now = Instant::now();
    let status_string = match compile(args) {
        Ok(_) => "SUCCESS".green(),
        Err(_) => "FAILURE".red(),
    }.bold();
    let elapsed = now.elapsed();
    eprintln!("\n[{}, took {:.2}s]", status_string, elapsed.as_secs_f32());
}

fn compile(mut _args: impl Iterator<Item = String>) -> Result<(), CortexError> {
    let fh = match FileHandler::new(String::from("samples/tokens.cx")) {
        Ok(fh) => fh,
        Err(e) => {
            e.display_msg();
            return Err(e);
        }
    };
    let mut parser = Parser::new(&fh)?;
    match parser.parse() {
        Ok(tree) => { 
            for node in tree {
                println!("{}", node.debug(0));
            }
            Ok(())
        },
        Err(e) => {
            e.display_with_line(&fh);
            Err(e)
        }
    }
}
