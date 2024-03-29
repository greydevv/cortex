#![feature(map_try_insert)]

use std::time::Instant;

use colored::Colorize;

use crate::sess::Session;
use crate::io::file::FilePath;

pub mod symbols;
pub mod sess;
pub mod args;
pub mod io;
pub mod lexer;
pub mod parser;
pub mod ast;

fn main() {
    let now = Instant::now();
    let sess_result = Session::initiate(FilePath::new("samples/debug.cx"));
    match sess_result {
        Ok(_) => (),
        Err(ref err) => eprintln!("{}", err),
    }
    let status_string = match sess_result {
        Ok(_) => "SUCCESS".green(),
        Err(_) => "FAILURE".red(),
    };
    eprintln!(
        "[{}, took {:.2}s]",
        status_string,
        now.elapsed().as_secs_f32()
    );
}
