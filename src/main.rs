#![feature(map_try_insert)]

use std::time::Instant;

use colored::Colorize;

use cortex_driver::compile;
use cortex_symbols::file::FilePath;

fn main() {
    let now = Instant::now();
    let compile_result = compile(FilePath::new("samples/debug.cx"));
    match compile_result {
        Ok(_) => (),
        Err(ref err) => eprintln!("{}", err),
    }
    let status_string = match compile_result {
        Ok(_) => "SUCCESS".green(),
        Err(_) => "FAILURE".red(),
    };
    eprintln!(
        "[{}, took {:.2}s]",
        status_string,
        now.elapsed().as_secs_f32()
    );
}
