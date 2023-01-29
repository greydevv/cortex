use std::env;
use std::error::Error;
use std::time::Instant;

use colored::Colorize;

fn main() {
    let mut args = env::args();
    args.next(); // skip executable name

    let now = Instant::now();
    let status_string = match compile(args) {
        Ok(()) => "SUCCESS".green(),
        Err(e) => {
            eprintln!("\n{}", e);
            "FAILURE".red()
        }
    }.bold();
    let elapsed = now.elapsed();
    eprintln!("\n[{}, took {:.2}s]", status_string, elapsed.as_secs_f32());
}

fn compile(mut _args: impl Iterator<Item = String>) -> Result<(), Box<dyn Error>> {
    Ok(())
}
