//! Helpers for parsing command line arguments.

use clap::{ Arg, ArgAction, command };
use clap::parser::ArgMatches;

use cortex_errors::{ Result, CortexError };

/// Parses command line arguments using the [`clap`](https://docs.rs/clap/latest/clap/) crate.
pub fn parse_args() -> Result<ArgMatches> {
    command!()
        .author("Greyson Murray, greyson.murray@gmail.com")
        .version("0.0.1")
        .arg(Arg::new("only-display-tokens")
             .long("only-toks")
             .help("only display output of lexer (token stream or errors)")
             .required(false)
             .action(ArgAction::SetTrue)
        )
        .try_get_matches()
        .map_err(|e| Box::new(CortexError::from(e)))
}
