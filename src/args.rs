//! Helpers for parsing command line arguments.

use clap::{ Arg, ArgAction, command };
use clap::parser::ArgMatches;

use crate::io::error::{ Result, CortexError };

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
            // Box::new(CortexError::ArgError("argument error".into()))
            // let ctx = e.context();
            // for (kind, val) in ctx {
            //     match kind {
            //         ContextKind::InvalidArg => {
            //             let msg = format!("invalid argument: {}", val);
            //             Err(Box::new(
            //                 CortexError::ArgError()
            //             ))
            //         },
            //         _ => todo!("handle arg error: {}", e)
            //     }
            // }
        // )
}
