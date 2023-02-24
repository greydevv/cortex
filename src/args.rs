use clap::{ Arg, ArgAction, command };
use clap::parser::ArgMatches;

use crate::io::error::Result;

pub fn parse_args() -> Result<ArgMatches> {
    command!()
        .author("Greyson Murray, greyson.murray@gmail.com")
        .version("0.0.1")
        .arg(Arg::new("debug")
             .short('d')
             .help("display token stream instead of compiling")
             .required(false)
             .action(ArgAction::SetTrue)
        )
        .try_get_matches()
        .map_err(|_| {
            todo!("implement argument errors")
        })
}
