use std::convert::From;
use std::fmt;

use crate::io::file::{ FilePos, FileSpan };
use crate::io::error::{ Result, CortexError };

pub enum TokenPresence {
    Optional(Count),
    Required(Count),
}

// we can create a grammar from structures like this
// in parser, create a Vec<Token> and ExpectedToken.tok will reference one of those tokens
// each time an AST obj is returned, clear the Vec and move to the next structure
// pub struct ExpectedToken<'a> {
//     tok: &'a Token,
//     count: Count,
// }

pub enum Count {
    Range(std::ops::Range<u8>),
    SomeToInf(u8),
    Some(u8),
    Inf,
}
