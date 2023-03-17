use std::fmt;

use crate::symbols::{ Literal, MaybeFrom };
use crate::io::error::{ Result, CortexError };

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TyKind {
    Int(usize),
    Bool,
    Str,
    Void,
    Infer,
    Lookup,
}

impl TyKind {
    pub fn compat(&self, other: &TyKind) -> Result {
        if *self == *other {
            Ok(())
        } else {
            Err(CortexError::TypeError(
                format!("expected type '{}' but got type '{}'", *self, *other)
            ).into())
        }
    }
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.literal())
    }
}

impl Literal for TyKind {
    fn literal(&self) -> String {
        match &self {
            TyKind::Int(size) => format!("i{}", size),
            TyKind::Bool => format!("bool"),
            TyKind::Str => format!("str"),
            TyKind::Void => String::from("void"),
            TyKind::Infer => String::from("Infer"),
            TyKind::Lookup => String::from("Lookup"),
        }
    }
}

impl MaybeFrom<String> for TyKind {
    fn maybe_from(value: &String) -> Option<TyKind> {
        match value.as_str() {
            "i32" => Some(TyKind::Int(32)),
            _ => None,
        }
    }
}
