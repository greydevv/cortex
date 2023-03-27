use std::fmt;

use crate::symbols::{ Literal, MaybeFrom };
use crate::io::error::{
    Result,
    CortexError,
    Diagnostic,
    DiagnosticKind,
};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TyKind {
    // UInt(IntSize),
    Int(IntSize),
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
            Err(CortexError(
                vec![
                    Diagnostic::new(
                        format!("expected type '{}' but got type '{}'", *self, *other),
                        DiagnosticKind::Error,
                        // some span needed here
                    )
                ]
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
            TyKind::Int(size) => format!("i{}", *size as u32),
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
            // "i8" => Some(TyKind::Int(IntSize::N8)),
            // "i16" => Some(TyKind::Int(IntSize::N16)),
            "i32" => Some(TyKind::Int(IntSize::N32)),
            // "i64" => Some(TyKind::Int(IntSize::N64)),
            // "i128" => Some(TyKind::Int(IntSize::N128)),
            _ => None,
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum IntSize {
    N8 = 8,
    N16 = 16,
    N32 = 32,
    N64 = 64,
    N128 = 128,
}


impl Literal for IntSize {
    fn literal(&self) -> String {
        (*self as u32).to_string()
    }
}
