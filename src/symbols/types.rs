//! Symbols relevant to typing.

use std::fmt;

use crate::symbols::{ Literal, MaybeFrom };

/// The various kinds of built-in types recognized by the compiler.
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TyKind {
    // UInt(IntSize),
    Int(IntSize),
    Bool,
    Str,
    Enum,
    EnumMember,
    Void,
    Infer,
    Lookup,
}

impl TyKind {
    /// Checks whether two types are compatible with one another, returning an error if not.
    pub fn compat(&self, other: &TyKind) -> Option<&TyKind> {
        if *self == *other {
            Some(self)
        } else {
            None
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
            TyKind::Enum => String::from("Enum"),
            TyKind::EnumMember => String::from("EnumMember"),
            TyKind::Void => String::from("void"),
            TyKind::Infer => String::from("Infer"),
            TyKind::Lookup => String::from("Lookup"),
        }
    }
}

impl MaybeFrom<String> for TyKind {
    fn maybe_from(value: &String) -> Option<TyKind> {
        match value.as_str() {
            "i8" => Some(TyKind::Int(IntSize::N8)),
            "i16" => Some(TyKind::Int(IntSize::N16)),
            "i32" => Some(TyKind::Int(IntSize::N32)),
            "i64" => Some(TyKind::Int(IntSize::N64)),
            "i128" => Some(TyKind::Int(IntSize::N128)),
            "bool" => Some(TyKind::Bool),
            _ => None,
        }
    }
}

/// The various sizes of both signed and unsigned integers.
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
