//! The various symbols used throughout the compilation process.
pub mod token;
pub mod kinds;
pub mod types;

pub use token::*;
pub use kinds::*;
pub use types::*;

/// A trait describing the length of a symbol.
///
/// This trait is used for symbols whose lengths are often queried where the `is_empty` method is
/// not necessary
pub trait Len {
    fn len(&self) -> usize;
}

/// Safe type conversions by way of [`Option`] when a type conversion may not be possible.
///
/// This method is different from [`TryFrom`] as it returns an [`Option`] instead of a [`Result`].
/// Therefore, this method should be used in cases in which the error information is not important.
/// Instead of using a placeholder when destructuring an error from [`TryFrom`], use `MaybeFrom`
/// to handle [`None`] to be more explicit in these cases.
///
/// ```rust
/// // TryFrom
/// match SomeType::try_from(other_type) {
///     Ok(converted_value) => ...,
///     Err(_) => ...
/// }
///
/// // MaybeFrom
/// match SomeType::maybe_from(&other_type) {
///     Some(converted_value) => ...,
///     None => ...
/// }
/// ```
pub trait MaybeFrom<T>: Sized {
    fn maybe_from(value: &T) -> Option<Self>;
}

/// A trait describing a [`String`] literal associated with a symbol.
pub trait Literal {
    fn literal(&self) -> String;
}
