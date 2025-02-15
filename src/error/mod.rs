use nom::error::{ErrorKind, FromExternalError, ParseError};

use crate::{Span, parser::lib::Source};

#[derive(Debug, Clone, PartialEq)]
#[repr(u32)]
#[non_exhaustive]
pub enum Error {
    Debug(Span, &'static str) = 0,
    InternalKind(Span, ErrorKind) = 1,
    InternalChar(Span, char) = 2,
    InvalidLiteral { position: Span, problem: String } = 3,
    Or(Vec<Error>) = 4,
    Multi(Vec<Error>) = 5,
}

impl<'a> ParseError<Source<'a>> for Error {
    fn from_char(input: Source<'a>, c: char) -> Self {
        Self::InternalChar(input.span(), c)
    }

    fn from_error_kind(input: Source<'a>, kind: nom::error::ErrorKind) -> Self {
        Self::InternalKind(input.span(), kind)
    }

    fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::Or(mut errors), Self::Or(mut other_errors)) => {
                errors.append(&mut other_errors);
                Self::Or(errors)
            }
            (Self::Or(mut errors), other) => {
                errors.push(other);
                Self::Or(errors)
            }
            (s, Self::Or(mut errors)) => {
                errors.push(s);
                Self::Or(errors)
            }
            (s, other) => Self::Or(vec![s, other]),
        }
    }

    fn append(input: Source<'a>, kind: nom::error::ErrorKind, other: Self) -> Self {
        let error = Self::from_error_kind(input, kind);
        match (error, other) {
            (Self::Multi(mut errors), Self::Multi(mut other_errors)) => {
                errors.append(&mut other_errors);
                Self::Multi(errors)
            }
            (Self::Multi(mut errors), other) => {
                errors.push(other);
                Self::Multi(errors)
            }
            (error, Self::Multi(mut errors)) => {
                errors.push(error);
                Self::Multi(errors)
            }
            (error, other) => Self::Multi(vec![error, other]),
        }
    }
}

impl<'a> FromExternalError<Source<'_>, Error> for Error {
    fn from_external_error(input: Source<'_>, kind: nom::error::ErrorKind, e: Self) -> Self {
        e
    }
}
