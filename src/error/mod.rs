use ariadne::{Cache, Color, Label, Report, ReportKind, sources};
use nom::error::{ErrorKind, FromExternalError, ParseError};

use crate::{Span, parser::lib::Source};

#[derive(Debug, Clone, PartialEq)]
#[repr(u32)]
#[non_exhaustive]
pub enum Error<'a> {
    Debug(Span, &'a str) = 0,
    InternalKind(Span, ErrorKind) = 1,
    InternalChar(Span, char) = 2,
    InvalidLiteral { position: Span, problem: String } = 3,
    Or(Vec<Error<'a>>) = 4,
    Multi(Vec<Error<'a>>) = 5,
    KeywordAsIdentifier(Source<'a>) = 6,
}

impl<'a> ParseError<Source<'a>> for Error<'a> {
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

impl<'a> FromExternalError<Source<'_>, Error<'a>> for Error<'a> {
    fn from_external_error(input: Source<'_>, kind: nom::error::ErrorKind, e: Self) -> Self {
        e
    }
}

impl<'a> Error<'a> {
    pub fn span(&self) -> Span {
        match self {
            Self::Debug(span, _) => span.clone(),
            Self::InternalKind(span, _) => span.clone(),
            Self::InternalChar(span, _) => span.clone(),
            Self::InvalidLiteral { position, .. } => position.clone(),
            Self::Or(errors) => {
                let first = errors.first().unwrap().span();
                errors
                    .iter()
                    .skip(1)
                    .fold(first, |span, error| span + error.span())
            }
            Self::Multi(errors) => {
                let first = errors.first().unwrap().span();
                errors
                    .iter()
                    .skip(1)
                    .fold(first, |span, error| span + error.span())
            }
            Self::KeywordAsIdentifier(source) => source.span(),
        }
    }

    pub fn message(&self) -> String {
        match self {
            Self::Debug(_, message) => message.to_string(),
            Self::InternalKind(_, kind) => format!("Internal error: {:?}", kind),
            Self::InternalChar(_, c) => format!("Internal error: {:?}", c),
            Self::InvalidLiteral { problem, .. } => format!("Invalid literal: {}", problem),
            Self::Or(errors) => errors
                .iter()
                .map(Error::message)
                .collect::<Vec<_>>()
                .join(", "),
            Self::Multi(errors) => errors
                .iter()
                .map(Error::message)
                .collect::<Vec<_>>()
                .join(", "),
            Self::KeywordAsIdentifier(_) => "Keyword used as identifier".to_string(),
        }
    }

    pub fn code(&self) -> u32 {
        // SAFETY: Because `Self` is marked `repr(u32)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u32>() }
    }

    pub fn report(&self) -> Report<'_, Span> {
        Report::build(ReportKind::Error, self.span())
            .with_message(self.message())
            .with_code(self.code())
            .with_label(
                Label::new(self.span())
                    .with_color(Color::Red)
                    .with_message(self.message()),
            )
            .finish()
    }

    pub fn cache(&self) -> impl Cache<&'static str> {
        if self.span().file.is_empty() {
            dbg!(self);
            dbg!(self.span());
        }
        let contents = std::fs::read_to_string(self.span().file).unwrap();
        sources(vec![(self.span().file, contents)])
    }
}
