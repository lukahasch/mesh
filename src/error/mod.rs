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
    InvalidLiteral {
        position: Span,
        problem: String,
    } = 3,
    KeywordAsIdentifier(Source<'a>) = 6,
    ExpectedFound {
        span: Span,
        expected: &'static str,
        found: Source<'a>,
    } = 7,
    UnterminatedString(Span) = 8,
    ExpectedOpeningParenthesis(Span, char) = 9,
    MatchingClosingParenthesisNotFound {
        opening: Span,
        open: char,
        close: char,
        end: Span,
    } = 10,
    ExpectedEof(Span) = 11,
    UnexpectedEof(Span) = 12,
}

impl<'a> ParseError<Source<'a>> for Error<'a> {
    fn from_char(input: Source<'a>, c: char) -> Self {
        Self::InternalChar(input.span(), c)
    }

    fn from_error_kind(input: Source<'a>, kind: nom::error::ErrorKind) -> Self {
        Self::InternalKind(input.span(), kind)
    }

    fn or(self, other: Self) -> Self {
        self
    }

    fn append(input: Source<'a>, kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a> FromExternalError<Source<'_>, Error<'a>> for Error<'a> {
    fn from_external_error(input: Source<'_>, kind: nom::error::ErrorKind, e: Self) -> Self {
        e
    }
}

impl<'a> Error<'a> {
    pub fn replaceable(&self) -> bool {
        match self {
            Self::Debug(_, _) => true,
            Self::InternalKind(_, _) => true,
            Self::InternalChar(_, _) => true,
            _ => false,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Debug(span, _) => span.clone(),
            Self::InternalKind(span, _) => span.clone(),
            Self::InternalChar(span, _) => span.clone(),
            Self::InvalidLiteral { position, .. } => position.clone(),
            Self::KeywordAsIdentifier(source) => source.span(),
            Self::ExpectedFound { span, .. } => span.clone(),
            Self::UnterminatedString(span) => span.clone(),
            Self::ExpectedOpeningParenthesis(span, _) => span.clone(),
            Self::MatchingClosingParenthesisNotFound { opening, end, .. } => opening.clone() + end,
            Self::ExpectedEof(span) => span.clone(),
            Self::UnexpectedEof(span) => span.clone(),
        }
    }

    pub fn message(&self) -> String {
        match self {
            Self::Debug(_, message) => message.to_string(),
            Self::InternalKind(_, kind) => format!("Internal error: {:?}", kind),
            Self::InternalChar(_, c) => format!("Internal error: {:?}", c),
            Self::InvalidLiteral { problem, .. } => format!("Invalid literal: {}", problem),
            Self::KeywordAsIdentifier(_) => "Keyword used as identifier".to_string(),
            Self::ExpectedFound {
                expected, found, ..
            } => {
                format!("Expected {}, found '{}'", expected, found.as_str())
            }
            Self::UnterminatedString { .. } => "Unterminated string".to_string(),
            Self::ExpectedOpeningParenthesis(_, kind) => {
                format!("Expected opening parenthesis '{}'", kind)
            }
            Self::MatchingClosingParenthesisNotFound { open, close, .. } => {
                format!(
                    "Matching closing parenthesis '{}' not found for '{}'",
                    close, open
                )
            }
            Self::ExpectedEof(_) => "Expected end of file".to_string(),
            Self::UnexpectedEof(_) => "Unexpected end of file".to_string(),
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
