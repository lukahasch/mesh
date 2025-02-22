use std::ops::Deref;

use crate::{Span, error::Error};
use nom::{
    Compare, IResult, Input, Needed, Offset, Parser,
    branch::alt,
    bytes::{
        complete::{tag, take_while},
        take_while1,
    },
    character::one_of,
    combinator::{eof, value},
    error::ParseError,
    multi::{count, many0},
};

use super::KEYWORDS;

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Source<'a> {
    pub original: &'a str,
    pub current: &'a str,
    pub file: &'static str,
    pub indent: usize,
}

impl<'a> Source<'a> {
    pub fn new(file: &'static str, content: &'a str) -> Self {
        Self {
            original: content,
            current: content,
            file,
            indent: 0,
        }
    }

    pub fn offset(&self) -> usize {
        self.current.as_ptr() as usize - self.original.as_ptr() as usize
    }

    pub fn char(&self) -> Option<char> {
        self.current.chars().next()
    }

    pub fn span(&self) -> Span {
        Span {
            file: self.file,
            range: self.offset()..self.offset() + self.current.len(),
        }
    }

    pub fn current(&self) -> Span {
        Span {
            file: self.file,
            range: self.offset()..self.offset(),
        }
    }

    pub fn as_str<'b>(&'b self) -> &'a str {
        self.current
    }

    pub fn leak(&self) -> Source<'static> {
        Source {
            original: Box::leak(Box::from(self.original)),
            current: Box::leak(Box::from(self.current)),
            file: Box::leak(Box::from(self.file)),
            indent: self.indent,
        }
    }

    pub fn eof(&self) -> Self {
        Self {
            original: self.original,
            current: "",
            file: self.file,
            indent: self.indent,
        }
    }

    pub fn back(&self, n: usize) -> Self {
        Self {
            original: self.original,
            current: if self.offset() >= n {
                &self.original[self.offset() - n..]
            } else {
                &self.original[..self.offset()]
            },
            file: self.file,
            indent: self.indent,
        }
    }
}

impl Deref for Source<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.current
    }
}

impl Input for Source<'_> {
    type Item = char;
    type Iter = impl Iterator<Item = char>;
    type IterIndices = impl Iterator<Item = (usize, char)>;

    fn input_len(&self) -> usize {
        self.current.input_len()
    }

    fn slice_index(&self, index: usize) -> Result<usize, Needed> {
        self.current.slice_index(index)
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.current.iter_indices()
    }

    fn iter_elements(&self) -> Self::Iter {
        self.current.iter_elements()
    }

    fn take(&self, index: usize) -> Self {
        Self {
            original: self.original,
            current: self.current.take(index),
            file: self.file,
            indent: self.indent,
        }
    }

    fn take_from(&self, index: usize) -> Self {
        Self {
            original: self.original,
            current: self.current.take_from(index),
            file: self.file,
            indent: self.indent,
        }
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        let (left, right) = self.current.take_split(index);
        (
            Self {
                original: self.original,
                current: left,
                file: self.file,
                indent: self.indent,
            },
            Self {
                original: self.original,
                current: right,
                file: self.file,
                indent: self.indent,
            },
        )
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.current.position(predicate)
    }
}

impl<'a> Offset for Source<'a> {
    fn offset(&self, second: &Self) -> usize {
        self.current.offset(second.current)
    }
}

impl<'b> Compare<&'b str> for Source<'_> {
    fn compare(&self, t: &'b str) -> nom::CompareResult {
        self.current.compare(t)
    }

    fn compare_no_case(&self, t: &'b str) -> nom::CompareResult {
        self.current.compare_no_case(t)
    }
}

pub fn ws<'a, P: Parser<Source<'a>>>(
    parser: P,
) -> impl Parser<
    Source<'a>,
    Output = <P as Parser<Source<'a>>>::Output,
    Error = <P as Parser<Source<'a>>>::Error,
> {
    take_while(|c| c == ' ' || c == '\t')
        .map(|_| ())
        .and(parser)
        .map(|(_, output)| output)
}

/// four spaces or one tab
fn one_indent(source: Source<'_>) -> IResult<Source<'_>, (), Error> {
    alt((value((), tag("    ")), value((), tag("\t"))))
        .map_err_word(|_: nom::Err<Error>, word| nom::Err::Error(Error::ExpectedIndent(word)))
        .parse_complete(source)
}

/// either a semicolon or a newline and the current number of indents
pub fn boundary<'a>(source: Source<'a>) -> IResult<Source<'a>, (), Error<'a>> {
    let next_indents: impl Parser<Source> =
        |source: Source<'a>| -> IResult<Source<'a>, (), Error<'a>> {
            value((), count(one_indent, source.indent)).parse_complete(source)
        };
    let mut a = ws(tag::<&str, Source<'_>, Error<'_>>(";"));
    let mut b = ws(tag("\n").and(next_indents));
    if let Ok((s, _)) = a.parse_complete(source) {
        Ok((s, ()))
    } else if let Ok((s, _)) = b.parse_complete(source) {
        Ok((s, ()))
    } else {
        Err(nom::Err::Error(Error::Debug(
            source.current(),
            "expected boundary",
        )))
    }
}

pub fn begin_block(source: Source<'_>) -> IResult<Source<'_>, (), Error<'_>> {
    one_indent(source).map(|(mut source, _)| {
        source.indent += 1;
        (source, ())
    })
}

/// use as boundary.or(end_block)
pub fn end_block(mut source: Source<'_>) -> IResult<Source<'_>, (), Error> {
    source.indent -= 1;
    Ok((source, ()))
}

pub fn debug<'a, P: Parser<Source<'a>>>(
    p: P,
) -> impl Parser<Source<'a>, Output = P::Output, Error = P::Error>
where
    P::Output: std::fmt::Debug,
    <P as Parser<Source<'a>>>::Error: std::fmt::Debug,
{
    (|debug_source| Ok((dbg!(debug_source), ())))
        .ignore_and(p)
        .map(|debug_output| dbg!(debug_output))
        .map_err(|debug_error| dbg!(debug_error))
}

pub trait IgnoreAnd<'a>: Parser<Source<'a>> {
    fn ignore_and<P: Parser<Source<'a>, Error = <Self as Parser<Source<'a>>>::Error>>(
        self,
        parser: P,
    ) -> impl Parser<Source<'a>, Output = P::Output, Error = <Self as Parser<Source<'a>>>::Error>;

    fn and_ignore<P: Parser<Source<'a>, Error = <Self as Parser<Source<'a>>>::Error>>(
        self,
        parser: P,
    ) -> impl Parser<
        Source<'a>,
        Output = <Self as Parser<Source<'a>>>::Output,
        Error = <Self as Parser<Source<'a>>>::Error,
    >;
}

/// return a singular word, this means either an identifier, the string of symbols or a space
pub fn word<'a>(source: Source<'a>) -> IResult<Source<'a>, Source<'a>, Error<'a>> {
    take_while1(|c: char| c.is_alphanumeric() || c == '_')
        .or(tag("\n").or(tag(" ").or(tag("\t")).or(tag(";"))))
        .or(take_while1(|c: char| {
            !(c.is_alphanumeric() || c == '_') && !c.is_whitespace()
        }))
        .parse_complete(source)
}

impl<'a, T> IgnoreAnd<'a> for T
where
    T: Parser<Source<'a>>,
{
    fn ignore_and<P: Parser<Source<'a>, Error = <Self as Parser<Source<'a>>>::Error>>(
        self,
        parser: P,
    ) -> impl Parser<Source<'a>, Output = P::Output, Error = <Self as Parser<Source<'a>>>::Error>
    {
        self.and(parser).map(|(_, output)| output)
    }

    fn and_ignore<P: Parser<Source<'a>, Error = <Self as Parser<Source<'a>>>::Error>>(
        self,
        parser: P,
    ) -> impl Parser<
        Source<'a>,
        Output = <Self as Parser<Source<'a>>>::Output,
        Error = <Self as Parser<Source<'a>>>::Error,
    > {
        self.and(parser).map(|(output, _)| output)
    }
}

pub trait Extensions<'a>: Parser<Source<'a>> {
    fn map_err<F, E>(
        self,
        f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = E>
    where
        F: FnMut(nom::Err<<Self as Parser<Source<'a>>>::Error>) -> nom::Err<E>,
        E: ParseError<Source<'a>>;

    fn map_span<F, O>(
        self,
        f: F,
    ) -> impl Parser<Source<'a>, Output = O, Error = <Self as Parser<Source<'a>>>::Error>
    where
        F: Fn(Span, <Self as Parser<Source<'a>>>::Output) -> O;

    fn map_err_word<F, E>(
        self,
        f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = E>
    where
        F: FnMut(nom::Err<<Self as Parser<Source<'a>>>::Error>, Source<'a>) -> nom::Err<E>,
        E: ParseError<Source<'a>>,
        Error<'a>: Into<E>;

    fn map_err_span<F, E>(
        self,
        f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = E>
    where
        F: FnMut(nom::Err<<Self as Parser<Source<'a>>>::Error>, Span) -> nom::Err<E>,
        E: ParseError<Source<'a>>;

    fn map_failure_span<F>(
        self,
        f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = Self::Error>
    where
        F: FnMut(<Self as Parser<Source<'a>>>::Error, Span) -> nom::Err<Self::Error>;
}

impl<'a, T> Extensions<'a> for T
where
    T: Parser<Source<'a>>,
{
    fn map_err<F, E>(
        mut self,
        mut f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = E>
    where
        F: FnMut(nom::Err<<Self as Parser<Source<'a>>>::Error>) -> nom::Err<E>,
        E: ParseError<Source<'a>>,
    {
        move |source| self.parse_complete(source).map_err(|e| f(e))
    }

    fn map_span<F, O>(
        mut self,
        f: F,
    ) -> impl Parser<Source<'a>, Output = O, Error = <Self as Parser<Source<'a>>>::Error>
    where
        F: Fn(Span, <Self as Parser<Source<'a>>>::Output) -> O,
    {
        move |source| {
            let (s2, output) = self.parse_complete(source)?;
            Ok((s2, f(source.current() + s2.current(), output)))
        }
    }

    fn map_err_word<F, E>(
        mut self,
        mut f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = E>
    where
        F: FnMut(nom::Err<<Self as Parser<Source<'a>>>::Error>, Source<'a>) -> nom::Err<E>,
        E: ParseError<Source<'a>>,
        Error<'a>: Into<E>,
    {
        move |source: Source<'a>| {
            let output = self.parse_complete(source);
            match output {
                Ok((s, o)) => Ok((s, o)),
                Err(e) => Err(f(
                    e,
                    ws(word)
                        .parse_complete(source)
                        .map_err(|e| match e {
                            nom::Err::Error(e) => nom::Err::Error(e.into()),
                            nom::Err::Failure(e) => nom::Err::Failure(e.into()),
                            nom::Err::Incomplete(n) => nom::Err::Incomplete(n),
                        })?
                        .1,
                )),
            }
        }
    }

    fn map_err_span<F, E>(
        mut self,
        mut f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = E>
    where
        F: FnMut(nom::Err<<Self as Parser<Source<'a>>>::Error>, Span) -> nom::Err<E>,
        E: ParseError<Source<'a>>,
    {
        move |source: Source<'a>| {
            let output = self.parse_complete(source);
            match output {
                Ok((s, o)) => Ok((s, o)),
                Err(e) => Err(f(e, source.current())),
            }
        }
    }

    fn map_failure_span<F>(
        mut self,
        mut f: F,
    ) -> impl Parser<Source<'a>, Output = <Self as Parser<Source<'a>>>::Output, Error = Self::Error>
    where
        F: FnMut(<Self as Parser<Source<'a>>>::Error, Span) -> nom::Err<Self::Error>,
    {
        move |source: Source<'a>| {
            let output = self.parse_complete(source);
            match output {
                Ok((s, o)) => Ok((s, o)),
                Err(nom::Err::Failure(e)) => Err(f(e, source.current())),
                Err(e) => Err(e),
            }
        }
    }
}

pub trait Inner {
    type E;
    fn inner(self) -> Self::E;
    fn inner_ref(&self) -> &Self::E;
}

impl<E> Inner for nom::Err<E> {
    type E = E;
    fn inner(self) -> E {
        match self {
            nom::Err::Error(e) => e,
            nom::Err::Failure(e) => e,
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }

    fn inner_ref(&self) -> &E {
        match self {
            nom::Err::Error(e) => e,
            nom::Err::Failure(e) => e,
            nom::Err::Incomplete(_) => unreachable!(),
        }
    }
}

pub fn surrounded<'a, P: Parser<Source<'a>, Error = Error<'a>>>(
    before: char,
    mut p: P,
    after: char,
) -> impl Parser<Source<'a>, Output = P::Output, Error = Error<'a>> {
    move |source: Source<'a>| {
        let mut buf = [0u8; 4];
        let before_s: &str = before.encode_utf8(&mut buf);
        let mut find = match ws(tag::<_, Source, Error>(before_s)).parse_complete(source) {
            Ok((s, _)) => s,
            Err(e) => {
                return Err(nom::Err::Error(Error::ExpectedOpeningParenthesis(
                    e.inner().span(),
                    before,
                )));
            }
        };
        let start = find;
        let mut count = 1;
        while count > 0 {
            let before_pos = find.position(|c| c == before);
            let after_pos = find.position(|c| c == after);
            if after_pos.is_none() {
                return Err(nom::Err::Failure(
                    Error::MatchingClosingParenthesisNotFound {
                        opening: source.current(),
                        open: before,
                        close: after,
                        end: find.current(),
                    },
                ));
            }
            if before_pos.is_none() || before_pos.unwrap() > after_pos.unwrap() {
                count -= 1;
                find = find.take_from(after_pos.unwrap() + 1);
            } else {
                count += 1;
                find = find.take_from(before_pos.unwrap() + 1);
            }
        }
        let source = start.take(find.offset() - start.offset() - 1);
        let (source, output) = p.parse_complete(source)?;
        if many0(one_of("\n; \t"))
            .and(eof::<Source, Error>)
            .parse_complete(source)
            .is_err()
        {
            return Err(nom::Err::Error(Error::ExpectedEof(source.span())));
        }
        Ok((find, output))
    }
}

pub fn not_eof<'a>(source: Source<'a>) -> IResult<Source<'a>, (), Error<'a>> {
    if many0(one_of("\n; \t"))
        .and(eof::<Source, Error>)
        .parse_complete(source)
        .is_ok()
    {
        return Err(nom::Err::Error(Error::UnexpectedEof(source.span())));
    }
    Ok((source, ()))
}

pub trait ReplaceError<'a, E>: Parser<Source<'a>, Error = Error<'a>> {
    fn replace_error(
        self,
        error: impl FnMut(Error<'a>) -> nom::Err<E>,
    ) -> impl Parser<Source<'a>, Output = Self::Output, Error = E>;

    fn replace_error_word(
        self,
        error: impl FnMut(Error<'a>, Source<'a>) -> nom::Err<E>,
    ) -> impl Parser<Source<'a>, Output = Self::Output, Error = E>;
}

impl<'a, P: Parser<Source<'a>, Error = Error<'a>, Output = O>, O> ReplaceError<'a, Error<'a>>
    for P
{
    fn replace_error(
        self,
        mut error: impl FnMut(Error<'a>) -> nom::Err<Error<'a>>,
    ) -> impl Parser<Source<'a>, Output = Self::Output, Error = Error<'a>> {
        self.map_err(move |e| {
            if e.inner_ref().replaceable() {
                error(e.inner())
            } else {
                e
            }
        })
    }

    fn replace_error_word(
        self,
        mut error: impl FnMut(Error<'a>, Source<'a>) -> nom::Err<Error<'a>>,
    ) -> impl Parser<Source<'a>, Output = Self::Output, Error = Error<'a>> {
        self.map_err_word(move |e, s| {
            if e.inner_ref().replaceable() {
                error(e.inner(), s)
            } else {
                e
            }
        })
    }
}

pub fn identifier(source: Source) -> IResult<Source, Source, Error> {
    if Some(true) == source.char().map(|c| c.is_ascii_alphabetic() || c == '_') {
        let (a, b) =
            source.split_at_position_complete(|c| !(c.is_ascii_alphanumeric() || c == '_'))?;
        if KEYWORDS.contains(&b.as_str()) {
            Err(nom::Err::Error(Error::KeywordAsIdentifier(b)))
        } else {
            Ok((a, b))
        }
    } else {
        Err(nom::Err::Error(Error::ExpectedFound {
            span: source.current(),
            expected: "identifier",
            found: word(source)?.1,
        }))
    }
}
