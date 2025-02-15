use std::ops::Deref;

use crate::{Span, error::Error};
use nom::{
    Compare, IResult, Input, Needed, Parser,
    branch::alt,
    bytes::complete::{tag, take_while},
    combinator::{eof, value},
    multi::count,
};

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
}

impl Deref for Source<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.current
    }
}

impl<'a> Input for Source<'a> {
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

impl<'a, 'b> Compare<&'b str> for Source<'a> {
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
fn one_indent<'a>(source: Source<'a>) -> IResult<Source<'a>, (), Error> {
    alt((value((), tag("    ")), value((), tag("\t")))).parse_complete(source)
}

/// either a semicolon or a newline and the current number of indents
pub fn boundary<'a>(source: Source<'a>) -> IResult<Source<'a>, (), Error> {
    let next_indents: impl Parser<Source> = |source: Source<'a>| -> IResult<Source<'a>, (), Error> {
        value((), count(one_indent, source.indent)).parse_complete(source)
    };
    let mut a = ws(tag::<&str, Source<'_>, Error>(";"));
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

pub fn begin_block<'a>(source: Source<'a>) -> IResult<Source<'a>, (), Error> {
    one_indent(source).map(|(mut source, _)| {
        source.indent += 1;
        (source, ())
    })
}

/// use as boundary.or(end_block)
pub fn end_block<'a>(mut source: Source<'a>) -> IResult<Source<'a>, (), Error> {
    source.indent -= 1;
    Ok((source, ()))
}

pub fn dbg<'a, P: Parser<Source<'a>>>(
    p: P,
) -> impl Parser<Source<'a>, Output = P::Output, Error = P::Error>
where
    P::Output: std::fmt::Debug,
{
    p.map(|output| dbg!(output))
}
