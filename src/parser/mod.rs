use lib::{
    Extensions, IgnoreAnd, Inner, ReplaceError, Source, begin_block, better_eof, boundary, debug,
    end_block, not_eof, surrounded, word, ws,
};
use nom::{
    Err, IResult, Input, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::one_of,
    combinator::{cut, eof, fail, opt, peek},
    multi::{many0, many1, separated_list0},
    number::recognize_float,
    sequence::preceded,
};
use nom_language::precedence::{Operation, precedence, unary_op};
use smallmap::Map;
use std::num::{ParseFloatError, ParseIntError};

use crate::{Expression, Field, Fields, ListContent, Node, Pattern, error::Error};

pub mod lib;

pub const KEYWORDS: [&str; 20] = [
    "if", "else", "then", "elseif", "while", "for", "true", "false", "match", "with", "let", "mut",
    "fn", "trait", "struct", "enum", "impl", "use", "where", "as",
];

pub const PARENS: [(char, char); 3] = [('(', ')'), ('[', ']'), ('{', '}')];

pub fn parse<'a>(source: Source<'a>) -> Result<Vec<Node<'a, ()>>, Error<'a>> {
    many1(ws(basics))
        .and_ignore(better_eof)
        .parse_complete(source)
        .map(|(_, nodes)| nodes)
        .map_err(|e| e.inner())
}

pub fn expression(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    alt((basics, list)).parse_complete(source)
}

pub fn list(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    surrounded('[', separated_list0(tag(","), list_content), ']')
        .map_span(|span, content| Node {
            tag: (),
            span,
            expression: Expression::List(content),
        })
        .parse_complete(source)
}

pub fn list_content(source: Source) -> IResult<Source, ListContent<'_, ()>, Error> {
    alt((
        expression.map(ListContent::Expression),
        preceded(tag("..."), expression).map(ListContent::Spread),
    ))
    .parse_complete(source)
}

pub fn basics(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    alt((
        integer.map(Expression::Integer),
        float.map(Expression::Float),
        string.map(Source::to_str).map(Expression::String),
        boolean.map(Expression::Boolean),
        identifier.map(Source::to_str).map(Expression::Variable),
    ))
    .map_span(|span, expression| Node {
        tag: (),
        span,
        expression,
    })
    .parse_complete(source)
}

pub fn identifier(source: Source) -> IResult<Source, Source, Error> {
    let (source, _) = not_eof(source)?;
    let (source, first) = take_while1(|c: char| c.is_ascii_alphabetic() || c == '_')(source)?;
    let (source, rest) = take_while(|c: char| c.is_ascii_alphanumeric() || c == '_')(source)?;
    let output = first + rest;
    if KEYWORDS.contains(&output.as_str()) {
        Err(Err::Error(Error::ExpectedIdentifierFoundKeyword {
            found: output,
        }))
    } else {
        Ok((source, output))
    }
}

pub fn integer(source: Source) -> IResult<Source, i64, Error> {
    let (source, _) = not_eof(source)?;
    take_while1::<_, _, Error>(|c: char| c.is_digit(10))
        .map_res(|output: Source| match output.parse::<i64>() {
            Ok(value) => Ok(value),
            Err(e) => Err(Error::InvalidLiteral {
                position: source.span(),
                problem: e.to_string(),
            }),
        })
        .parse_complete(source)
}

pub fn float(source: Source) -> IResult<Source, f64, Error> {
    let (source, _) = not_eof(source)?;
    recognize_float()
        .map_res(|output: Source| match output.parse::<f64>() {
            Ok(value) => Ok(value),
            Err(e) => Err(Error::InvalidLiteral {
                position: source.span(),
                problem: e.to_string(),
            }),
        })
        .parse_complete(source)
}

pub fn string(source: Source) -> IResult<Source, Source, Error> {
    let (source, _) = not_eof(source)?;
    let (source, _) = tag("\"")(source)?;
    let (source, content) = take_while(|c: char| c != '"')(source)?;
    let (source, _) = tag("\"")(source)?;
    Ok((source, content))
}

pub fn boolean(source: Source) -> IResult<Source, bool, Error> {
    let (source, _) = not_eof(source)?;
    alt((tag("true").map(|_| true), tag("false").map(|_| false))).parse_complete(source)
}
