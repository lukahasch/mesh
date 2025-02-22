use std::num::{ParseFloatError, ParseIntError};

use nom::{
    Err, IResult, Parser,
    branch::alt,
    bytes::{tag, take_while, take_while1},
    combinator::cut,
    multi::separated_list0,
};

use crate::{Expression, Node, error::Error};

use super::{
    expression,
    lib::{Extensions, IgnoreAnd, Source, begin_block, debug, end_block, identifier, surrounded},
    program,
};

pub fn comment(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    tag("//")
        .ignore_and(take_while(|c| c != '\n'))
        .map_span(|span, source| Node {
            tag: (),
            span,
            expression: Expression::Comment(source.as_str()),
        })
        .parse_complete(source)
}

pub fn number(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    take_while1(|c: char| c.is_numeric() || c == '.')
        .map_res(|source: Source| {
            Ok(Node {
                tag: (),
                span: source.span(),
                expression: if source.contains('.') {
                    Expression::Float(source.as_str().parse().map_err(|e: ParseFloatError| {
                        Error::InvalidLiteral {
                            position: source.span(),
                            problem: e.to_string(),
                        }
                    })?)
                } else {
                    Expression::Integer(source.as_str().parse().map_err(|e: ParseIntError| {
                        Error::InvalidLiteral {
                            position: source.span(),
                            problem: e.to_string(),
                        }
                    })?)
                },
            })
        })
        .parse_complete(source)
}

pub fn string(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    tag::<_, Source, _>("\"")
        .ignore_and(take_while(|c: char| c != '"'))
        .and_ignore(cut(tag("\"")))
        .map_failure_span(|e: Error, span| Err::Failure(Error::UnterminatedString(e.span() + span)))
        .map_span(|span, source| Node {
            tag: (),
            span,
            expression: Expression::String(source.as_str()),
        })
        .parse_complete(source)
}

pub fn boolean(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    alt((
        tag("true").map(|source: Source| Node {
            tag: (),
            span: source.span(),
            expression: Expression::Boolean(true),
        }),
        tag("false").map(|source: Source| Node {
            tag: (),
            span: source.span(),
            expression: Expression::Boolean(false),
        }),
    ))
    .parse_complete(source)
}

pub fn variable(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    dbg!("variabl", source);
    identifier
        .map(|source: Source| Node {
            tag: (),
            span: source.span(),
            expression: Expression::Variable(source.as_str()),
        })
        .parse_complete(source)
}

pub fn list(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    surrounded('[', cut(separated_list0(tag(","), expression)), ']')
        .map_span(|span, list| Node {
            tag: (),
            span,
            expression: Expression::List(list),
        })
        .parse_complete(source)
}

pub fn block(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    begin_block
        .and(cut(program))
        .and(cut(end_block))
        .map_span(|span, ((_, nodes), _)| Node {
            tag: (),
            span,
            expression: Expression::Block(nodes),
        })
        .parse_complete(source)
}
