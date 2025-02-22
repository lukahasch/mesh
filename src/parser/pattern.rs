use crate::{Expression, Pattern, error::Error};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::tag,
    combinator::{cut, fail, opt},
    multi::separated_list0,
    number,
    sequence::preceded,
};
use nom_language::precedence::{Operation, precedence, unary_op};

use super::{
    expression,
    lib::*,
    simple::{boolean, string},
};

pub fn ignore(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    tag("_")
        .map(|_: Source| Pattern::Ignore)
        .parse_complete(source)
}

pub fn rest(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    tag("..")
        .and(opt(identifier))
        .map(|(_, source)| Pattern::Rest(source.map(|source| source.as_str())))
        .parse_complete(source)
}

pub fn number_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    number::recognize_float()
        .map(|source: Source| {
            if let Ok(f) = source.as_str().parse() {
                Pattern::Integer(f)
            } else {
                Pattern::Float(source.as_str().parse().unwrap())
            }
        })
        .parse_complete(source)
}

pub fn string_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    string
        .map(|node| match node.expression {
            Expression::String(s) => Pattern::String(s),
            _ => unreachable!(),
        })
        .parse_complete(source)
}

pub fn boolean_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    boolean
        .map(|node| match node.expression {
            Expression::Boolean(b) => Pattern::Boolean(b),
            _ => unreachable!(),
        })
        .parse_complete(source)
}

pub fn capture(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    identifier
        .and(opt(preceded(ws(tag(":")), expression)))
        .map(|(source, r#type)| Pattern::Capture {
            name: source.as_str(),
            r#type: r#type.map(Box::new),
        })
        .parse_complete(source)
}

pub fn list_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    surrounded('[', cut(separated_list0(tag(","), pattern)), ']')
        .map(Pattern::List)
        .parse_complete(source)
}

pub fn destructure(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    opt(surrounded('(', cut(expression), ')'))
        .and(surrounded(
            '{',
            cut(separated_list0(
                ws(tag(",")),
                (ws(identifier), ws(tag(":")), cut(pattern)),
            )),
            '}',
        ))
        .map(|(r#type, fields)| Pattern::Destructure {
            r#type: r#type.map(Box::new),
            fields: fields
                .into_iter()
                .map(|(identifier, _, pattern)| (identifier.as_str(), pattern))
                .collect(),
        })
        .parse_complete(source)
}

pub fn variant(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    identifier
        .and(surrounded(
            '(',
            cut(separated_list0(tag(","), pattern)),
            ')',
        ))
        .map(|(source, fields)| Pattern::Variant {
            name: source.as_str(),
            fields,
        })
        .parse_complete(source)
}

pub fn pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    let primary = |source| {
        ws(alt((
            rest,
            number_pattern,
            string_pattern,
            boolean_pattern,
            variant,
            capture,
            list_pattern,
            destructure,
            ignore,
            surrounded('(', cut(pattern), ')'),
        )))
        .parse_complete(source)
    };
    enum Operator {
        Dereference,
        Reference,
    }
    precedence(
        alt((
            unary_op(3, ws(tag("*")).map(|_| Operator::Dereference)),
            unary_op(3, ws(tag("&")).map(|_| Operator::Reference)),
        )),
        fail(),
        fail(),
        primary,
        |op: Operation<Operator, (), (), Pattern<'_, ()>>| match op {
            Operation::Prefix(Operator::Dereference, arg) => {
                Ok(Pattern::Dereference(Box::new(arg)))
            }
            Operation::Prefix(Operator::Reference, arg) => Ok(Pattern::Reference(Box::new(arg))),
            _ => unreachable!(),
        },
    )
    .parse_complete(source)
}
