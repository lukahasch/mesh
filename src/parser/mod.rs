use lib::{Source, begin_block, boundary, dbg, end_block, ws};
use nom::{
    IResult, Input, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    combinator::{eof, fail, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
};
use nom_language::precedence::{Operation, Unary, precedence, unary_op};
use std::num::{ParseFloatError, ParseIntError};

use crate::{Expression, Node, Pattern, error::Error};

pub mod lib;

pub const KEYWORDS: [&str; 12] = [
    "fn", "let", "if", "else", "while", "for", "match", "struct", "enum", "type", "do", "in",
];

pub fn identifier(source: Source) -> IResult<Source, Source, Error> {
    if Some(true) == source.char().map(|c| c.is_ascii_alphabetic()) {
        let (a, b) =
            source.split_at_position_complete(|c| !(c.is_ascii_alphanumeric() || c == '_'))?;
        if KEYWORDS.contains(&b.as_str()) {
            Err(nom::Err::Error(Error::KeywordAsIdentifier(a)))
        } else {
            Ok((a, b))
        }
    } else {
        Err(nom::Err::Error(Error::Debug(
            source.current(),
            "expected identifier",
        )))
    }
}

pub fn comment(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    tag("//")
        .and(take_while(|c| c != '\n'))
        .map(|(slashes, source): (Source, Source)| Node {
            tag: (),
            span: source.span() + slashes.span(),
            expression: Expression::Comment(source.as_str()),
        })
        .parse(source)
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
        .parse(source)
}

pub fn string(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    tag::<_, Source, _>("\"")
        .and(take_while1(|c: char| c != '"'))
        .and(tag("\""))
        .map(|((b, source), e)| Node {
            tag: (),
            span: source.span() + b.span() + e.span(),
            expression: Expression::String(source.as_str()),
        })
        .parse(source)
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
    .parse(source)
}

pub fn variable(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    identifier
        .map(|source: Source| Node {
            tag: (),
            span: source.span(),
            expression: Expression::Variable(source.as_str()),
        })
        .parse(source)
}

pub fn list(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    delimited(tag("["), separated_list0(tag(","), expression), tag("]"))
        .map(|list| Node {
            tag: (),
            span: list
                .iter()
                .fold(begin.clone(), |acc, node| acc + &node.span)
                + 1,
            expression: Expression::List(list),
        })
        .parse(source)
}

pub fn block(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.span();
    begin_block
        .and(program)
        .and(end_block)
        .map(|((_, nodes), _)| Node {
            tag: (),
            span: nodes
                .iter()
                .fold(begin.clone(), |acc, node| acc + &node.span),
            expression: Expression::Block(nodes),
        })
        .parse_complete(source)
}

pub fn ignore(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    tag("_").map(|_: Source| Pattern::Ignore).parse(source)
}

pub fn rest(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    tag("..")
        .and(opt(identifier))
        .map(|(_, source)| Pattern::Rest(source.map(|source| source.as_str())))
        .parse(source)
}

pub fn number_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    number
        .map(|node| match node.expression {
            Expression::Integer(i) => Pattern::Integer(i),
            Expression::Float(f) => Pattern::Float(f),
            _ => unreachable!(),
        })
        .parse(source)
}

pub fn string_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    string
        .map(|node| match node.expression {
            Expression::String(s) => Pattern::String(s),
            _ => unreachable!(),
        })
        .parse(source)
}

pub fn boolean_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    boolean
        .map(|node| match node.expression {
            Expression::Boolean(b) => Pattern::Boolean(b),
            _ => unreachable!(),
        })
        .parse(source)
}

pub fn capture(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    identifier
        .and(opt(preceded(ws(tag(":")), expression)))
        .map(|(source, r#type)| Pattern::Capture {
            name: source.as_str(),
            r#type: r#type.map(Box::new),
        })
        .parse(source)
}

pub fn list_pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    delimited(tag("["), separated_list0(tag(","), pattern), tag("]"))
        .map(|list| Pattern::List(list))
        .parse(source)
}

pub fn destructure(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    opt(delimited(tag("("), expression, tag(")")))
        .and(delimited(
            ws(tag("{")),
            separated_list0(ws(tag(",")), (ws(identifier), ws(tag(":")), pattern)),
            ws(tag("}")),
        ))
        .map(|(r#type, fields)| Pattern::Destructure {
            r#type: r#type.map(Box::new),
            fields: fields
                .into_iter()
                .map(|(identifier, _, pattern)| (identifier.as_str(), pattern))
                .collect(),
        })
        .parse(source)
}

pub fn variant(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    identifier
        .and(delimited(
            tag("("),
            separated_list0(tag(","), pattern),
            tag(")"),
        ))
        .map(|(source, fields)| Pattern::Variant {
            name: source.as_str(),
            fields,
        })
        .parse(source)
}

pub fn pattern(source: Source) -> IResult<Source, Pattern<'_, ()>, Error> {
    ws(alt((
        ignore,
        rest,
        number_pattern,
        string_pattern,
        boolean_pattern,
        variant,
        capture,
        list_pattern,
        destructure,
    )))
    .parse(source)
}

/// function expression fn(generics)(arguments) -> return_type: expr
pub fn function(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("fn")(source)?;
    let (source, identifier) = opt(ws(identifier)).parse(source)?;
    let (source, first) =
        delimited(tag("("), separated_list0(ws(tag(",")), pattern), tag(")")).parse(source)?;
    let (source, second) = opt(delimited(
        tag("("),
        separated_list0(ws(tag(",")), pattern),
        tag(")"),
    ))
    .parse(source)?;
    let (source, r#type) = opt(preceded(ws(tag("->")), expression)).parse(source)?;
    let (source, colon) = opt(ws(tag(":"))).parse(source)?;
    let (source, body) = if colon.is_some() {
        let (source, _) = opt(boundary).parse(source)?;
        expression.map(Some).parse(source)?
    } else {
        (source, None)
    };
    let (generics, args) = if second.is_some() {
        (first, second.unwrap())
    } else {
        (vec![], first)
    };
    let end = source.current();
    match identifier {
        Some(identifier) => Ok((
            source,
            Node {
                tag: (),
                span: begin.clone() + end.clone(),
                expression: Expression::Let {
                    pattern: Pattern::Capture {
                        name: identifier.as_str(),
                        r#type: None,
                    },
                    value: Box::new(Node {
                        tag: (),
                        span: begin + end,
                        expression: Expression::Function {
                            generics,
                            args,
                            r#type: r#type.map(Box::new),
                            body: body.map(|node| Box::new(node)),
                        },
                    }),
                },
            },
        )),
        None => Ok((
            source,
            Node {
                tag: (),
                span: begin + end,
                expression: Expression::Function {
                    generics,
                    args,
                    r#type: r#type.map(Box::new),
                    body: body.map(|node| Box::new(node)),
                },
            },
        )),
    }
}

pub fn r#let(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("let")(source)?;
    let (source, pattern) = pattern.parse(source)?;
    let (source, _) = ws(tag("=")).parse(source)?;
    let (source, value) = expression.parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::Let {
                pattern,
                value: Box::new(value),
            },
        },
    ))
}

pub fn assign(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, pattern) = pattern.parse(source)?;
    let (source, _) = ws(tag("=")).parse(source)?;
    let (source, value) = expression.parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::Assign {
                pattern,
                value: Box::new(value),
            },
        },
    ))
}

pub fn r#if(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("if")(source)?;
    let (source, condition) = expression.parse(source)?;
    let (source, _) = ws(tag("then")).parse(source)?;
    let (source, then) = expression.parse(source)?;
    let (source, otherwise) = opt(ws(tag("else")).and(expression)).parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::If {
                condition: Box::new(condition),
                then: Box::new(then),
                otherwise: otherwise.map(|(_, node)| Box::new(node)),
            },
        },
    ))
}

pub fn r#while(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("while")(source)?;
    let (source, condition) = expression.parse(source)?;
    let (source, _) = ws(tag("do")).parse(source)?;
    let (source, body) = expression.parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
        },
    ))
}

pub fn r#for(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("for")(source)?;
    let (source, pattern) = pattern.parse(source)?;
    let (source, _) = ws(tag("in")).parse(source)?;
    let (source, iterable) = expression.parse(source)?;
    let (source, _) = ws(tag("do")).parse(source)?;
    let (source, body) = expression.parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::For {
                pattern,
                iterable: Box::new(iterable),
                body: Box::new(body),
            },
        },
    ))
}

pub fn r#match(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("match")(source)?;
    let (source, value) = expression.parse(source)?;
    let (source, _) = ws(tag("with")).parse(source)?;
    let (source, _) = boundary.and(begin_block).parse(source)?;
    let (source, arms) =
        separated_list0(boundary, (pattern, ws(tag("=>")), expression)).parse(source)?;
    let (source, _) = boundary.or(end_block).parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin.clone() + end.clone(),
            expression: Expression::Match {
                value: Box::new(value),
                arms: arms
                    .into_iter()
                    .map(|(pattern, _, expression)| (pattern, expression))
                    .collect(),
            },
        },
    ))
}

pub fn r#struct<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error> {
    let begin = source.current();
    let (source, _) = tag("struct")(source)?;
    let (source, name) = ws(identifier).parse(source)?;
    let (source, generics) = opt(delimited(
        tag("("),
        separated_list0(tag(","), pattern),
        tag(")"),
    ))
    .parse(source)?;
    let (source, _) = ws(tag(":")).parse(source)?;
    let (source, _) = boundary.parse(source)?;
    let (source, _) = begin_block.parse(source)?;
    let (source, fields) =
        separated_list0(boundary, (identifier, ws(tag(":")), expression)).parse(source)?;
    let (source, _) = boundary.parse(source)?;
    let (source, body) = program.parse(source)?;
    let (source, _) = end_block.parse(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::Struct {
                name: name.as_str(),
                generics: generics.unwrap_or_default(),
                fields: fields
                    .into_iter()
                    .map(|(name, _, r#type)| (name.as_str(), r#type))
                    .collect(),
                body,
            },
        },
    ))
}

pub fn expression<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error> {
    let primary = |source| {
        alt((
            block,
            ws(alt((
                r#struct, r#let, r#if, r#while, r#for, r#match, function, comment, list, boolean,
                string, number, assign, variable,
            ))),
        ))
        .parse(source)
    };
    pub enum Operator<'a> {
        Access(&'a str),
        Call(Vec<Node<'a, ()>>),
    }
    precedence(
        fail(),
        alt((
            unary_op(
                1,
                (ws(tag(".")), ws(identifier))
                    .map(|(_, identifier)| Operator::Access(identifier.as_str())),
            ),
            unary_op(
                0,
                (
                    ws(tag("(")),
                    ws(separated_list0(ws(tag(",")), expression)),
                    ws(tag(")")),
                )
                    .map(|(_, args, _)| Operator::Call(args)),
            ),
        )),
        fail(),
        primary,
        |op: Operation<(), Operator, (), Node<'a, ()>>| match op {
            Operation::Postfix(arg, Operator::Access(field)) => Ok(Node {
                tag: (),
                span: arg.span.clone() + field.len(),
                expression: Expression::Access {
                    object: Box::new(arg),
                    field,
                },
            }),
            Operation::Postfix(arg, Operator::Call(args)) => Ok(Node {
                tag: (),
                span: args
                    .iter()
                    .fold(arg.span.clone(), |acc, node| acc + &node.span),
                expression: Expression::Call {
                    function: Box::new(arg),
                    args,
                },
            }),
            _ => unreachable!(),
        },
    )
    .parse(source)
}

pub fn program(source: Source) -> IResult<Source, Vec<Node<'_, ()>>, Error> {
    separated_list0(
        boundary,
        expression
            .map(Some)
            .or(take_while(|c| c == ' ' || c == '\t')
                .and(tag("\n"))
                .map(|_| None)),
    )
    .and(opt(boundary))
    .parse(source)
    .map(|(source, (nodes, _))| (source, nodes.into_iter().filter_map(|node| node).collect()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Span;

    #[test]
    fn test_comment() {
        let source = Source::new("test", "// hello world");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 14),
            expression: Expression::Comment(" hello world"),
        };
        assert_eq!(comment.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_number() {
        let source = Source::new("test", "123");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 3),
            expression: Expression::Integer(123),
        };
        assert_eq!(number.parse_complete(source), Ok((source.eof(), expected)));

        let source = Source::new("test", "123.456");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 7),
            expression: Expression::Float(123.456),
        };
        assert_eq!(number.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_string() {
        let source = Source::new("test", "\"hello world\"");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 13),
            expression: Expression::String("hello world"),
        };
        assert_eq!(string.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_boolean() {
        let source = Source::new("test", "true");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 4),
            expression: Expression::Boolean(true),
        };
        assert_eq!(boolean.parse_complete(source), Ok((source.eof(), expected)));

        let source = Source::new("test", "false");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 5),
            expression: Expression::Boolean(false),
        };
        assert_eq!(boolean.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_variable() {
        let source = Source::new("test", "hello_world");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 11),
            expression: Expression::Variable("hello_world"),
        };
        assert_eq!(
            variable.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_list() {
        let source = Source::new("test", "[1, 2, 3]");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 9),
            expression: Expression::List(vec![
                Node {
                    tag: (),
                    span: Span::new("test", 1, 2),
                    expression: Expression::Integer(1),
                },
                Node {
                    tag: (),
                    span: Span::new("test", 4, 5),
                    expression: Expression::Integer(2),
                },
                Node {
                    tag: (),
                    span: Span::new("test", 7, 8),
                    expression: Expression::Integer(3),
                },
            ]),
        };
        assert_eq!(list.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_block() {
        let source = Source::new("test", "    10\n    20\n    30");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 20),
            expression: Expression::Block(vec![
                Node {
                    tag: (),
                    span: Span::new("test", 4, 6),
                    expression: Expression::Integer(10),
                },
                Node {
                    tag: (),
                    span: Span::new("test", 11, 13),
                    expression: Expression::Integer(20),
                },
                Node {
                    tag: (),
                    span: Span::new("test", 18, 20),
                    expression: Expression::Integer(30),
                },
            ]),
        };
        assert_eq!(block.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_if() {
        let source = Source::new("test", "if true then 20 else 40");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 23),
            expression: Expression::If {
                condition: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 3, 7),
                    expression: Expression::Boolean(true),
                }),
                then: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 13, 15),
                    expression: Expression::Integer(20),
                }),
                otherwise: Some(Box::new(Node {
                    tag: (),
                    span: Span::new("test", 21, 23),
                    expression: Expression::Integer(40),
                })),
            },
        };
        assert_eq!(r#if.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_while() {
        let source = Source::new("test", "while true do 20");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 16),
            expression: Expression::While {
                condition: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 6, 10),
                    expression: Expression::Boolean(true),
                }),
                body: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 14, 16),
                    expression: Expression::Integer(20),
                }),
            },
        };
        assert_eq!(r#while.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_number_pattern() {
        let source = Source::new("test", "123");
        let expected = Pattern::Integer(123);
        assert_eq!(
            number_pattern.parse_complete(source),
            Ok((source.eof(), expected))
        );

        let source = Source::new("test", "123.456");
        let expected = Pattern::Float(123.456);
        assert_eq!(
            number_pattern.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_string_pattern() {
        let source = Source::new("test", "\"hello world\"");
        let expected = Pattern::String("hello world");
        assert_eq!(
            string_pattern.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_boolean_pattern() {
        let source = Source::new("test", "true");
        let expected = Pattern::Boolean(true);
        assert_eq!(
            boolean_pattern.parse_complete(source),
            Ok((source.eof(), expected))
        );

        let source = Source::new("test", "false");
        let expected = Pattern::Boolean(false);
        assert_eq!(
            boolean_pattern.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_capture_pattern() {
        let source = Source::new("test", "hello");
        let expected = Pattern::Capture {
            name: "hello",
            r#type: None,
        };
        assert_eq!(capture.parse_complete(source), Ok((source.eof(), expected)));

        let source = Source::new("test", "hello: i64");
        let expected = Pattern::Capture {
            name: "hello",
            r#type: Some(Box::new(Node {
                tag: (),
                span: Span::new("test", 7, 10),
                expression: Expression::Variable("i64"),
            })),
        };
        assert_eq!(capture.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_list_pattern() {
        let source = Source::new("test", "[1, 2, 3]");
        let expected = Pattern::List(vec![
            Pattern::Integer(1),
            Pattern::Integer(2),
            Pattern::Integer(3),
        ]);
        assert_eq!(
            list_pattern.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_destructure_pattern() {
        let source = Source::new("test_one", "(i64) { x: 10, y: 20 }");
        let expected = Pattern::Destructure {
            r#type: Some(Box::new(Node {
                tag: (),
                span: Span::new("test_one", 1, 4),
                expression: Expression::Variable("i64"),
            })),
            fields: vec![("x", Pattern::Integer(10)), ("y", Pattern::Integer(20))]
                .into_iter()
                .collect(),
        };
        assert_eq!(
            destructure.parse_complete(source),
            Ok((source.eof(), expected))
        );

        let source = Source::new("test_two", "{ x: 10, y: 20 }");
        let expected = Pattern::Destructure {
            r#type: None,
            fields: vec![("x", Pattern::Integer(10)), ("y", Pattern::Integer(20))]
                .into_iter()
                .collect(),
        };
        assert_eq!(
            destructure.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_variant_pattern() {
        let source = Source::new("test", "Some(10)");
        let expected = Pattern::Variant {
            name: "Some",
            fields: vec![Pattern::Integer(10)],
        };
        assert_eq!(variant.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_ignore_pattern() {
        let source = Source::new("test", "_");
        let expected = Pattern::Ignore;
        assert_eq!(ignore.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_rest_pattern() {
        let source = Source::new("test", "..");
        let expected = Pattern::Rest(None);
        assert_eq!(rest.parse_complete(source), Ok((source.eof(), expected)));

        let source = Source::new("test", "..hello");
        let expected = Pattern::Rest(Some("hello"));
        assert_eq!(rest.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_for() {
        let source = Source::new("test", "for i in [1, 2, 3] do 20");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 24),
            expression: Expression::For {
                pattern: Pattern::Capture {
                    name: "i",
                    r#type: None,
                },
                iterable: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 9, 18),
                    expression: Expression::List(vec![
                        Node {
                            tag: (),
                            span: Span::new("test", 10, 11),
                            expression: Expression::Integer(1),
                        },
                        Node {
                            tag: (),
                            span: Span::new("test", 13, 14),
                            expression: Expression::Integer(2),
                        },
                        Node {
                            tag: (),
                            span: Span::new("test", 16, 17),
                            expression: Expression::Integer(3),
                        },
                    ]),
                }),
                body: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 22, 24),
                    expression: Expression::Integer(20),
                }),
            },
        };
        assert_eq!(r#for.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_match() {
        let source = Source::new("test", "match x with\n    10 => 20\n    y => y");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 36),
            expression: Expression::Match {
                value: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 6, 7),
                    expression: Expression::Variable("x"),
                }),
                arms: vec![
                    (
                        Pattern::Integer(10),
                        Node {
                            tag: (),
                            span: Span::new("test", 23, 25),
                            expression: Expression::Integer(20),
                        },
                    ),
                    (
                        Pattern::Capture {
                            name: "y",
                            r#type: None,
                        },
                        Node {
                            tag: (),
                            span: Span::new("test", 35, 36),
                            expression: Expression::Variable("y"),
                        },
                    ),
                ]
                .into_iter()
                .collect(),
            },
        };
        assert_eq!(r#match.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_function_expression() {
        let source = Source::new("test", "fn(O)(x: O) -> i64: 10");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 22),
            expression: Expression::Function {
                generics: vec![Pattern::Capture {
                    name: "O",
                    r#type: None,
                }],
                args: vec![Pattern::Capture {
                    name: "x",
                    r#type: Some(Box::new(Node {
                        tag: (),
                        span: Span::new("test", 9, 10),
                        expression: Expression::Variable("O"),
                    })),
                }],
                r#type: Some(Box::new(Node {
                    tag: (),
                    span: Span::new("test", 15, 18),
                    expression: Expression::Variable("i64"),
                })),
                body: Some(Box::new(Node {
                    tag: (),
                    span: Span::new("test", 20, 22),
                    expression: Expression::Integer(10),
                })),
            },
        };
        assert_eq!(
            function.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_function_definition() {
        let source = Source::new("test", "fn f(x: i64) -> i64: 10");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 23),
            expression: Expression::Let {
                pattern: Pattern::Capture {
                    name: "f",
                    r#type: None,
                },
                value: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 0, 23),
                    expression: Expression::Function {
                        generics: vec![],
                        args: vec![Pattern::Capture {
                            name: "x",
                            r#type: Some(Box::new(Node {
                                tag: (),
                                span: Span::new("test", 8, 11),
                                expression: Expression::Variable("i64"),
                            })),
                        }],
                        r#type: Some(Box::new(Node {
                            tag: (),
                            span: Span::new("test", 16, 19),
                            expression: Expression::Variable("i64"),
                        })),
                        body: Some(Box::new(Node {
                            tag: (),
                            span: Span::new("test", 21, 23),
                            expression: Expression::Integer(10),
                        })),
                    },
                }),
            },
        };
        assert_eq!(
            function.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_function_head() {
        let source = Source::new("test", "fn f(x: i64) -> i64");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 19),
            expression: Expression::Let {
                pattern: Pattern::Capture {
                    name: "f",
                    r#type: None,
                },
                value: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 0, 19),
                    expression: Expression::Function {
                        generics: vec![],
                        args: vec![Pattern::Capture {
                            name: "x",
                            r#type: Some(Box::new(Node {
                                tag: (),
                                span: Span::new("test", 8, 11),
                                expression: Expression::Variable("i64"),
                            })),
                        }],
                        r#type: Some(Box::new(Node {
                            tag: (),
                            span: Span::new("test", 16, 19),
                            expression: Expression::Variable("i64"),
                        })),
                        body: None,
                    },
                }),
            },
        };
        assert_eq!(
            function.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_call() {
        let source = Source::new("test", "f(10, 20)");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 8),
            expression: Expression::Call {
                function: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 0, 1),
                    expression: Expression::Variable("f"),
                }),
                args: vec![
                    Node {
                        tag: (),
                        span: Span::new("test", 2, 4),
                        expression: Expression::Integer(10),
                    },
                    Node {
                        tag: (),
                        span: Span::new("test", 6, 8),
                        expression: Expression::Integer(20),
                    },
                ],
            },
        };
        assert_eq!(
            expression.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_let() {
        let source = Source::new("test", "let x = 10");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 10),
            expression: Expression::Let {
                pattern: Pattern::Capture {
                    name: "x",
                    r#type: None,
                },
                value: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 8, 10),
                    expression: Expression::Integer(10),
                }),
            },
        };
        assert_eq!((r#let).parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_assign() {
        let source = Source::new("test", "x = 10");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 6),
            expression: Expression::Assign {
                pattern: Pattern::Capture {
                    name: "x",
                    r#type: None,
                },
                value: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 4, 6),
                    expression: Expression::Integer(10),
                }),
            },
        };
        assert_eq!(assign.parse_complete(source), Ok((source.eof(), expected)));
    }

    #[test]
    fn test_struct() {
        let source = Source::new(
            "test",
            "struct Point:\n    x: i64\n    y: i64\n    fn add(self: Point, other: Point) -> Point:\n        10",
        );
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 94),
            expression: Expression::Struct {
                name: "Point",
                generics: vec![],
                fields: vec![
                    (
                        "x",
                        Node {
                            tag: (),
                            span: Span::new("test", 21, 24),
                            expression: Expression::Variable("i64"),
                        },
                    ),
                    (
                        "y",
                        Node {
                            tag: (),
                            span: Span::new("test", 32, 35),
                            expression: Expression::Variable("i64"),
                        },
                    ),
                ]
                .into_iter()
                .collect(),
                body: vec![Node {
                    tag: (),
                    span: Span::new("test", 40, 94),
                    expression: Expression::Let {
                        pattern: Pattern::Capture {
                            name: "add",
                            r#type: None,
                        },
                        value: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 40, 94),
                            expression: Expression::Function {
                                generics: vec![],
                                args: vec![
                                    Pattern::Capture {
                                        name: "self",
                                        r#type: Some(Box::new(Node {
                                            tag: (),
                                            span: Span::new("test", 53, 58),
                                            expression: Expression::Variable("Point"),
                                        })),
                                    },
                                    Pattern::Capture {
                                        name: "other",
                                        r#type: Some(Box::new(Node {
                                            tag: (),
                                            span: Span::new("test", 67, 72),
                                            expression: Expression::Variable("Point"),
                                        })),
                                    },
                                ],
                                r#type: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 77, 82),
                                    expression: Expression::Variable("Point"),
                                })),
                                body: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 88, 94),
                                    expression: Expression::Block(vec![Node {
                                        tag: (),
                                        span: Span::new("test", 92, 94),
                                        expression: Expression::Integer(10),
                                    }]),
                                })),
                            },
                        }),
                    },
                }],
            },
        };
        assert_eq!(
            r#struct.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }
}
