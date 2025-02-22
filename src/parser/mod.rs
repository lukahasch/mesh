use lib::{
    Extensions, IgnoreAnd, Inner, ReplaceError, Source, begin_block, boundary, debug, end_block,
    not_eof, surrounded, word, ws,
};
use nom::{
    Err, IResult, Input, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::one_of,
    combinator::{cut, eof, fail, opt, peek},
    multi::{many0, separated_list0},
    sequence::preceded,
};
use nom_language::precedence::{Operation, precedence, unary_op};
use smallmap::Map;
use std::num::{ParseFloatError, ParseIntError};

use crate::{Expression, Field, Fields, Node, Pattern, error::Error};

pub mod lib;
pub mod pattern;
pub mod simple;

use lib::*;
use pattern::*;
use simple::*;

pub const KEYWORDS: [&str; 14] = [
    "fn",
    "let",
    "if",
    "else",
    "while",
    "for",
    "match",
    "struct",
    "enum",
    "type",
    "do",
    "in",
    "interface",
    "impl",
];

fn parse_parameter_list(source: Source) -> IResult<Source, Vec<Pattern<'_, ()>>, Error> {
    surrounded('(', cut(separated_list0(ws(tag(",")), pattern)), ')').parse_complete(source)
}

/// The function parser rewritten in a more functional style.
pub fn function(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    // Record the starting position.
    let begin = source.current();
    let (source, _) = ws(tag("fn"))
        .map_err_word(|_: nom::Err<Error>, found| {
            nom::Err::Error(Error::ExpectedKeywordFound {
                keyword: "fn",
                found,
            })
        })
        .parse_complete(source)?;

    dbg!("function start");

    // Build a combined parser that consumes:
    // - The "fn" keyword and an optional identifier
    // - The first parameter list
    // - Optionally a second parameter list
    // - Optionally a return type after "->"
    // - Optionally a colon followed by an optional boundary and an expression (the function body)
    let (source, (maybe_ident, first_params, maybe_second_params, ret_type, body)) = cut((
        opt(ws(identifier)),
        parse_parameter_list,
        opt(parse_parameter_list),
        opt(preceded(ws(tag("->")), cut(expression))),
        opt(preceded(
            ws(tag(":")),
            preceded(opt(boundary), cut(expression)),
        )),
    ))
    .parse_complete(source)?;

    dbg!("parse_complete");

    let (generics, args) = match maybe_second_params {
        Some(second_params) => (first_params, second_params),
        None => (Vec::new(), first_params),
    };

    let end = source.current();

    let function_expr = Expression::Function {
        generics,
        args,
        r#type: ret_type.map(Box::new),
        body: body.map(Box::new),
    };

    let node_expr = if let Some(ident) = maybe_ident {
        Expression::Let {
            pattern: Pattern::Capture {
                name: ident.as_str(),
                r#type: None,
            },
            value: Box::new(Node {
                tag: (),
                span: begin.clone() + end.clone(),
                expression: function_expr,
            }),
        }
    } else {
        function_expr
    };

    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: node_expr,
        },
    ))
}

pub fn r#let(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    tag("let")
        .ignore_and(cut(pattern).replace_error_word(|e, w| {
            Err::Failure(Error::ExpectedFound {
                span: e.span(),
                expected: "pattern",
                found: w,
            })
        }))
        .and_ignore(cut(ws(tag("="))))
        .and(cut(expression))
        .map_span(|span, (pattern, value)| Node {
            tag: (),
            span,
            expression: Expression::Let {
                pattern,
                value: Box::new(value),
            },
        })
        .parse_complete(source)
}

pub fn assign(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    let begin = source.current();
    let (source, pattern) = pattern.parse_complete(source)?;
    let (source, _) = ws(tag("=")).parse_complete(source)?;
    let (source, value) = cut(expression).parse_complete(source)?;
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
    let (source, condition) = cut(expression).parse_complete(source)?;
    let (source, _) = cut(ws(tag("then"))).parse_complete(source)?;
    let (source, then) = cut(expression).parse_complete(source)?;
    let (source, otherwise) = opt(ws(tag("else")).and(cut(expression))).parse_complete(source)?;
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
    let (source, condition) = cut(expression).parse_complete(source)?;
    let (source, _) = ws(tag("do")).parse_complete(source)?;
    let (source, body) = cut(expression).parse_complete(source)?;
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
    let (source, pattern) = cut(pattern).parse_complete(source)?;
    let (source, _) = ws(cut(tag("in"))).parse_complete(source)?;
    let (source, iterable) = cut(expression).parse_complete(source)?;
    let (source, _) = ws(cut(tag("do"))).parse_complete(source)?;
    let (source, body) = cut(expression).parse_complete(source)?;
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
    let (source, value) = expression.parse_complete(source)?;
    let (source, _) = ws(tag("with")).parse_complete(source)?;
    let (source, _) = boundary.and(begin_block).parse_complete(source)?;
    let (source, arms) =
        separated_list0(boundary, (pattern, ws(tag("=>")), expression)).parse_complete(source)?;
    let (source, _) = boundary.or(end_block).parse_complete(source)?;
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

pub fn attribute(source: Source) -> IResult<Source, Node<'_, ()>, Error> {
    tag("#")
        .ignore_and(surrounded('[', expression, ']'))
        .parse_complete(source)
}

pub fn field(source: Source) -> IResult<Source, Field<'_, ()>, Error> {
    separated_list0(boundary, attribute)
        .and(identifier)
        .and(
            ws(tag(":"))
                .replace_error_word(|e, w| {
                    Err::Failure(Error::ExpectedFound {
                        span: e.span(),
                        expected: "type",
                        found: w,
                    })
                })
                .and(cut(expression)),
        )
        .map(|((attributes, name), (_, value))| Field {
            name: name.as_str(),
            r#type: Box::new(value),
            attributes,
        })
        .parse_complete(source)
}

pub fn r#struct(source: Source<'_>) -> IResult<Source<'_>, Node<'_, ()>, Error> {
    let (source, _) = tag("struct")(source)?;
    cut(ws(identifier)
        .and(opt(surrounded(
            '(',
            separated_list0(tag(","), pattern),
            ')',
        )))
        .and(opt(preceded(ws(tag("where")), expression)))
        .and_ignore(ws(tag(":")))
        .and_ignore(boundary)
        .and_ignore(begin_block)
        .and(separated_list0(boundary, field))
        .and(program)
        .and_ignore(end_block)
        .map_span(|span, ((((name, generics), r#where), fields), body)| Node {
            tag: (),
            span,
            expression: Expression::Struct {
                name: name.as_str(),
                generics: generics.unwrap_or_default(),
                fields,
                body,
                r#where: r#where.map(Box::new),
            },
        }))
    .parse_complete(source)
}

pub fn r#enum<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error<'a>> {
    let begin = source.current();
    let (source, _) = tag("enum")(source)?;
    let (source, name) = ws(identifier).parse_complete(source)?;
    let (source, generics) =
        opt(surrounded('(', separated_list0(tag(","), pattern), ')')).parse_complete(source)?;
    let (source, r#where) = opt(preceded(ws(tag("where")), expression)).parse_complete(source)?;
    let (source, _) = ws(tag(":")).parse_complete(source)?;
    let (source, _) = boundary.parse_complete(source)?;
    let (source, _) = begin_block.parse_complete(source)?;
    let (source, variants) = separated_list0(
        boundary,
        (
            identifier,
            opt(surrounded('(', separated_list0(tag(","), expression), ')')),
        ),
    )
    .parse_complete(source)?;
    let (source, body) = program.parse_complete(source)?;
    let (source, _) = end_block.parse_complete(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::Enum {
                name: name.as_str(),
                generics: generics.unwrap_or_default(),
                variants: variants
                    .into_iter()
                    .map(|(name, fields)| (name.as_str(), fields.unwrap_or_default()))
                    .collect(),
                body,
                r#where: r#where.map(Box::new),
            },
        },
    ))
}

pub fn interface<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error<'a>> {
    let begin = source.current();
    let (source, _) = tag("interface")(source)?;
    let (source, name) = ws(identifier).parse_complete(source)?;
    let (source, generics) =
        opt(surrounded('(', separated_list0(tag(","), pattern), ')')).parse_complete(source)?;
    let (source, r#where) = opt(preceded(ws(tag("where")), expression)).parse_complete(source)?;
    let (source, _) = ws(tag(":")).parse_complete(source)?;
    let (source, _) = boundary.parse_complete(source)?;
    let (source, _) = begin_block.parse_complete(source)?;
    let (source, body) = program.parse_complete(source)?;
    let (source, _) = end_block.parse_complete(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::Interface {
                name: name.as_str(),
                generics: generics.unwrap_or_default(),
                body,
                r#where: r#where.map(Box::new),
            },
        },
    ))
}

pub fn implementation<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error<'a>> {
    // impl <generics> <interface> for <type> where <where>: <fields> <body>
    let begin = source.current();
    let (source, _) = tag("impl")(source)?;
    let (source, generics) =
        opt(surrounded('(', separated_list0(tag(","), pattern), ')')).parse_complete(source)?;
    let (source, interface) = expression.parse_complete(source)?;
    let (source, _) = ws(tag("for")).parse_complete(source)?;
    let (source, r#type) = expression.parse_complete(source)?;
    let (source, r#where) = opt(preceded(ws(tag("where")), expression)).parse_complete(source)?;
    let (source, _) = ws(tag(":")).parse_complete(source)?;
    let (source, _) = boundary.parse_complete(source)?;
    let (source, _) = begin_block.parse_complete(source)?;
    let (source, body) = program.parse_complete(source)?;
    let (source, _) = end_block.parse_complete(source)?;
    let end = source.current();
    Ok((
        source,
        Node {
            tag: (),
            span: begin + end,
            expression: Expression::Implementation {
                generics: generics.unwrap_or_default(),
                interface: Box::new(interface),
                r#type: Box::new(r#type),
                body,
                r#where: r#where.map(Box::new),
            },
        },
    ))
}

pub fn struct_construction<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error<'a>> {
    surrounded('(', cut(expression), ')')
        .and(surrounded(
            '{',
            cut(separated_list0(
                ws(tag(",")),
                (ws(identifier), ws(tag(":")), expression),
            )),
            '}',
        ))
        .map_span(|span, (r#type, fields)| Node {
            tag: (),
            span,
            expression: Expression::StructConstruction {
                r#type: Box::new(r#type),
                fields: fields
                    .into_iter()
                    .map(|(name, _, value)| (name.as_str(), value))
                    .collect(),
            },
        })
        .parse_complete(source)
}

pub fn expression<'a>(source: Source<'a>) -> IResult<Source<'a>, Node<'a, ()>, Error<'a>> {
    not_eof(source)?;
    dbg!("expression", source);
    let primary = |source| {
        alt((
            block,
            ws(alt((
                struct_construction,
                implementation,
                interface,
                r#struct,
                r#enum,
                r#let,
                r#if,
                r#while,
                r#for,
                r#match,
                function,
                comment,
                assign,
                list,
                boolean,
                string,
                number,
                variable,
                surrounded('(', cut(expression), ')'),
            ))),
        ))
        .parse_complete(source)
    };
    pub enum Operator<'a> {
        Access(&'a str),
        Call(Vec<Node<'a, ()>>),
        Reference,
        Dereference,
    }
    precedence(
        alt((
            unary_op(3, ws(tag("&")).map(|_| Operator::Reference)),
            unary_op(3, ws(tag("*")).map(|_| Operator::Dereference)),
        )),
        alt((
            unary_op(
                0,
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
        |op: Operation<Operator, Operator, (), Node<'a, ()>>| match op {
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
            Operation::Prefix(Operator::Dereference, arg) => Ok(Node {
                tag: (),
                span: arg.span.clone(),
                expression: Expression::Dereference(Box::new(arg)),
            }),
            Operation::Prefix(Operator::Reference, arg) => Ok(Node {
                tag: (),
                span: arg.span.clone(),
                expression: Expression::Reference(Box::new(arg)),
            }),
            _ => unreachable!(),
        },
    )
    .parse_complete(source)
}

/// consume all empty lines but stop before the \n of the last empty line
pub fn empty_lines<T>(mut source: Source) -> IResult<Source, Option<T>, Error> {
    while let Ok((s, _)) = ws(tag::<&str, Source, Error>("\n").or(tag(";"))).parse_complete(source)
    {
        source = s;
    }
    Ok((source.back(1), None))
}

pub fn seperator(source: Source) -> IResult<Source, (), Error> {
    empty_lines::<()>
        .and(boundary)
        .map(|_| ())
        .parse_complete(source)
}

pub fn program(source: Source) -> IResult<Source, Vec<Node<'_, ()>>, Error> {
    let (source, _) = opt(seperator).parse_complete(source)?;
    separated_list0(seperator, expression).parse_complete(source)
}

pub fn parser(source: Source) -> IResult<Source, Vec<Node<'_, ()>>, Error> {
    let (source, res) = program.parse_complete(source)?;
    let (source, _) = many0(one_of("\n; \t")).and(eof).parse_complete(source)?;
    Ok((source, res))
}

#[cfg(test)]
mod tests {
    use std::process::exit;

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
    fn test_nested_block() {
        let source = Source::new("test", "    10\n        20\n        30");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 28),
            expression: Expression::Block(vec![
                Node {
                    tag: (),
                    span: Span::new("test", 4, 6),
                    expression: Expression::Integer(10),
                },
                Node {
                    tag: (),
                    span: Span::new("test", 11, 28),
                    expression: Expression::Block(vec![
                        Node {
                            tag: (),
                            span: Span::new("test", 15, 17),
                            expression: Expression::Integer(20),
                        },
                        Node {
                            tag: (),
                            span: Span::new("test", 26, 28),
                            expression: Expression::Integer(30),
                        },
                    ]),
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
            expression.parse_complete(source),
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
            expression.parse_complete(source),
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
            expression.parse_complete(source),
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
            "struct Point:\n    x: i64\n    y: i64\n    fn add(self, other: Point) -> Point:\n        10",
        );
        let mut s2 = Source::new("test", " Point:");
        s2.indent = 1;
        dbg!(expression.parse_complete(s2).unwrap());
        let expected = Node {
            tag: (),
            span: Span::new("test", 6, 87),
            expression: Expression::Struct {
                name: "Point",
                generics: vec![],
                r#where: None,
                fields: vec![
                    Field {
                        name: "x",
                        r#type: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 21, 24),
                            expression: Expression::Variable("i64"),
                        }),
                        attributes: vec![],
                    },
                    Field {
                        name: "y",
                        r#type: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 32, 35),
                            expression: Expression::Variable("i64"),
                        }),
                        attributes: vec![],
                    },
                ],
                body: vec![Node {
                    tag: (),
                    span: Span::new("test", 40, 87),
                    expression: Expression::Let {
                        pattern: Pattern::Capture {
                            name: "add",
                            r#type: None,
                        },
                        value: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 40, 87),
                            expression: Expression::Function {
                                generics: vec![],
                                args: vec![
                                    Pattern::Capture {
                                        name: "self",
                                        r#type: None,
                                    },
                                    Pattern::Capture {
                                        name: "other",
                                        r#type: Some(Box::new(Node {
                                            tag: (),
                                            span: Span::new("test", 60, 65),
                                            expression: Expression::Variable("Point"),
                                        })),
                                    },
                                ],
                                r#type: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 70, 75),
                                    expression: Expression::Variable("Point"),
                                })),
                                body: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 81, 87),
                                    expression: Expression::Block(vec![Node {
                                        tag: (),
                                        span: Span::new("test", 85, 87),
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

    #[test]
    fn test_enum() {
        let source = Source::new(
            "test",
            "enum Option(T):\n    None\n    Some(T)\n    fn map(T2)(f: fn(_: T) -> T2) -> Option(T2):\n        None",
        );
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 98),
            expression: Expression::Enum {
                name: "Option",
                generics: vec![Pattern::Capture {
                    name: "T",
                    r#type: None,
                }],
                r#where: None,
                variants: vec![
                    ("None", vec![]),
                    (
                        "Some",
                        vec![Node {
                            tag: (),
                            span: Span::new("test", 34, 35),
                            expression: Expression::Variable("T"),
                        }],
                    ),
                ],
                body: vec![Node {
                    tag: (),
                    span: Span::new("test", 41, 98),
                    expression: Expression::Let {
                        pattern: Pattern::Capture {
                            name: "map",
                            r#type: None,
                        },
                        value: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 41, 98),
                            expression: Expression::Function {
                                generics: vec![Pattern::Capture {
                                    name: "T2",
                                    r#type: None,
                                }],
                                args: vec![Pattern::Capture {
                                    name: "f",
                                    r#type: Some(Box::new(Node {
                                        tag: (),
                                        span: Span::new("test", 55, 69),
                                        expression: Expression::Function {
                                            generics: vec![],
                                            args: vec![Pattern::Capture {
                                                name: "_",
                                                r#type: Some(Box::new(Node {
                                                    tag: (),
                                                    span: Span::new("test", 61, 62),
                                                    expression: Expression::Variable("T"),
                                                })),
                                            }],
                                            r#type: Some(Box::new(Node {
                                                tag: (),
                                                span: Span::new("test", 67, 69),
                                                expression: Expression::Variable("T2"),
                                            })),
                                            body: None,
                                        },
                                    })),
                                }],
                                r#type: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 74, 83),
                                    expression: Expression::Call {
                                        function: Box::new(Node {
                                            tag: (),
                                            span: Span::new("test", 74, 80),
                                            expression: Expression::Variable("Option"),
                                        }),
                                        args: vec![Node {
                                            tag: (),
                                            span: Span::new("test", 81, 83),
                                            expression: Expression::Variable("T2"),
                                        }],
                                    },
                                })),
                                body: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 90, 98),
                                    expression: Expression::Block(vec![Node {
                                        tag: (),
                                        span: Span::new("test", 94, 98),
                                        expression: Expression::Variable("None"),
                                    }]),
                                })),
                            },
                        }),
                    },
                }],
            },
        };
        assert_eq!(
            expression.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_interface() {
        let source = Source::new(
            "test",
            "interface Add(Other) where 10:\n    fn add(self: Self, other: Other) -> Self",
        );
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 75),
            expression: Expression::Interface {
                name: "Add",
                generics: vec![Pattern::Capture {
                    name: "Other",
                    r#type: None,
                }],
                body: vec![Node {
                    tag: (),
                    span: Span::new("test", 35, 75),
                    expression: Expression::Let {
                        pattern: Pattern::Capture {
                            name: "add",
                            r#type: None,
                        },
                        value: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 35, 75),
                            expression: Expression::Function {
                                generics: vec![],
                                args: vec![
                                    Pattern::Capture {
                                        name: "self",
                                        r#type: Some(Box::new(Node {
                                            tag: (),
                                            span: Span::new("test", 48, 52),
                                            expression: Expression::Variable("Self"),
                                        })),
                                    },
                                    Pattern::Capture {
                                        name: "other",
                                        r#type: Some(Box::new(Node {
                                            tag: (),
                                            span: Span::new("test", 61, 66),
                                            expression: Expression::Variable("Other"),
                                        })),
                                    },
                                ],
                                r#type: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 71, 75),
                                    expression: Expression::Variable("Self"),
                                })),
                                body: None,
                            },
                        }),
                    },
                }],
                r#where: Some(Box::new(Node {
                    tag: (),
                    span: Span::new("test", 27, 29),
                    expression: Expression::Integer(10),
                })),
            },
        };
        assert_eq!(
            r#interface.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_implementation() {
        let source = Source::new(
            "test",
            "impl(O, E) Try(O, E) for Result(O,E):\n    fn try(self): self",
        );
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 60),
            expression: Expression::Implementation {
                generics: vec![
                    Pattern::Capture {
                        name: "O",
                        r#type: None,
                    },
                    Pattern::Capture {
                        name: "E",
                        r#type: None,
                    },
                ],
                interface: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 11, 19),
                    expression: Expression::Call {
                        function: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 11, 14),
                            expression: Expression::Variable("Try"),
                        }),
                        args: vec![
                            Node {
                                tag: (),
                                span: Span::new("test", 15, 16),
                                expression: Expression::Variable("O"),
                            },
                            Node {
                                tag: (),
                                span: Span::new("test", 18, 19),
                                expression: Expression::Variable("E"),
                            },
                        ],
                    },
                }),
                r#type: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 25, 35),
                    expression: Expression::Call {
                        function: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 25, 31),
                            expression: Expression::Variable("Result"),
                        }),
                        args: vec![
                            Node {
                                tag: (),
                                span: Span::new("test", 32, 33),
                                expression: Expression::Variable("O"),
                            },
                            Node {
                                tag: (),
                                span: Span::new("test", 34, 35),
                                expression: Expression::Variable("E"),
                            },
                        ],
                    },
                }),
                body: vec![Node {
                    tag: (),
                    span: Span::new("test", 42, 60),
                    expression: Expression::Let {
                        pattern: Pattern::Capture {
                            name: "try",
                            r#type: None,
                        },
                        value: Box::new(Node {
                            tag: (),
                            span: Span::new("test", 42, 60),
                            expression: Expression::Function {
                                generics: vec![],
                                args: vec![Pattern::Capture {
                                    name: "self",
                                    r#type: None,
                                }],
                                r#type: None,
                                body: Some(Box::new(Node {
                                    tag: (),
                                    span: Span::new("test", 56, 60),
                                    expression: Expression::Variable("self"),
                                })),
                            },
                        }),
                    },
                }],
                r#where: None,
            },
        };
        assert_eq!(
            implementation.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }

    #[test]
    fn test_access() {
        let source = Source::new("test", "x.y");
        let expected = Node {
            tag: (),
            span: Span::new("test", 0, 2),
            expression: Expression::Access {
                object: Box::new(Node {
                    tag: (),
                    span: Span::new("test", 0, 1),
                    expression: Expression::Variable("x"),
                }),
                field: "y",
            },
        };
        assert_eq!(
            expression.parse_complete(source),
            Ok((source.eof(), expected))
        );
    }
}
