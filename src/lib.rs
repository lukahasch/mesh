#![feature(impl_trait_in_assoc_type, impl_trait_in_bindings)]

use smallmap::Map;
use std::ops::{Add, Range};

pub mod error;
pub mod fmt;
pub mod parser;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: &'static str,
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node<'a, Tag> {
    pub tag: Tag,
    pub span: Span,
    pub expression: Expression<'a, Tag>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'a, Tag> {
    name: &'a str,
    r#type: Box<Node<'a, Tag>>,
    attributes: Vec<Node<'a, Tag>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ListContent<'a, Tag> {
    Expression(Node<'a, Tag>),
    Spread(Node<'a, Tag>),
}

pub type Parameters<'a, Tag> = Vec<Pattern<'a, Tag>>;
pub type Variant<'a, Tag> = (&'a str, Vec<Node<'a, Tag>>);
pub type Fields<'a, Tag> = Map<&'a str, Field<'a, Tag>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a, Tag> {
    Integer(i64),
    Float(f64),
    String(&'a str),
    Boolean(bool),
    Variable(&'a str),
    List(Vec<ListContent<'a, Tag>>),
    StructConstruction {
        r#type: Box<Node<'a, Tag>>,
        fields: Map<&'a str, Node<'a, Tag>>,
    },

    Block(Vec<Node<'a, Tag>>),
    If {
        condition: Box<Node<'a, Tag>>,
        then: Box<Node<'a, Tag>>,
        otherwise: Option<Box<Node<'a, Tag>>>,
    },
    While {
        condition: Box<Node<'a, Tag>>,
        body: Box<Node<'a, Tag>>,
    },
    For {
        pattern: Pattern<'a, Tag>,
        iterable: Box<Node<'a, Tag>>,
        body: Box<Node<'a, Tag>>,
    },
    Match {
        value: Box<Node<'a, Tag>>,
        arms: Vec<(Pattern<'a, Tag>, Node<'a, Tag>)>,
    },

    Function {
        generics: Parameters<'a, Tag>,
        args: Parameters<'a, Tag>,
        r#type: Option<Box<Node<'a, Tag>>>,
        body: Option<Box<Node<'a, Tag>>>,
    },
    Call {
        function: Box<Node<'a, Tag>>,
        args: Vec<Node<'a, Tag>>,
    },

    Let {
        pattern: Pattern<'a, Tag>,
        value: Box<Node<'a, Tag>>,
    },
    Assign {
        pattern: Pattern<'a, Tag>,
        value: Box<Node<'a, Tag>>,
    },

    Struct {
        name: &'a str,
        generics: Parameters<'a, Tag>,
        fields: Fields<'a, Tag>,
        body: Vec<Node<'a, Tag>>,
        r#where: Option<Box<Node<'a, Tag>>>,
    },
    Enum {
        name: &'a str,
        generics: Parameters<'a, Tag>,
        variants: Vec<Variant<'a, Tag>>,
        body: Vec<Node<'a, Tag>>,
        r#where: Option<Box<Node<'a, Tag>>>,
    },
    Interface {
        name: &'a str,
        generics: Parameters<'a, Tag>,
        body: Vec<Node<'a, Tag>>,
        r#where: Option<Box<Node<'a, Tag>>>,
    },
    Implementation {
        generics: Parameters<'a, Tag>,
        r#type: Box<Node<'a, Tag>>,
        interface: Box<Node<'a, Tag>>,
        body: Vec<Node<'a, Tag>>,
        r#where: Option<Box<Node<'a, Tag>>>,
    },

    Access {
        object: Box<Node<'a, Tag>>,
        field: &'a str,
    },
    ConstantAccess {
        object: Box<Node<'a, Tag>>,
        field: &'a str,
    },
    Reference(Box<Node<'a, Tag>>),
    Dereference(Box<Node<'a, Tag>>),

    Attributed {
        attributes: Vec<Node<'a, Tag>>,
        expression: Box<Node<'a, Tag>>,
    },

    Comment(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum Pattern<'a, Tag> {
    Integer(i64),
    Float(f64),
    String(&'a str),
    Boolean(bool),

    Capture { name: &'a str, r#type: Option<Box<Node<'a, Tag>>> },

    List(Vec<Pattern<'a, Tag>>),
    Destructure { r#type: Option<Box<Node<'a, Tag>>>, fields: Map<&'a str, Pattern<'a, Tag>> },
    Variant { name: &'a str, fields: Vec<Pattern<'a, Tag>> },

    Ignore,
    Spread(Box<Pattern<'a, Tag>>),
    WithName { name: &'a str, pattern: Box<Pattern<'a, Tag>> },

    Dereference(Box<Pattern<'a, Tag>>),
    Reference(Box<Pattern<'a, Tag>>),
}

impl<'a> Node<'a, ()> {
    pub fn new(span: Span, expr: Expression<'a, ()>) -> Self {
        Self {
            tag: (),
            span,
            expression: expr,
        }
    }
}

impl Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Span {
        Span {
            file: self.file,
            range: self.range.start.min(other.range.start)..other.range.end.max(self.range.end),
        }
    }
}

impl<'a> Add<&'a Span> for Span {
    type Output = Span;

    fn add(self, other: &'a Span) -> Span {
        Span {
            file: if self.file == other.file {
                self.file
            } else if self.file.is_empty() {
                other.file
            } else if other.file.is_empty() {
                self.file
            } else {
                "mixed"
            },
            range: self.range.start.min(other.range.start)..other.range.end.max(self.range.end),
        }
    }
}

impl Add<usize> for Span {
    type Output = Span;

    fn add(self, other: usize) -> Span {
        Span {
            file: self.file,
            range: self.range.start..self.range.end + other,
        }
    }
}

impl Span {
    pub fn new(file: &'static str, start: usize, end: usize) -> Self {
        Span {
            file,
            range: start..end,
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = &'static str;

    fn source(&self) -> &Self::SourceId {
        &self.file
    }

    fn start(&self) -> usize {
        self.range.start
    }

    fn end(&self) -> usize {
        self.range.end
    }
}
