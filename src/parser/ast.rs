use nom::lib::std::collections::BTreeMap;
use std::{fmt, fmt::Display};

pub(crate) type Program = Block;
pub(crate) type Block = Vec<Statement>;
pub(crate) type Ident = String;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Statement {
    Let(Ident, Expr),
    Return(Expr),
    Expression(Expr),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
    If {
        condition: Box<Expr>,
        consequence: Block,
        alternative: Option<Block>,
    },
    Function {
        params: Vec<Ident>,
        body: Block,
    },
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    HashLiteral(BTreeMap<Expr, Expr>),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd, Hash)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd, Hash)]
pub enum Infix {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

impl Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Multiply => write!(f, "*"),
            Infix::Divide => write!(f, "/"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::GreaterThanEqual => write!(f, ">="),
            Infix::LessThanEqual => write!(f, "<="),
            Infix::GreaterThan => write!(f, ">"),
            Infix::LessThan => write!(f, "<"),
        }
    }
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}
