pub(crate) type Program = Block;
pub(crate) type Block = Vec<Statement>;
pub(crate) type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Ident, Expr),
    Return(Expr),
    Expression(Expr),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
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
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
