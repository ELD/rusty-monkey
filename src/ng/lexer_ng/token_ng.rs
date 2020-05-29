use std::fmt::{Debug, Display, Formatter, Result};

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub span: (usize, usize),
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "Token {{ type: {:?}, line: {}, span: {}..{} }}",
            self.token_type, self.line, self.span.0, self.span.1
        )
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenType {
    Eof,

    // Symbols
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    NotEq,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    // Complex Types
    Ident(String),
    Int(i64),
    String(String),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TokenType::Eof => write!(f, "EOF"),
            TokenType::Comma => write!(f, ","),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::LBrace => write!(f, "{{"),
            TokenType::RBrace => write!(f, "}}"),
            TokenType::LBracket => write!(f, "["),
            TokenType::RBracket => write!(f, "]"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Assign => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Bang => write!(f, "!"),
            TokenType::Asterisk => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Lt => write!(f, "<"),
            TokenType::Gt => write!(f, ">"),
            TokenType::LtEq => write!(f, "<="),
            TokenType::GtEq => write!(f, ">="),
            TokenType::Eq => write!(f, "=="),
            TokenType::NotEq => write!(f, "!="),
            TokenType::Function => write!(f, "fn"),
            TokenType::Let => write!(f, "let"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::Return => write!(f, "return"),
            TokenType::Ident(ref ident) => write!(f, "Ident({})", ident),
            TokenType::Int(num) => write!(f, "Int({})", num),
            TokenType::String(s) => write!(f, r#"String("{}")"#, s),
        }
    }
}

impl Debug for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TokenType::Eof => write!(f, "TokenType::Eof"),
            TokenType::Comma => write!(f, "TokenType::Comma"),
            TokenType::Semicolon => write!(f, "TokenType::Semicolon"),
            TokenType::LParen => write!(f, "TokenType::LParen"),
            TokenType::RParen => write!(f, "TokenType::RParen"),
            TokenType::LBrace => write!(f, "TokenType::LBrace"),
            TokenType::RBrace => write!(f, "TokenType::RBrace"),
            TokenType::LBracket => write!(f, "TokenType::LBracket"),
            TokenType::RBracket => write!(f, "TokenType::RBracket"),
            TokenType::Colon => write!(f, "TokenType::Colon"),
            TokenType::Assign => write!(f, "TokenType::Assign"),
            TokenType::Plus => write!(f, "TokenType::Plus"),
            TokenType::Minus => write!(f, "TokenType::Minus"),
            TokenType::Bang => write!(f, "TokenType::Bang"),
            TokenType::Asterisk => write!(f, "TokenType::Asterisk"),
            TokenType::Slash => write!(f, "TokenType::Slash"),
            TokenType::Lt => write!(f, "TokenType::Lt"),
            TokenType::Gt => write!(f, "TokenType::Gt"),
            TokenType::LtEq => write!(f, "TokenType::LtEq"),
            TokenType::GtEq => write!(f, "TokenType::GtEq"),
            TokenType::Eq => write!(f, "TokenType::Eq"),
            TokenType::NotEq => write!(f, "TokenType::NotEq"),
            TokenType::Function => write!(f, "TokenType::Function"),
            TokenType::Let => write!(f, "TokenType::Let"),
            TokenType::True => write!(f, "TokenType::True"),
            TokenType::False => write!(f, "TokenType::False"),
            TokenType::If => write!(f, "TokenType::If"),
            TokenType::Else => write!(f, "TokenType::Else"),
            TokenType::Return => write!(f, "TokenType::Return"),
            TokenType::Ident(ref ident) => write!(f, "TokenType::Ident({})", ident),
            TokenType::Int(num) => write!(f, "TokenType::Int({})", num),
            TokenType::String(s) => write!(f, r#"TokenType::String("{}")"#, s),
        }
    }
}
