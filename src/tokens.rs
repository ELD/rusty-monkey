#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Eof,

    // Symbols
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
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
}
