use nom::{Compare, CompareResult, InputIter, InputLength, InputTake, UnspecializedInput};
use std::{iter::Enumerate, slice::Iter};

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

impl ToString for Token {
    fn to_string(&self) -> String {
        match &self {
            Token::Eof => "EOF".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Bang => "!".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Lt => "<".to_string(),
            Token::Gt => ">".to_string(),
            Token::LtEq => "<=".to_string(),
            Token::GtEq => ">=".to_string(),
            Token::Eq => "==".to_string(),
            Token::NotEq => "!=".to_string(),
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
            Token::Ident(ref ident) => format!("Ident({})", ident),
            Token::Int(num) => format!("Int({})", num),
            Token::String(s) => format!(r#"String("{}")"#, s),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TokenSlice<'a> {
    pub slice: &'a [Token],
}

impl<'a> TokenSlice<'a> {
    pub fn from_tokens(tokens: &'a [Token]) -> Self {
        Self { slice: tokens }
    }
}

impl<'a> Compare<TokenSlice<'a>> for TokenSlice<'a> {
    fn compare(&self, t: TokenSlice<'a>) -> CompareResult {
        let pos = self.slice.iter().zip(t.slice).position(|(a, b)| a != b);

        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.slice.len() >= t.slice.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }

    fn compare_no_case(&self, t: TokenSlice<'a>) -> CompareResult {
        self.compare(t)
    }
}

impl<'a> InputLength for TokenSlice<'a> {
    fn input_len(&self) -> usize {
        self.slice.len()
    }
}

impl<'a> InputTake for TokenSlice<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            slice: &self.slice[..count],
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (front, back) = self.slice.split_at(count);
        (Self { slice: back }, Self { slice: front })
    }
}

impl<'a> InputIter for TokenSlice<'a> {
    type Item = &'a Token;
    type Iter = Enumerate<Iter<'a, Token>>;
    type IterElem = Iter<'a, Token>;

    fn iter_indices(&self) -> Self::Iter {
        self.slice.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.slice.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.slice.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.slice.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a> UnspecializedInput for TokenSlice<'a> {}
