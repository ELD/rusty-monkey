pub mod token;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::alpha1,
    combinator::{map, peek},
    error::{convert_error, ParseError, VerboseError},
    multi::many0,
    sequence::preceded,
    Err, IResult,
};
use token::Token;

pub struct Lexer;

impl Default for Lexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer {
    pub fn new() -> Self {
        Self
    }

    pub fn lex_input(&self, i: &str) -> Result<Vec<Token>, String> {
        let symbol_parsers = alt((
            Self::static_token_lexer_generator::<VerboseError<&str>>("==", Token::Eq),
            Self::static_token_lexer_generator::<VerboseError<&str>>("=", Token::Assign),
            Self::static_token_lexer_generator::<VerboseError<&str>>("+", Token::Plus),
            Self::static_token_lexer_generator::<VerboseError<&str>>("-", Token::Minus),
            Self::static_token_lexer_generator::<VerboseError<&str>>("!=", Token::NotEq),
            Self::static_token_lexer_generator::<VerboseError<&str>>("!", Token::Bang),
            Self::static_token_lexer_generator::<VerboseError<&str>>("*", Token::Asterisk),
            Self::static_token_lexer_generator::<VerboseError<&str>>("/", Token::Slash),
            Self::static_token_lexer_generator::<VerboseError<&str>>("<", Token::Lt),
            Self::static_token_lexer_generator::<VerboseError<&str>>(">", Token::Gt),
            Self::static_token_lexer_generator::<VerboseError<&str>>("<=", Token::LtEq),
            Self::static_token_lexer_generator::<VerboseError<&str>>(">=", Token::GtEq),
            Self::static_token_lexer_generator::<VerboseError<&str>>("(", Token::LParen),
            Self::static_token_lexer_generator::<VerboseError<&str>>(")", Token::RParen),
            Self::static_token_lexer_generator::<VerboseError<&str>>("{", Token::LBrace),
            Self::static_token_lexer_generator::<VerboseError<&str>>("}", Token::RBrace),
            Self::static_token_lexer_generator::<VerboseError<&str>>(",", Token::Comma),
            Self::static_token_lexer_generator::<VerboseError<&str>>(";", Token::Semicolon),
        ));

        let keyword_parsers = alt((
            Self::static_token_lexer_generator::<VerboseError<&str>>("let", Token::Let),
            Self::static_token_lexer_generator::<VerboseError<&str>>("fn", Token::Function),
            Self::static_token_lexer_generator::<VerboseError<&str>>("true", Token::True),
            Self::static_token_lexer_generator::<VerboseError<&str>>("false", Token::False),
            Self::static_token_lexer_generator::<VerboseError<&str>>("if", Token::If),
            Self::static_token_lexer_generator::<VerboseError<&str>>("else", Token::Else),
            Self::static_token_lexer_generator::<VerboseError<&str>>("return", Token::Return),
        ));

        let dynamic_parsers = alt((
            Self::number_lexer::<VerboseError<&str>>(),
            Self::ident_lexer::<VerboseError<&str>>(),
        ));

        let parser_result = many0(alt((symbol_parsers, keyword_parsers, dynamic_parsers)))(i);

        let mut tokens = match parser_result {
            Ok((_, tokens)) => tokens,
            Err(Err::Error(e)) | Err(Err::Failure(e)) => return Err(convert_error(i, e)),
            Err(Err::Incomplete(e)) => panic!("unreachable: {:?}", e),
        };

        tokens.push(Token::Eof);

        Ok(tokens)
    }

    fn static_token_lexer_generator<'a, E>(
        symbol: &'a str,
        token: Token,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Token, E>
    where
        E: ParseError<&'a str>,
    {
        map(preceded(Self::whitespace, tag(symbol)), move |_| {
            token.clone()
        })
    }

    fn ident_lexer<'a, E>() -> impl FnMut(&'a str) -> IResult<&'a str, Token, E>
    where
        E: ParseError<&'a str>,
    {
        map(
            preceded(
                Self::whitespace,
                preceded(
                    peek(alt((alpha1, tag("_")))),
                    take_while1(|c: char| c.is_alphanumeric() || c == '_'),
                ),
            ),
            |ident: &str| Token::Ident(ident.into()),
        )
    }

    fn number_lexer<'a, E>() -> impl FnMut(&'a str) -> IResult<&'a str, Token, E>
    where
        E: ParseError<&'a str>,
    {
        map(
            preceded(Self::whitespace, take_while1(|c: char| c.is_numeric())),
            |number: &str| Token::Int(number.parse::<i64>().unwrap()),
        )
    }

    fn whitespace<'a, E>(i: &'a str) -> IResult<&'a str, &'a str, E>
    where
        E: ParseError<&'a str>,
    {
        let whitespace_chars = " \t\r\n";

        take_while(move |c| whitespace_chars.contains(c))(i)
    }
}

#[cfg(test)]

mod test {
    use super::*;
    use nom::{error::ErrorKind, Err::Error};

    #[test]
    fn tokens_simple() {
        let input = "=+(){},;";

        let expected_tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        let actual_tokens = Lexer::new().lex_input(input).unwrap();

        assert_eq!(actual_tokens.len(), expected_tokens.len());

        expected_tokens
            .into_iter()
            .zip(actual_tokens)
            .for_each(|(expected, actual)| assert_eq!(actual, expected));
    }

    #[test]
    fn tokens_complex() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
        "#;

        let expected_tokens = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        let actual_tokens = Lexer::new().lex_input(input).unwrap();

        assert_eq!(actual_tokens.len(), expected_tokens.len());

        expected_tokens
            .into_iter()
            .zip(actual_tokens)
            .for_each(|(expected, actual)| assert_eq!(actual, expected));
    }

    #[test]
    fn parser_test() {
        let inputs = vec![
            ("five", Ok(("", Token::Ident("five".into())))),
            ("+=", Err(Error(("+=", ErrorKind::Tag)))),
            ("two", Ok(("", Token::Ident("two".into())))),
            ("my_thing2", Ok(("", Token::Ident("my_thing2".into())))),
            (
                "_private_ident",
                Ok(("", Token::Ident("_private_ident".into()))),
            ),
            (
                "1nvalid_ident",
                Err(Error(("1nvalid_ident", ErrorKind::Tag))),
            ),
        ];

        inputs.into_iter().for_each(|input| {
            let result: IResult<&str, Token> = Lexer::ident_lexer()(input.0);

            assert_eq!(result, input.1);
        });
    }
}
