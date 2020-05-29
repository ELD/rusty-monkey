pub mod token_ng;

use crate::util::convert_error_span;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::alpha1,
    combinator::{map, peek},
    error::{ParseError, VerboseError},
    multi::many0,
    sequence::preceded,
    Err, IResult,
};
use nom_locate::LocatedSpan;
use token_ng::{Token, TokenType};

pub type Span<'a> = LocatedSpan<&'a str>;

pub struct Lexer;

impl Lexer {
    pub fn lex_input(i: &str) -> Result<Vec<Token>, String> {
        let symbol_parsers = alt((
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("==", TokenType::Eq),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("=", TokenType::Assign),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("+", TokenType::Plus),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("-", TokenType::Minus),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("!=", TokenType::NotEq),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("!", TokenType::Bang),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("*", TokenType::Asterisk),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("/", TokenType::Slash),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("<", TokenType::Lt),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(">", TokenType::Gt),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("<=", TokenType::LtEq),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(">=", TokenType::GtEq),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("(", TokenType::LParen),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(")", TokenType::RParen),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("{", TokenType::LBrace),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("}", TokenType::RBrace),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(",", TokenType::Comma),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(";", TokenType::Semicolon),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("[", TokenType::LBracket),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("]", TokenType::RBracket),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(":", TokenType::Colon),
        ));

        let keyword_parsers = alt((
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("let", TokenType::Let),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("fn", TokenType::Function),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("true", TokenType::True),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("false", TokenType::False),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("if", TokenType::If),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>("else", TokenType::Else),
            Self::static_token_lexer_generator::<VerboseError<Span<'_>>>(
                "return",
                TokenType::Return,
            ),
        ));

        let dynamic_parsers = alt((
            Self::string_literal_lexer::<VerboseError<Span<'_>>>(),
            Self::number_lexer::<VerboseError<Span<'_>>>(),
            Self::ident_lexer::<VerboseError<Span<'_>>>(),
        ));

        let parser_result =
            many0(alt((symbol_parsers, keyword_parsers, dynamic_parsers)))(Span::new(i));

        let mut tokens = match parser_result {
            Ok((_, tokens)) => tokens,
            Err(Err::Error(e)) | Err(Err::Failure(e)) => return Err(convert_error_span(i, e)),
            Err(Err::Incomplete(e)) => panic!("unreachable: {:?}", e),
        };

        tokens.push(Token {
            token_type: TokenType::Eof,
            line: std::u32::MAX,
            span: (0, 0),
        });

        Ok(tokens)
    }

    fn static_token_lexer_generator<'a, E>(
        symbol: &'a str,
        token_type: TokenType,
    ) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token, E>
    where
        E: ParseError<Span<'a>>,
    {
        map(
            preceded(Self::whitespace, tag(symbol)),
            move |span: Span<'a>| {
                let begin = span.get_utf8_column();
                Token {
                    token_type: token_type.clone(),
                    line: span.location_line(),
                    span: (begin, begin + span.fragment().len() - 1),
                }
            },
        )
    }

    fn string_literal_lexer<'a, E>() -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token, E>
    where
        E: ParseError<Span<'a>>,
    {
        move |input: Span<'a>| {
            let (i, _) = preceded(Self::whitespace, tag("\""))(input)?;
            let (i, string_literal) = take_while(|c: char| c != '"')(i)?;
            let (i, _) = preceded(Self::whitespace, tag("\""))(i)?;

            let begin = string_literal.get_utf8_column();
            let token = Token {
                token_type: TokenType::String(string_literal.fragment().to_string()),
                line: string_literal.location_line(),
                span: (begin, begin + string_literal.fragment().len() - 1),
            };

            Ok((i, token))
        }
    }

    fn ident_lexer<'a, E>() -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token, E>
    where
        E: ParseError<Span<'a>>,
    {
        map(
            preceded(
                Self::whitespace,
                preceded(
                    peek(alt((alpha1, tag("_")))),
                    take_while1(|c: char| c.is_alphanumeric() || c == '_'),
                ),
            ),
            |ident: Span<'a>| {
                let begin = ident.get_utf8_column();
                Token {
                    token_type: TokenType::Ident((*ident.fragment()).into()),
                    line: ident.location_line(),
                    span: (begin, begin + ident.fragment().len() - 1),
                }
            },
        )
    }

    fn number_lexer<'a, E>() -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token, E>
    where
        E: ParseError<Span<'a>>,
    {
        map(
            preceded(Self::whitespace, take_while1(|c: char| c.is_numeric())),
            |number: Span<'a>| {
                let begin = number.get_utf8_column();
                Token {
                    token_type: TokenType::Int(number.fragment().parse::<i64>().unwrap()),
                    line: number.location_line(),
                    span: (begin, begin + number.fragment().len() - 1),
                }
            },
        )
    }

    fn whitespace<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Span<'a>, E>
    where
        E: ParseError<Span<'a>>,
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
            TokenType::Assign,
            TokenType::Plus,
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Comma,
            TokenType::Semicolon,
            TokenType::Eof,
        ];

        let actual_tokens = Lexer::lex_input(input).unwrap();

        assert_eq!(actual_tokens.len(), expected_tokens.len());

        expected_tokens
            .into_iter()
            .zip(actual_tokens)
            .for_each(|(expected, actual)| assert_eq!(actual.token_type, expected));
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
    "foobar"
    "foo bar"
    [1, 2];
    {"foo": "bar"}
            "#;

        let expected_tokens = vec![
            TokenType::Let,
            TokenType::Ident("five".to_string()),
            TokenType::Assign,
            TokenType::Int(5),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident("ten".to_string()),
            TokenType::Assign,
            TokenType::Int(10),
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident("add".to_string()),
            TokenType::Assign,
            TokenType::Function,
            TokenType::LParen,
            TokenType::Ident("x".to_string()),
            TokenType::Comma,
            TokenType::Ident("y".to_string()),
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::Ident("x".to_string()),
            TokenType::Plus,
            TokenType::Ident("y".to_string()),
            TokenType::Semicolon,
            TokenType::RBrace,
            TokenType::Semicolon,
            TokenType::Let,
            TokenType::Ident("result".to_string()),
            TokenType::Assign,
            TokenType::Ident("add".to_string()),
            TokenType::LParen,
            TokenType::Ident("five".to_string()),
            TokenType::Comma,
            TokenType::Ident("ten".to_string()),
            TokenType::RParen,
            TokenType::Semicolon,
            TokenType::Bang,
            TokenType::Minus,
            TokenType::Slash,
            TokenType::Asterisk,
            TokenType::Int(5),
            TokenType::Semicolon,
            TokenType::Int(5),
            TokenType::Lt,
            TokenType::Int(10),
            TokenType::Gt,
            TokenType::Int(5),
            TokenType::Semicolon,
            TokenType::If,
            TokenType::LParen,
            TokenType::Int(5),
            TokenType::Lt,
            TokenType::Int(10),
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::Return,
            TokenType::True,
            TokenType::Semicolon,
            TokenType::RBrace,
            TokenType::Else,
            TokenType::LBrace,
            TokenType::Return,
            TokenType::False,
            TokenType::Semicolon,
            TokenType::RBrace,
            TokenType::Int(10),
            TokenType::Eq,
            TokenType::Int(10),
            TokenType::Semicolon,
            TokenType::Int(10),
            TokenType::NotEq,
            TokenType::Int(9),
            TokenType::Semicolon,
            TokenType::String("foobar".into()),
            TokenType::String("foo bar".into()),
            TokenType::LBracket,
            TokenType::Int(1),
            TokenType::Comma,
            TokenType::Int(2),
            TokenType::RBracket,
            TokenType::Semicolon,
            TokenType::LBrace,
            TokenType::String("foo".to_string()),
            TokenType::Colon,
            TokenType::String("bar".to_string()),
            TokenType::RBrace,
            TokenType::Eof,
        ];

        let actual_tokens = Lexer::lex_input(input).unwrap();

        assert_eq!(actual_tokens.len(), expected_tokens.len());

        expected_tokens
            .into_iter()
            .zip(actual_tokens)
            .for_each(|(expected, actual)| assert_eq!(actual.token_type, expected));
    }

    #[test]
    fn parser_test() {
        let inputs = vec![
            (
                "five",
                Ok((
                    unsafe { Span::new_from_raw_offset(4, 1, "", ()) },
                    Token {
                        token_type: TokenType::Ident("five".into()),
                        line: 1,
                        span: (1, 4),
                    },
                )),
            ),
            ("+=", Err(Error((Span::new("+="), ErrorKind::Tag)))),
            (
                "two",
                Ok((
                    unsafe { Span::new_from_raw_offset(3, 1, "", ()) },
                    Token {
                        token_type: TokenType::Ident("two".into()),
                        line: 1,
                        span: (1, 3),
                    },
                )),
            ),
            (
                "my_thing2",
                Ok((
                    unsafe { Span::new_from_raw_offset(9, 1, "", ()) },
                    Token {
                        token_type: TokenType::Ident("my_thing2".into()),
                        line: 1,
                        span: (1, 9),
                    },
                )),
            ),
            (
                "_private_ident",
                Ok((
                    unsafe { Span::new_from_raw_offset(14, 1, "", ()) },
                    Token {
                        token_type: TokenType::Ident("_private_ident".into()),
                        line: 1,
                        span: (1, 14),
                    },
                )),
            ),
            (
                "1nvalid_ident",
                Err(Error((Span::new("1nvalid_ident"), ErrorKind::Tag))),
            ),
        ];

        inputs.into_iter().for_each(|input| {
            let result: IResult<Span<'_>, Token> = Lexer::ident_lexer()(Span::new(input.0));

            assert_eq!(result, input.1);
        });
    }
}
