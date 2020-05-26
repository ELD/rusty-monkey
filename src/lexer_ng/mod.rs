pub mod token_ng;

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
use nom_locate::LocatedSpan;
use token_ng::{Token, TokenType};

type Span<'a> = LocatedSpan<&'a str>;

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
            Err(Err::Error(e)) | Err(Err::Failure(e)) => return Err(format!("{:?}", e)),
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
            move |span: Span<'a>| Token {
                token_type: token_type.clone(),
                line: span.location_line(),
                span: (0, 0),
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

            let token = Token {
                token_type: TokenType::String(string_literal.fragment().to_string()),
                line: string_literal.location_line(),
                span: (0, 0),
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
            |ident: Span<'a>| Token {
                token_type: TokenType::Ident((*ident.fragment()).into()),
                line: ident.location_line(),
                span: (0, 0),
            },
        )
    }

    fn number_lexer<'a, E>() -> impl Fn(Span<'a>) -> IResult<Span<'a>, Token, E>
    where
        E: ParseError<Span<'a>>,
    {
        map(
            preceded(Self::whitespace, take_while1(|c: char| c.is_numeric())),
            |number: Span<'a>| Token {
                token_type: TokenType::Int(number.fragment().parse::<i64>().unwrap()),
                line: number.location_line(),
                span: (0, 0),
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

// #[cfg(test)]
// mod test {
//     use super::*;
//     use nom::{error::ErrorKind, Err::Error};

//     #[test]
//     fn tokens_simple() {
//         let input = "=+(){},;";

//         let expected_tokens = vec![
//             Token::Assign,
//             Token::Plus,
//             Token::LParen,
//             Token::RParen,
//             Token::LBrace,
//             Token::RBrace,
//             Token::Comma,
//             Token::Semicolon,
//             Token::Eof,
//         ];

//         let actual_tokens = Lexer::new().lex_input(input).unwrap();

//         assert_eq!(actual_tokens.len(), expected_tokens.len());

//         expected_tokens
//             .into_iter()
//             .zip(actual_tokens)
//             .for_each(|(expected, actual)| assert_eq!(actual, expected));
//     }

//     #[test]
//     fn tokens_complex() {
//         let input = r#"let five = 5;
// let ten = 10;

// let add = fn(x, y) {
//   x + y;
// };

// let result = add(five, ten);
// !-/*5;
// 5 < 10 > 5;

// if (5 < 10) {
//     return true;
// } else {
//     return false;
// }

// 10 == 10;
// 10 != 9;
// "foobar"
// "foo bar"
// [1, 2];
// {"foo": "bar"}
//         "#;

//         let expected_tokens = vec![
//             Token::Let,
//             Token::Ident("five".to_string()),
//             Token::Assign,
//             Token::Int(5),
//             Token::Semicolon,
//             Token::Let,
//             Token::Ident("ten".to_string()),
//             Token::Assign,
//             Token::Int(10),
//             Token::Semicolon,
//             Token::Let,
//             Token::Ident("add".to_string()),
//             Token::Assign,
//             Token::Function,
//             Token::LParen,
//             Token::Ident("x".to_string()),
//             Token::Comma,
//             Token::Ident("y".to_string()),
//             Token::RParen,
//             Token::LBrace,
//             Token::Ident("x".to_string()),
//             Token::Plus,
//             Token::Ident("y".to_string()),
//             Token::Semicolon,
//             Token::RBrace,
//             Token::Semicolon,
//             Token::Let,
//             Token::Ident("result".to_string()),
//             Token::Assign,
//             Token::Ident("add".to_string()),
//             Token::LParen,
//             Token::Ident("five".to_string()),
//             Token::Comma,
//             Token::Ident("ten".to_string()),
//             Token::RParen,
//             Token::Semicolon,
//             Token::Bang,
//             Token::Minus,
//             Token::Slash,
//             Token::Asterisk,
//             Token::Int(5),
//             Token::Semicolon,
//             Token::Int(5),
//             Token::Lt,
//             Token::Int(10),
//             Token::Gt,
//             Token::Int(5),
//             Token::Semicolon,
//             Token::If,
//             Token::LParen,
//             Token::Int(5),
//             Token::Lt,
//             Token::Int(10),
//             Token::RParen,
//             Token::LBrace,
//             Token::Return,
//             Token::True,
//             Token::Semicolon,
//             Token::RBrace,
//             Token::Else,
//             Token::LBrace,
//             Token::Return,
//             Token::False,
//             Token::Semicolon,
//             Token::RBrace,
//             Token::Int(10),
//             Token::Eq,
//             Token::Int(10),
//             Token::Semicolon,
//             Token::Int(10),
//             Token::NotEq,
//             Token::Int(9),
//             Token::Semicolon,
//             Token::String("foobar".into()),
//             Token::String("foo bar".into()),
//             Token::LBracket,
//             Token::Int(1),
//             Token::Comma,
//             Token::Int(2),
//             Token::RBracket,
//             Token::Semicolon,
//             Token::LBrace,
//             Token::String("foo".to_string()),
//             Token::Colon,
//             Token::String("bar".to_string()),
//             Token::RBrace,
//             Token::Eof,
//         ];

//         let actual_tokens = Lexer::new().lex_input(input).unwrap();

//         assert_eq!(actual_tokens.len(), expected_tokens.len());

//         expected_tokens
//             .into_iter()
//             .zip(actual_tokens)
//             .for_each(|(expected, actual)| assert_eq!(actual, expected));
//     }

//     #[test]
//     fn parser_test() {
//         let inputs = vec![
//             ("five", Ok(("", Token::Ident("five".into())))),
//             ("+=", Err(Error(("+=", ErrorKind::Tag)))),
//             ("two", Ok(("", Token::Ident("two".into())))),
//             ("my_thing2", Ok(("", Token::Ident("my_thing2".into())))),
//             (
//                 "_private_ident",
//                 Ok(("", Token::Ident("_private_ident".into()))),
//             ),
//             (
//                 "1nvalid_ident",
//                 Err(Error(("1nvalid_ident", ErrorKind::Tag))),
//             ),
//         ];

//         inputs.into_iter().for_each(|input| {
//             let result: IResult<&str, Token> = Lexer::ident_lexer()(input.0);

//             assert_eq!(result, input.1);
//         });
//     }
// }
