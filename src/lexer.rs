use crate::tokens::Token;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::alpha1,
    combinator::{map, peek},
    error::ParseError,
    multi::many0,
    sequence::preceded,
    IResult,
};

pub fn lex_input(i: &str) -> Vec<Token> {
    let symbol_parsers = alt((
        static_token_lexer_generator("==", Token::Eq),
        static_token_lexer_generator("=", Token::Assign),
        static_token_lexer_generator("+", Token::Plus),
        static_token_lexer_generator("-", Token::Minus),
        static_token_lexer_generator("!=", Token::NotEq),
        static_token_lexer_generator("!", Token::Bang),
        static_token_lexer_generator("*", Token::Asterisk),
        static_token_lexer_generator("/", Token::Slash),
        static_token_lexer_generator("<", Token::Lt),
        static_token_lexer_generator(">", Token::Gt),
        static_token_lexer_generator("(", Token::LParen),
        static_token_lexer_generator(")", Token::RParen),
        static_token_lexer_generator("{", Token::LBrace),
        static_token_lexer_generator("}", Token::RBrace),
        static_token_lexer_generator(",", Token::Comma),
        static_token_lexer_generator(";", Token::Semicolon),
    ));

    let keyword_parsers = alt((
        static_token_lexer_generator("let", Token::Let),
        static_token_lexer_generator("fn", Token::Function),
        static_token_lexer_generator("true", Token::True),
        static_token_lexer_generator("false", Token::False),
        static_token_lexer_generator("if", Token::If),
        static_token_lexer_generator("else", Token::Else),
        static_token_lexer_generator("return", Token::Return),
    ));

    let dynamic_parsers = alt((number_lexer(), ident_lexer()));

    let parser_result = many0(alt((symbol_parsers, keyword_parsers, dynamic_parsers)))(i);

    let mut tokens = match parser_result {
        Ok((_, tokens)) => tokens,
        Err(e) => panic!("{:?}", e),
    };

    tokens.push(Token::Eof);
    tokens
}

fn static_token_lexer_generator<'a, E>(
    symbol: &'a str,
    token: Token,
) -> impl Fn(&'a str) -> IResult<&'a str, Token, E>
where
    E: ParseError<&'a str>,
{
    map(preceded(whitespace, tag(symbol)), move |_| token.clone())
}

fn ident_lexer<'a, E>() -> impl Fn(&'a str) -> IResult<&'a str, Token, E>
where
    E: ParseError<&'a str>,
{
    map(
        preceded(
            whitespace,
            preceded(
                peek(alt((alpha1, tag("_")))),
                take_while1(|c: char| c.is_alphanumeric() || c == '_'),
            ),
        ),
        |ident: &str| Token::Ident(ident.into()),
    )
}

fn number_lexer<'a>() -> impl Fn(&'a str) -> IResult<&'a str, Token> {
    map(
        preceded(whitespace, take_while1(|c: char| c.is_numeric())),
        |number: &str| Token::Int(number.parse::<i64>().unwrap()),
    )
}

fn whitespace<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let whitespace_chars = " \t\r\n";

    take_while(move |c| whitespace_chars.contains(c))(i)
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::error::ErrorKind;
    use nom::Err::Error;

    #[test]
    fn test_tokens_simple() {
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

        let actual_tokens = lex_input(input);

        assert_eq!(actual_tokens.len(), expected_tokens.len());
        expected_tokens
            .into_iter()
            .zip(actual_tokens)
            .for_each(|(expected, actual)| assert_eq!(actual, expected));
    }

    #[test]
    fn test_tokens_complex() {
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

        let actual_tokens = lex_input(input);

        assert_eq!(actual_tokens.len(), expected_tokens.len());
        expected_tokens
            .into_iter()
            .zip(actual_tokens)
            .for_each(|(expected, actual)| assert_eq!(actual, expected));
    }

    #[test]
    fn ident_parser_test() {
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
            let result: IResult<&str, Token> = ident_lexer()(input.0);

            assert_eq!(result, input.1);
        });
    }
}
