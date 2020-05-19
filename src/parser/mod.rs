pub mod ast;

use crate::{
    lexer::token::{Token, TokenSlice},
    parser::ast::{Expr, Ident, Infix, Literal, Precedence, Prefix, Program, Statement},
};
use nom::{
    branch::alt,
    bytes::complete::{take, take_while},
    combinator::{map, peek},
    error::{make_error, ErrorKind},
    multi::many0,
    Err, IResult, InputIter, InputLength, Needed,
};
use std::collections::BTreeMap;

pub struct Parser;

impl Parser {
    pub fn parse(tokens: TokenSlice<'_>) -> Result<Program, String> {
        let program = match many0(Self::parse_statement())(tokens) {
            Ok((_, statements)) => statements,
            _ => panic!("parse error"),
        };

        Ok(program)
    }

    fn parse_statement<'a>() -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Statement> {
        alt((
            Self::parse_let_statement,
            Self::parse_return_statement,
            Self::parse_expression_statement,
        ))
    }

    fn parse_let_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Statement> {
        let (i, _) = Self::tag(Token::Let)(input)?;
        let (i, ident) = Self::parse_ident()(i)?;
        let (i, _) = Self::tag(Token::Assign)(i)?;

        let (i, expr) = Self::parse_expression(Precedence::Lowest)(i)?;
        let (i, _) = Self::parse_to_semicolon_if_exists()(i)?;

        Ok((i, Statement::Let(ident, expr)))
    }

    fn parse_return_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Statement> {
        let (i, _) = Self::tag(Token::Return)(input)?;

        let (i, expr) = Self::parse_expression(Precedence::Lowest)(i)?;
        let (i, _) = Self::parse_to_semicolon_if_exists()(i)?;
        Ok((i, Statement::Return(expr)))
    }

    fn parse_expression_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Statement> {
        let (i, expr) = Self::parse_expression(Precedence::Lowest)(input)?;
        let (i, _) = Self::parse_to_semicolon_if_exists()(i)?;

        Ok((i, Statement::Expression(expr)))
    }

    fn parse_expression<'a>(
        precedence: Precedence,
    ) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Expr> {
        move |input: TokenSlice<'_>| {
            let (mut i, mut left) = alt((
                map(Self::parse_literal(), Expr::Literal),
                map(Self::parse_ident(), Expr::Ident),
                Self::parse_prefix,
                Self::parse_paren,
                Self::parse_if,
                Self::parse_function,
                Self::parse_array,
                Self::parse_hash,
            ))(input)?;

            while !Self::peek_semicolon(i.clone()) && precedence < Self::peek_precedence(i.clone())
            {
                if !Self::peek_operator(i.clone()) {
                    break;
                }

                let (inner_i, inner_left) = Self::parse_infix(i.clone(), left.clone())?;
                left = inner_left;
                i = inner_i;
            }

            Ok((i, left))
        }
    }

    fn parse_paren(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Expr> {
        let (i, _) = Self::tag(Token::LParen)(input)?;
        let (i, expr) = Self::parse_expression(Precedence::Lowest)(i)?;
        let (i, _) = Self::tag(Token::RParen)(i)?;

        Ok((i, expr))
    }

    fn parse_prefix(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Expr> {
        let (i, prefix) = alt((
            map(Self::tag(Token::Bang), |_| Prefix::Bang),
            map(Self::tag(Token::Minus), |_| Prefix::Minus),
        ))(input)?;

        let (i, expr) = Self::parse_expression(Precedence::Prefix)(i)?;

        Ok((i, Expr::Prefix(prefix, Box::new(expr))))
    }

    fn parse_infix(input: TokenSlice<'_>, left: Expr) -> IResult<TokenSlice<'_>, Expr> {
        let (i, expr) = if Self::peek_tag(input.clone(), Token::LParen) {
            let (i, _) = Self::tag(Token::LParen)(input)?;
            let (i, arguments) = Self::parse_call_args()(i)?;
            let (i, _) = Self::tag(Token::RParen)(i)?;
            (
                i,
                Expr::Call {
                    function: Box::new(left),
                    arguments,
                },
            )
        } else if Self::peek_tag(input.clone(), Token::LBracket) {
            Self::parse_index(left)(input)?
        } else {
            let (i, (operator, precedence)) = Self::parse_operator()(input.clone())?;

            let (i, right) = Self::parse_expression(precedence)(i)?;
            (
                i,
                Expr::Infix(operator.unwrap(), Box::new(left), Box::new(right)),
            )
        };
        Ok((i, expr))
    }

    fn parse_if(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Expr> {
        let (i, _) = Self::tag(Token::If)(input)?;
        let (i, _) = Self::tag(Token::LParen)(i)?;
        let (i, condition) = Self::parse_expression(Precedence::Lowest)(i)?;
        let (i, _) = Self::tag(Token::RParen)(i)?;
        let (i, _) = Self::tag(Token::LBrace)(i)?;
        let (i, consequence) = many0(Self::parse_statement())(i)?;
        let (i, _) = Self::tag(Token::RBrace)(i)?;

        if !Self::peek_tag(i.clone(), Token::Else) {
            return Ok((
                i,
                Expr::If {
                    condition: Box::new(condition),
                    consequence,
                    alternative: None,
                },
            ));
        }

        let (i, _) = Self::tag(Token::Else)(i)?;
        let (i, _) = Self::tag(Token::LBrace)(i)?;
        let (i, alternative) = many0(Self::parse_statement())(i)?;
        let (i, _) = Self::tag(Token::RBrace)(i)?;

        Ok((
            i,
            Expr::If {
                condition: Box::new(condition),
                consequence,
                alternative: Some(alternative),
            },
        ))
    }

    fn parse_function(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Expr> {
        let (i, _) = Self::tag(Token::Function)(input)?;
        let (i, _) = Self::tag(Token::LParen)(i)?;
        let (i, params) = Self::parse_function_params()(i)?;
        let (i, _) = Self::tag(Token::RParen)(i)?;
        let (i, _) = Self::tag(Token::LBrace)(i)?;
        let (i, body) = many0(Self::parse_statement())(i)?;
        let (i, _) = Self::tag(Token::RBrace)(i)?;

        Ok((i, Expr::Function { params, body }))
    }

    fn parse_function_params<'a>(
    ) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<Ident>> {
        move |input: TokenSlice<'_>| {
            if Self::peek_tag(input.clone(), Token::RParen) {
                return Ok((input, vec![]));
            }

            let (i, ident) = Self::parse_ident()(input)?;
            let (i, mut idents) = many0(Self::parse_identifier_list)(i)?;
            idents.insert(0, ident);

            Ok((i, idents))
        }
    }

    fn parse_call_args<'a>() -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Vec<Expr>> {
        move |input: TokenSlice<'_>| {
            if Self::peek_tag(input.clone(), Token::RParen) {
                return Ok((input, vec![]));
            }

            let (i, expr) = Self::parse_expression(Precedence::Lowest)(input)?;
            let (i, mut args) = many0(Self::parse_expr_list)(i)?;
            args.insert(0, expr);

            Ok((i, args))
        }
    }

    fn parse_identifier_list(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Ident> {
        let (i, _) = Self::tag(Token::Comma)(input)?;
        let (i, ident) = Self::parse_ident()(i)?;

        Ok((i, ident))
    }

    fn parse_expr_list(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Expr> {
        let (i, _) = Self::tag(Token::Comma)(input)?;
        let (i, expr) = Self::parse_expression(Precedence::Lowest)(i)?;

        Ok((i, expr))
    }

    fn parse_array(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Expr> {
        let (i, _) = Self::tag(Token::LBracket)(input)?;
        let (i, expr) = Self::parse_expression(Precedence::Lowest)(i)?;
        let (i, mut expr_list) = many0(Self::parse_expr_list)(i)?;
        expr_list.insert(0, expr);
        let (i, _) = Self::tag(Token::RBracket)(i)?;

        Ok((i, Expr::Array(expr_list)))
    }

    fn parse_index<'a>(left: Expr) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Expr> {
        move |input: TokenSlice<'_>| {
            let (i, _) = Self::tag(Token::LBracket)(input)?;
            let (i, index) = Self::parse_expression(Precedence::Lowest)(i)?;
            let (i, _) = Self::tag(Token::RBracket)(i)?;

            Ok((i, Expr::Index(Box::new(left.clone()), Box::new(index))))
        }
    }

    fn parse_hash<'a>(input: TokenSlice<'a>) -> IResult<TokenSlice<'a>, Expr> {
        let (i, _) = Self::tag(Token::LBrace)(input)?;
        let (i, pairs) = map(
            many0(
                |input: TokenSlice<'a>| -> IResult<TokenSlice<'a>, (Expr, Expr)> {
                    let (i, key) = Self::parse_expression(Precedence::Lowest)(input)?;
                    let (i, _) = Self::tag(Token::Colon)(i)?;
                    let (i, value) = Self::parse_expression(Precedence::Lowest)(i)?;
                    let (i, _) = if Self::peek_tag(i.clone(), Token::Comma) {
                        Self::tag(Token::Comma)(i)?
                    } else {
                        (i.clone(), i.clone())
                    };

                    Ok((i, (key, value)))
                },
            ),
            |flat_pairs: Vec<(Expr, Expr)>| {
                flat_pairs
                    .iter()
                    .map(|(key, value)| (key.clone(), value.clone()))
                    .collect::<BTreeMap<Expr, Expr>>()
            },
        )(i.clone())?;

        let (i, _) = Self::tag(Token::RBrace)(i)?;

        Ok((i, Expr::HashLiteral(pairs)))
    }

    fn peek_tag(input: TokenSlice<'_>, tag: Token) -> bool {
        let result = peek(Self::tag(tag))(input);
        match result {
            Ok(..) => true,
            _ => false,
        }
    }

    fn tag<'a>(
        tag: Token,
    ) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, TokenSlice<'a>> {
        move |input: TokenSlice<'_>| {
            let i2 = input.clone();

            let (i, res) = take(1usize)(input)?;
            if res.input_len() == 0 {
                Err(Err::Incomplete(Needed::new(1)))
            } else if res.iter_elements().next() == Some(&tag) {
                Ok((i, res))
            } else {
                Err(Err::Error(make_error(i2, ErrorKind::Count)))
            }
        }
    }

    fn parse_ident<'a, O>() -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, O>
    where
        O: From<&'a String>,
    {
        move |input: TokenSlice<'_>| {
            let (i, res) = take(1usize)(input)?;

            if res.input_len() == 0 {
                Err(Err::Incomplete(Needed::new(1)))
            } else if let Some(Token::Ident(ident)) = res.iter_elements().next() {
                Ok((i, ident.into()))
            } else {
                Err(Err::Error(make_error(i, ErrorKind::Tag)))
            }
        }
    }

    fn parse_literal<'a>() -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Literal> {
        move |input: TokenSlice<'_>| {
            let (i, res) = take(1usize)(input)?;

            if res.input_len() == 0 {
                Err(Err::Incomplete(Needed::new(1)))
            } else {
                match res.iter_elements().next() {
                    Some(Token::True) => Ok((i, Literal::Bool(true))),
                    Some(Token::False) => Ok((i, Literal::Bool(false))),
                    Some(Token::String(s)) => Ok((i, Literal::String(s.clone()))),
                    Some(Token::Int(int)) => Ok((i, Literal::Int(*int))),
                    _ => Err(Err::Error(make_error(i, ErrorKind::Tag))),
                }
            }
        }
    }

    fn peek_precedence(input: TokenSlice<'_>) -> Precedence {
        match peek(Self::parse_operator())(input) {
            Ok((_, res)) => res.1,
            _ => Precedence::Lowest,
        }
    }

    fn peek_operator(input: TokenSlice<'_>) -> bool {
        match peek(Self::parse_operator())(input) {
            Ok(..) => true,
            _ => false,
        }
    }

    fn parse_operator<'a>(
    ) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, (Option<Infix>, Precedence)> {
        alt((
            map(Self::tag(Token::Plus), |_| {
                (Some(Infix::Plus), Precedence::Sum)
            }),
            map(Self::tag(Token::Minus), |_| {
                (Some(Infix::Minus), Precedence::Sum)
            }),
            map(Self::tag(Token::Asterisk), |_| {
                (Some(Infix::Multiply), Precedence::Product)
            }),
            map(Self::tag(Token::Slash), |_| {
                (Some(Infix::Divide), Precedence::Product)
            }),
            map(Self::tag(Token::Eq), |_| {
                (Some(Infix::Equal), Precedence::Equals)
            }),
            map(Self::tag(Token::NotEq), |_| {
                (Some(Infix::NotEqual), Precedence::Equals)
            }),
            map(Self::tag(Token::Gt), |_| {
                (Some(Infix::GreaterThan), Precedence::LessGreater)
            }),
            map(Self::tag(Token::Lt), |_| {
                (Some(Infix::LessThan), Precedence::LessGreater)
            }),
            map(Self::tag(Token::GtEq), |_| {
                (Some(Infix::GreaterThanEqual), Precedence::LessGreater)
            }),
            map(Self::tag(Token::LtEq), |_| {
                (Some(Infix::LessThanEqual), Precedence::LessGreater)
            }),
            map(Self::tag(Token::LParen), |_| (None, Precedence::Call)),
            map(Self::tag(Token::LBracket), |_| (None, Precedence::Index)),
        ))
    }

    fn peek_semicolon(input: TokenSlice<'_>) -> bool {
        let result: IResult<TokenSlice<'_>, TokenSlice<'_>> = peek(take(1usize))(input);
        match result {
            Ok((_, res)) => res.iter_elements().next() == Some(&Token::Semicolon),
            _ => false,
        }
    }

    fn parse_to_semicolon_if_exists<'a>(
    ) -> impl FnMut(TokenSlice<'a>) -> IResult<TokenSlice<'a>, TokenSlice<'a>> {
        move |input: TokenSlice<'_>| {
            if Self::peek_semicolon(input.clone()) {
                let (i, _) = take_while(|token| token != &Token::Semicolon)(input.clone())?;
                take(1usize)(i)
            } else {
                Ok((input.clone(), input.clone()))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        lexer::{token::TokenSlice, Lexer},
        parser::{
            ast::{Expr, Ident, Infix, Literal, Prefix, Program, Statement},
            Parser,
        },
    };
    use std::collections::BTreeMap;

    #[test]
    fn let_statements() {
        struct TestData {
            input: &'static str,
            expected_ident: Ident,
            expected_expr: Expr,
        }

        let test_data = vec![
            TestData {
                input: "let x = 5;",
                expected_ident: "x".to_string(),
                expected_expr: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "let y = true;",
                expected_ident: "y".to_string(),
                expected_expr: Expr::Literal(Literal::Bool(true)),
            },
            TestData {
                input: "let foobar = y;",
                expected_ident: "foobar".to_string(),
                expected_expr: Expr::Ident("y".to_string()),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let program = parse(test_datum.input);

            assert_eq!(program.len(), 1);
            assert!(matches!(program[0], Statement::Let(_, _)));

            if let Statement::Let(ident, expr) = &program[0] {
                assert_identifier(&Expr::Ident(ident.into()), &test_datum.expected_ident);
                assert_literal_expression(&expr, &test_datum.expected_expr);
            }
        });
    }

    #[test]
    fn return_statement() {
        struct TestData {
            input: &'static str,
            expected_expr: Expr,
        }

        let test_data = vec![
            TestData {
                input: "return 5;",
                expected_expr: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "return 3 + 4;",
                expected_expr: Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(3))),
                    Box::new(Expr::Literal(Literal::Int(4))),
                ),
            },
            TestData {
                input: "return add(3, 4);",
                expected_expr: Expr::Call {
                    function: Box::new(Expr::Ident("add".to_string())),
                    arguments: vec![
                        Expr::Literal(Literal::Int(3)),
                        Expr::Literal(Literal::Int(4)),
                    ],
                },
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let program = parse(test_datum.input);

            assert_eq!(program.len(), 1);
            assert!(matches!(program[0], Statement::Return(_)));

            if let Statement::Return(expr) = &program[0] {
                assert_literal_expression(expr, &test_datum.expected_expr);
            }
        });
    }

    #[test]
    fn identifier_expression() {
        let input = r#"foobar;"#;

        let program = parse(input);

        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(_)));

        if let Statement::Expression(expr) = &program[0] {
            assert_literal_expression(expr, &Expr::Ident("foobar".to_string()));
        }
    }

    #[test]
    fn integer_literal_expression() {
        let input = r#"5;"#;

        let program = parse(input);

        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(_)));

        if let Statement::Expression(expr) = &program[0] {
            assert_literal_expression(expr, &Expr::Literal(Literal::Int(5)));
        }
    }

    #[test]
    fn prefix_expressions() {
        struct TestData {
            input: &'static str,
            operator: Prefix,
            integer_value: Expr,
        }

        let test_data = vec![
            TestData {
                input: "!5;",
                operator: Prefix::Bang,
                integer_value: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "-15;",
                operator: Prefix::Minus,
                integer_value: Expr::Literal(Literal::Int(15)),
            },
            TestData {
                input: "!true;",
                operator: Prefix::Bang,
                integer_value: Expr::Literal(Literal::Bool(true)),
            },
            TestData {
                input: "!false;",
                operator: Prefix::Bang,
                integer_value: Expr::Literal(Literal::Bool(false)),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let program = parse(test_datum.input);

            assert_eq!(program.len(), 1);
            assert!(matches!(program[0], Statement::Expression(_)));

            if let Statement::Expression(Expr::Prefix(operator, right_expr)) = &program[0] {
                assert_eq!(operator, &test_datum.operator);
                assert_literal_expression(right_expr, &test_datum.integer_value);
            }
        })
    }

    #[test]
    fn infix_expressions() {
        struct TestData {
            input: &'static str,
            left: Expr,
            operator: Infix,
            right: Expr,
        }

        let test_data = vec![
            TestData {
                input: "5 + 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::Plus,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 - 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::Minus,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 * 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::Multiply,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 / 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::Divide,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 > 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::GreaterThan,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 < 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::LessThan,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 == 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::Equal,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "5 != 5",
                left: Expr::Literal(Literal::Int(5)),
                operator: Infix::NotEqual,
                right: Expr::Literal(Literal::Int(5)),
            },
            TestData {
                input: "true == true",
                left: Expr::Literal(Literal::Bool(true)),
                operator: Infix::Equal,
                right: Expr::Literal(Literal::Bool(true)),
            },
            TestData {
                input: "true != false",
                left: Expr::Literal(Literal::Bool(true)),
                operator: Infix::NotEqual,
                right: Expr::Literal(Literal::Bool(false)),
            },
            TestData {
                input: "false == false",
                left: Expr::Literal(Literal::Bool(false)),
                operator: Infix::Equal,
                right: Expr::Literal(Literal::Bool(false)),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let program = parse(test_datum.input);
            assert_eq!(program.len(), 1);
            assert!(matches!(program[0], Statement::Expression(_)));

            if let Statement::Expression(expr) = &program[0] {
                assert_infix_expression(
                    expr,
                    &test_datum.left,
                    test_datum.operator,
                    &test_datum.right,
                );
            }
        })
    }

    #[test]
    fn operator_precedence() {
        struct TestData {
            input: &'static str,
            ast: Expr,
        }

        let test_data = vec![
            TestData {
                input: "-a * b",
                ast: Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Prefix(
                        Prefix::Minus,
                        Box::new(Expr::Ident("a".to_string())),
                    )),
                    Box::new(Expr::Ident("b".to_string())),
                ),
            },
            TestData {
                input: "a + b / c",
                ast: Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident("a".to_string())),
                    Box::new(Expr::Infix(
                        Infix::Divide,
                        Box::new(Expr::Ident("b".to_string())),
                        Box::new(Expr::Ident("c".to_string())),
                    )),
                ),
            },
            TestData {
                input: "5 > 4 == 3 < 4",
                ast: Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Infix(
                        Infix::GreaterThan,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::LessThan,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                ),
            },
            TestData {
                input: "(5 + 5) * 2",
                ast: Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Int(2))),
                ),
            },
            TestData {
                input: "-50 + 100 + -50",
                ast: Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Prefix(
                            Prefix::Minus,
                            Box::new(Expr::Literal(Literal::Int(50))),
                        )),
                        Box::new(Expr::Literal(Literal::Int(100))),
                    )),
                    Box::new(Expr::Prefix(
                        Prefix::Minus,
                        Box::new(Expr::Literal(Literal::Int(50))),
                    )),
                ),
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let program = parse(test_datum.input);
            assert_eq!(program.len(), 1);
            if let Statement::Expression(expr) = &program[0] {
                assert_eq!(expr, &test_datum.ast);
            }
        });
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";

        let tokens = Lexer::new().lex_input(input).unwrap();
        let program = Parser::parse(TokenSlice::from_tokens(&tokens)).unwrap();

        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(Expr::If { .. })));

        if let Statement::Expression(Expr::If {
            condition,
            consequence,
            ..
        }) = &program[0]
        {
            assert_infix_expression(
                condition,
                &Expr::Ident("x".to_string()),
                Infix::LessThan,
                &Expr::Ident("y".to_string()),
            );
            assert_eq!(consequence.len(), 1);
            if let Statement::Expression(expr) = &consequence[0] {
                assert_identifier(expr, "x");
            }
        }
    }

    #[test]
    fn if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let program = parse(input);

        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(Expr::If { .. })));

        if let Statement::Expression(Expr::If {
            condition,
            consequence,
            alternative,
        }) = &program[0]
        {
            assert_infix_expression(
                condition,
                &Expr::Ident("x".to_string()),
                Infix::LessThan,
                &Expr::Ident("y".to_string()),
            );
            assert_eq!(consequence.len(), 1);
            if let Statement::Expression(expr) = &consequence[0] {
                assert_identifier(expr, "x");
            }
            assert!(alternative.is_some());
            if let Some(Statement::Expression(alt_expr)) = alternative.clone().unwrap().first() {
                assert_identifier(alt_expr, "y");
            }
        }
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";
        let expected_params = vec![Expr::Ident("x".to_string()), Expr::Ident("y".to_string())];
        let (expected_infix, expected_left, expected_right) = (
            Infix::Plus,
            Expr::Ident("x".to_string()),
            Expr::Ident("y".to_string()),
        );

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(
            program[0],
            Statement::Expression(Expr::Function { .. })
        ));

        if let Statement::Expression(Expr::Function { params, body }) = &program[0] {
            assert_eq!(params.len(), 2);
            params
                .iter()
                .zip(expected_params)
                .for_each(|(actual_param, expected_param)| {
                    assert_literal_expression(&Expr::Ident(actual_param.clone()), &expected_param)
                });

            assert_eq!(body.len(), 1);
            assert!(matches!(
                body[0],
                Statement::Expression(Expr::Infix(_, _, _))
            ));
            if let Some(Statement::Expression(expr)) = body.first() {
                assert_infix_expression(expr, &expected_left, expected_infix, &expected_right);
            }
        }
    }

    #[test]
    fn function_parameter_parsing() {
        struct TestData {
            input: &'static str,
            params: Vec<Ident>,
        }

        let test_data = vec![
            TestData {
                input: "fn() {};",
                params: vec![],
            },
            TestData {
                input: "fn(x) {};",
                params: vec!["x".to_string()],
            },
            TestData {
                input: "fn(x, y, z) {};",
                params: vec!["x".to_string(), "y".to_string(), "z".to_string()],
            },
        ];

        test_data.into_iter().for_each(|test_datum| {
            let program = parse(test_datum.input);
            assert_eq!(program.len(), 1);
            assert!(matches!(
                program[0],
                Statement::Expression(Expr::Function { .. })
            ));

            if let Statement::Expression(Expr::Function { params, .. }) = &program[0] {
                params
                    .iter()
                    .zip(test_datum.params)
                    .for_each(|(actual, expected)| assert_eq!(actual, &expected))
            }
        });
    }

    #[test]
    fn call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(
            program[0],
            Statement::Expression(Expr::Call { .. })
        ));

        if let Statement::Expression(Expr::Call {
            function,
            arguments,
        }) = &program[0]
        {
            assert!(matches!(**function, Expr::Ident(_)));
            assert_identifier(function, "add");
            assert_literal_expression(&arguments[0], &Expr::Literal(Literal::Int(1)));
            assert_infix_expression(
                &arguments[1],
                &Expr::Literal(Literal::Int(2)),
                Infix::Multiply,
                &Expr::Literal(Literal::Int(3)),
            );
            assert_infix_expression(
                &arguments[2],
                &Expr::Literal(Literal::Int(4)),
                Infix::Plus,
                &Expr::Literal(Literal::Int(5)),
            );
        }
    }

    #[test]
    fn string_literal_expr() {
        let input = r#""hello world""#;

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(_)));
        if let Statement::Expression(expr) = &program[0] {
            assert_literal_expression(
                expr,
                &Expr::Literal(Literal::String("hello world".to_string())),
            );
        }
    }

    #[test]
    fn parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(Expr::Array(..))));
        if let Statement::Expression(Expr::Array(elements)) = &program[0] {
            assert_eq!(elements.len(), 3);
            assert_literal_expression(&elements[0], &Expr::Literal(Literal::Int(1)));
            assert_infix_expression(
                &elements[1],
                &Expr::Literal(Literal::Int(2)),
                Infix::Multiply,
                &Expr::Literal(Literal::Int(2)),
            );
            assert_infix_expression(
                &elements[2],
                &Expr::Literal(Literal::Int(3)),
                Infix::Plus,
                &Expr::Literal(Literal::Int(3)),
            );
        }
    }

    #[test]
    fn parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(program[0], Statement::Expression(Expr::Index(..))));
        if let Statement::Expression(Expr::Index(left, index_expr)) = &program[0] {
            assert_identifier(left, "myArray");
            assert_infix_expression(
                index_expr,
                &Expr::Literal(Literal::Int(1)),
                Infix::Plus,
                &Expr::Literal(Literal::Int(1)),
            );
        }
    }

    #[test]
    fn parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;
        let mut expected = BTreeMap::new();
        expected.insert(
            Expr::Literal(Literal::String("one".to_string())),
            Expr::Literal(Literal::Int(1)),
        );
        expected.insert(
            Expr::Literal(Literal::String("two".to_string())),
            Expr::Literal(Literal::Int(2)),
        );
        expected.insert(
            Expr::Literal(Literal::String("three".to_string())),
            Expr::Literal(Literal::Int(3)),
        );

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(
            program[0],
            Statement::Expression(Expr::HashLiteral(..))
        ));
        if let Statement::Expression(Expr::HashLiteral(actual)) = &program[0] {
            assert_eq!(actual, &expected);
        }
    }

    #[test]
    fn parsing_empty_hash_literal() {
        let input = "{}";
        let expected = BTreeMap::new();

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(
            program[0],
            Statement::Expression(Expr::HashLiteral(..))
        ));
        if let Statement::Expression(Expr::HashLiteral(actual)) = &program[0] {
            assert_eq!(actual, &expected);
        }
    }

    #[test]
    fn parsing_hash_literal_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
        let mut expected = BTreeMap::new();
        expected.insert(
            Expr::Literal(Literal::String("one".to_string())),
            Expr::Infix(
                Infix::Plus,
                Box::new(Expr::Literal(Literal::Int(0))),
                Box::new(Expr::Literal(Literal::Int(1))),
            ),
        );
        expected.insert(
            Expr::Literal(Literal::String("two".to_string())),
            Expr::Infix(
                Infix::Minus,
                Box::new(Expr::Literal(Literal::Int(10))),
                Box::new(Expr::Literal(Literal::Int(8))),
            ),
        );
        expected.insert(
            Expr::Literal(Literal::String("three".to_string())),
            Expr::Infix(
                Infix::Divide,
                Box::new(Expr::Literal(Literal::Int(15))),
                Box::new(Expr::Literal(Literal::Int(5))),
            ),
        );

        let program = parse(input);
        assert_eq!(program.len(), 1);
        assert!(matches!(
            program[0],
            Statement::Expression(Expr::HashLiteral(..))
        ));
        if let Statement::Expression(Expr::HashLiteral(actual)) = &program[0] {
            actual.iter().zip(expected).for_each(
                |((actual_key, actual_value), (expected_key, expected_value))| {
                    assert_literal_expression(actual_key, &expected_key);
                    assert_eq!(actual_value, &expected_value);
                },
            );
        }
    }

    fn parse(input: &str) -> Program {
        let tokens = Lexer::new().lex_input(input).unwrap();
        Parser::parse(TokenSlice::from_tokens(&tokens)).unwrap()
    }

    fn assert_identifier(expr: &Expr, expected_ident: &str) {
        assert!(matches!(expr, Expr::Ident(_)));
        if let Expr::Ident(ident) = expr {
            assert_eq!(ident, expected_ident);
        }
    }

    fn assert_literal_expression(expr: &Expr, expected: &Expr) {
        assert_eq!(expr, expected);
    }

    fn assert_infix_expression(expr: &Expr, left: &Expr, op: Infix, right: &Expr) {
        assert!(matches!(expr, Expr::Infix(_, _, _)));
        if let Expr::Infix(actual_op, actual_left, actual_right) = expr {
            assert_literal_expression(actual_left, left);
            assert_eq!(actual_op, &op);
            assert_literal_expression(actual_right, right);
        }
    }
}
