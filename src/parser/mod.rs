pub mod ast;

use crate::parser::ast::{
    Block, Expr, Ident, Infix, Literal, Precedence, Prefix, Program, Statement,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while, take_while1},
    character::complete::alpha1,
    combinator::{map, peek},
    error::ParseError,
    multi::many0,
    sequence::{delimited, preceded},
    Err::Error,
    IResult,
};

pub struct Parser {
    pub statements: Program,
}

impl Parser {
    pub fn parse_program(input: &str) -> Self {
        let statements = match many0(Self::parse_statement())(input) {
            Ok((_, statements)) => statements,
            Err(e) => panic!("{}", e),
        };

        Self { statements }
    }

    fn parse_statement<'a>() -> impl FnMut(&'a str) -> IResult<&'a str, Statement> {
        alt((
            Self::parse_let_statement,
            Self::parse_return_statement,
            Self::parse_expression_statement,
        ))
    }

    fn parse_let_statement(input: &str) -> IResult<&str, Statement> {
        let (i, _) = preceded(Self::whitespace, tag("let"))(input)?;

        let (i, ident) = map(Self::parse_ident, |ident| ident)(i)?;

        let (i, _) = preceded(Self::whitespace, tag("="))(i)?;
        let (i, expr) = Self::parse_expression(i, Precedence::Lowest)?;

        let (i, _) = if Self::peek_semicolon(i).is_ok() {
            Self::parse_semicolon(i)?
        } else {
            (i, i)
        };

        Ok((i, Statement::Let(ident, expr)))
    }

    fn parse_return_statement(input: &str) -> IResult<&str, Statement> {
        let (i, _) = preceded(Self::whitespace, tag("return"))(input)?;

        let (i, expr) = Self::parse_expression(i, Precedence::Lowest)?;

        let (i, _) = if Self::peek_semicolon(i).is_ok() {
            Self::parse_semicolon(i)?
        } else {
            (i, i)
        };

        Ok((i, Statement::Return(expr)))
    }

    fn parse_expression_statement(input: &str) -> IResult<&str, Statement> {
        let (i, expr) = Self::parse_expression(input, Precedence::Lowest)?;

        let peek_semicolon: IResult<&str, &str> = peek(tag(";"))(i);
        let returned_input = if peek_semicolon.is_ok() {
            take(1usize)(i)?.0
        } else {
            i
        };

        Ok((returned_input, Statement::Expression(expr)))
    }

    fn parse_ident_expression(input: &str) -> IResult<&str, Expr> {
        map(Self::parse_ident, Expr::Ident)(input)
    }

    fn parse_integer_expression(input: &str) -> IResult<&str, Expr> {
        map(Self::parse_integer, |integer| {
            Expr::Literal(Literal::Int(integer))
        })(input)
    }

    fn parse_boolean_expression(input: &str) -> IResult<&str, Expr> {
        map(Self::parse_boolean, |boolean_literal| {
            Expr::Literal(Literal::Bool(boolean_literal))
        })(input)
    }

    fn parse_ident(input: &str) -> IResult<&str, Ident> {
        map(
            preceded(
                Self::whitespace,
                preceded(
                    peek(alt((alpha1, tag("_")))),
                    take_while1(|c: char| c.is_alphanumeric() || c == '_'),
                ),
            ),
            |ident: &str| ident.into(),
        )(input)
    }

    fn parse_integer(input: &str) -> IResult<&str, i64> {
        map(
            preceded(Self::whitespace, take_while1(|c: char| c.is_numeric())),
            |number: &str| number.parse::<i64>().unwrap(),
        )(input)
    }

    fn parse_boolean(input: &str) -> IResult<&str, bool> {
        map(
            preceded(Self::whitespace, alt((tag("true"), tag("false")))),
            |bool_literal| bool_literal == "true",
        )(input)
    }

    fn parse_expression(input: &str, precedence: Precedence) -> IResult<&str, Expr> {
        let (mut i, mut left) = alt((
            Self::parse_boolean_expression,
            Self::parse_integer_expression,
            Self::parse_prefix_expression,
            Self::parse_paren_expression,
            Self::parse_if_expression,
            Self::parse_fn_literal_expression,
            Self::parse_ident_expression,
        ))(input)?;

        while Self::peek_semicolon(i).is_err() && precedence < Self::peek_precedence(i) {
            if peek(Self::parse_operator)(i).is_err() {
                break;
            }

            let (inner_i, inner_left) = Self::parse_infix_expression(i, left.clone())?;
            left = inner_left;
            i = inner_i;
        }

        Ok((i, left))
    }

    fn parse_prefix_expression(input: &str) -> IResult<&str, Expr> {
        let peek: IResult<&str, &str> =
            peek(preceded(Self::whitespace, alt((tag("!"), tag("-")))))(input);

        if peek.is_err() {
            return Err(Error((input, nom::error::ErrorKind::Tag)));
        }

        let (i, prefix) = map(
            preceded(Self::whitespace, alt((tag("!"), tag("-")))),
            |prefix_op| {
                if prefix_op == "!" {
                    Prefix::Bang
                } else {
                    Prefix::Minus
                }
            },
        )(input)?;

        let (i, expr) = Self::parse_expression(i, Precedence::Prefix)?;

        Ok((i, Expr::Prefix(prefix, Box::new(expr))))
    }

    fn parse_infix_expression(input: &str, left: Expr) -> IResult<&str, Expr> {
        let (i, expr) = if Self::peek_tag("(", input).is_ok() {
            let (i, arguments) = Self::parse_call_arguments(input)?;
            (
                i,
                Expr::Call {
                    function: Box::new(left),
                    arguments,
                },
            )
        } else {
            let (i, (precedence, operator)) = Self::parse_operator(input)?;

            let (i, right) = Self::parse_expression(i, precedence)?;
            (
                i,
                Expr::Infix(operator.unwrap(), Box::new(left), Box::new(right)),
            )
        };

        Ok((i, expr))
    }

    fn parse_paren_expression(input: &str) -> IResult<&str, Expr> {
        let (i, _) = preceded(Self::whitespace, tag("("))(input)?;
        let (i, expr) = Self::parse_expression(i, Precedence::Lowest)?;
        let (i, _) = preceded(Self::whitespace, tag(")"))(i)?;

        Ok((i, expr))
    }

    fn parse_if_expression(input: &str) -> IResult<&str, Expr> {
        let (i, _) = preceded(Self::whitespace, tag("if"))(input)?;
        let (i, condition) = Self::parse_expression(i, Precedence::Lowest)?;
        let (i, _) = preceded(Self::whitespace, tag("{"))(i)?;
        let (i, consequence) = Self::parse_block_statement(i)?;

        let (i, alternative) = if Self::peek_else(i).is_ok() {
            let (i, _) = Self::parse_else(i)?;
            let (i, alternative) = Self::parse_block_statement(i)?;
            (i, Some(alternative))
        } else {
            (i, None)
        };

        Ok((
            i,
            Expr::If {
                condition: Box::new(condition),
                consequence,
                alternative,
            },
        ))
    }

    fn parse_block_statement(mut input: &str) -> IResult<&str, Block> {
        let mut statements = Block::new();
        while Self::peek_right_brace(input).is_err() {
            let (inner_input, statement) = Self::parse_statement()(input)?;
            statements.push(statement);
            input = inner_input;
        }

        let (input, _) = preceded(Self::whitespace, tag("}"))(input)?;

        Ok((input, statements))
    }

    fn parse_fn_literal_expression(input: &str) -> IResult<&str, Expr> {
        let (i, _) = preceded(Self::whitespace, tag("fn"))(input)?;
        let (i, _) = preceded(Self::whitespace, tag("("))(i)?;
        let (i, params) = Self::parse_fn_params(i)?;
        let (i, _) = preceded(Self::whitespace, tag("{"))(i)?;

        let (i, body) = Self::parse_block_statement(i)?;

        Ok((i, Expr::Function { params, body }))
    }

    fn parse_fn_params(input: &str) -> IResult<&str, Vec<Ident>> {
        let mut idents = Vec::new();

        if Self::peek_right_paren(input).is_ok() {
            let (i, _) = Self::parse_right_paren(input)?;
            return Ok((i, idents));
        }

        let (mut i, ident) = Self::parse_ident(input)?;
        idents.push(ident);

        while Self::peek_comma(i).is_ok() {
            let (inner_i, _) = Self::parse_comma(i)?;
            let (inner_i, ident) = Self::parse_ident(inner_i)?;
            idents.push(ident);
            i = inner_i;
        }

        let (i, _) = preceded(Self::whitespace, tag(")"))(i)?;

        Ok((i, idents))
    }

    fn parse_call_arguments(input: &str) -> IResult<&str, Vec<Expr>> {
        let (i, _) = preceded(Self::whitespace, tag("("))(input)?;

        let (i, expr) = Self::parse_expression(i, Precedence::Lowest)?;

        let (i, mut expr_list) = many0(Self::parse_comma_expr)(i)?;

        let (i, _) = preceded(Self::whitespace, tag(")"))(i)?;

        expr_list.insert(0, expr);

        Ok((i, expr_list))
    }

    fn parse_comma_expr(input: &str) -> IResult<&str, Expr> {
        let (i, _) = preceded(Self::whitespace, tag(","))(input)?;
        let (i, expr) = Self::parse_expression(i, Precedence::Lowest)?;

        Ok((i, expr))
    }

    fn parse_operator(input: &str) -> IResult<&str, (Precedence, Option<Infix>)> {
        preceded(
            Self::whitespace,
            alt((
                map(tag("+"), |_| (Precedence::Sum, Some(Infix::Plus))),
                map(tag("-"), |_| (Precedence::Sum, Some(Infix::Minus))),
                map(tag("*"), |_| (Precedence::Product, Some(Infix::Multiply))),
                map(tag("/"), |_| (Precedence::Product, Some(Infix::Divide))),
                map(tag("=="), |_| (Precedence::Equals, Some(Infix::Equal))),
                map(tag("!="), |_| (Precedence::Equals, Some(Infix::NotEqual))),
                map(tag(">="), |_| {
                    (Precedence::LessGreater, Some(Infix::GreaterThanEqual))
                }),
                map(tag("<="), |_| {
                    (Precedence::LessGreater, Some(Infix::LessThanEqual))
                }),
                map(tag(">"), |_| {
                    (Precedence::LessGreater, Some(Infix::GreaterThan))
                }),
                map(tag("<"), |_| {
                    (Precedence::LessGreater, Some(Infix::LessThan))
                }),
                map(tag("("), |_| (Precedence::Call, None)),
            )),
        )(input)
    }

    fn parse_else(input: &str) -> IResult<&str, &str> {
        delimited(
            preceded(Self::whitespace, tag("else")),
            Self::whitespace,
            preceded(Self::whitespace, tag("{")),
        )(input)
    }

    fn parse_comma(input: &str) -> IResult<&str, &str> {
        preceded(Self::whitespace, tag(","))(input)
    }

    fn parse_right_paren(input: &str) -> IResult<&str, &str> {
        preceded(Self::whitespace, tag(")"))(input)
    }

    fn parse_semicolon(input: &str) -> IResult<&str, &str> {
        preceded(Self::whitespace, tag(";"))(input)
    }

    fn whitespace<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        let whitespace_chars = " \t\r\n";

        take_while(move |c| whitespace_chars.contains(c))(i)
    }

    fn peek_tag<'a>(tag_literal: &'a str, input: &'a str) -> IResult<&'a str, &'a str> {
        peek(preceded(Self::whitespace, tag(tag_literal)))(input)
    }

    fn peek_precedence(input: &str) -> Precedence {
        let peek_op_res: IResult<&str, (Precedence, Option<Infix>)> =
            peek(Self::parse_operator)(input);

        if let Ok((_, op)) = peek_op_res {
            op.0
        } else {
            Precedence::Lowest
        }
    }

    fn peek_semicolon(input: &str) -> IResult<&str, &str> {
        peek(Self::parse_semicolon)(input)
    }

    fn peek_comma(input: &str) -> IResult<&str, &str> {
        peek(Self::parse_comma)(input)
    }

    fn peek_else(input: &str) -> IResult<&str, &str> {
        peek(Self::parse_else)(input)
    }

    fn peek_right_brace(input: &str) -> IResult<&str, &str> {
        peek(preceded(Self::whitespace, tag("}")))(input)
    }

    fn peek_right_paren(input: &str) -> IResult<&str, &str> {
        peek(Self::parse_right_paren)(input)
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{
        ast::{Expr, Ident, Infix, Literal, Prefix, Statement},
        Parser,
    };

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
            let program = Parser::parse_program(test_datum.input);

            assert_eq!(program.statements.len(), 1);
            assert!(matches!(program.statements[0], Statement::Let(_, _)));

            if let Statement::Let(ident, expr) = &program.statements[0] {
                assert_identifier(&Expr::Ident(ident.into()), &test_datum.expected_ident);
                assert_literal_expression(expr, &test_datum.expected_expr);
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
            let program = Parser::parse_program(test_datum.input);

            assert_eq!(program.statements.len(), 1);
            assert!(matches!(program.statements[0], Statement::Return(_)));

            if let Statement::Return(expr) = &program.statements[0] {
                assert_literal_expression(expr, &test_datum.expected_expr);
            }
        });
    }

    #[test]
    fn identifier_expression() {
        let input = r#"foobar;"#;

        let program = Parser::parse_program(input);

        assert_eq!(program.statements.len(), 1);
        assert!(matches!(program.statements[0], Statement::Expression(_)));

        if let Statement::Expression(expr) = &program.statements[0] {
            assert_literal_expression(expr, &Expr::Ident("foobar".to_string()));
        }
    }

    #[test]
    fn integer_literal_expression() {
        let input = r#"5;"#;

        let program = Parser::parse_program(input);

        assert_eq!(program.statements.len(), 1);
        assert!(matches!(program.statements[0], Statement::Expression(_)));

        if let Statement::Expression(expr) = &program.statements[0] {
            assert_literal_expression(expr, &Expr::Literal(Literal::Int(5)));
        }
    }

    #[test]
    fn parsing_prefix_expressions() {
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
            let program = Parser::parse_program(test_datum.input);

            assert_eq!(program.statements.len(), 1);
            assert!(matches!(program.statements[0], Statement::Expression(_)));

            if let Statement::Expression(Expr::Prefix(operator, right_expr)) =
                &program.statements[0]
            {
                assert_eq!(operator, &test_datum.operator);
                assert_literal_expression(right_expr, &test_datum.integer_value);
            }
        })
    }

    #[test]
    fn parsing_infix_expressions() {
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
            let program = Parser::parse_program(test_datum.input);
            assert_eq!(program.statements.len(), 1);
            assert!(matches!(program.statements[0], Statement::Expression(_)));

            if let Statement::Expression(expr) = &program.statements[0] {
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
            let program = Parser::parse_program(test_datum.input);
            if let Statement::Expression(expr) = &program.statements[0] {
                assert_eq!(expr, &test_datum.ast);
            }
        });
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";

        let program = Parser::parse_program(input);

        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::Expression(Expr::If { .. })
        ));

        if let Statement::Expression(Expr::If {
            condition,
            consequence,
            ..
        }) = &program.statements[0]
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

        let program = Parser::parse_program(input);

        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::Expression(Expr::If { .. })
        ));

        if let Statement::Expression(Expr::If {
            condition,
            consequence,
            alternative,
        }) = &program.statements[0]
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

        let program = Parser::parse_program(input);
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::Expression(Expr::Function { .. })
        ));

        if let Statement::Expression(Expr::Function { params, body }) = &program.statements[0] {
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
            let program = Parser::parse_program(test_datum.input);
            assert_eq!(program.statements.len(), 1);
            assert!(matches!(
                program.statements[0],
                Statement::Expression(Expr::Function { .. })
            ));

            if let Statement::Expression(Expr::Function { params, .. }) = &program.statements[0] {
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

        let program = Parser::parse_program(input);
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0],
            Statement::Expression(Expr::Call { .. })
        ));

        if let Statement::Expression(Expr::Call {
            function,
            arguments,
        }) = &program.statements[0]
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
