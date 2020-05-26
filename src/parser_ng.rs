use crate::{
    lexer::token::{Token, TokenSlice},
    parser::ast::{Expr, Program, Statement},
};
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::map,
    error::{make_error, ErrorKind, VerboseError},
    multi::many1,
    Err, IResult, InputIter, InputLength, Needed,
};

pub struct ParserNg;

impl ParserNg {
    pub fn parse(tokens: TokenSlice<'_>) -> Result<Program, VerboseError<TokenSlice<'_>>> {
        match Self::parse_statements(tokens) {
            Ok((_, program)) => Ok(program),
            Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(e),
            e => panic!("{:?}", e),
        }
    }

    fn parse_statements<'a>(
        tokens: TokenSlice<'a>,
    ) -> IResult<TokenSlice<'a>, Program, VerboseError<TokenSlice<'a>>> {
        many1(Self::parse_statement)(tokens)
    }

    fn parse_statement<'a>(
        input: TokenSlice<'a>,
    ) -> IResult<TokenSlice<'a>, Statement, VerboseError<TokenSlice<'a>>> {
        alt((Self::parse_let_statement, Self::parse_return_statement))(input)
    }

    fn parse_let_statement<'a>(
        input: TokenSlice<'a>,
    ) -> IResult<TokenSlice<'a>, Statement, VerboseError<TokenSlice<'a>>> {
        map(Self::tag(Token::Let), |_| {
            Statement::Let("thing".to_string(), Expr::Nil)
        })(input)
    }

    fn parse_return_statement<'a>(
        input: TokenSlice<'a>,
    ) -> IResult<TokenSlice<'a>, Statement, VerboseError<TokenSlice<'a>>> {
        map(Self::tag(Token::Return), |_| Statement::Return(Expr::Nil))(input)
    }

    fn tag<'a>(
        tag: Token,
    ) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Token, VerboseError<TokenSlice<'a>>>
    {
        move |input: TokenSlice<'_>| match take::<_, _, VerboseError<_>>(1usize)(input.clone()) {
            Ok((i, res)) if res.iter_elements().next() == Some(&tag) => Ok((i, tag.clone())),
            Ok((_, res)) if res.input_len() == 0 => Err(Err::Incomplete(Needed::Size(1))),
            Ok((i, _)) => Err(Err::Error(make_error(i, ErrorKind::Tag))),
            _ => Err(Err::Error(make_error(input, ErrorKind::Count))),
        }
    }
}
