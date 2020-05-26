use libmonkey::{
    lexer::{token::TokenSlice, Lexer},
    lexer_ng::{self, token_ng},
    parser_ng::ParserNg,
    util::convert_error_tokenslice,
};
use token_ng::{Token, TokenType};

fn main() {
    let tokens1 = Lexer::new().lex_input("let a = 10;").unwrap();
    let tokens2 = Lexer::new().lex_input("return 10;").unwrap();
    let tokens3 = Lexer::new().lex_input("fn(x) { x + y; }").unwrap();
    let tokens4ng = lexer_ng::Lexer::lex_input(
        r#"let five = 5;
    let ten = 10;
    let add = fn(x, y) {
        return x + y;
    };
    
    puts(add(five, ten));
    let myArray = [1, 2, 3, 4, 5];
    puts(first(myArray));"#,
    );

    let ast1 = ParserNg::parse(TokenSlice::from_tokens(&tokens1));
    let ast2 = ParserNg::parse(TokenSlice::from_tokens(&tokens2));
    let ast3 = ParserNg::parse(TokenSlice::from_tokens(&tokens3));

    match ast1 {
        Ok(program) => eprintln!("program: {:?}", program),
        _ => panic!("unreachale!"),
    };

    match ast2 {
        Ok(program) => eprintln!("program: {:?}", program),
        _ => panic!("unreachale!"),
    };

    match ast3 {
        Err(e) => eprintln!(
            "{}",
            convert_error_tokenslice(TokenSlice::from_tokens(&tokens3), e)
        ),
        _ => panic!("unreachable!"),
    };

    let token_ng = Token {
        token_type: TokenType::Let,
        line: 1,
        span: (0, 2),
    };

    eprintln!("Token NG: {:?}", token_ng);
    eprintln!("Lexer NG: {:?}", tokens4ng);
}
