use std::{
    io,
    io::{stdin, stdout, BufRead, Write},
};

use crate::{
    eval::{environment::Environment, Evaluator},
    lexer::{token::TokenSlice, Lexer},
    parser::Parser,
};
use std::{cell::RefCell, rc::Rc};

pub const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let mut buffer = String::new();
    let stdin = stdin();
    let mut stdout = stdout();

    let environment = Rc::new(RefCell::new(Environment::new()));

    loop {
        write!(stdout, "{}", PROMPT)?;

        stdout.flush()?;

        stdin.lock().read_line(&mut buffer)?;

        let tokens = Lexer::new().lex_input(&buffer).unwrap();
        let program = Parser::parse(TokenSlice::from_tokens(&tokens)).unwrap();
        let result = Evaluator::new_with_custom_environment(environment.clone()).eval(program);

        writeln!(stdout, "{}", result)?;

        buffer.clear();
    }
}
