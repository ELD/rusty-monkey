use std::{
    io,
    io::{stdin, stdout, BufRead, Write},
};

use crate::{eval::Evaluator, parser::Parser};

pub const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let mut buffer = String::new();
    let mut evaluator = Evaluator::new();

    let stdin = stdin();

    let mut stdout = stdout();

    loop {
        write!(stdout, "{}", PROMPT)?;

        stdout.flush()?;

        stdin.lock().read_line(&mut buffer)?;

        let program = Parser::parse_program(&buffer);
        let result = evaluator.eval(program.statements);

        writeln!(stdout, "result: {}", result)?;

        buffer.clear();
    }
}
