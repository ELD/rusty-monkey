use std::{
    io,
    io::{stdin, stdout, BufRead, Write},
};

use crate::parser::Parser;

pub const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let mut buffer = String::new();

    let stdin = stdin();

    let mut stdout = stdout();

    loop {
        write!(stdout, "{}", PROMPT)?;

        stdout.flush()?;

        stdin.lock().read_line(&mut buffer)?;

        let ast = Parser::parse_program(&buffer);
        ast.statements.into_iter().for_each(|statement| {
            writeln!(stdout, "{:?}", statement).unwrap();
        });

        buffer.clear();
    }
}
