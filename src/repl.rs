use std::{
    io,
    io::{stdin, stdout, BufRead, Write},
};

use crate::lexer::lex_input;

pub const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let mut buffer = String::new();
    let stdin = stdin();
    let mut stdout = stdout();

    loop {
        write!(stdout, "{}", PROMPT)?;
        stdout.flush()?;
        stdin.lock().read_line(&mut buffer)?;

        let tokens = lex_input(&buffer);

        for token in tokens {
            writeln!(stdout, "{:?}", token)?;
        }

        buffer.clear();
    }
}
