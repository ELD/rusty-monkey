use std::{
    io,
    io::{stdin, stdout, BufRead, Write},
};

use crate::lexer::Lexer;

pub const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let mut buffer = String::new();

    let stdin = stdin();

    let mut stdout = stdout();

    loop {
        write!(stdout, "{}", PROMPT)?;

        stdout.flush()?;

        stdin.lock().read_line(&mut buffer)?;

        let tokens = Lexer::new().lex_input(&buffer);
        // let ast = Parser::parse_program(&tokens);

        writeln!(stdout, "{:?}", tokens)?;
        // writeln!(stdout, "{:?}", ast)?;

        buffer.clear();
    }
}
