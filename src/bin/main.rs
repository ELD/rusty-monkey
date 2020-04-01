use libmonkey::repl;
use std::io;

fn main() -> io::Result<()> {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::start()
}
