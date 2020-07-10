pub mod eval;
pub mod lexer;
pub mod parser;
pub mod repl;

#[cfg(test)]
mod test {
    #[test]
    fn it_works() {
        assert_eq!(4, 2 + 2);
    }
}
