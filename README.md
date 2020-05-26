# Rusty Monkey

A Monkey implementation in Rust using parser combinators for the lexer and parser
from the book _Writing an Interpreter in Go_.

## Todo
- [ ] Better parser error handling
- [ ] Evaluator optimizations (if any)
- [ ] Implement the compiler elements from _Writing a Compiler in Go_

## Ideas
- Expand the `Token` type to have a line number and span
  ```rust
  struct Token {
      type: TokenType,
      line: u32,
      span: (u32, u32),
  }

  struct TokenType {
      Let,
      Return,
      ..
  }

  struct TokenStream {
      slice: &'a [Token],
  }
  ```