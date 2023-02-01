#![allow(clippy::upper_case_acronyms)]
#![allow(dead_code, unused)]

mod ast;
mod lexer;
mod repl;
mod token;

fn main() {
    repl::REPL::run();
}
