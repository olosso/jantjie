use crate::ast::*;
use crate::evaluator::*;
use crate::lexer::*;
use crate::parser::*;
use std::io::{self, Write};

pub struct REPL {}

impl REPL {
    pub fn run() -> io::Result<()> {
        println!("Welcome to the REPL.");
        println!("Enter 'q' to quit the intrepreter.");
        loop {
            print!(">>> ");
            io::stdout().flush().unwrap();
            let mut buffer = String::new();
            io::stdin().read_line(&mut buffer)?;
            if buffer.as_str().trim() == "q" {
                break;
            }
            let mut lexer = Lexer::new(buffer);
            let mut parser = Parser::new(lexer);
            match parser.parse_program() {
                Ok(program) => {
                    let value = eval(&program).inspect();
                    dbg!(value);
                }
                Err(e) => {
                    println!("You got an error: {e}\nBut that's okay 😻");
                    continue;
                }
            }
        }

        println!("Farewell!");
        Ok(())
    }
}
