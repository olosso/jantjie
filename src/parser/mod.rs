use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    /// Parser::new
    /// Create a new parser given a lexer.
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    /// Parser::next_token
    /// Advance the lexer of the parser to receive a new token. Also updates the peek_token.
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }

    /// Parser::parse_program
    /// Goes through tokens found by the lexer and trys to find the statements.
    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        /// REVIEW It's kinda intense!
        /// Gets a new token given by the lexer, and tries to parse a statement based on the token.
        /// Pushes each statement to the program.
        loop {
            let current_token = &self.current_token;
            match current_token {
                Some(Token {
                    token_type: x,
                    literal: l,
                }) => {
                    if *x == TokenType::EOF {
                        break;
                    } else if let Some(statement) = self.parse_statement() {
                        program.statements.push(statement);
                    }
                }
                None => panic!(),
            }
            self.next_token();
        }

        program
    }

    /// Parser::parse_statement
    /// Parses each different type of statement.
    fn parse_statement(&self) -> Option<Statement> {
        if let Some(Token {
            token_type: t,
            literal: l,
        }) = &self.current_token
        {
            match t {
                TokenType::Let => Some(self.parse_let_statement()),
                _ => panic!(),
            }
        } else {
            None
        }
    }

    /// Parser::parse_let_statement
    fn parse_let_statement(&self) -> Statement {
        todo!()
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn test_parse_let_statement() {
        let input = String::from(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program: Program = parser.parse_program();

        assert_eq!(
            program.statements.len(),
            3,
            "Expected program to have 3 statements, but received {:?}",
            program.statements.len()
        );

        let expected_identifiers = vec!["x", "y", "foobar"];

        for (i, identifier) in expected_identifiers.into_iter().enumerate() {
            let statement = &program.statements[i];
            test_let_statement(statement, String::from(identifier));
        }
    }

    fn test_let_statement(statement: &Statement, identifier: String) {
        assert_eq!(statement.token_literal(), "let");
        //assert_eq!(statement.name.token.literal, identifier);
        assert!(matches!(statement, Statement::Let(_, _)))
    }
}
