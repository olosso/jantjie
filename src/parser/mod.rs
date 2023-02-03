mod tests;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

#[derive(Debug)]
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
                    } else if x.is_statement() {
                        program.statements.push(self.parse_statement());
                    }
                }
                None => {
                    panic!("Current token doesn't have a value after parser initialization.")
                }
            }
            self.next_token();
        }

        program
    }

    /// Parser::parse_statement
    /// Parses each different type of statement.
    fn parse_statement(&mut self) -> Statement {
        // NOTE This unwrap should never panic.
        let Token {
            token_type: tt,
            literal: l,
        } = &self.current_token.as_ref().unwrap();

        match tt {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => panic!("Attempt to parse a non-statement token as a statement token: {tt:?}"),
        }
    }

    /// Parser::parse_let_statement
    fn parse_let_statement(&mut self) -> Statement {
        // REVIEW Can this unwrap ever panic?
        if !self.expect_peek(TokenType::Ident) {
            panic!("let statement not followed by an identifier.")
        }

        let ident = self.current_token.as_ref().unwrap();
        // REVIEW Is cloning necessary? Done here to avoid using references that would lead to lifetimes.
        let statement = Statement::Let(ident.clone(), Some(Expression::Placeholder));

        if !self.expect_peek(TokenType::Assign) {
            panic!("Identifier not followed by an assignment in let statement.")
        }

        // TODO Ignoring expression for now
        while !self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_return_statement(&mut self) -> Statement {
        let statement = Statement::Return(Some(Expression::Placeholder));

        // TODO Ignoring expression for now
        while !self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn cur_tokentype_is(&self, tt: TokenType) -> bool {
        self.current_token.as_ref().unwrap().token_type == tt
    }

    fn peek_tokentype_is(&self, tt: TokenType) -> bool {
        self.peek_token.as_ref().unwrap().token_type == tt
    }

    fn expect_peek(&mut self, tt: TokenType) -> bool {
        if self.peek_tokentype_is(tt) {
            self.next_token();
            true
        } else {
            false
        }
    }
}
