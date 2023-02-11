mod tests;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    EMPTY, // Empty identifier _
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // foo(X)
}

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    prefix_parse_fns: HashMap<TokenType, fn() -> Option<Expression>>,
    infix_parse_fns: HashMap<TokenType, fn(Expression) -> Option<Expression>>,
}

impl Parser {
    /// Parser::new
    /// Create a new parser given a lexer.
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            prefix_parse_fns: Self::register_prefixes(),
            infix_parse_fns: Self::register_infixes(),
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
                    if x == &TokenType::EOF {
                        break;
                    } else if x != &TokenType::Semicolon {
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
            _ => self.parse_expression_statement(),
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

    fn parse_expression_statement(&mut self) -> Statement {
        let mut statement = Statement::Expr(
            self.current_token.as_ref().unwrap().clone(),
            self.parse_expression(Precedence::LOWEST),
        );

        // TODO Ignoring expression for now
        if self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_expression(&mut self, p: Precedence) -> Option<Expression> {
        let prefix = self
            .prefix_parse_fns
            .get(&self.current_token.as_ref().unwrap().token_type);

        if let Some(f) = prefix {
            Some(f().unwrap())
        } else {
            None
        }
    }

    fn parse_prefix_ident() -> Option<Expression> {
        Some(Expression::Placeholder)
    }

    fn parse_prefix_minus() -> Option<Expression> {
        Some(Expression::Placeholder)
    }

    fn parse_infix_minus(left: Expression) -> Option<Expression> {
        Some(Expression::Placeholder)
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

    fn register_prefixes() -> HashMap<TokenType, fn() -> Option<Expression>> {
        let mut prefix_fns = HashMap::new();

        prefix_fns.insert(
            TokenType::Ident,
            Self::parse_prefix_ident as fn() -> Option<Expression>,
        );
        prefix_fns.insert(
            TokenType::Minus,
            Self::parse_prefix_minus as fn() -> Option<Expression>, // NOTE This cast is redundant, since the compiler tries to cast this item the same way as the first item, but it is here for the sake of clarity.
        );

        prefix_fns
    }

    fn register_infixes() -> HashMap<TokenType, fn(Expression) -> Option<Expression>> {
        let mut infix_fns = HashMap::new();

        infix_fns.insert(
            TokenType::Minus,
            Self::parse_infix_minus as fn(Expression) -> Option<Expression>,
        );

        infix_fns
    }
}
