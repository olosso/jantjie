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

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, for<'a> fn(&'a Self) -> Option<Expression>>,
    infix_parse_fns: HashMap<TokenType, fn(Expression) -> Option<Expression>>,
}

impl Parser {
    /// Parser::new
    /// Create a new parser given a lexer.
    fn new(lexer: Lexer) -> Self {
        // NOTE SOF of file tokens should be rewritten immediately. If not, something has gone wrong.
        let mut parser = Parser {
            lexer,
            current_token: Token {
                token_type: TokenType::SOF,
                literal: String::from("Start of file"),
            },
            peek_token: Token {
                token_type: TokenType::SOF,
                literal: String::from("Start of file"),
            },
            prefix_parse_fns: Self::register_prefixes(),
            infix_parse_fns: Self::register_infixes(),
        };

        parser.next_token();
        parser.next_token();
        // TODO Maybe check here that SOF tokens have been rewritten? They should be.

        parser
    }

    /// Parser::next_token
    /// Advance the lexer of the parser to receive a new token. Also updates the peek_token.
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// Parser::parse_program
    /// Goes through tokens found by the lexer and trys to find the statements.
    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        /*
         * Loops over the Token produced by the Lexer.
         * Forwards the execution to a parser function if it finds something interesting.
         * Ends execution when EOF Token is encountered.
         */
        loop {
            if self.current_token.token_type == TokenType::EOF {
                break;
            } else if self.current_token.token_type != TokenType::Semicolon {
                program.statements.push(self.parse_statement());
            }
            self.next_token();
        }

        program
    }

    /// Parser::parse_statement
    /// Parses each different type of statement.
    fn parse_statement(&mut self) -> Statement {
        let Token {
            token_type: tt,
            literal: l,
        } = &self.current_token;

        match tt {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    /// Parser::parse_let_statement
    fn parse_let_statement(&mut self) -> Statement {
        if !self.expect_peek(TokenType::Ident) {
            panic!("let statement not followed by an identifier.")
        }

        let ident = &self.current_token;
        // REVIEW Is cloning necessary? Done here to avoid using references that would lead to lifetimes.
        let statement = Statement::Let(ident.clone(), Some(Expression::Placeholder));

        if !self.expect_peek(TokenType::Assign) {
            panic!("Identifier not followed by an assignment in let statement.")
        }

        while !self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_return_statement(&mut self) -> Statement {
        let statement = Statement::Return(Some(Expression::Placeholder));

        while !self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let mut statement = Statement::Expr(
            self.current_token.clone(),
            self.parse_expression(Precedence::LOWEST),
        );

        if self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        statement
    }

    ///
    /// Forwards to the correct parsing function based on registered parsing functions.
    ///
    fn parse_expression(&mut self, p: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);

        if let Some(f) = prefix {
            Some(f(self).unwrap())
        } else {
            todo!("Expression parsing not yet implemented for this expression type!")
        }
    }

    ///
    /// Parses the current token that has been identified as a Token::LiteralInteger.
    ///
    fn parse_literal_integer(&self) -> Option<Expression> {
        match &self.current_token.literal.parse::<i32>() {
            Ok(x) => Some(Expression::IntegerLiteral(*x)),
            Err(err) => panic!("{}", err),
        }
    }

    fn parse_prefix_ident(&self) -> Option<Expression> {
        Some(Expression::Placeholder)
    }

    fn parse_prefix_minus(&self) -> Option<Expression> {
        Some(Expression::Placeholder)
    }

    fn parse_infix_minus(left: Expression) -> Option<Expression> {
        Some(Expression::Placeholder)
    }

    fn cur_tokentype_is(&self, tt: TokenType) -> bool {
        self.current_token.token_type == tt
    }

    fn peek_tokentype_is(&self, tt: TokenType) -> bool {
        self.peek_token.token_type == tt
    }

    fn expect_peek(&mut self, tt: TokenType) -> bool {
        if self.peek_tokentype_is(tt) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn register_prefixes() -> HashMap<TokenType, for<'a> fn(&'a Self) -> Option<Expression>> {
        let mut prefix_fns = HashMap::new();

        prefix_fns.insert(
            TokenType::Ident,
            Self::parse_prefix_ident as for<'a> fn(&'a Parser) -> Option<Expression>,
        );
        prefix_fns.insert(
            TokenType::Minus,
            Self::parse_prefix_minus, // NOTE This cast is redundant, since the compiler tries to cast this item the same way as the first item, but it is here for the sake of clarity.
        );
        prefix_fns.insert(TokenType::Int, Self::parse_literal_integer);

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
