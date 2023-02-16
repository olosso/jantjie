mod tests;

use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;

/// By this ordering, take this expression:
///
/// 1 + 2 == -3 * 8 < ten()
///
/// The result would be evaluated like so:
/// (((1 + 2) - 1) == (((-3) * 8) < (ten())))
/// 1. ten() (=> 10)
/// 2. -3 (=> -3)
/// 3. -3 * 8 (=> -24)
/// 4. 1 + 2 (=> 3)
/// 5. 3 - 1 (=> 2)
/// 6. -24 < 10 (=> true)
/// 7. 2 == true (=> Error :))
#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    EMPTY,       // Empty identifier _
    LOWEST,      // IntLiterals, Identifiers
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
    prefix_parse_fns: HashMap<TokenType, for<'a> fn(&'a mut Self) -> Expression>,
    infix_parse_fns: HashMap<TokenType, for<'a> fn(&'a mut Self, Expression) -> Expression>,
}

impl Parser {
    /// Parser::new
    /// Create a new parser given a lexer.
    fn new(lexer: Lexer) -> Self {
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
            prefix_parse_fns: Self::prefixes(),
            infix_parse_fns: Self::infixes(),
        };

        parser.next_token();
        parser.next_token();

        assert!(!parser.cur_tokentype_is(TokenType::SOF));
        assert!(!parser.peek_tokentype_is(TokenType::SOF));

        parser
    }

    fn prefixes() -> HashMap<TokenType, for<'a> fn(&'a mut Self) -> Expression> {
        let mut prefix_fns = HashMap::new();

        prefix_fns.insert(
            TokenType::Ident,
            Self::parse_identifier as for<'a> fn(&'a mut Self) -> Expression,
        );
        prefix_fns.insert(TokenType::True, Self::parse_bool);
        prefix_fns.insert(TokenType::False, Self::parse_bool);
        prefix_fns.insert(TokenType::Int, Self::parse_integer_literal);
        prefix_fns.insert(TokenType::Bang, Self::parse_prefix_expression);
        prefix_fns.insert(TokenType::Minus, Self::parse_prefix_expression);

        prefix_fns
    }

    fn infixes() -> HashMap<TokenType, for<'a> fn(&'a mut Self, Expression) -> Expression> {
        let mut infix_fns = HashMap::new();

        infix_fns.insert(
            TokenType::Plus,
            Self::parse_infix_expression as for<'a> fn(&'a mut Self, Expression) -> Expression,
        );
        infix_fns.insert(TokenType::Minus, Self::parse_infix_expression);
        infix_fns.insert(TokenType::Asterisk, Self::parse_infix_expression);
        infix_fns.insert(TokenType::Slash, Self::parse_infix_expression);
        infix_fns.insert(TokenType::Equal, Self::parse_infix_expression);
        infix_fns.insert(TokenType::NotEqual, Self::parse_infix_expression);
        infix_fns.insert(TokenType::GT, Self::parse_infix_expression);
        infix_fns.insert(TokenType::LT, Self::parse_infix_expression);

        infix_fns
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
        while self.current_token.token_type != TokenType::EOF {
            let statement = self.parse_statement();
            if let Some(x) = statement {
                program.statements.push(x);
            }
            self.next_token();
        }
        program
    }

    /// Forwards the Parser to the correct statement parsing function based on the current token type.
    /// Let and Return statements are easy to identify: The statement must begin with those keywords.
    /// If neither of those is the case, then the statement is interpreted as a ExpressionStatement.
    fn parse_statement(&mut self) -> Option<Statement> {
        let statement = match &self.current_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };

        Some(statement)
    }

    /// Parses the following statement type:
    /// let <identifier> = <expression>;
    /// This is the most complicated statement in the language.
    fn parse_let_statement(&mut self) -> Statement {
        assert!(self.cur_tokentype_is(TokenType::Let));

        // current_token is Let
        let let_token = self.current_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            panic!("Let statement not followed by an identifier.")
        };
        // current_token is Ident
        let name = Expression::Identifier(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        );

        if !self.expect_peek(TokenType::Assign) {
            panic!("Identifier not followed by an assignment in let statement.")
        };
        // current_token is Assign
        //
        if !(self.expect_peek(TokenType::Ident)
            || self.expect_peek(TokenType::Int)
            || self.expect_peek(TokenType::Minus)
            || self.expect_peek(TokenType::Bang))
        {
            panic!("Identifier not followed by something other than expression.")
        };

        let expression = self
            .parse_expression(Precedence::LOWEST)
            .expect("Failed to parse expression in LetStatement.");

        if !self.expect_peek(TokenType::Semicolon) {
            panic!("LetStatement not finished with semicolon.")
        };

        Statement::Let(let_token, name, expression)
    }

    /// Parses the following statement type:
    /// return <expression>;
    fn parse_return_statement(&mut self) -> Statement {
        assert!(self.cur_tokentype_is(TokenType::Return));

        let return_token = self.current_token.clone();

        if !(self.expect_peek(TokenType::Ident)
            || self.expect_peek(TokenType::Int)
            || self.expect_peek(TokenType::Minus)
            || self.expect_peek(TokenType::Bang))
        {
            panic!("Identifier not followed by something other than expression.")
        };

        let expression = self
            .parse_expression(Precedence::LOWEST)
            .expect("Failed to parse expression in LetStatement.");

        if !self.expect_peek(TokenType::Semicolon) {
            panic!("LetStatement not finished with semicolon.")
        };

        Statement::Return(return_token, expression)
    }

    /// Parses the following statement type:
    /// <expression>;
    fn parse_expression_statement(&mut self) -> Statement {
        assert!(
            self.cur_tokentype_is(TokenType::Int)
                || self.cur_tokentype_is(TokenType::Ident)
                || self.cur_tokentype_is(TokenType::Minus)
                || self.cur_tokentype_is(TokenType::Bang)
                || self.cur_tokentype_is(TokenType::True)
                || self.cur_tokentype_is(TokenType::False)
        );

        let token = self.current_token.clone();

        /*
         * This is the starting point to parsing an Expression statement, this means that the
         * first symbol must have been either a Identifier, Number or one of the Prefix operators.
         * We haven't actually parsed anything yet, so we start with the Precedence LOWEST.
         */
        let expression = self
            .parse_expression(Precedence::LOWEST)
            .expect("Failed to parse ExpressionStatement.");

        /*
         * If a semicolon is found after the expression statement, it is ignored.
         * This makes the semicolon following the expression optional.
         */
        if self.peek_tokentype_is(TokenType::Semicolon) {
            self.next_token();
        }

        Statement::Expr(token, expression)
    }

    /// This is the master expression parsing function. It parses every expression found/expected in statements.
    /// Let's assume the expression is -1 + 2 * 3;, which should result in ((-1) + (2 * 3))
    /// 0. parse_expression(LOWEST) (*1)
    /// 1. Current token is -, which has a prefix function. Get it and call it.
    /// 2. Before the prefix creates a PrefixExpression and returns,
    ///    it advances the lexer and calls parse_expression(PREFIX) (*2).
    /// 3. Current token is 1. It has prefix handler. Get it and call it.
    /// 4. The prefix function for IntTokens simply creates an IntegerLiteral expression.
    /// 3. (*2) Continues. The loop condition fails, since the
    ///    next token (+) has lower precedence.
    /// 4. (*2) Returns Expression::Int(1)
    /// 5. The prefix function from the 2. step continues and returns a Expression::Prefix(-, 1)
    /// 6. (*1) Continues. The Expression::Prefix(-, 1) is assigned to left.
    /// 7. The loop condition evaluates to true, because precedence LOWEST < SUM.
    /// 8. Peek token is +, which has a infix function. Get it.
    /// 9. Advance lexer, now current token is +.
    /// 10. Call the infix handler retrieved in 8 with the Expression::Prefix(-, 1) as an argument.
    /// 11. The infix handler stores the precedence of +, and advances the lexer. Current token is 2.
    /// 12. parse_expression(SUM) (*3)
    /// 13. 2 has a prefix handler, it creates a IntegerLiteral(2), which is assigned to left.
    /// 14. The loop condition evaluates to true, because SUM < PRODUCT.
    /// 15. Get the infix function of *.
    /// 16. Advance the lexer, current token is now *.
    /// 17. Call the infix handler retrieved in 15 with the IntegerLiteral(2) as an argument.
    /// 18. The infix handler stores the precedence of *, and advances the lexer. Current token is 3.
    /// 19. parse_expression(LOWEST) (*4)
    /// 20. 3 has a prefix handler, it creates a IntegerLiteral(3), which is assigned to left.
    /// 21. The loop condition evaluates to false, since the next token is ;
    /// 22. (*4) Returns IntegerLiteral(3)
    /// 23. The infix handler from step 18 continues.
    /// 24. It returns a Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3))
    /// 25. (*3) Continues with left = Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3)).
    /// 26. The loop condition evaluates to false, since the next token is ;
    /// 27. (*3) Returns left = Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3)).
    /// 28. The infix call from step 11 continues.
    /// 29. It returns
    ///     Expression::Infix(
    ///         Expression::Prefix(-, 1),
    ///         +,
    ///         Expression::Infix(IntegerLiteral(2), *, IntegerLiteral(3))
    ///     )
    /// 30. (*1) Continues, and the loop condition evaluates to false.
    /// 31. (*1) Returns the Expression formed in step 29.
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix_handler =
            if let Some(f) = self.prefix_parse_fns.get(&self.current_token.token_type) {
                f
            } else {
                eprintln!(
                    "PARSE_EXPRESSION: Didnt find prefix handler for {:?}",
                    &self.current_token
                );
                return None;
            };

        let mut left = prefix_handler(self);

        while !self.peek_tokentype_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            let peek_token_type = &self.peek_token.token_type.clone();
            let infix_handler = if let Some(f) = self.infix_parse_fns.get(peek_token_type) {
                *f
            } else {
                return Some(left);
            };

            self.next_token();
            left = infix_handler(self, left);
        }

        Some(left)
    }

    /// Token { ident, "foo" } => Identifier { Token { ident, "foo" }, "foo" }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_bool(&mut self) -> Expression {
        assert!(self.cur_tokentype_is(TokenType::True) || self.cur_tokentype_is(TokenType::False));

        Expression::Bool(
            self.current_token.clone(),
            self.cur_tokentype_is(TokenType::True),
        )
    }

    /// Token { ident, "foo" } => Identifier { Token { ident, "foo" }, "foo" }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_identifier(&mut self) -> Expression {
        assert!(self.cur_tokentype_is(TokenType::Ident));

        Expression::Identifier(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        )
    }

    /// Token { int, "1" } => IntegerLiteral { Token { int, "1" }, 1 }
    /// Note that this is a prefix function -> It can be a leaf in the AST.
    fn parse_integer_literal(&mut self) -> Expression {
        assert!(self.cur_tokentype_is(TokenType::Int));

        let current_token = self.current_token.clone();
        let int: i32 = current_token
            .literal
            .parse()
            .expect("Failed to parse assumed Integer Token");

        Expression::IntegerLiteral(self.current_token.clone(), int)
    }

    /// This is only called from parse_expression.
    fn parse_prefix_expression(&mut self) -> Expression {
        assert!(self.cur_tokentype_is(TokenType::Minus) || self.cur_tokentype_is(TokenType::Bang));

        /*
         * Store the current token information.
         * Since this token is a prefix, it is one of the following: -, !.
         * So for example
         * current_token = Token { Minus, "-" }
         * literal = "-"
         */
        let current_token = self.current_token.clone();
        let literal = self.current_token.literal.clone();

        self.next_token();

        /*
         * We continue expression parsing by setting the Precedence to Prefix,
         * which means it has high associative power.
         * Indeed, only a function call has higher precedence.
         * The expression -1 + 2 will be evaluated ((-1) + 2), and not -(1 + 2).
         */
        let right = self
            .parse_expression(Precedence::PREFIX)
            .expect("Failed to parse Right node of PrefixExpression");

        Expression::Prefix(current_token, literal, Box::new(right))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        assert!(
            self.cur_tokentype_is(TokenType::Plus)
                || self.cur_tokentype_is(TokenType::Minus)
                || self.cur_tokentype_is(TokenType::Asterisk)
                || self.cur_tokentype_is(TokenType::Slash)
                || self.cur_tokentype_is(TokenType::Equal)
                || self.cur_tokentype_is(TokenType::NotEqual)
                || self.cur_tokentype_is(TokenType::LT)
                || self.cur_tokentype_is(TokenType::GT)
        );

        // Store the current values of token, because next we are going to forward the lexer.
        let current_token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        // Need the precedence of this operator to compare to the ones following.
        let precedence = current_token.precedence().unwrap();

        // What comes next, I wonder.
        self.next_token();

        /*
         * Whatever comes out of here will be set to the right child node of an Infix expression.
         *
         * The important bit here, is that for this parse_expression the current token will be
         * something like an identifier, while the precedence will be that of the previous infix operator.
         * This allows the function to compare the Precedence of the previous operator with the next ones,
         * which determines which operator get to keep the current token.
         */
        let right = self
            .parse_expression(precedence)
            .expect("Failed to parse Right node of InfixExpression");

        Expression::Infix(current_token, Box::new(left), operator, Box::new(right))
    }

    // Helper function.
    fn cur_tokentype_is(&self, tt: TokenType) -> bool {
        self.current_token.token_type == tt
    }

    /// Does the next token have the token type of the argument?
    fn peek_tokentype_is(&self, tt: TokenType) -> bool {
        self.peek_token.token_type == tt
    }

    /// Checks if the next token has the token type of the argument.
    /// If it does, then it also advances the current token.
    fn expect_peek(&mut self, tt: TokenType) -> bool {
        if self.peek_tokentype_is(tt) {
            self.next_token();
            true
        } else {
            false
        }
    }

    /// Return the Precedence of the current token.
    /// If token doesn't have a Precedence (symbol isn't used in expressions) returns Precedence::LOWEST.
    fn cur_precedence(&self) -> Precedence {
        match self.current_token.precedence() {
            Some(precedence) => precedence,
            None => Precedence::LOWEST,
        }
    }

    /// Return the Precedence of the next token.
    /// If token doesn't have a Precedence (symbol isn't used in expressions) returns Precedence::LOWEST.
    fn peek_precedence(&self) -> Precedence {
        match self.peek_token.precedence() {
            Some(precedence) => precedence,
            None => Precedence::LOWEST,
        }
    }

    /// A helper function for debugging purposes.
    pub fn print_tokens(&self) {
        println!(
            "###PARSER_TOKENS###:\nCURRENT_TOKEN: {:?}\nNEXT_TOKEN: {:?}",
            self.current_token, self.peek_token
        );
    }
}
