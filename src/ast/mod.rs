use crate::token::Token;

/// Node
pub trait Node {
    fn token_literal(&self) -> String {
        String::from("I'm just some node...")
    }
}

/// Expression
/// REVIEW Maybe this should be a struct?
#[derive(Debug, Clone)]
pub enum Expression {
    Placeholder,
}

impl Expression {
    fn expression_node(&self) {
        println!("I'm an expression node!")
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        todo!()
    }
}

/// Statement
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Token, Option<Expression>), // Token is for the identifier, expression is for the value
    Return(Option<Expression>),
}

impl Statement {
    fn statement_node(&self) {
        println!("I'm a statement node!")
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(t, e) => t.literal.clone(),
            _ => String::from("Not yet implemented"),
        }
    }
}

/// Program
/// REVIEW Currently Program is a Vec of Statements that are also Nodes, but it also itself is a Node.
/// I'm not sure what the consequences of this is.
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            String::from("")
        } else {
            self.statements[0].token_literal()
        }
    }
}

/// Identifier
//pub struct Identifier {
//    pub token: Token,
//    //value: String,
//}

#[cfg(test)]
mod ast_tests {
    use super::*;
    use crate::token::*;

    #[test]
    fn test_ast() {
        let statement = Statement::Let(
            Token {
                token_type: TokenType::Ident,
                literal: String::from("x"),
            },
            None,
        );

        assert!(matches!(statement, Statement::Let(_, _)));
    }

    #[test]
    #[ignore]
    fn test_whitespace() {}
}
