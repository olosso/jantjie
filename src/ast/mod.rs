use crate::token::Token;

/// Node
pub trait Node {
    fn token_literal(&self) -> String {
        String::from("I'm just some node...")
    }
}

/// Expression
/// REVIEW Maybe this should be a struct?
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
pub enum Statement {
    Let(String, Expression),
    If(Expression),
    Return(Option<Expression>),
}

impl Statement {
    fn statement_node(&self) {
        println!("I'm a statement node!")
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        let literal = match self {
            Statement::Let(s, x) => "LetStatement",
            _ => "Not yet implemented",
        };

        String::from(literal)
    }
}

/// Program
/// REVIEW Currently Program is a Vec of Statements that are also Nodes, but it also itself is a Node.
/// I'm not sure what the consequences of this is.
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
#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    //value: String,
}

#[cfg(test)]
mod ast_tests {
    use super::*;
    use crate::token::*;

    #[test]
    fn test_ast() {
        let statement = Statement::Let(String::from(""), Expression::Placeholder);

        assert!(matches!(statement, Statement::Let(_, _)));
    }

    #[test]
    #[ignore]
    fn test_whitespace() {}
}
