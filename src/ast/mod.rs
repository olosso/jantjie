use crate::token::Token;

/// Node
pub trait Node {
    fn to_string(&self) -> String;
    fn node_type(&self) -> String;
    fn token_literal(&self) -> String {
        self.node_type()
    }
}

/// Expression
// First creating some type synonyms for the enum fields
type Operator = String;
type Left = Box<Expression>;
type Right = Box<Expression>;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Placeholder,                         // For initialization and stuff
    Bool(Token, bool),                   // true,  Token = Token {True, "true"}
    IntegerLiteral(Token, i32),          // 42,    Token = Token {Int, 42}
    Identifier(Token, String),           // foo,   Token = Token {Ident, "foo"}
    Prefix(Token, Operator, Right),      // !true, Token = Token {Bang, "!"}
    Infix(Token, Left, Operator, Right), // a + b, Token = Token {Plus, "+"}, Operator = "+"
}

impl Expression {
    pub fn left(&self) -> Option<&Left> {
        if let Expression::Infix(_, l, ..) = self {
            Some(l)
        } else {
            None
        }
    }

    pub fn right(&self) -> Option<&Right> {
        if let Expression::Prefix(_, _, r) | Expression::Infix(_, _, _, r) = self {
            Some(r)
        } else {
            None
        }
    }

    pub fn int(&self) -> Option<i32> {
        if let Expression::IntegerLiteral(_, i) = self {
            Some(*i)
        } else {
            None
        }
    }

    pub fn op(&self) -> Option<Operator> {
        if let Expression::Prefix(_, o, _) | Expression::Infix(_, _, o, _) = self {
            Some(o.to_string())
        } else {
            None
        }
    }

    pub fn buul(&self) -> Option<bool> {
        if let Expression::Bool(_, b) = self {
            Some(*b)
        } else {
            None
        }
    }
}

impl Node for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Bool(t, _) => t.literal.clone(),
            Expression::Identifier(_, s) => s.to_string(),
            Expression::IntegerLiteral(t, _) => t.literal.clone(),
            Expression::Prefix(_, o, r) => {
                format!("({}{})", o, r.to_string())
            }
            Expression::Infix(_, l, o, r) => {
                format!("({} {} {})", l.to_string(), o, r.to_string())
            }
            _ => panic!(),
        }
    }

    fn token_literal(&self) -> String {
        match self {
            Expression::Bool(t, _) => t.literal.clone(),
            Expression::Identifier(t, ..) => t.literal.clone(),
            Expression::IntegerLiteral(t, ..) => t.literal.clone(),
            Expression::Prefix(t, ..) => t.literal.clone(),
            Expression::Infix(t, ..) => t.literal.clone(),
            _ => panic!(),
        }
    }

    fn node_type(&self) -> String {
        format!("Expression node: {self:?}")
    }
}

/// Statement
type Name = Expression;
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Token, Name, Expression), // let <identifier> = <expression>; - Token is the Let token
    Return(Token, Expression),    // return (<expression>); - Token is the Return token
    Expr(Token, Expression),      // <expression>(;) - Token is the first Token of the expression.
}

impl Statement {
    pub fn literal(&self) -> String {
        match self {
            Statement::Let(t, ..) => t.literal.clone(),
            Statement::Return(t, ..) => t.literal.clone(),
            Statement::Expr(t, _) => t.literal.clone(),
        }
    }

    pub fn name(&self) -> Option<String> {
        match self {
            Statement::Let(_, n, ..) => Some(n.to_string()),
            _ => None,
        }
    }

    pub fn expr(&self) -> &Expression {
        match self {
            Statement::Let(.., e) => e,
            Statement::Return(.., e) => e,
            Statement::Expr(.., e) => e,
        }
    }

    pub fn token(&self) -> Option<&Token> {
        match self {
            Statement::Let(t, ..) | Statement::Expr(t, _) | Statement::Return(t, ..) => Some(t),
        }
    }
}

impl Node for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Let(t, n, e) => {
                format!(
                    "{} {} = {};",
                    t.literal,
                    self.name().unwrap(),
                    e.to_string()
                )
            }
            Statement::Return(t, e) => {
                format!("{} {};", t.literal, e.to_string())
            }
            Statement::Expr(t, e) => {
                format!("{}", e.to_string())
            }
        }
    }

    fn token_literal(&self) -> String {
        match self {
            Statement::Let(t, ..) => t.literal.clone(),
            Statement::Return(t, ..) => t.literal.clone(),
            Statement::Expr(t, ..) => t.literal.clone(),
        }
    }

    fn node_type(&self) -> String {
        format!("Statement node: {self:?}")
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
    fn to_string(&self) -> String {
        let mut buffer = String::new();

        for statement in self.statements.iter() {
            buffer.push_str(&statement.to_string());
        }

        buffer
    }

    fn node_type(&self) -> String {
        "Program node".to_string()
    }
}

#[cfg(test)]
mod ast_tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_program_string() {
        let program = Program {
            statements: vec![Statement::Let(
                Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                Expression::Identifier(
                    Token {
                        token_type: TokenType::Ident,
                        literal: "foo".to_string(),
                    },
                    "foo".to_string(),
                ),
                Expression::Identifier(
                    Token {
                        token_type: TokenType::Ident,
                        literal: "bar".to_string(),
                    },
                    "bar".to_string(),
                ),
            )],
        };

        assert_eq!(program.to_string(), "let foo = bar;")
    }
}
