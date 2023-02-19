use crate::evaluator::*;
use crate::object::{Environment, Object};
use crate::parser::ParseError;
use crate::token::{Token, TokenType};

/// Node
pub trait Node {
    fn to_string(&self) -> String;
    fn node_type(&self) -> String;
    fn token(&self) -> &Token;
    fn token_literal(&self) -> String {
        self.node_type()
    }
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        Ok(Object::Null)
    }
}

/// Expression
// First creating some type synonyms for the enum fields
type Operator = String;
type Left = Box<Expression>;
type Right = Box<Expression>;
type Condition = Box<Expression>;
type Consequence = Box<Statement>;
type Alternative = Box<Statement>;
type Body = Box<Statement>;
type Params = Vec<Expression>;
type Arguments = Vec<Expression>;
#[derive(Debug, Clone)]
pub enum Expression {
    Bool(Token, bool),                   // true,  Token = Token {True, "true"}
    IntegerLiteral(Token, i32),          // 42,    Token = Token {Int, 42}
    Identifier(Token, String),           // foo,   Token = Token {Ident, "foo"}
    Prefix(Token, Operator, Right),      // !true, Token = Token {Bang, "!"}
    Infix(Token, Left, Operator, Right), // a + b, Token = Token {Plus, "+"}, Operator = "+"
    /*
     * if (<expression>) { <statement[]> } else { <statement[]> }
     * Token = Token {If, "if"}
     */
    If(Token, Condition, Consequence, Option<Alternative>),
    /*
     *  func (<expression(=identifier)[]>) { <statement[]> }
     *  Token = Token { Function, "func" }
     */
    Func(Token, Params, Body),
    /*
     *  <identifier> (<expression[]>) { <statement[]> }
     *  Token = Token { LParen, "(" }
     */
    Call(Token, Box<Name>, Arguments),
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

    pub fn condition(&self) -> Option<&Condition> {
        if let Expression::If(_, c, ..) = self {
            Some(c)
        } else {
            None
        }
    }

    pub fn consequence(&self) -> Option<&Consequence> {
        if let Expression::If(_, _, c, ..) = self {
            Some(c)
        } else {
            None
        }
    }

    /*
     * Option, option...
     * The first option is there because this function might be called on
     * some Expression other than an If.
     * The second option is there because an IfExpression might not have an alternative.
     */
    pub fn alternative(&self) -> Option<&Option<Consequence>> {
        if let Expression::If(_, _, _, a) = self {
            Some(a)
        } else {
            None
        }
    }

    pub fn params(&self) -> Option<&Params> {
        if let Expression::Func(_, p, ..) = self {
            Some(p)
        } else {
            None
        }
    }

    pub fn body(&self) -> Option<&Body> {
        if let Expression::Func(_, _, b) = self {
            Some(b)
        } else {
            None
        }
    }

    pub fn name(&self) -> Option<&Name> {
        if let Expression::Call(_, n, _) = self {
            Some(n)
        } else {
            None
        }
    }

    pub fn args(&self) -> Option<&Arguments> {
        if let Expression::Call(_, _, a) = self {
            Some(a)
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
            Expression::If(t, condition, consequence, alt) => {
                if let Some(alt) = alt {
                    format!(
                        "({} {} {{ {} }} else {{ {} }})",
                        t.literal,
                        condition.to_string(),
                        consequence.to_string(),
                        alt.to_string()
                    )
                } else {
                    format!(
                        "({} {} {{ {} }})",
                        t.literal,
                        condition.to_string(),
                        consequence.to_string(),
                    )
                }
            }
            Expression::Func(t, p, b) => {
                format!(
                    "{}({}) {{ {} }}",
                    t.literal,
                    p.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    b.to_string()
                )
            }

            Expression::Call(t, n, a) => {
                format!(
                    "{}({})",
                    n.token_literal(),
                    a.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            }
            _ => panic!(),
        }
    }

    fn token(&self) -> &Token {
        match self {
            Expression::Bool(t, ..) => &t,
            Expression::Identifier(t, ..) => &t,
            Expression::IntegerLiteral(t, ..) => &t,
            Expression::Prefix(t, ..) => &t,
            Expression::Infix(t, ..) => &t,
            Expression::If(t, ..) => &t,
            Expression::Func(t, ..) => &t,
            Expression::Call(t, ..) => &t,
        }
    }

    fn token_literal(&self) -> String {
        match self {
            Expression::Bool(t, ..) => t.literal.clone(),
            Expression::Identifier(t, ..) => t.literal.clone(),
            Expression::IntegerLiteral(t, ..) => t.literal.clone(),
            Expression::Prefix(t, ..) => t.literal.clone(),
            Expression::Infix(t, ..) => t.literal.clone(),
            Expression::If(t, ..) => t.literal.clone(),
            Expression::Func(t, ..) => t.literal.clone(),
            Expression::Call(t, ..) => t.literal.clone(),
            _ => panic!(),
        }
    }

    fn node_type(&self) -> String {
        format!("Expression node: {self:?}")
    }

    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        match &self {
            Expression::Identifier(_, name) => eval_identifier(name, env),
            Expression::IntegerLiteral(_, i) => Ok(Object::Integer(*i)),
            Expression::Bool(_, b) => Ok(Object::Boolean(*b)),
            Expression::Prefix(t, l, e) => match t.token_type {
                TokenType::Bang => eval_bang(e, env),
                TokenType::Minus => eval_minus(e, env),
                _ => panic!("This shouldn't happen."),
            },
            Expression::Infix(_, l, op, r) => eval_infix(l, op, r, env),
            Expression::If(_, cond, cons, alt) => eval_ifelse(cond, cons, alt, env),
            _ => todo!(
                "The Expression you're trying to evaluate doesn't have an evaluation function yet!"
            ),
        }
    }
}

/// Statement
type Name = Expression;
#[derive(Debug, Clone)]
pub enum Statement {
    Let(Token, Name, Expression), // let <identifier> = <expression>; - Token is the Let token
    Return(Token, Expression),    // return <expression>; - Token is the Return token
    Expr(Token, Expression),      // <expression>(;) - Token is the first Token of the expression.
    Block(Token, Vec<Statement>),
}

impl Statement {
    pub fn literal(&self) -> String {
        match self {
            Statement::Let(t, ..) => t.literal.clone(),
            Statement::Return(t, ..) => t.literal.clone(),
            Statement::Expr(t, _) => t.literal.clone(),
            Statement::Block(t, _) => t.literal.clone(),
        }
    }

    pub fn name(&self) -> Option<String> {
        match self {
            Statement::Let(_, n, ..) => Some(n.to_string()),
            _ => None,
        }
    }

    pub fn expr(&self) -> Option<&Expression> {
        match self {
            Statement::Let(.., e) => Some(e),
            Statement::Return(.., e) => Some(e),
            Statement::Expr(.., e) => Some(e),
            Statement::Block(..) => None,
        }
    }

    pub fn token(&self) -> Option<&Token> {
        match self {
            Statement::Let(t, ..)
            | Statement::Expr(t, _)
            | Statement::Return(t, ..)
            | Statement::Block(t, ..) => Some(t),
        }
    }

    pub fn len(&self) -> usize {
        if let Statement::Block(_, v) = self {
            v.len()
        } else {
            1
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
            Statement::Expr(_, e) => e.to_string(),
            Statement::Block(_, e) => e
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("; "),
        }
    }

    fn token(&self) -> &Token {
        match self {
            Statement::Let(t, ..) => &t,
            Statement::Return(t, ..) => &t,
            Statement::Expr(t, ..) => &t,
            Statement::Block(t, ..) => &t,
        }
    }

    fn token_literal(&self) -> String {
        match self {
            Statement::Let(t, ..) => t.literal.clone(),
            Statement::Return(t, ..) => t.literal.clone(),
            Statement::Expr(t, ..) => t.literal.clone(),
            Statement::Block(t, ..) => t.literal.clone(),
        }
    }

    fn node_type(&self) -> String {
        format!("Statement node: {self:?}")
    }

    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        match &self {
            Statement::Block(_, statements) => eval_block(statements, env),
            Statement::Return(_, e) => eval_return(e, env),
            Statement::Let(_, ident, expr) => eval_let(expr, ident, env),
            _ => self.expr().unwrap().eval(env), // This unwrap is safe,
                                                 // because these statements must contains Expressions
        }
    }
}

/// Program
/// REVIEW Currently Program is a Vec of Statements that are also Nodes, but it also itself is a Node.
/// I'm not sure what the consequences of this is.
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    token: Token,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Program {
            statements,
            token: Token::new(TokenType::Program, "main".to_string()),
        }
    }

    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

impl Node for Program {
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn node_type(&self) -> String {
        "Program node".to_string()
    }

    fn token(&self) -> &Token {
        &self.token
    }

    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let mut result = Object::Null;
        for statement in self.statements.iter() {
            result = statement.eval(env)?;

            if let Object::Return(result) = result {
                return Ok(*result);
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod ast_tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_program_string() {
        let program = Program::new(vec![Statement::Let(
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
        )]);

        assert_eq!(program.to_string(), "let foo = bar;")
    }
}
