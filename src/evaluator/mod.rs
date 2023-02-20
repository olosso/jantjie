mod tests;

use std::fmt;

use crate::ast::*;
use crate::object::*;
use crate::token::{Token, TokenType};

pub fn eval(program: &Program, global: &mut Environment) -> Result<Object, EvalError> {
    let mut value = Ok(Object::Null);
    for statement in program.statements.iter() {
        value = match statement {
            Statement::Expr(_, expr) => eval_expression(expr, global),
            Statement::Return(_, expr) => eval_return(expr, global),
            Statement::Let(_, name, expr) => eval_let(name, expr, global), // NOTE Only let is allowed to mutate the environment.
            Statement::Block(_, statements) => eval_block(statements, global),
        };

        if let Ok(Object::Return(value)) = value {
            return Ok(*value);
        };
    }

    value
}

fn eval_statement(statement: &Statement, env: &mut Environment) -> Result<Object, EvalError> {
    let mut value = Ok(Object::Null);
    value = match statement {
        Statement::Expr(_, expr) => eval_expression(expr, env),
        Statement::Return(_, expr) => eval_return(expr, env),
        Statement::Let(_, name, expr) => eval_let(name, expr, env), // NOTE Only let is allowed to mutate the environment.
        Statement::Block(_, statements) => eval_block(statements, env),
    };

    value
}

pub fn eval_block(statements: &[Statement], env: &Environment) -> Result<Object, EvalError> {
    let mut result = Object::Null;
    let mut block_env = Environment::local(env);
    for statement in statements {
        result = eval_statement(statement, &mut block_env)?;

        /*
         * Note that the Return is not unwrapped!
         * It is propagated upwards to Program.eval() to
         * short-circuit it.
         */
        if let Object::Return(..) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_expression(expr: &Expression, env: &Environment) -> Result<Object, EvalError> {
    match expr {
        Expression::Bool(_, b) => Ok(Object::Boolean(*b)),
        Expression::IntegerLiteral(_, i) => Ok(Object::Integer(*i)),
        Expression::Prefix(_, op, expr) => eval_prefix(expr, op, env),
        Expression::Infix(_, left, op, right) => eval_infix(left, op, right, env),
        Expression::Identifier(_, ident) => eval_identifier(ident, env),
        Expression::If(_, cond, cons, alt) => eval_ifelse(cond, cons, alt, env),
        _ => Ok(Object::Null),
    }
}

fn eval_prefix(expr: &Expression, op: &str, env: &Environment) -> Result<Object, EvalError> {
    match op {
        "-" => eval_minus(expr, env),
        "!" => eval_bang(expr, env),
        _ => panic!("Lexer shouldn't allow this to happen."),
    }
}

/*
 * MINUS
 */
pub fn eval_minus(expr: &Expression, env: &Environment) -> Result<Object, EvalError> {
    match expr {
        // !int
        Expression::IntegerLiteral(_, i) => Ok(Object::Integer(-(*i))),
        _ => Err(EvalError::new_t(
            "Minus operator not supported for".to_string(),
            expr.token().clone(),
        )),
    }
}

/*
 * BANGBANG
 */
pub fn eval_bang(expr: &Expression, env: &Environment) -> Result<Object, EvalError> {
    match eval_expression(expr, env) {
        Ok(Object::Boolean(b)) => Ok(Object::Boolean(!b)),
        Ok(Object::Null) => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn bang_error(expr: &Expression) -> EvalError {
    EvalError::new(format!(
        "Bang not supported for the value {:?}.",
        expr.token().token_type
    ))
}

/*
 * Infix
 */
pub fn eval_infix(
    left: &Expression,
    op: &str,
    right: &Expression,
    env: &Environment,
) -> Result<Object, EvalError> {
    let left = eval_expression(left, env)?;
    let right = eval_expression(right, env)?;
    match left {
        Object::Integer(a) => {
            if let Object::Integer(b) = right {
                eval_integer_infix(a, op, b)
            } else {
                Err(EvalError::new(format!(
                    "Arithmetic not supported between types {:?} and {:?}.",
                    left.obtype(),
                    right.obtype()
                )))
            }
        }
        Object::Boolean(a) => {
            if let Object::Boolean(b) = right {
                eval_bool_infix(a, op, b)
            } else {
                Ok(Object::Null)
            }
        }
        _ => todo!(),
    }
}

fn eval_integer_infix(a: i32, op: &str, b: i32) -> Result<Object, EvalError> {
    Ok(match op {
        "+" => Object::Integer(a + b),
        "-" => Object::Integer(a - b),
        "*" => Object::Integer(a * b),
        "/" => Object::Integer(a / b),

        "==" => Object::Boolean(a == b),
        "!=" => Object::Boolean(a != b),
        "<" => Object::Boolean(a < b),
        ">" => Object::Boolean(a > b),

        _ => {
            return Err(EvalError(
                format!("Operator \"{op}\" not supported for integers"),
                None,
            ))
        }
    })
}

fn eval_bool_infix(a: bool, op: &str, b: bool) -> Result<Object, EvalError> {
    Ok(match op {
        "==" => Object::Boolean(a == b),
        "!=" => Object::Boolean(a != b),

        _ => {
            return Err(EvalError(
                format!("Operator '{op}' not supported for booleans."),
                None,
            ))
        }
    })
}

pub fn eval_return(expr: &Expression, env: &Environment) -> Result<Object, EvalError> {
    Ok(Object::Return(Box::new(eval_expression(expr, env)?)))
}

/*
 * IfElse
 */
pub fn eval_ifelse(
    cond: &Expression,
    cons: &Statement,
    alt: &Option<Box<Statement>>,
    env: &Environment,
) -> Result<Object, EvalError> {
    if eval_expression(cond, env)?.as_bool() {
        if let Statement::Block(_, statements) = cons {
            eval_block(statements, env)
        } else {
            panic!("Lexer shouldn't allow a non-block statement in If");
        }
    } else {
        match alt {
            None => Ok(Object::Null),
            Some(alt) => {
                if let Statement::Block(_, statements) = &**alt {
                    eval_block(statements, env)
                } else {
                    panic!("Lexer shouldn't allow a non-block statement in If");
                }
            }
        }
    }
}

/*
 * Let
 */
pub fn eval_let(
    ident: &Expression,
    expr: &Expression,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let value = eval_expression(expr, env)?;
    let name = ident.token_literal();
    env.add(&name, &value);

    Ok(value)
}

pub fn eval_identifier(name: &String, env: &Environment) -> Result<Object, EvalError> {
    match env.get(name) {
        Some(value) => Ok(value.copy()),
        None => Err(EvalError::new(format!(
            "The symbol '{}' is not bound to any value.",
            &name
        ))),
    }
}

#[derive(Debug)]
pub struct EvalError(pub String, pub Option<Token>);
impl EvalError {
    fn msg(&self) -> String {
        match &self.1 {
            None => self.0.to_owned(),
            Some(t) => format!(
                "{}. Problem encountered at token {:?}",
                self.0.to_owned(),
                t
            ),
        }
    }

    pub fn new(s: String) -> Self {
        EvalError(s, None)
    }
    pub fn new_t(s: String, t: Token) -> Self {
        EvalError(s, Some(t))
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg())
    }
}
