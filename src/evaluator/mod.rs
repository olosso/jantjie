mod tests;

use std::fmt;

use crate::ast::*;
use crate::object::*;
use crate::token::{Token, TokenType};

pub fn eval(program: &Program) -> Result<Object, EvalError> {
    program.eval()
}

/*
 * MINUS
 */
pub fn eval_minus(expr: &Expression) -> Result<Object, EvalError> {
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
pub fn eval_bang(expr: &Expression) -> Result<Object, EvalError> {
    match expr.eval() {
        Ok(Object::Boolean(b)) => Ok(Object::Boolean(!b)),
        Ok(Object::Null) => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn bang_error(expr: &Expression) -> EvalError {
    EvalError::new(format!(
        "Bang not supported for the value {}",
        expr.token_literal()
    ))
}

/*
 * Infix
 */
pub fn eval_infix(left: &Expression, op: &str, right: &Expression) -> Result<Object, EvalError> {
    let left = left.eval();
    let right = right.eval();
    match left {
        Ok(Object::Integer(a)) => {
            if let Ok(Object::Integer(b)) = right {
                eval_integer_infix(a, op, b)
            } else {
                Ok(Object::Null)
            }
        }
        Ok(Object::Boolean(a)) => {
            if let Ok(Object::Boolean(b)) = right {
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

pub fn eval_return(expr: &Expression) -> Result<Object, EvalError> {
    Ok(Object::Return(Box::new(expr.eval()?)))
}

pub fn eval_block(statements: &[Statement]) -> Result<Object, EvalError> {
    let mut result = Object::Null;
    for statement in statements {
        result = statement.eval()?;

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

/*
 * IfElse
 */
pub fn eval_ifelse(
    cond: &Expression,
    cons: &Statement,
    alt: &Option<Box<Statement>>,
) -> Result<Object, EvalError> {
    if cond.eval()?.as_bool() {
        cons.eval()
    } else {
        match alt {
            None => Ok(Object::Null),
            Some(alt) => alt.eval(),
        }
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
