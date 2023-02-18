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
        _ => Ok(Object::Null),
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

#[derive(Debug)]
pub struct EvalError(String, Option<Token>);
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
