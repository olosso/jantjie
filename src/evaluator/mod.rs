mod tests;

use std::fmt;

use crate::ast::*;
use crate::object::*;
use crate::token::{Token, TokenType};

pub fn eval(program: &Program, env: &mut Environment) -> Result<Object, EvalError> {
    program.eval(env)
}

/*
 * MINUS
 */
pub fn eval_minus(expr: &Expression, env: &mut Environment) -> Result<Object, EvalError> {
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
pub fn eval_bang(expr: &Expression, env: &mut Environment) -> Result<Object, EvalError> {
    match expr.eval(env) {
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
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let left = left.eval(env)?;
    let right = right.eval(env)?;
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

pub fn eval_return(expr: &Expression, env: &mut Environment) -> Result<Object, EvalError> {
    Ok(Object::Return(Box::new(expr.eval(env)?)))
}

pub fn eval_block(statements: &[Statement], env: &mut Environment) -> Result<Object, EvalError> {
    let mut result = Object::Null;
    for statement in statements {
        result = statement.eval(env)?;

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
    env: &mut Environment,
) -> Result<Object, EvalError> {
    if cond.eval(env)?.as_bool() {
        cons.eval(env)
    } else {
        match alt {
            None => Ok(Object::Null),
            Some(alt) => alt.eval(env),
        }
    }
}

/*
 * Let
 */
pub fn eval_let(
    expr: &Expression,
    ident: &Expression,
    env: &mut Environment,
) -> Result<Object, EvalError> {
    let value = expr.eval(env)?;
    let name = ident.token_literal();
    env.add(&name, &value);

    Ok(value)
}

pub fn eval_identifier(name: &String, env: &mut Environment) -> Result<Object, EvalError> {
    match env.get(name) {
        Some(value) => Ok(value.copy()),
        None => Err(EvalError::new(format!(
            "There is no bound symbol {}",
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
