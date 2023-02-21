use crate::ast::{Body, Expression, Node, Params, Statement};
use core::cmp::PartialEq;
use std::{collections::HashMap, ops::Deref};

/*
 * @TYPE
 */
#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    NULL,
    INTEGER,
    BOOLEAN,
    RETURN,
    FUNCTION,
}

/*
 * @OBJECT
 */
#[derive(Debug)]
pub enum Object {
    Null,
    Integer(i32),
    Boolean(bool),
    Return(Box<Self>),
    Function(Vec<Expression>, Statement, Environment),
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Object::Null => {
                if let Object::Null = other {
                    true
                } else {
                    false
                }
            }
            Object::Integer(a) => {
                if let Object::Integer(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Object::Boolean(a) => {
                if let Object::Boolean(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Object::Return(a) => {
                if let Object::Return(b) = other {
                    **a == **b
                } else {
                    false
                }
            }
            Object::Function(f1, _, _) => {
                // TODO
                // if let Object::Function(f2, _, _) = other {
                //     f1.token_literal() == f2.token_literal()
                // } else {
                //     false
                // }
                false
            }
        }
    }
}

impl Eq for Object {}

impl Object {
    pub fn obtype(&self) -> Type {
        match self {
            Object::Null => Type::NULL,
            Object::Integer(_) => Type::INTEGER,
            Object::Boolean(_) => Type::BOOLEAN,
            Object::Return(_) => Type::RETURN,
            Object::Function(..) => Type::FUNCTION,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::Return(v) => v.inspect(),
            Object::Function(params, body, env) => {
                format!(
                    "func({}) {{ {} }}",
                    params
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    body.to_string(),
                    // env.to_str()
                )
            }
        }
    }

    /*
     * @FUNCTION_HELPERS
     */
    pub fn body(&self) -> Option<String> {
        match self {
            Object::Function(_, b, _) => Some(format!("{{ {} }}", b.to_string())),
            _ => None,
        }
    }

    pub fn params(&self) -> Option<String> {
        match self {
            Object::Function(p, _, _) => Some(format!(
                "({})",
                p.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
            )),
            _ => None,
        }
    }

    pub fn env(&self) -> Option<String> {
        match self {
            Object::Function(_, _, e) => Some(e.inspect()),
            _ => None,
        }
    }

    /*
     * @NULL_HELPERS
     */
    pub fn is_null(&self) -> bool {
        *self == Object::Null
    }

    /*
     * Integers
     */
    pub fn as_int(&self) -> Option<i32> {
        match self {
            Object::Integer(i) => Some(*i),
            _ => None,
        }
    }

    /*
     * Booleans in this language are truthy: Every value can be coerced into
     * a bool, and values are false:
     * Boolean(false)
     * None
     * Integer(0)
     *
     * Everything else is true.
     */
    pub fn as_bool(&self) -> bool {
        match &self {
            Object::Boolean(b) => *b,
            Object::Null | Object::Integer(0) => false,
            _ => true,
        }
    }

    pub fn copy(&self) -> Object {
        match self {
            Self::Null => Self::Null,
            Self::Integer(i) => Self::Integer(*i),
            Self::Boolean(b) => Self::Boolean(*b),
            Self::Return(obj) => Self::Null, // TODO This is a quick fix. Not sure what to do.
            Self::Function(params, body, env) => {
                Self::Function(params.clone(), body.clone(), env.clone())
            }
        }
    }
}

use std::cell::RefCell;
use std::rc::Rc;
#[derive(Debug, Clone)]
pub struct Environment(
    Rc<RefCell<HashMap<String, Object>>>,
    Option<Rc<Environment>>,
);

impl Environment {
    pub fn global() -> Self {
        Environment(Rc::new(RefCell::new(HashMap::new())), None)
    }

    pub fn local(other: &Rc<Self>) -> Self {
        Environment(
            Rc::new(RefCell::new(HashMap::new())),
            Some(Rc::clone(other)),
        )
    }

    pub fn inspect(&self) -> String {
        let mut msg = String::new();
        for (name, obj) in self.0.borrow().iter() {
            msg.push_str(
                format!("{}: {}\n", name.as_str(), obj.inspect())
                    .to_owned()
                    .as_str(),
            )
        }
        msg
    }

    pub fn add(&self, name: &str, obj: &Object) {
        self.0.borrow_mut().insert(name.to_string(), obj.copy());
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        let binding = self.0.borrow();
        let obj = binding.get(name);
        match obj {
            Some(x) => Some(x.copy()),
            None => match &self.1 {
                Some(env) => env.get(name),
                None => None,
            },
        }
    }
}
