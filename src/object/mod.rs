use crate::ast::{Body, Node, Params, Statement};
use core::cmp::PartialEq;
use std::rc::Rc;
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
    Function(Params, Body, Environment),
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
            Object::Function(t, body, env) => {
                let mut s = String::new();
                s.push('\n');

                s.push_str(&env.to_str());

                if let Statement::Block(t, statements) = &**body {
                    for statement in statements.iter() {
                        s.push_str(&statement.to_string());
                    }
                };

                s
            }
        }
    }

    /*
     * Nulls
     */
    pub fn is_null(&self) -> bool {
        *self == Object::Null
    }

    /*
     * Integers
     */
    pub fn as_int(&self) -> Option<i32> {
        match &self {
            Object::Integer(i) => Some(*i),
            _ => None,
        }
    }

    /*
     * Booleans
     *
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
            Self::Function(n, b, e) => Self::Null, // TODO This is a quick fix. Not sure what to do.
        }
    }
}

#[derive(Debug)]
pub struct Environment(HashMap<String, Object>);

impl Environment {
    pub fn new() -> Self {
        Environment(HashMap::new())
    }

    pub fn to_str(&self) -> String {
        let mut msg = String::new();
        for (name, obj) in &self.0 {
            msg.push_str(
                format!("{}: {:?}\n", name.as_str(), *obj)
                    .to_owned()
                    .as_str(),
            )
        }
        msg
    }

    pub fn add(&mut self, name: &str, obj: &Object) {
        self.0.insert(name.to_string(), obj.copy());
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        self.0.get(name)
    }
}
