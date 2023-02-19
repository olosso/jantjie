use std::{collections::HashMap, ops::Deref};

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    NULL,
    INTEGER,
    BOOLEAN,
    RETURN,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Null,
    Integer(i32),
    Boolean(bool),
    Return(Box<Object>),
}

impl Object {
    pub fn obtype(&self) -> Type {
        match self {
            Object::Null => Type::NULL,
            Object::Integer(_) => Type::INTEGER,
            Object::Boolean(_) => Type::BOOLEAN,
            Object::Return(_) => Type::RETURN,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::Return(v) => v.inspect(),
        }
    }

    /*
     * Nulls
     */
    const NULL: Object = Object::Null;
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
