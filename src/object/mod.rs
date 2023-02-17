#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    NULL,
    INTEGER,
    BOOLEAN,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Null,
    Integer(i32),
    Boolean(bool),
}

impl Object {
    pub fn obtype(&self) -> Type {
        match self {
            Object::Null => Type::NULL,
            Object::Integer(_) => Type::INTEGER,
            Object::Boolean(_) => Type::BOOLEAN,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Null => "null".to_string(),
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
        }
    }

    pub fn is_null(&self) -> bool {
        *self == Object::Null
    }

    pub fn as_int(&self) -> Option<i32> {
        match &self {
            Object::Integer(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match &self {
            Object::Boolean(b) => Some(*b),
            _ => None,
        }
    }
}
