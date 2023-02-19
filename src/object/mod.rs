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
}
