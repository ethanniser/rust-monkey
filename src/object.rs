use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    None,
    Integer(isize),
    Boolean(bool),
    ReturnValue(Box<Object>),
}

impl Object {
    pub fn to_bool(&self) -> bool {
        match self {
            Object::None => false,
            Object::Integer(integer) => *integer != 0,
            Object::Boolean(boolean) => *boolean,
            Object::ReturnValue(value) => value.to_bool(),
        }
    }

    pub fn to_type(&self) -> String {
        match self {
            Object::None => "None".to_string(),
            Object::Integer(_) => "Integer".to_string(),
            Object::Boolean(_) => "Boolean".to_string(),
            Object::ReturnValue(value) => value.to_type(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::None => write!(f, "None"),
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::ReturnValue(value) => write!(f, "{}", value),
        }
    }
}
