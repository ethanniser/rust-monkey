use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
}

impl Object {
    pub fn to_bool(&self) -> bool {
        match self {
            Object::Integer(integer) => *integer != 0,
            Object::Boolean(boolean) => *boolean,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
        }
    }
}
