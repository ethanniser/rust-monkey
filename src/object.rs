use std::{fmt::Display, rc::Rc};

use crate::{
    ast::{BlockExpression, FunctionLiteral, IdentifierLiteral},
    environment::Env,
    evaluator::BuiltInFunction,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    None,
    Integer(isize),
    Boolean(bool),
    ReturnValue(Box<Rc<Object>>),
    Function(Function),
    String(String),
    BuiltIn(BuiltInFunction),
}

impl Object {
    pub fn to_bool(&self) -> bool {
        match self {
            Object::None => false,
            Object::Integer(integer) => *integer != 0,
            Object::Boolean(boolean) => *boolean,
            Object::ReturnValue(value) => value.to_bool(),
            Object::Function(_) => true,
            Object::String(string) => !string.is_empty(),
            Object::BuiltIn(_) => true,
        }
    }

    pub fn to_type(&self) -> String {
        match self {
            Object::None => "none".to_string(),
            Object::Integer(_) => "Integer".to_string(),
            Object::Boolean(_) => "Boolean".to_string(),
            Object::ReturnValue(value) => value.to_type(),
            Object::Function(_) => "Function".to_string(),
            Object::String(_) => "String".to_string(),
            Object::BuiltIn(_) => "Function".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::None => Ok(()),
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::String(string) => write!(f, "{}", string),
            Object::ReturnValue(value) => write!(f, "{}", value),
            Object::Function(function) => write!(
                f,
                "{}",
                FunctionLiteral {
                    parameters: function.parameters.clone(),
                    body: function.body.clone()
                }
            ),
            Object::BuiltIn(built_in) => write!(f, "BUILT IN FUNCTION: {}", built_in),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockExpression,
    pub env: Env,
}
