use crate::ast::{BlockExpression, FunctionLiteral, IdentifierLiteral};
use crate::interpreter::{built_in_functions::BuiltInFunction, environment::Env};
use std::{collections::HashMap, fmt::Display, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    None,
    Integer(isize),
    Boolean(bool),
    ReturnValue(Box<Rc<Object>>),
    Function(Function),
    String(String),
    BuiltIn(BuiltInFunction),
    Array(Vec<Rc<Object>>),
    Hash(HashMap<HashKey, Rc<Object>>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum HashKey {
    Integer(isize),
    Boolean(bool),
    String(String),
}

impl Display for HashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HashKey::Integer(integer) => write!(f, "{integer}"),
            HashKey::Boolean(boolean) => write!(f, "{boolean}"),
            HashKey::String(string) => write!(f, "{string}"),
        }
    }
}

impl Object {
    pub fn to_bool(&self) -> bool {
        match self {
            Object::None => unreachable!("should be guarded against earlier"),
            Object::ReturnValue(value) => value.to_bool(),
            Object::Boolean(boolean) => *boolean,
            _ => true,
        }
    }

    pub fn to_type(&self) -> String {
        match self {
            Object::None => "none".to_string(),
            Object::Integer(_) => "integer".to_string(),
            Object::Boolean(_) => "boolean".to_string(),
            Object::ReturnValue(value) => value.to_type(),
            Object::Function(_) => "function".to_string(),
            Object::String(_) => "string".to_string(),
            Object::BuiltIn(_) => "function".to_string(),
            Object::Array(_) => "array".to_string(),
            Object::Hash(_) => "hash".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::None => Ok(()),
            Object::Integer(integer) => write!(f, "{integer}"),
            Object::Boolean(boolean) => write!(f, "{boolean}"),
            Object::String(string) => write!(f, "{string}"),
            Object::ReturnValue(value) => write!(f, "{value}"),
            Object::Function(function) => write!(
                f,
                "{}",
                FunctionLiteral {
                    parameters: function.parameters.clone(),
                    body: function.body.clone()
                }
            ),
            Object::BuiltIn(built_in) => write!(f, "{built_in}"),
            Object::Array(array) => {
                let items = array
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{items}]")
            }
            Object::Hash(hash) => {
                let items = hash
                    .iter()
                    .map(|(key, value)| format!("{key}: {value}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{items}}}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: BlockExpression,
    pub env: Env,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        let envs_equal = self.env.borrow().store == other.env.borrow().store;
        let rest_equal = self.parameters == other.parameters && self.body == other.body;
        envs_equal && rest_equal
    }
}
