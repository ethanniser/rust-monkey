use crate::ast::*;
use crate::object::*;

#[derive(Debug, PartialEq)]
pub enum EvalError {
    UnknownOperator(String, Object, Object),
    UnknownIdentifier(String),
    TypeMismatch(String, Object, Object),
    ReturnValue(Object),
}

pub fn eval(node: Node) -> Result<Object, EvalError> {};