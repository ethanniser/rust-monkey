use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use super::built_in_functions::*;
use super::environment::{Env, Environment};
use super::object::{Function, HashKey, Object};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Debug, PartialEq)]
pub struct PrefixMismatch {
    pub operator: PrefixOperator,
    pub right: Rc<Object>,
}

#[derive(Debug, PartialEq)]
pub struct InfixMismatch {
    pub left: Rc<Object>,
    pub operator: InfixOperator,
    pub right: Rc<Object>,
}

#[derive(Debug, PartialEq)]
pub enum TypeMismatch {
    Prefix(PrefixMismatch),
    Infix(InfixMismatch),
    Index(IndexMismatch),
}

#[derive(Debug, PartialEq)]
pub struct IndexMismatch {
    pub left: Rc<Object>,
    pub index: Rc<Object>,
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    UnknownIdentifier(String),
    TypeMismatch(TypeMismatch),
    CallOnNonFunction(Rc<Object>),
    ArgumentMismatch { expected: usize, got: usize },
    BuiltInFunction(BuiltInFunctionError),
    IndexOutOfBounds { index: isize, length: isize },
    UnhashableKey(Rc<Object>),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnknownIdentifier(identifier) => {
                write!(f, "unknown identifier: {identifier}")
            }
            EvalError::TypeMismatch(type_mismatch) => match type_mismatch {
                TypeMismatch::Prefix(prefix_mismatch) => write!(
                    f,
                    "Type Mismatch: Tried to use operator \"{}\" on type \"{}\"",
                    prefix_mismatch.operator,
                    prefix_mismatch.right.to_type()
                ),
                TypeMismatch::Infix(infix_mismatch) => write!(
                    f,
                    "Type Mismatch: Tried to use operator \"{}\" on types \"{}\" and \"{}\"",
                    infix_mismatch.operator,
                    infix_mismatch.left.to_type(),
                    infix_mismatch.right.to_type()
                ),
                TypeMismatch::Index(IndexMismatch { left, index }) => {
                    write!(
                        f,
                        "Type Mismatch: Tried to index {} with type {}",
                        left.to_type(),
                        index.to_type()
                    )
                }
            },
            EvalError::CallOnNonFunction(object) => {
                write!(f, "Tried to call non-function object: {object}")
            }
            EvalError::ArgumentMismatch { expected, got } => {
                write!(f, "Expected {expected} arguments, got {got}")
            }
            EvalError::BuiltInFunction(bif_error) => write!(f, "{bif_error}"),
            EvalError::IndexOutOfBounds { index, length } => write!(
                f,
                "Index out of bounds: index {index} is out of bounds for array of length {length}"
            ),
            EvalError::UnhashableKey(object) => {
                write!(f, "Unhashable key: {object}")
            }
        }
    }
}

pub trait Node {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError>;
}

impl Node for Statement {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        match self {
            Statement::Let(let_statement) => let_statement.eval(env),
            Statement::Return(return_statement) => return_statement.eval(env),
            Statement::Expression(expression_statement) => expression_statement.eval(env),
            Statement::Assign(assign_statement) => assign_statement.eval(env),
        }
    }
}

impl Node for AssignStatement {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let identifier = self.name.value.clone();
        let value = self.value.eval(env)?;
        env.borrow_mut().set(identifier, Rc::clone(&value));
        Ok(Rc::new(Object::None))
    }
}

impl Node for LetStatement {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let value = self.value.eval(env)?;
        env.borrow_mut()
            .set(self.name.value.clone(), Rc::clone(&value));
        Ok(Rc::new(Object::None))
    }
}

impl Node for ReturnStatement {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let value = match &self.return_value {
            Some(return_value) => return_value.eval(env)?,
            None => Rc::new(Object::None),
        };
        Ok(Rc::new(Object::ReturnValue(Box::new(Rc::clone(&value)))))
    }
}

impl Node for ExpressionStatement {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        Ok(match self {
            ExpressionStatement::NonTerminating(expression) => expression.eval(env)?,
            ExpressionStatement::Terminating(expression) => {
                expression.eval(env)?;
                Rc::new(Object::None)
            }
        })
    }
}

impl Node for BlockExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let mut result = Rc::new(Object::None);

        for statement in self.statements.iter() {
            result = statement.eval(env)?;

            if let Object::ReturnValue(value) = &*result {
                return Ok(Rc::new(Object::ReturnValue(Box::new(Rc::clone(value)))));
            }
        }

        Ok(result)
    }
}

impl Node for Expression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        match self {
            Expression::Bool(boolean_literal) => {
                Ok(Rc::new(Object::Boolean(boolean_literal.value)))
            }
            Expression::Int(integer_literal) => Ok(Rc::new(Object::Integer(integer_literal.value))),
            Expression::Ident(identifier_literal) => identifier_literal.eval(env),
            Expression::String(string_literal) => {
                Ok(Rc::new(Object::String(string_literal.value.clone())))
            }
            Expression::Prefix(prefix_expression) => prefix_expression.eval(env),
            Expression::Infix(infix_expression) => infix_expression.eval(env),
            Expression::If(if_expression) => if_expression.eval(env),
            Expression::Fn(function_literal) => function_literal.eval(env),
            Expression::Call(call_expression) => call_expression.eval(env),
            Expression::Block(block_expression) => block_expression.eval(env),
            Expression::NoneLiteral => Ok(Rc::new(Object::None)),
            Expression::Array(array_literal) => array_literal.eval(env),
            Expression::Index(index_expression) => index_expression.eval(env),
            Expression::Hash(hash_literal) => hash_literal.eval(env),
        }
    }
}

impl Node for HashLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let mut hash = HashMap::new();
        for (key, value) in self.pairs.iter() {
            let key = match (*key.eval(env)?).clone() {
                Object::String(string) => HashKey::String(string),
                Object::Integer(integer) => HashKey::Integer(integer),
                Object::Boolean(boolean) => HashKey::Boolean(boolean),
                object => {
                    return Err(EvalError::UnhashableKey(Rc::new(object)));
                }
            };
            let value = value.eval(env)?;
            hash.insert(key, value);
        }
        Ok(Rc::new(Object::Hash(hash)))
    }
}

impl Node for IndexExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let array = (*self.left.eval(env)?).clone();
        let index = (*self.index.eval(env)?).clone();

        match (array, index) {
            (Object::Array(array), Object::Integer(index)) => index_array(array, index),
            (Object::String(string), Object::Integer(index)) => index_string(string, index),
            (Object::Hash(hash), index) => index_hash(hash, index),
            (array, index) => Err(EvalError::TypeMismatch(TypeMismatch::Index(
                IndexMismatch {
                    left: Rc::new(array),
                    index: Rc::new(index),
                },
            ))),
        }
    }
}

fn index_hash(hash: HashMap<HashKey, Rc<Object>>, index: Object) -> Result<Rc<Object>, EvalError> {
    let key = match index {
        Object::String(string) => HashKey::String(string),
        Object::Integer(integer) => HashKey::Integer(integer),
        Object::Boolean(boolean) => HashKey::Boolean(boolean),
        object => {
            return Err(EvalError::UnhashableKey(Rc::new(object)));
        }
    };
    match hash.get(&key) {
        Some(value) => Ok(Rc::clone(value)),
        None => Ok(Rc::new(Object::None)),
    }
}

fn index_string(string: String, index: isize) -> Result<Rc<Object>, EvalError> {
    let string_length = string.len() as isize;

    if index >= 0 {
        string
            .chars()
            .nth(index as usize)
            .ok_or(EvalError::IndexOutOfBounds {
                index,
                length: string_length,
            })
            .map(|v| Rc::new(Object::String(v.to_string())))
    } else {
        string
            .chars()
            .nth((string_length + index) as usize)
            .ok_or(EvalError::IndexOutOfBounds {
                index,
                length: string_length,
            })
            .map(|v| Rc::new(Object::String(v.to_string())))
    }
}

fn index_array(array: Vec<Rc<Object>>, index: isize) -> Result<Rc<Object>, EvalError> {
    let array_length = array.len() as isize;

    if index >= 0 {
        array
            .get(index as usize)
            .ok_or(EvalError::IndexOutOfBounds {
                index,
                length: array_length,
            })
            .map(Rc::clone)
    } else {
        array
            .get((array_length + index) as usize)
            .ok_or(EvalError::IndexOutOfBounds {
                index,
                length: array_length,
            })
            .map(Rc::clone)
    }
}

impl Node for ArrayLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let elements = self
            .elements
            .iter()
            .map(|e| e.eval(env))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Rc::new(Object::Array(elements)))
    }
}

impl Node for CallExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let left = self.left.eval(env)?;

        let args = self
            .arguments
            .iter()
            .map(|arg| arg.eval(env))
            .collect::<Result<Vec<Rc<Object>>, EvalError>>()?;

        let Function {
            parameters,
            body,
            env,
        } = match left.as_ref() {
            Object::Function(function) => function,
            Object::BuiltIn(BuiltInFunction { function, .. }) => match function(args, env) {
                Ok(object) => return Ok(object),
                Err(error) => return Err(EvalError::BuiltInFunction(error)),
            },
            other => return Err(EvalError::CallOnNonFunction(Rc::new(other.clone()))),
        };

        if args.len() != parameters.len() {
            return Err(EvalError::ArgumentMismatch {
                expected: parameters.len(),
                got: args.len(),
            });
        }

        parameters.iter().zip(args.iter()).for_each(|(param, arg)| {
            (**env)
                .borrow_mut()
                .set(param.value.clone(), Rc::clone(arg))
        });

        body.eval(env)
    }
}

impl Node for FunctionLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        Ok(Rc::new(Object::Function(Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: Environment::new_enclosed(env),
        })))
    }
}

impl Node for IdentifierLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        match env.borrow_mut().get(&self.value) {
            Some(value) => Ok(value),
            None => Err(EvalError::UnknownIdentifier(self.value.clone())),
        }
    }
}

impl Node for PrefixExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let right = self.right.eval(env)?;

        match self.operator {
            PrefixOperator::Bang => Ok(Rc::new(Object::Boolean(!right.to_bool()))),
            PrefixOperator::Minus => match &*right {
                Object::Integer(integer) => Ok(Rc::new(Object::Integer(-integer))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Prefix(
                    PrefixMismatch {
                        operator: PrefixOperator::Minus,
                        right: Rc::clone(&right),
                    },
                ))),
            },
        }
    }
}

impl Node for InfixExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;

        match self.operator {
            InfixOperator::DoubleAmpersand => Ok(Rc::new(Object::Boolean(
                (*left).to_bool() && (*right).to_bool(),
            ))),
            InfixOperator::DoublePipe => Ok(Rc::new(Object::Boolean(
                (*left).to_bool() || (*right).to_bool(),
            ))),
            InfixOperator::Equal => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Boolean(left == right)))
                }
                (Object::Boolean(left), Object::Boolean(right)) => {
                    Ok(Rc::new(Object::Boolean(left == right)))
                }
                (Object::String(left), Object::String(right)) => {
                    Ok(Rc::new(Object::Boolean(left == right)))
                }
                (Object::Array(left), Object::Array(right)) => {
                    Ok(Rc::new(Object::Boolean(left == right)))
                }
                (Object::None, Object::None) => Ok(Rc::new(Object::Boolean(true))),
                (Object::None, _) => Ok(Rc::new(Object::Boolean(false))),
                (_, Object::None) => Ok(Rc::new(Object::Boolean(false))),
                (left, right) => Ok(Rc::new(Object::Boolean(left.to_bool() == right.to_bool()))),
            },
            InfixOperator::NotEqual => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Boolean(left != right)))
                }
                (Object::Boolean(left), Object::Boolean(right)) => {
                    Ok(Rc::new(Object::Boolean(left != right)))
                }
                (Object::String(left), Object::String(right)) => {
                    Ok(Rc::new(Object::Boolean(left != right)))
                }
                (Object::Array(left), Object::Array(right)) => {
                    Ok(Rc::new(Object::Boolean(left != right)))
                }
                (left, right) => Ok(Rc::new(Object::Boolean(left.to_bool() != right.to_bool()))),
            },
            InfixOperator::GreaterThan => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Boolean(left > right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::GreaterThan,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::LessThan => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Boolean(left < right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::LessThan,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::Plus => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Integer(left + right)))
                }
                (Object::String(left), Object::String(right)) => {
                    Ok(Rc::new(Object::String(format!("{left}{right}"))))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::Plus,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::Minus => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Integer(left - right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::Minus,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::Asterisk => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Integer(left * right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::Asterisk,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::Slash => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Integer(left / right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::Slash,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::Percent => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Integer(left % right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::Percent,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::DoubleSlash => match (&*left, &*right) {
                (Object::Integer(left), Object::Integer(right)) => {
                    Ok(Rc::new(Object::Integer(left / right)))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        operator: InfixOperator::DoubleSlash,
                        left: Rc::clone(&left),
                        right: Rc::clone(&right),
                    },
                ))),
            },
            InfixOperator::LBracket => unreachable!("Parser shoud generate a CallExpression"),
            InfixOperator::LParen => unreachable!("Parser shoud generate a CallExpression"),
        }
    }
}

impl Node for IfExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let condition = self.condition.eval(env)?;

        if condition.to_bool() {
            self.consequence.eval(env)
        } else {
            match &self.alternative {
                Some(alternative) => alternative.eval(env),
                None => Ok(Rc::new(Object::None)),
            }
        }
    }
}

impl Node for Program {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let mut result = Rc::new(Object::None);

        for statement in self.statements.iter() {
            result = statement.eval(env)?;

            if let Object::ReturnValue(value) = &*result {
                return Ok(Rc::clone(value));
            }
        }

        Ok(result)
    }
}

pub fn test_eval(input: String) -> Result<Rc<Object>, EvalError> {
    let lexer = Lexer::new(input.clone());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        eprintln!("parsing error for input: \"{input}\"");
        for error in parser.errors.iter() {
            eprintln!("ERROR: {error}");
        }
        eprintln!();
    }
    let env = &Environment::new();
    // let std = get_std_ast();
    // std.eval(env).unwrap();
    program.eval(env)
}

pub fn test_vs_expectation(pairs: Vec<(&str, Result<Object, EvalError>)>) {
    for (input, expectation) in pairs.iter() {
        let program_result = &(match test_eval(input.to_string()) {
            Ok(object) => Ok((*object).clone()),
            Err(error) => Err(error),
        });
        eprintln!("input: {input:?}");
        eprintln!("expectation: {expectation:?}");
        assert_eq!(program_result, expectation);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::interpreter::object::HashKey;

    use super::*;

    #[test]
    fn integer_literal() {
        let pairs = vec![("5", Ok(Object::Integer(5)))];
        test_vs_expectation(pairs);
    }

    #[test]
    fn boolean_literal() {
        let pairs = vec![
            ("true", Ok(Object::Boolean(true))),
            ("false", Ok(Object::Boolean(false))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn bang_operator() {
        let pairs = vec![
            ("!true", Ok(Object::Boolean(false))),
            ("!false", Ok(Object::Boolean(true))),
            ("!5", Ok(Object::Boolean(false))),
            ("!!true", Ok(Object::Boolean(true))),
            ("!!false", Ok(Object::Boolean(false))),
            ("!!5", Ok(Object::Boolean(true))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn minus_operator() {
        let pairs = vec![
            ("-5", Ok(Object::Integer(-5))),
            ("--5", Ok(Object::Integer(5))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn integer_expressions() {
        let pairs = vec![
            ("5 + 5 + 5 + 5 - 10", Ok(Object::Integer(10))),
            ("2 * 2 * 2 * 2 * 2", Ok(Object::Integer(32))),
            ("-50 + 100 + -50", Ok(Object::Integer(0))),
            ("5 * 2 + 10", Ok(Object::Integer(20))),
            ("5 + 2 * 10", Ok(Object::Integer(25))),
            ("20 + 2 * -10", Ok(Object::Integer(0))),
            ("50 / 2 * 2 + 10", Ok(Object::Integer(60))),
            ("2 * (5 + 10)", Ok(Object::Integer(30))),
            ("3 * 3 * 3 + 10", Ok(Object::Integer(37))),
            ("3 * (3 * 3) + 10", Ok(Object::Integer(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Ok(Object::Integer(50))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn boolean_expressions() {
        let pairs = vec![
            ("true", Ok(Object::Boolean(true))),
            ("false", Ok(Object::Boolean(false))),
            ("1 < 2", Ok(Object::Boolean(true))),
            ("1 > 2", Ok(Object::Boolean(false))),
            ("1 < 1", Ok(Object::Boolean(false))),
            ("1 > 1", Ok(Object::Boolean(false))),
            ("1 == 1", Ok(Object::Boolean(true))),
            ("1 != 1", Ok(Object::Boolean(false))),
            ("1 == 2", Ok(Object::Boolean(false))),
            ("1 != 2", Ok(Object::Boolean(true))),
            ("true == true", Ok(Object::Boolean(true))),
            ("false == false", Ok(Object::Boolean(true))),
            ("true == false", Ok(Object::Boolean(false))),
            ("true != false", Ok(Object::Boolean(true))),
            ("false != true", Ok(Object::Boolean(true))),
            ("(1 < 2) == true", Ok(Object::Boolean(true))),
            ("(1 < 2) == false", Ok(Object::Boolean(false))),
            ("(1 > 2) == true", Ok(Object::Boolean(false))),
            ("(1 > 2) == false", Ok(Object::Boolean(true))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn if_expressions() {
        let pairs = vec![
            ("if (1 > 2) { 10 } else { 20 }", Ok(Object::Integer(20))),
            ("if (1 < 2) { 10 } else { 20 }", Ok(Object::Integer(10))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn return_statements() {
        let pairs = vec![
            ("return 10;", Ok(Object::Integer(10))),
            ("return 10; 9;", Ok(Object::Integer(10))),
            ("return 2 * 5; 9;", Ok(Object::Integer(10))),
            ("9; return 2 * 5; 9;", Ok(Object::Integer(10))),
            (
                "
                if (10 > 1) { 
                  if (10 > 1) { 
                    return 10; 
                  }
                  return 1; 
                }",
                Ok(Object::Integer(10)),
            ),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn none() {
        let pairs = vec![
            ("let x = 5;", Ok(Object::None)),
            ("none == true", Ok(Object::Boolean(false))),
            ("none == false", Ok(Object::Boolean(false))),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn let_statements() {
        let pairs = vec![
            ("let a = 4; a", Ok(Object::Integer(4))),
            ("let a = 5 * 5; a", Ok(Object::Integer(25))),
            ("let a = 3; let b = a; b", Ok(Object::Integer(3))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c",
                Ok(Object::Integer(15)),
            ),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    // #[ignore]
    fn function_object() {
        let pairs = vec![(
            "fn(x) { x + 2 }",
            Ok(Object::Function(Function {
                parameters: vec![IdentifierLiteral {
                    value: "x".to_string(),
                }],
                body: BlockExpression {
                    statements: vec![Statement::Expression(ExpressionStatement::NonTerminating(
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Ident(IdentifierLiteral {
                                value: "x".to_string(),
                            })),
                            operator: InfixOperator::Plus,
                            right: Box::new(Expression::Int(IntegerLiteral { value: 2 })),
                        }),
                    ))],
                },
                env: Environment::new_enclosed(&Environment::new()),
            })),
        )];
        test_vs_expectation(pairs);
    }

    #[test]
    fn function_calling() {
        let pairs = vec![
            (
                "let identity = fn(x) { x }; identity(5)",
                Ok(Object::Integer(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5)",
                Ok(Object::Integer(5)),
            ),
            (
                "let double = fn(x) { x * 2 }; double(5)",
                Ok(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5, 5)",
                Ok(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))",
                Ok(Object::Integer(20)),
            ),
            ("fn(x) { x }(5)", Ok(Object::Integer(5))),
        ];
        test_vs_expectation(pairs);
    }

    #[test]
    fn closures() {
        let pairs = vec![(
            "
            let newAdder = fn(x) {
              fn(y) { x + y }
            };
            
            let addTwo = newAdder(2);
            addTwo(2)
            ",
            Ok(Object::Integer(4)),
        )];
        test_vs_expectation(pairs);
    }

    #[test]
    fn strings() {
        let pairs = vec![
            (
                r#""hello world""#,
                Ok(Object::String("hello world".to_string())),
            ),
            (r#""""#, Ok(Object::String("".to_string()))),
            (
                r#""hello" + " " + "world""#,
                Ok(Object::String("hello world".to_string())),
            ),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn assignment() {
        let pairs = vec![
            ("let a = 5; a = 10; a", Ok(Object::Integer(10))),
            ("let a = 5; a = 10 * 2; a", Ok(Object::Integer(20))),
            ("let a = 5; let b = a; a = 10; b", Ok(Object::Integer(5))),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn arary_literals() {
        let pairs = vec![(
            "[1, 2 * 2, 3 + 3]",
            Ok(Object::Array(vec![
                Rc::new(Object::Integer(1)),
                Rc::new(Object::Integer(4)),
                Rc::new(Object::Integer(6)),
            ])),
        )];

        test_vs_expectation(pairs);
    }

    #[test]
    fn index_expressions() {
        let pairs = vec![
            ("[1, 2, 3][0]", Ok(Object::Integer(1))),
            ("[1, 2, 3][1]", Ok(Object::Integer(2))),
            ("[1, 2, 3][2]", Ok(Object::Integer(3))),
            ("[1, 2, 3][-1]", Ok(Object::Integer(3))),
            ("[1, 2, 3][-2]", Ok(Object::Integer(2))),
            ("[1, 2, 3][-3]", Ok(Object::Integer(1))),
            ("let i = 0; [1][i]", Ok(Object::Integer(1))),
            ("[1, 2, 3][1 + 1]", Ok(Object::Integer(3))),
            (
                "let myArray = [1, 2, 3]; myArray[2]",
                Ok(Object::Integer(3)),
            ),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
                Ok(Object::Integer(6)),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Ok(Object::Integer(2)),
            ),
            (
                "[1, 2, 3][3]",
                Err(EvalError::IndexOutOfBounds {
                    index: 3,
                    length: 3,
                }),
            ),
            (
                "[1, 2, 3][-4]",
                Err(EvalError::IndexOutOfBounds {
                    index: -4,
                    length: 3,
                }),
            ),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn index_string() {
        let pairs = vec![
            (r#""abc"[0]"#, Ok(Object::String("a".to_string()))),
            (r#""abc"[1]"#, Ok(Object::String("b".to_string()))),
            (r#""abc"[2]"#, Ok(Object::String("c".to_string()))),
            (
                r#""abc"[3]"#,
                Err(EvalError::IndexOutOfBounds {
                    index: 3,
                    length: 3,
                }),
            ),
            (r#""abc"[-1]"#, Ok(Object::String("c".to_string()))),
            (r#""abc"[-2]"#, Ok(Object::String("b".to_string()))),
            (r#""abc"[-3]"#, Ok(Object::String("a".to_string()))),
            (
                r#""abc"[-4]"#,
                Err(EvalError::IndexOutOfBounds {
                    index: -4,
                    length: 3,
                }),
            ),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn additional_operators() {
        let pairs = vec![
            ("5 // 2", Ok(Object::Integer(2))),
            ("6 % 2", Ok(Object::Integer(0))),
            ("true || false", Ok(Object::Boolean(true))),
            ("true && true", Ok(Object::Boolean(true))),
            ("true && false", Ok(Object::Boolean(false))),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn hash_literal() {
        let pairs = vec![(
            r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
                }
            "#,
            Ok(Object::Hash({
                let mut hash = HashMap::new();
                hash.insert(
                    HashKey::String("one".to_string()),
                    Rc::new(Object::Integer(1)),
                );
                hash.insert(
                    HashKey::String("two".to_string()),
                    Rc::new(Object::Integer(2)),
                );
                hash.insert(
                    HashKey::String("three".to_string()),
                    Rc::new(Object::Integer(3)),
                );
                hash.insert(HashKey::Integer(4), Rc::new(Object::Integer(4)));
                hash.insert(HashKey::Boolean(true), Rc::new(Object::Integer(5)));
                hash.insert(HashKey::Boolean(false), Rc::new(Object::Integer(6)));
                hash
            })),
        )];

        test_vs_expectation(pairs);
    }

    #[test]
    fn indexing_hash() {
        let pairs = vec![
            (
                r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
                }["one"]
            "#,
                Ok(Object::Integer(1)),
            ),
            (
                r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
                }[true]
            "#,
                Ok(Object::Integer(5)),
            ),
            (
                r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: "four",
                true: 5,
                false: 6
                }[4]
            "#,
                Ok(Object::String("four".to_string())),
            ),
            (
                r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: "four",
                true: 5,
                false: 6
                }[56]
            "#,
                Ok(Object::None),
            ),
        ];

        test_vs_expectation(pairs);
    }

    mod errors {

        use std::vec;

        use super::*;

        use std::rc::Rc;

        #[test]
        fn type_mismatch() {
            let pairs = vec![(
                "5 + true;",
                Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::new(Object::Integer(5)),
                        operator: InfixOperator::Plus,
                        right: Rc::new(Object::Boolean(true)),
                    },
                ))),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn unknown_prefix_operator() {
            let pairs = vec![(
                "-true;",
                Err(EvalError::TypeMismatch(TypeMismatch::Prefix(
                    PrefixMismatch {
                        operator: PrefixOperator::Minus,
                        right: Rc::new(Object::Boolean(true)),
                    },
                ))),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn unknown_infix_operator() {
            let pairs = vec![
                (
                    "true + false;",
                    Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                        InfixMismatch {
                            left: Rc::new(Object::Boolean(true)),
                            operator: InfixOperator::Plus,
                            right: Rc::new(Object::Boolean(false)),
                        },
                    ))),
                ),
                (
                    r#""string" - "string""#,
                    Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                        InfixMismatch {
                            left: Rc::new(Object::String("string".to_string())),
                            operator: InfixOperator::Minus,
                            right: Rc::new(Object::String("string".to_string())),
                        },
                    ))),
                ),
            ];

            test_vs_expectation(pairs);
        }

        #[test]
        fn nested_error() {
            let pairs = vec![(
                "
                if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                    return 1;
                  }
                ",
                Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::new(Object::Boolean(true)),
                        operator: InfixOperator::Plus,
                        right: Rc::new(Object::Boolean(false)),
                    },
                ))),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn unknown_identifier() {
            let pairs = vec![(
                "foobar;",
                Err(EvalError::UnknownIdentifier("foobar".to_string())),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn uncallable_value() {
            let pairs = vec![(
                "let x = 5;
                x()
                ",
                Err(EvalError::CallOnNonFunction(Rc::new(Object::Integer(5)))),
            )];
            test_vs_expectation(pairs);
        }

        #[test]
        fn index_invalid_type() {
            let pairs = vec![(
                r#"43[0]"#,
                Err(EvalError::TypeMismatch(TypeMismatch::Index(
                    IndexMismatch {
                        index: Rc::new(Object::Integer(0)),
                        left: Rc::new(Object::Integer(43)),
                    },
                ))),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn unhashable_hash_key() {
            let pairs = vec![(
                r#"{[1,2,3]: 1, 2: 2}"#,
                Err(EvalError::UnhashableKey(Rc::new(Object::Array(vec![
                    Rc::new(Object::Integer(1)),
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(3)),
                ])))),
            )];

            test_vs_expectation(pairs);
        }
    }
}
