use std::fmt::Display;
use std::rc::Rc;

use crate::ast::*;
use crate::environment::{Env, Environment};
use crate::object::{Function, Object};

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
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    UnknownIdentifier(String),
    TypeMismatch(TypeMismatch),
    CallOnNonFunction(Rc<Object>),
    ArgumentMismatch { expected: usize, got: usize },
    BuiltInFunction(BuiltInFunctionError),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnknownIdentifier(identifier) => {
                write!(f, "unknown identifier: {}", identifier)
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
            },
            EvalError::CallOnNonFunction(object) => {
                write!(f, "Tried to call non-function object: {}", object)
            }
            EvalError::ArgumentMismatch { expected, got } => {
                write!(f, "Expected {} arguments, got {}", expected, got)
            }
            EvalError::BuiltInFunction(bif_error) => write!(f, "{}", bif_error),
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

            match &*result {
                Object::ReturnValue(value) => {
                    return Ok(Rc::new(Object::ReturnValue(Box::new(Rc::clone(value)))))
                }
                _ => (),
            }
        }

        Ok(result)
    }
}

impl Node for Expression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        match self {
            Expression::Boolean(boolean_literal) => {
                Ok(Rc::new(Object::Boolean(boolean_literal.value)))
            }
            Expression::Int(integer_literal) => Ok(Rc::new(Object::Integer(integer_literal.value))),
            Expression::Identifier(identifier_literal) => identifier_literal.eval(env),
            Expression::String(string_literal) => {
                Ok(Rc::new(Object::String(string_literal.value.clone())))
            }
            Expression::Prefix(prefix_expression) => prefix_expression.eval(env),
            Expression::Infix(infix_expression) => infix_expression.eval(env),
            Expression::If(if_expression) => if_expression.eval(env),
            Expression::Function(function_literal) => function_literal.eval(env),
            Expression::Call(call_expression) => call_expression.eval(env),
            Expression::Block(block_expression) => block_expression.eval(env),
            Expression::NoneLiteral => Ok(Rc::new(Object::None)),
            Expression::Array(array_literal) => array_literal.eval(env),
            Expression::Index(index_expression) => index_expression.eval(env),
        }
    }
}

impl Node for IndexExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        todo!();
    }
}

impl Node for ArrayLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        todo!();
    }
}

impl Node for CallExpression {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        let function = match &self.function {
            CallableExpression::Identifier(identifier) => match env.borrow().get(&identifier.value)
            {
                Some(object) => object,
                None => return Err(EvalError::UnknownIdentifier(identifier.value.clone())),
            },
            CallableExpression::Function(function) => function.eval(env)?,
        };

        let args = self
            .arguments
            .iter()
            .map(|arg| arg.eval(env))
            .collect::<Result<Vec<Rc<Object>>, EvalError>>()?;

        let Function {
            parameters,
            body,
            env,
        } = match function.as_ref() {
            Object::Function(function) => function,
            Object::BuiltIn(BuiltInFunction { function, .. }) => match function(args) {
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

        let env = Environment::new_enclosed(&env);

        parameters
            .iter()
            .zip(args.iter())
            .for_each(|(param, arg)| env.borrow_mut().set(param.value.clone(), Rc::clone(arg)));

        body.eval(&env)
    }
}

impl Node for FunctionLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        Ok(Rc::new(Object::Function(Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: Rc::clone(env),
        })))
    }
}

impl Node for IdentifierLiteral {
    fn eval(&self, env: &Env) -> Result<Rc<Object>, EvalError> {
        match env.borrow().get(&self.value) {
            Some(value) => Ok(value.clone()),
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

        match (left.as_ref(), right.as_ref()) {
            (Object::Integer(l_val), Object::Integer(r_val)) => match self.operator {
                InfixOperator::Plus => Ok(Rc::new(Object::Integer(l_val + r_val))),
                InfixOperator::Minus => Ok(Rc::new(Object::Integer(l_val - r_val))),
                InfixOperator::Asterisk => Ok(Rc::new(Object::Integer(l_val * r_val))),
                InfixOperator::Slash => Ok(Rc::new(Object::Integer(l_val / r_val))),
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(l_val == r_val))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(l_val != r_val))),
                InfixOperator::GreaterThan => Ok(Rc::new(Object::Boolean(l_val > r_val))),
                InfixOperator::LessThan => Ok(Rc::new(Object::Boolean(l_val < r_val))),
            },
            (Object::Boolean(l_val), Object::Boolean(r_val)) => match self.operator {
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(l_val == r_val))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(l_val != r_val))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::clone(&left),
                        operator: self.operator,
                        right: Rc::clone(&right),
                    },
                ))),
            },
            (Object::Boolean(l_val), Object::Integer(r_val)) => match self.operator {
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(
                    *l_val == Object::Integer(*r_val).to_bool(),
                ))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(
                    *l_val != Object::Integer(*r_val).to_bool(),
                ))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::clone(&left),
                        operator: self.operator,
                        right: Rc::clone(&right),
                    },
                ))),
            },
            (Object::Integer(l_val), Object::Boolean(r_val)) => match self.operator {
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(
                    Object::Integer(*l_val).to_bool() == *r_val,
                ))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(
                    Object::Integer(*l_val).to_bool() != *r_val,
                ))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::clone(&left),
                        operator: self.operator,
                        right: Rc::clone(&right),
                    },
                ))),
            },
            (Object::String(l_val), Object::String(r_val)) => match self.operator {
                InfixOperator::Plus => Ok(Rc::new(Object::String(format!("{}{}", l_val, r_val)))),
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(l_val == r_val))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(l_val != r_val))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            (Object::None, Object::None) => match self.operator {
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(true))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(false))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::clone(&left),
                        operator: self.operator,
                        right: Rc::clone(&right),
                    },
                ))),
            },
            (Object::None, _) => match self.operator {
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(false))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(true))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::clone(&left),
                        operator: self.operator,
                        right: Rc::clone(&right),
                    },
                ))),
            },
            (_, Object::None) => match self.operator {
                InfixOperator::Equal => Ok(Rc::new(Object::Boolean(false))),
                InfixOperator::NotEqual => Ok(Rc::new(Object::Boolean(true))),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Rc::clone(&left),
                        operator: self.operator,
                        right: Rc::clone(&right),
                    },
                ))),
            },
            (_, _) => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                InfixMismatch {
                    left: Rc::clone(&left),
                    operator: self.operator,
                    right: Rc::clone(&right),
                },
            ))),
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

            match &*result {
                Object::ReturnValue(value) => return Ok(Rc::clone(value)),
                _ => (),
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::object::Function;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Result<Rc<Object>, EvalError> {
        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            eprintln!("parsing error for input: \"{input}\"");
            for error in parser.errors.iter() {
                eprintln!("ERROR: {}", error);
            }
            eprintln!();
        }
        let ref env = Environment::new();
        return program.eval(env);
    }

    pub fn test_vs_code(pairs: Vec<(&str, Result<Object, EvalError>)>) {
        for (input, expectation) in pairs.iter() {
            let ref program_result = match test_eval(input.to_string()) {
                Ok(object) => match Rc::try_unwrap(object).ok() {
                    Some(object) => Ok(object),
                    None => panic!("object is not unique"),
                },
                Err(error) => Err(error),
            };
            assert_eq!(program_result, expectation);
        }
    }

    #[test]
    fn integer_literal() {
        let pairs = vec![("5", Ok(Object::Integer(5)))];
        test_vs_code(pairs);
    }

    #[test]
    fn boolean_literal() {
        let pairs = vec![
            ("true", Ok(Object::Boolean(true))),
            ("false", Ok(Object::Boolean(false))),
        ];
        test_vs_code(pairs);
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
        test_vs_code(pairs);
    }

    #[test]
    fn minus_operator() {
        let pairs = vec![
            ("-5", Ok(Object::Integer(-5))),
            ("--5", Ok(Object::Integer(5))),
        ];
        test_vs_code(pairs);
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
        test_vs_code(pairs);
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
        test_vs_code(pairs);
    }

    #[test]
    fn if_expressions() {
        let pairs = vec![
            ("if (1 > 2) { 10 } else { 20 }", Ok(Object::Integer(20))),
            ("if (1 < 2) { 10 } else { 20 }", Ok(Object::Integer(10))),
        ];
        test_vs_code(pairs);
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
        test_vs_code(pairs);
    }

    #[test]
    fn none() {
        let pairs = vec![
            ("let x = 5;", Ok(Object::None)),
            ("none == true", Ok(Object::Boolean(false))),
            ("none == false", Ok(Object::Boolean(false))),
        ];

        test_vs_code(pairs);
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
        test_vs_code(pairs);
    }

    #[test]
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
                            left: Box::new(Expression::Identifier(IdentifierLiteral {
                                value: "x".to_string(),
                            })),
                            operator: InfixOperator::Plus,
                            right: Box::new(Expression::Int(IntegerLiteral { value: 2 })),
                        }),
                    ))],
                },
                env: Environment::new(),
            })),
        )];
        test_vs_code(pairs);
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
        test_vs_code(pairs);
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
        test_vs_code(pairs);
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

        test_vs_code(pairs);
    }

    #[test]
    fn assignment() {
        let pairs = vec![
            ("let a = 5; a = 10; a", Ok(Object::Integer(10))),
            ("let a = 5; a = 10 * 2; a", Ok(Object::Integer(20))),
            ("let a = 5; let b = a; a = 10; b", Ok(Object::Integer(5))),
        ];

        test_vs_code(pairs);
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

            test_vs_code(pairs);
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

            test_vs_code(pairs);
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

            test_vs_code(pairs);
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

            test_vs_code(pairs);
        }

        #[test]
        fn unknown_identifier() {
            let pairs = vec![(
                "foobar;",
                Err(EvalError::UnknownIdentifier("foobar".to_string())),
            )];

            test_vs_code(pairs);
        }

        #[test]
        fn uncallable_value() {
            let pairs = vec![(
                "let x = 5;
                x()
                ",
                Err(EvalError::CallOnNonFunction(Rc::new(Object::Integer(5)))),
            )];
            test_vs_code(pairs);
        }
    }
}

pub use built_in_functions::*;

pub mod built_in_functions {
    use super::*;
    use std::fmt::Formatter;

    pub type BuiltInFunctionType = fn(Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError>;

    #[derive(Debug, PartialEq)]
    pub struct BuiltInFunctionError {
        pub function: BuiltInFunction,
        pub error: BIFInnerError,
        pub message: Option<String>,
    }

    impl Display for BuiltInFunctionError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let preface = format!("Error in built in function '{}': ", self.function);
            let error = match &self.error {
                BIFInnerError::WrongNumberOfArguments { expected, got } => format!(
                    "Wrong number of arguments. Expected {}, got {}",
                    expected, got
                ),
                BIFInnerError::WrongArgumentType { expected, got } => {
                    format!(
                        "Wrong argument type. Expected: {}. Got: {}",
                        expected,
                        got.to_type()
                    )
                }
            };
            let message = match self.message.clone() {
                Some(message) => format!("\n{}", message),
                None => "".to_string(),
            };

            write!(f, "{}{}{}", preface, error, message)
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum BIFInnerError {
        WrongNumberOfArguments {
            expected: usize,
            got: usize,
        },
        WrongArgumentType {
            expected: ObjectExpectation,
            got: Rc<Object>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum ObjectExpectation {
        One(Rc<Object>),
        Many(Vec<Rc<Object>>),
    }

    impl Display for ObjectExpectation {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                ObjectExpectation::One(object) => write!(f, "{}", object.to_type()),
                ObjectExpectation::Many(objects) => {
                    let type_union = objects
                        .iter()
                        .map(|o| o.to_type())
                        .collect::<Vec<_>>()
                        .join(" | ");
                    write!(f, "{}", type_union)
                }
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct BuiltInFunction {
        pub function: BuiltInFunctionType,
        type_signature: &'static str,
    }

    impl Display for BuiltInFunction {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.type_signature)
        }
    }

    pub struct BuiltIns {
        pub functions: Vec<(String, BuiltInFunction)>,
    }

    mod type_signatures {
        pub const LEN: &str = "len(x: string) -> integer";
    }

    impl BuiltIns {
        pub fn new() -> Self {
            let functions = vec![(
                "len".to_string(),
                BuiltInFunction {
                    function: len,
                    type_signature: type_signatures::LEN,
                },
            )];

            Self {
                functions: functions,
            }
        }
    }

    fn len(args: Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError> {
        if args.len() != 1 {
            return Err(BuiltInFunctionError {
                error: BIFInnerError::WrongNumberOfArguments {
                    expected: 1,
                    got: args.len(),
                },
                function: BuiltInFunction {
                    function: len,
                    type_signature: type_signatures::LEN,
                },
                message: None,
            });
        }

        match &*args[0] {
            Object::String(s) => Ok(Rc::new(Object::Integer(s.len() as isize))),
            _ => Err(BuiltInFunctionError {
                error: BIFInnerError::WrongArgumentType {
                    expected: ObjectExpectation::One(Rc::new(Object::String("".to_string()))),
                    got: args[0].clone(),
                },
                function: BuiltInFunction {
                    function: len,
                    type_signature: type_signatures::LEN,
                },
                message: None,
            }),
        }
    }

    #[cfg(test)]
    mod tests {

        use crate::evaluator::tests::test_vs_code;

        use super::*;

        #[test]
        fn length() {
            let pairs = vec![
                (r#"len("")"#, Ok(Object::Integer(0))),
                (r#"len("four")"#, Ok(Object::Integer(4))),
                (r#"len("hello world")"#, Ok(Object::Integer(11))),
                (
                    "len(1)",
                    Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                        error: BIFInnerError::WrongArgumentType {
                            expected: ObjectExpectation::One(Rc::new(Object::String(
                                "".to_string(),
                            ))),
                            got: Rc::new(Object::Integer(1)),
                        },
                        function: BuiltInFunction {
                            function: len,
                            type_signature: type_signatures::LEN,
                        },
                        message: None,
                    })),
                ),
                (
                    r#"len("one", "two")"#,
                    Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                        error: BIFInnerError::WrongNumberOfArguments {
                            expected: 1,
                            got: 2,
                        },
                        function: BuiltInFunction {
                            function: len,
                            type_signature: type_signatures::LEN,
                        },
                        message: None,
                    })),
                ),
                (
                    "len(5)",
                    Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                        error: BIFInnerError::WrongArgumentType {
                            expected: ObjectExpectation::One(Rc::new(Object::String(
                                "".to_string(),
                            ))),
                            got: Rc::new(Object::Integer(5)),
                        },
                        function: BuiltInFunction {
                            function: len,
                            type_signature: type_signatures::LEN,
                        },
                        message: None,
                    })),
                ),
            ];

            test_vs_code(pairs);
        }
    }
}
