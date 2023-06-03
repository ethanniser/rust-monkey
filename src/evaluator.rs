use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::ast::*;
use crate::environment::Environment;
use crate::object::{Function, Object};

#[derive(Debug, PartialEq)]
pub struct PrefixMismatch {
    pub operator: PrefixOperator,
    pub right: Object,
}

#[derive(Debug, PartialEq)]
pub struct InfixMismatch {
    pub left: Object,
    pub operator: InfixOperator,
    pub right: Object,
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
        }
    }
}

pub trait Node {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError>;
}

impl Node for Statement {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        match self {
            Statement::Let(let_statement) => let_statement.eval(env),
            Statement::Return(return_statement) => return_statement.eval(env),
            Statement::Expression(expression_statement) => expression_statement.eval(env),
        }
    }
}

impl Node for LetStatement {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let value = self.value.eval(env)?;
        env.set(self.name.value.clone(), value);
        Ok(Object::None)
    }
}

impl Node for ReturnStatement {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let value = match self.return_value {
            Some(ref return_value) => return_value.eval(env)?,
            None => Object::None,
        };
        Ok(Object::ReturnValue(Box::new(value)))
    }
}

impl Node for ExpressionStatement {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        Ok(match self {
            ExpressionStatement::NonTerminating(expression) => expression.eval(env)?,
            ExpressionStatement::Terminating(expression) => {
                expression.eval(env)?;
                Object::None
            }
        })
    }
}

impl Node for BlockExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let mut result = Object::None;

        for statement in self.statements.iter() {
            result = statement.eval(env)?;

            match result {
                Object::ReturnValue(value) => return Ok(Object::ReturnValue(value)),
                _ => (),
            }
        }

        Ok(result)
    }
}

impl Node for Expression {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        match self {
            Expression::Boolean(boolean_literal) => Ok(Object::Boolean(boolean_literal.value)),
            Expression::Int(integer_literal) => Ok(Object::Integer(integer_literal.value)),
            Expression::Identifier(identifier_literal) => identifier_literal.eval(env),
            Expression::Prefix(prefix_expression) => prefix_expression.eval(env),
            Expression::Infix(infix_expression) => infix_expression.eval(env),
            Expression::If(if_expression) => if_expression.eval(env),
            Expression::Function(function_literal) => function_literal.eval(env),
            Expression::Call(call_expression) => call_expression.eval(env),
            Expression::Block(block_expression) => block_expression.eval(env),
            Expression::NoneLiteral => Ok(Object::None),
        }
    }
}

impl Node for CallExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let function = match &self.function {
            CallableExpression::Identifier(identifier) => match env.get(&identifier.value) {
                Some(Object::Function(function)) => function.clone(),
                _ => return Err(EvalError::UnknownIdentifier(identifier.value.clone())),
            },
            CallableExpression::Function(function) => match function.eval(env)? {
                Object::Function(function) => function.clone(),
                _ => unreachable!("function literal should always evaluate to a function"),
            },
        };

        let evaluated_args = self
            .arguments
            .iter()
            .map(|arg| arg.eval(env))
            .collect::<Result<Vec<Object>, EvalError>>()?;

        todo!();
    }
}

impl Node for FunctionLiteral {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        Ok(Object::Function(Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: Rc::new(RefCell::new(Environment::new_enclosed(env))),
        }))
    }
}

impl Node for IdentifierLiteral {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        match env.get(&self.value) {
            Some(value) => Ok(value.clone()),
            None => Err(EvalError::UnknownIdentifier(self.value.clone())),
        }
    }
}

impl Node for PrefixExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let right = self.right.eval(env)?;

        match self.operator {
            PrefixOperator::Bang => Ok(Object::Boolean(!right.to_bool())),
            PrefixOperator::Minus => match right {
                Object::Integer(integer) => Ok(Object::Integer(-integer)),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Prefix(
                    PrefixMismatch {
                        operator: PrefixOperator::Minus,
                        right,
                    },
                ))),
            },
        }
    }
}

impl Node for InfixExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;

        match (left.clone(), right.clone()) {
            (Object::Integer(l_val), Object::Integer(r_val)) => match self.operator {
                InfixOperator::Plus => Ok(Object::Integer(l_val + r_val)),
                InfixOperator::Minus => Ok(Object::Integer(l_val - r_val)),
                InfixOperator::Asterisk => Ok(Object::Integer(l_val * r_val)),
                InfixOperator::Slash => Ok(Object::Integer(l_val / r_val)),
                InfixOperator::Equal => Ok(Object::Boolean(l_val == r_val)),
                InfixOperator::NotEqual => Ok(Object::Boolean(l_val != r_val)),
                InfixOperator::GreaterThan => Ok(Object::Boolean(l_val > r_val)),
                InfixOperator::LessThan => Ok(Object::Boolean(l_val < r_val)),
            },
            (Object::Boolean(l_val), Object::Boolean(r_val)) => match self.operator {
                InfixOperator::Equal => Ok(Object::Boolean(l_val == r_val)),
                InfixOperator::NotEqual => Ok(Object::Boolean(l_val != r_val)),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            (Object::Boolean(l_val), Object::Integer(r_val)) => match self.operator {
                InfixOperator::Equal => {
                    Ok(Object::Boolean(l_val == Object::Integer(r_val).to_bool()))
                }
                InfixOperator::NotEqual => {
                    Ok(Object::Boolean(l_val != Object::Integer(r_val).to_bool()))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            (Object::Integer(l_val), Object::Boolean(r_val)) => match self.operator {
                InfixOperator::Equal => {
                    Ok(Object::Boolean(Object::Integer(l_val).to_bool() == r_val))
                }
                InfixOperator::NotEqual => {
                    Ok(Object::Boolean(Object::Integer(l_val).to_bool() != r_val))
                }
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            // (Object::String(l_val), Object::String(r_val)) => match self.operator {
            //     InfixOperator::Plus => Ok(Object::String(format!("{}{}", l_val, r_val))),
            //     InfixOperator::Equal => Ok(Object::Boolean(l_val == r_val)),
            //     InfixOperator::NotEqual => Ok(Object::Boolean(l_val != r_val)),
            //     _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
            //         InfixMismatch {
            //             left,
            //             operator: self.operator,
            //             right,
            //         },
            //     ))),
            // },
            (Object::None, Object::None) => match self.operator {
                InfixOperator::Equal => Ok(Object::Boolean(true)),
                InfixOperator::NotEqual => Ok(Object::Boolean(false)),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            (Object::None, _) => match self.operator {
                InfixOperator::Equal => Ok(Object::Boolean(false)),
                InfixOperator::NotEqual => Ok(Object::Boolean(true)),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            (_, Object::None) => match self.operator {
                InfixOperator::Equal => Ok(Object::Boolean(false)),
                InfixOperator::NotEqual => Ok(Object::Boolean(true)),
                _ => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left,
                        operator: self.operator,
                        right,
                    },
                ))),
            },
            (left, right) => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                InfixMismatch {
                    left: left,
                    operator: self.operator,
                    right: right,
                },
            ))),
        }
    }
}

impl Node for IfExpression {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let condition = self.condition.eval(env)?;

        if condition.to_bool() {
            self.consequence.eval(env)
        } else {
            match &self.alternative {
                Some(alternative) => alternative.eval(env),
                None => Ok(Object::None),
            }
        }
    }
}

impl Node for Program {
    fn eval(&self, env: &mut Environment) -> Result<Object, EvalError> {
        let mut result = Object::None;

        for statement in self.statements.iter() {
            result = statement.eval(env)?;

            match result {
                Object::ReturnValue(value) => return Ok(*value),
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

    fn test_eval(input: String) -> Result<Object, EvalError> {
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
        let ref mut env = Environment::new();
        return program.eval(env);
    }

    fn test_vs_expectation(input: &str, expected: Result<Object, EvalError>) {
        let result = test_eval(input.to_string());
        assert_eq!(result, expected);
    }

    fn test_vs_code(pairs: Vec<(&str, Result<Object, EvalError>)>) {
        for (input, expectation) in pairs.iter() {
            assert_eq!(&test_eval(input.to_string()), expectation);
        }
    }

    #[test]
    fn integer_literal() {
        let input = "5";
        let expected = Ok(Object::Integer(5));
        test_vs_expectation(input, expected);
    }

    #[test]
    fn boolean_literal() {
        let input = "true";
        let expected = Ok(Object::Boolean(true));
        test_vs_expectation(input, expected);
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
                env: Rc::new(RefCell::new(Environment::new())),
            })),
        )];
        test_vs_code(pairs);
    }

    #[test]
    fn function_calling() {
        let pairs = vec![
            (
                "let identity = fn(x) { x }; identity(5);",
                Ok(Object::Integer(5)),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Ok(Object::Integer(5)),
            ),
            (
                "let double = fn(x) { x * 2 }; double(5);",
                Ok(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5, 5);",
                Ok(Object::Integer(10)),
            ),
            (
                "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));",
                Ok(Object::Integer(20)),
            ),
            ("fn(x) { x }(5)", Ok(Object::Integer(5))),
        ];
        test_vs_code(pairs);
    }

    mod errors {

        use std::vec;

        use super::*;

        #[test]
        fn type_mismatch() {
            let pairs = vec![(
                "5 + true;",
                Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Object::Integer(5),
                        operator: InfixOperator::Plus,
                        right: Object::Boolean(true),
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
                        right: Object::Boolean(true),
                    },
                ))),
            )];

            test_vs_code(pairs);
        }

        #[test]
        fn unknown_infix_operator() {
            let pairs = vec![(
                "true + false;",
                Err(EvalError::TypeMismatch(TypeMismatch::Infix(
                    InfixMismatch {
                        left: Object::Boolean(true),
                        operator: InfixOperator::Plus,
                        right: Object::Boolean(false),
                    },
                ))),
            )];

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
                        left: Object::Boolean(true),
                        operator: InfixOperator::Plus,
                        right: Object::Boolean(false),
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
    }
}
