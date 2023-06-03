use std::fmt::Display;

use crate::ast::*;
use crate::object::Object;

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
    fn eval(&self) -> Result<Object, EvalError>;
}

impl Node for Statement {
    fn eval(&self) -> Result<Object, EvalError> {
        match self {
            Statement::Let(let_statement) => let_statement.eval(),
            Statement::Return(return_statement) => return_statement.eval(),
            Statement::Expression(expression_statement) => expression_statement.eval(),
        }
    }
}

impl Node for LetStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        let _value = self.value.eval()?;
        Ok(Object::None)
    }
}

impl Node for ReturnStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        let value = match self.return_value {
            Some(ref return_value) => return_value.eval()?,
            None => Object::None,
        };
        Ok(Object::ReturnValue(Box::new(value)))
    }
}

impl Node for ExpressionStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        Ok(match self {
            ExpressionStatement::NonTerminating(expression) => expression.eval()?,
            ExpressionStatement::Terminating(expression) => {
                expression.eval()?;
                Object::None
            }
        })
    }
}

impl Node for BlockExpression {
    fn eval(&self) -> Result<Object, EvalError> {
        let mut result = Object::None;

        for statement in self.statements.iter() {
            result = statement.eval()?;

            match result {
                Object::ReturnValue(value) => return Ok(Object::ReturnValue(value)),
                _ => (),
            }
        }

        Ok(result)
    }
}

impl Node for Expression {
    fn eval(&self) -> Result<Object, EvalError> {
        match self {
            Expression::Boolean(boolean_literal) => Ok(Object::Boolean(boolean_literal.value)),
            Expression::Int(integer_literal) => Ok(Object::Integer(integer_literal.value)),
            Expression::Identifier(_identifier_literal) => unimplemented!("Identifier"),
            Expression::Prefix(prefix_expression) => prefix_expression.eval(),
            Expression::Infix(infix_expression) => infix_expression.eval(),
            Expression::If(if_expression) => if_expression.eval(),
            Expression::Function(_function_literal) => unimplemented!("Function"),
            Expression::Call(_call_expression) => unimplemented!("Call"),
            Expression::Block(block_expression) => block_expression.eval(),
            Expression::NoneLiteral => Ok(Object::None),
        }
    }
}

impl Node for PrefixExpression {
    fn eval(&self) -> Result<Object, EvalError> {
        let right = self.right.eval()?;

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
    fn eval(&self) -> Result<Object, EvalError> {
        let left = self.left.eval()?;
        let right = self.right.eval()?;

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
    fn eval(&self) -> Result<Object, EvalError> {
        let condition = self.condition.eval()?;

        if condition.to_bool() {
            self.consequence.eval()
        } else {
            match &self.alternative {
                Some(alternative) => alternative.eval(),
                None => Ok(Object::None),
            }
        }
    }
}

impl Node for Program {
    fn eval(&self) -> Result<Object, EvalError> {
        let mut result = Object::None;

        for statement in self.statements.iter() {
            result = statement.eval()?;

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
        return program.eval();
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
    }
}
