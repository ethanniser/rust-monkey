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
    BlockNoValue,
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
            EvalError::BlockNoValue => write!(f, "Block did not return a value"),
        }
    }
}

pub trait Node {
    fn eval(&self) -> Result<Option<Object>, EvalError>;
}

impl Node for Statement {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        match self {
            Statement::Let(let_statement) => let_statement.eval(),
            Statement::Return(return_statement) => return_statement.eval(),
            Statement::Expression(expression_statement) => expression_statement.eval(),
            Statement::Block(block_statement) => block_statement.eval(),
        }
    }
}

impl Node for LetStatement {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        let value = self.value.eval()?;
        Ok(value)
    }
}

impl Node for ReturnStatement {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        let value = self.return_value.eval()?;
        Ok(value)
    }
}

impl Node for ExpressionStatement {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        let value = self.expression.eval()?;
        Ok(value)
    }
}

impl Node for BlockStatement {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        let mut result = None;

        for statement in self.statements.iter() {
            result = Some(Node::eval(statement)?);
        }

        result.ok_or(EvalError::BlockNoValue)
    }
}

impl Node for Expression {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        match self {
            Expression::Boolean(boolean_literal) => Ok(Object::Boolean(boolean_literal.value)),
            Expression::Int(integer_literal) => Ok(Object::Integer(integer_literal.value)),
            Expression::Identifier(identifier_literal) => unimplemented!("Identifier"),
            Expression::Prefix(prefix_expression) => prefix_expression.eval(),
            Expression::Infix(infix_expression) => infix_expression.eval(),
            Expression::If(if_expression) => if_expression.eval(),
            Expression::Function(function_literal) => unimplemented!("Function"),
            Expression::Call(call_expression) => unimplemented!("Call"),
        }
    }
}

impl Node for PrefixExpression {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
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
    fn eval(&self) -> Result<Option<Object>, EvalError> {
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
            // (left, right) => Err(EvalError::TypeMismatch(TypeMismatch::Infix(
            //     InfixMismatch {
            //         left: left,
            //         operator: self.operator,
            //         right: right,
            //     },
            // ))),
        }
    }
}

impl Node for IfExpression {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        let condition = self.condition.eval()?;

        if condition.to_bool() {
            self.consequence.eval()
        } else {
            self.alternative.eval()
        }
    }
}

impl Node for Program {
    fn eval(&self) -> Result<Option<Object>, EvalError> {
        let mut result = None;

        for statement in self.statements.iter() {
            result = Node::eval(statement)?;
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Result<Option<Object>, EvalError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        return program.eval();
    }

    fn test_vs_expectation(input: &str, expected: Option<Object>) {
        let result = test_eval(input.to_string());
        assert_eq!(result, Ok(expected));
    }

    fn test_vs_code(pairs: Vec<(&str, Option<Object>)>) {
        for (input, expectation) in pairs.iter() {
            assert_eq!(
                &test_eval(input.to_string()).expect("test code should not error"),
                expectation
            );
        }
    }

    #[test]
    fn integer_literal() {
        let input = "5";
        let expected = Some(Object::Integer(5));
        test_vs_expectation(input, expected);
    }

    #[test]
    fn boolean_literal() {
        let input = "true";
        let expected = Some(Object::Boolean(true));
        test_vs_expectation(input, expected);
    }

    #[test]
    fn bang_operator() {
        let pairs: Vec<(&str, Option<Object>)> = vec![
            ("!true", Some(Object::Boolean(false))),
            ("!false", Some(Object::Boolean(true))),
            ("!5", Some(Object::Boolean(false))),
            ("!!true", Some(Object::Boolean(true))),
            ("!!false", Some(Object::Boolean(false))),
            ("!!5", Some(Object::Boolean(true))),
        ];
        test_vs_code(pairs);
    }

    #[test]
    fn minus_operator() {
        let pairs = vec![
            ("-5", Some(Object::Integer(-5))),
            ("--5", Some(Object::Integer(5))),
        ];
        test_vs_code(pairs);
    }

    #[test]
    fn integer_expressions() {
        let pairs = vec![
            ("5 + 5 + 5 + 5 - 10", Some(Object::Integer(10))),
            ("2 * 2 * 2 * 2 * 2", Some(Object::Integer(32))),
            ("-50 + 100 + -50", Some(Object::Integer(0))),
            ("5 * 2 + 10", Some(Object::Integer(20))),
            ("5 + 2 * 10", Some(Object::Integer(25))),
            ("20 + 2 * -10", Some(Object::Integer(0))),
            ("50 / 2 * 2 + 10", Some(Object::Integer(60))),
            ("2 * (5 + 10)", Some(Object::Integer(30))),
            ("3 * 3 * 3 + 10", Some(Object::Integer(37))),
            ("3 * (3 * 3) + 10", Some(Object::Integer(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(Object::Integer(50))),
        ];
        test_vs_code(pairs);
    }

    #[test]
    fn boolean_expressions() {
        let pairs = vec![
            ("true", Some(Object::Boolean(true))),
            ("false", Some(Object::Boolean(false))),
            ("1 < 2", Some(Object::Boolean(true))),
            ("1 > 2", Some(Object::Boolean(false))),
            ("1 < 1", Some(Object::Boolean(false))),
            ("1 > 1", Some(Object::Boolean(false))),
            ("1 == 1", Some(Object::Boolean(true))),
            ("1 != 1", Some(Object::Boolean(false))),
            ("1 == 2", Some(Object::Boolean(false))),
            ("1 != 2", Some(Object::Boolean(true))),
            ("true == true", Some(Object::Boolean(true))),
            ("false == false", Some(Object::Boolean(true))),
            ("true == false", Some(Object::Boolean(false))),
            ("true != false", Some(Object::Boolean(true))),
            ("false != true", Some(Object::Boolean(true))),
            ("(1 < 2) == true", Some(Object::Boolean(true))),
            ("(1 < 2) == false", Some(Object::Boolean(false))),
            ("(1 > 2) == true", Some(Object::Boolean(false))),
            ("(1 > 2) == false", Some(Object::Boolean(true))),
        ];
        test_vs_code(pairs);
    }

    #[test]
    fn if_expressions() {
        let pairs = vec![
            ("if (1 > 2) { 10 } else { 20 }", Some(Object::Integer(20))),
            ("if (1 < 2) { 10 } else { 20 }", Some(Object::Integer(10))),
        ];
        test_vs_code(pairs);
    }
}
