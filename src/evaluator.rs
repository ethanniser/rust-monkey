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

pub trait Node {
    fn eval(&self) -> Result<Object, EvalError>;
}

impl Node for Statement {
    fn eval(&self) -> Result<Object, EvalError> {
        match self {
            Statement::Let(let_statement) => let_statement.eval(),
            Statement::Return(return_statement) => return_statement.eval(),
            Statement::Expression(expression_statement) => expression_statement.eval(),
            Statement::Block(block_statement) => block_statement.eval(),
        }
    }
}

impl Node for LetStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        let value = self.value.eval()?;
        Ok(value)
    }
}

impl Node for ReturnStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        let value = self.return_value.eval()?;
        Ok(value)
    }
}

impl Node for ExpressionStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        let value = self.expression.eval()?;
        Ok(value)
    }
}

impl Node for BlockStatement {
    fn eval(&self) -> Result<Object, EvalError> {
        unimplemented!()
    }
}

impl Node for Expression {
    fn eval(&self) -> Result<Object, EvalError> {
        match self {
            Expression::Boolean(boolean) => Ok(Object::Boolean(boolean.value)),
            Expression::Int(integer) => Ok(Object::Integer(integer.value)),
            Expression::Identifier(identifier) => unimplemented!(),
            Expression::Prefix(prefix_expression) => prefix_expression.eval(),
            Expression::Infix(infix) => unimplemented!(),
            Expression::If(if_expression) => unimplemented!(),
            Expression::Function(function) => unimplemented!(),
            Expression::Call(call) => unimplemented!(),
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

pub fn eval_program(program: &Program) -> Result<Option<Object>, EvalError> {
    let mut result = None;

    for statement in program.statements.iter() {
        result = Some(Node::eval(statement)?);
    }

    Ok(result)
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
        return eval_program(&program);
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
}
