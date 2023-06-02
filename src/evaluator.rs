use crate::ast::*;
use crate::object::Object;

#[derive(Debug, PartialEq)]
pub enum EvalError {
    UnknownOperator(String, Object, Object),
    UnknownIdentifier(String),
    TypeMismatch(String, Object, Object),
    ReturnValue(Object),
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
            Expression::Prefix(prefix) => unimplemented!(),
            Expression::Infix(infix) => unimplemented!(),
            Expression::If(if_expression) => unimplemented!(),
            Expression::Function(function) => unimplemented!(),
            Expression::Call(call) => unimplemented!(),
        }
    }
}

pub fn eval_program(program: &Program) -> Result<Object, EvalError> {
    let mut result = Object::Null;

    for statement in program.statements.iter() {
        result = Node::eval(statement)?;
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Result<Object, EvalError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        return eval_program(&program);
    }

    fn test_vs_expectation(input: &str, expected: Object) {
        let result = test_eval(input.to_string());
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn integer_literal() {
        let input = "5";
        let expected = Object::Integer(5);
        test_vs_expectation(input, expected);
    }

    #[test]
    fn boolean_literal() {
        let input = "true";
        let expected = Object::Boolean(true);
        test_vs_expectation(input, expected);
    }
}
