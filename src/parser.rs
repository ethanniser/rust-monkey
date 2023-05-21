use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::HashMap;

impl Token {
    fn get_precedence(&self) -> Precedence {
        match self {
            Token::Eq => Precedence::Equals,
            Token::NotEq => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Slash => Precedence::Product,
            Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParserError>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParserError>;

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub message: String,
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParserError>,
    pub prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    pub infix_parse_fns: HashMap<Token, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(Token::Identifier("".to_string()), Parser::parse_identifier);
        parser.register_prefix(Token::Int(0), Parser::parse_interger_literal);
        parser.register_prefix(Token::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(Token::Minus, Parser::parse_prefix_expression);

        parser.register_infix(Token::Plus, Parser::parse_infix_expression);
        parser.register_infix(Token::Minus, Parser::parse_infix_expression);
        parser.register_infix(Token::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(Token::Slash, Parser::parse_infix_expression);
        parser.register_infix(Token::Eq, Parser::parse_infix_expression);
        parser.register_infix(Token::NotEq, Parser::parse_infix_expression);
        parser.register_infix(Token::Lt, Parser::parse_infix_expression);
        parser.register_infix(Token::Gt, Parser::parse_infix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), ParserError> {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError {
                message: format!(
                    "expected next token to be {token:?}, found {:?} instead",
                    self.peek_token
                ),
            })
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.cur_token.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_peek(Token::Identifier("".to_string()))?;

        let name = match self.cur_token.clone() {
            Token::Identifier(ref name) => Identifier {
                value: name.clone(),
            },
            other_token => {
                return Err(ParserError {
                    message: format!(
                        "expected next token to be indetifier, found: {other_token:?}",
                    ),
                })
            }
        };

        self.expect_peek(Token::Assign)?;

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(LetStatement {
            name,
            value: Expression::Temp(Temp),
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(ReturnStatement {
            return_value: Expression::Temp(Temp),
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(ExpressionStatement { expression }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token);

        match prefix {
            Some(prefix) => {
                let mut left_expression = prefix(self)?;

                while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
                    let infix = self.infix_parse_fns.get(&self.peek_token).cloned();

                    if let Some(infix) = infix {
                        self.next_token();
                        left_expression = infix(self, left_expression)?;
                    } else {
                        break;
                    }
                }

                Ok(left_expression)
            }
            None => Err(ParserError {
                message: format!(
                    "no prefix parse function for {cur_token:?} found",
                    cur_token = self.cur_token
                ),
            }),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::Identifier(ref identifier) => Ok(Expression::Identifier(Identifier {
                value: identifier.clone(),
            })),
            other_token => Err(ParserError {
                message: format!("expected next token to be identifier, found: {other_token:?}",),
            }),
        }
    }

    fn parse_interger_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::Int(value) => Ok(Expression::Int(IntegerLiteral { value })),
            other_token => Err(ParserError {
                message: format!(
                    "expected next token to be integer literal, found: {other_token:?}",
                ),
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = match self.cur_token.clone() {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
            other_token => {
                return Err(ParserError {
                    message: format!(
                        "expected next token to be prefix operator, found: {other_token:?}",
                    ),
                })
            }
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(PrefixExpression {
            operator,
            right: Box::new(right),
        }))
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.get_precedence()
    }

    fn cur_precedence(&self) -> Precedence {
        self.cur_token.get_precedence()
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let operator = match self.cur_token.clone() {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Asterisk,
            Token::Slash => InfixOperator::Slash,
            Token::Eq => InfixOperator::Equal,
            Token::NotEq => InfixOperator::NotEqual,
            Token::Lt => InfixOperator::LessThan,
            Token::Gt => InfixOperator::GreaterThan,
            other_token => {
                return Err(ParserError {
                    message: format!(
                        "expected next token to be infix operator, found: {other_token:?}",
                    ),
                })
            }
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    pub fn register_prefix(&mut self, token: Token, function: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, function);
    }

    pub fn register_infix(&mut self, token: Token, function: InfixParseFn) {
        self.infix_parse_fns.insert(token, function);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn let_statement() {
        let input = "
                let x = 5;
                let y = 10;
                let foobar = 838383;
                "
        .to_string();

        let expectation = [
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "x".to_string(),
                },
                value: Expression::Temp(Temp),
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "y".to_string(),
                },
                value: Expression::Temp(Temp),
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "foobar".to_string(),
                },
                value: Expression::Temp(Temp),
            }),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
        assert!(parser.errors.is_empty(), "parser.errors is not empty");

        assert!(
            program.statements.len() == 3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }
    }

    #[test]
    fn return_statement() {
        let input = "
                return 5;
                return 10;
                return 993322;
                "
        .to_string();

        let expectation = [
            Statement::Return(ReturnStatement {
                return_value: Expression::Temp(Temp),
            }),
            Statement::Return(ReturnStatement {
                return_value: Expression::Temp(Temp),
            }),
            Statement::Return(ReturnStatement {
                return_value: Expression::Temp(Temp),
            }),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
        assert!(parser.errors.is_empty(), "parser.errors is not empty");

        assert!(
            program.statements.len() == 3,
            "program.statements does not contain 3 statements, got: {}",
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }
    }

    #[test]
    fn indetifier_expression() {
        let input = "
                foobar;
                "
        .to_string();

        let expectation = [Statement::Expression(ExpressionStatement {
            expression: Expression::Identifier(Identifier {
                value: "foobar".to_string(),
            }),
        })];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
        assert!(parser.errors.is_empty(), "parser.errors is not empty");
        assert!(
            program.statements.len() == 1,
            "program.statements does not contain 1 statements, got: {}",
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }
    }

    #[test]
    fn interger_literal() {
        let input = "
                5;
                "
        .to_string();

        let expectation = [Statement::Expression(ExpressionStatement {
            expression: Expression::Int(IntegerLiteral { value: 5 }),
        })];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
        assert!(parser.errors.is_empty(), "parser.errors is not empty");
        assert!(
            program.statements.len() == 1,
            "program.statements does not contain 1 statements, got: {}",
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }
    }

    #[test]
    fn prefix_expression() {
        let input = "
                -15;
                !foobar;
                "
        .to_string();

        let expectation = [
            Statement::Expression(ExpressionStatement {
                expression: Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Minus,
                    right: Box::new(Expression::Int(IntegerLiteral { value: 15 })),
                }),
            }),
            Statement::Expression(ExpressionStatement {
                expression: Expression::Prefix(PrefixExpression {
                    operator: PrefixOperator::Bang,
                    right: Box::new(Expression::Identifier(Identifier {
                        value: "foobar".to_string(),
                    })),
                }),
            }),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
        assert!(parser.errors.is_empty(), "parser.errors is not empty");
        assert!(
            program.statements.len() == 2,
            "program.statements does not contain 2 statements, got: {}",
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }
    }

    #[test]
    fn infix_expression() {
        let input = "
                a + b / c
                5 < 4 != 3 > 4
                "
        .to_string();

        let expectation = [
            Statement::Expression(ExpressionStatement {
                expression: Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "a".to_string(),
                    })),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Identifier(Identifier {
                            value: "b".to_string(),
                        })),
                        operator: InfixOperator::Slash,
                        right: Box::new(Expression::Identifier(Identifier {
                            value: "c".to_string(),
                        })),
                    })),
                }),
            }),
            Statement::Expression(ExpressionStatement {
                expression: Expression::Infix(InfixExpression {
                    left: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Int(IntegerLiteral { value: 5 })),
                        operator: InfixOperator::LessThan,
                        right: Box::new(Expression::Int(IntegerLiteral { value: 4 })),
                    })),
                    operator: InfixOperator::NotEqual,
                    right: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Int(IntegerLiteral { value: 3 })),
                        operator: InfixOperator::GreaterThan,
                        right: Box::new(Expression::Int(IntegerLiteral { value: 4 })),
                    })),
                }),
            }),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
        assert!(parser.errors.is_empty(), "parser.errors is not empty");
        assert!(
            program.statements.len() == 2,
            "program.statements does not contain 2 statements, got: {}",
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }

        // let pairs = [
        //     ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        //     (
        //         "3 + 4 * 5 == 3 * 1 + 4 * 5",
        //         "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        //     ),
        // ];

        // for (input, expectation) in pairs.iter() {
        //     let input_lexer = Lexer::new(input.to_string());
        //     let mut input_parser = Parser::new(input_lexer);

        //     let expectation_lexer = Lexer::new(expectation.to_string());
        //     let mut expectation_parser = Parser::new(expectation_lexer);

        //     let input_program = input_parser.parse_program();
        //     let expectation_program = expectation_parser.parse_program();

        //     eprintln!("{:?}", input_parser.errors);
        //     eprintln!("{:?}", expectation_parser.errors);
        //     assert!(
        //         input_parser.errors.is_empty(),
        //         "input_parser.errors is not empty"
        //     );
        //     assert!(
        //         expectation_parser.errors.is_empty(),
        //         "expectation_parser.errors is not empty"
        //     );

        //     assert_eq!(input_program.statements, expectation_program.statements);
        // }
    }
}
