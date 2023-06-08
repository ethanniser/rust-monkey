use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::fmt::Display;

impl Token {
    fn get_precedence(&self) -> Precedence {
        match self {
            Token::Eq | Token::NotEq | Token::DoubleAmpersand | Token::DoublePipe => {
                Precedence::Equals
            }
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk | Token::Percent => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Prefix,
    Infix,
}

#[derive(Debug, PartialEq)]
pub enum ParserExpectation {
    Statement(Statement),
    Expression(Expression),
    Token(Token),
    Operator(Operator),
}

#[derive(Debug, PartialEq)]
pub enum Amount {
    One(ParserExpectation),
    Many(Vec<ParserExpectation>),
}

impl Display for Amount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Amount::One(node) => write!(f, "{:?}", node),
            Amount::Many(nodes) => {
                let mut nodes_str = String::new();
                for node in nodes {
                    nodes_str.push_str(&format!("{:?}, ", node));
                }
                write!(f, "{}", nodes_str)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    FoundOtherThanExpectedToken {
        expected: Amount,
        found: ParserExpectation,
    },
    NoPrefixParseFnFound {
        token: Token,
    },
    NoInfixParseFnFound {
        token: Token,
    },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::FoundOtherThanExpectedToken { expected, found } => {
                write!(f, "Expected {}, found {:?}", expected, found)
            }
            ParserError::NoPrefixParseFnFound { token } => {
                write!(f, "No prefix parse function found for {:?}", token)
            }
            ParserError::NoInfixParseFnFound { token } => {
                write!(f, "No infix parse function found for {:?}", token)
            }
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: Vec::new(),
        };

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
            Err(ParserError::FoundOtherThanExpectedToken {
                expected: Amount::One(ParserExpectation::Token(token)),
                found: ParserExpectation::Token(self.peek_token.clone()),
            })
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.get_precedence()
    }

    fn cur_precedence(&self) -> Precedence {
        self.cur_token.get_precedence()
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
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Identifier(_) if self.peek_token_is(Token::Assign) => {
                self.parse_assign_statement()
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.expect_peek(Token::Identifier("".to_string()))?;

        let name = match self.cur_token.clone() {
            Token::Identifier(ref name) => IdentifierLiteral {
                value: name.clone(),
            },
            other_token => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: Amount::One(ParserExpectation::Token(Token::Identifier(
                        String::new(),
                    ))),
                    found: ParserExpectation::Token(other_token),
                })
            }
        };

        self.expect_peek(Token::Assign)?;

        self.next_token();

        let statement = Statement::Let(LetStatement {
            name,
            value: self.parse_expression(Precedence::Lowest)?,
        });

        self.expect_peek(Token::Semicolon)?;

        Ok(statement)
    }

    fn parse_assign_statement(&mut self) -> Result<Statement, ParserError> {
        let name = match self.cur_token.clone() {
            Token::Identifier(ref name) => IdentifierLiteral {
                value: name.clone(),
            },
            _ => unreachable!("parse_assign_statement can only be called after an indentifier"),
        };

        self.expect_peek(Token::Assign)?;

        self.next_token();

        let statement = Statement::Assign(AssignStatement {
            name,
            value: self.parse_expression(Precedence::Lowest)?,
        });

        self.expect_peek(Token::Semicolon)?;

        Ok(statement)
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token();

        if self.cur_token_is(Token::Semicolon) {
            return Ok(Statement::Return(ReturnStatement { return_value: None }));
        }

        let statement = Statement::Return(ReturnStatement {
            return_value: Some(self.parse_expression(Precedence::Lowest)?),
        });

        self.expect_peek(Token::Semicolon)?;

        Ok(statement)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
            Ok(Statement::Expression(ExpressionStatement::Terminating(
                expression,
            )))
        } else {
            Ok(Statement::Expression(ExpressionStatement::NonTerminating(
                expression,
            )))
        }
    }

    // ! Expects current token to be beginning of expression
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut left = self.parse_prefix_expression()?;
        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            if let Some(operator) = self.parse_infix_operator() {
                self.next_token();
                let infix = self.parse_infix_expression(left, operator)?;
                left = infix;
            } else {
                break;
            }
        }

        Ok(left)
    }

    // ! Expects left side of infix is current token
    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::Int(i) => Ok(Expression::Int(IntegerLiteral { value: i })),
            Token::String(s) => Ok(Expression::String(StringLiteral { value: s })),
            Token::Identifier(s) => Ok(Expression::Identifier(IdentifierLiteral { value: s })),
            Token::True => Ok(Expression::Boolean(BooleanLiteral { value: true })),
            Token::False => Ok(Expression::Boolean(BooleanLiteral { value: false })),
            Token::None => Ok(Expression::NoneLiteral),
            Token::LParen => {
                self.next_token();

                let expression = self.parse_expression(Precedence::Lowest)?;

                if !self.peek_token_is(Token::RParen) {
                    return Err(ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::RParen)),
                        found: ParserExpectation::Token(self.peek_token.clone()),
                    });
                }

                self.next_token();

                Ok(expression)
            }
            Token::Bang | Token::Minus => {
                let operator = match self.cur_token {
                    Token::Bang => PrefixOperator::Bang,
                    Token::Minus => PrefixOperator::Minus,
                    _ => unreachable!(),
                };

                self.next_token();

                let right = self.parse_expression(Precedence::Prefix)?;

                Ok(Expression::Prefix(PrefixExpression {
                    operator,
                    right: Box::new(right),
                }))
            }
            Token::If => Ok(Expression::If(self.parse_if_expression()?)),
            Token::Function => Ok(Expression::Function(self.parse_function_literal()?)),
            Token::LBracket => {
                let elements = self.parse_expression_list(Token::RBracket)?;
                Ok(Expression::Array(ArrayLiteral { elements }))
            }
            Token::LBrace => {
                self.next_token();
                Ok(Expression::Block(self.parse_block_expression()?))
            }
            _ => Err(ParserError::NoPrefixParseFnFound {
                token: self.cur_token.clone(),
            }),
        }
    }

    // ! Expects peek token to be operator
    fn parse_infix_operator(&mut self) -> Option<InfixOperator> {
        match self.peek_token.clone() {
            Token::Plus => Some(InfixOperator::Plus),
            Token::Minus => Some(InfixOperator::Minus),
            Token::Asterisk => Some(InfixOperator::Asterisk),
            Token::Slash => Some(InfixOperator::Slash),
            Token::DoubleSlash => Some(InfixOperator::DoubleSlash),
            Token::Eq => Some(InfixOperator::Equal),
            Token::NotEq => Some(InfixOperator::NotEqual),
            Token::Lt => Some(InfixOperator::LessThan),
            Token::Gt => Some(InfixOperator::GreaterThan),
            Token::Percent => Some(InfixOperator::Percent),
            Token::DoubleAmpersand => Some(InfixOperator::DoubleAmpersand),
            Token::DoublePipe => Some(InfixOperator::DoublePipe),
            Token::LParen => Some(InfixOperator::LParen),
            Token::LBracket => Some(InfixOperator::LBracket),
            _ => None,
        }
    }

    // ! Expects peek token to be start of right side of infix expression
    // ! Current token is operator
    fn parse_infix_expression(
        &mut self,
        left: Expression,
        operator: InfixOperator,
    ) -> Result<Expression, ParserError> {
        match operator {
            InfixOperator::LParen => {
                let arguments = self.parse_expression_list(Token::RParen)?;
                return Ok(Expression::Call(CallExpression {
                    left: Box::new(left),
                    arguments,
                }));
            }
            InfixOperator::LBracket => {
                self.next_token();
                let right = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(Token::RBracket)?;
                return Ok(Expression::Index(IndexExpression {
                    left: Box::new(left),
                    index: Box::new(right),
                }));
            }
            _ => {}
        }

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_if_expression(&mut self) -> Result<IfExpression, ParserError> {
        self.expect_peek(Token::LParen)?;

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen)?;
        self.expect_peek(Token::LBrace)?;

        let consequence = self.parse_block_expression()?;

        if self.peek_token_is(Token::Else) {
            self.next_token();
            self.expect_peek(Token::LBrace)?;
            let alternative = self.parse_block_expression()?;

            Ok(IfExpression {
                condition: Box::new(condition),
                consequence,
                alternative: Some(alternative),
            })
        } else {
            Ok(IfExpression {
                condition: Box::new(condition),
                consequence,
                alternative: None,
            })
        }
    }

    fn parse_block_expression(&mut self) -> Result<BlockExpression, ParserError> {
        let mut statements = Vec::new();

        self.next_token();

        loop {
            if self.cur_token_is(Token::RBrace) {
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }

        Ok(BlockExpression { statements })
    }

    fn parse_function_literal(&mut self) -> Result<FunctionLiteral, ParserError> {
        self.expect_peek(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::LBrace)?;

        Ok(FunctionLiteral {
            parameters,
            body: self.parse_block_expression()?,
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentifierLiteral>, ParserError> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        let identifier = match self.cur_token.clone() {
            Token::Identifier(identifier) => identifier,
            other_token => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: Amount::One(ParserExpectation::Token(Token::Identifier(
                        String::new(),
                    ))),
                    found: ParserExpectation::Token(other_token),
                })
            }
        };

        identifiers.push(IdentifierLiteral { value: identifier });

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            let identifier = match self.cur_token.clone() {
                Token::Identifier(identifier) => identifier,
                other_token => {
                    return Err(ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: ParserExpectation::Token(other_token),
                    })
                }
            };

            identifiers.push(IdentifierLiteral { value: identifier });
        }

        self.expect_peek(Token::RParen)?;

        Ok(identifiers)
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>, ParserError> {
        let mut arguments = Vec::new();

        if self.peek_token_is(end.clone()) {
            self.next_token();
            return Ok(arguments);
        }

        self.next_token();

        let argument = self.parse_expression(Precedence::Lowest)?;

        arguments.push(argument);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            let argument = self.parse_expression(Precedence::Lowest)?;

            arguments.push(argument);
        }

        self.expect_peek(end)?;

        Ok(arguments)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_vs_expectation(input: &str, expectation: Program) {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            eprintln!("{:?}", parser.errors);
        }
        assert!(parser.errors.is_empty(), "parser.errors is not empty");

        let num_statements = program.statements.len();

        assert!(
            program.statements.len() == num_statements,
            "program.statements does not contain {} statements, got: {}",
            num_statements,
            program.statements.len()
        );

        for (index, (statement, expectation)) in program
            .statements
            .iter()
            .zip(expectation.statements.iter())
            .enumerate()
        {
            assert_eq!(statement, expectation, "test[{index}] - statement");
        }
    }

    fn test_vs_code(pairs: Vec<(&str, &str)>) {
        for (input, expectation) in pairs.iter() {
            let input_lexer = Lexer::new(input.to_string());
            let mut input_parser = Parser::new(input_lexer);

            let expectation_lexer = Lexer::new(expectation.to_string());
            let mut expectation_parser = Parser::new(expectation_lexer);

            let input_program = input_parser.parse_program();
            let expectation_program = expectation_parser.parse_program();

            if !input_parser.errors.is_empty() {
                eprintln!("{:?}", input_parser.errors);
            }
            if !expectation_parser.errors.is_empty() {
                eprintln!("{:?}", expectation_parser.errors);
            }
            assert!(
                input_parser.errors.is_empty(),
                "input_parser.errors is not empty"
            );
            assert!(
                expectation_parser.errors.is_empty(),
                "expectation_parser.errors is not empty"
            );

            assert_eq!(input_program.statements, expectation_program.statements);
        }
    }

    #[test]
    fn let_statement() {
        let input = "
                let x = 5;
                let y = 10;
                let foobar = 838383;
                ";

        let expectation = Program {
            statements: vec![
                Statement::Let(LetStatement {
                    name: IdentifierLiteral {
                        value: "x".to_string(),
                    },
                    value: Expression::Int(IntegerLiteral { value: 5 }),
                }),
                Statement::Let(LetStatement {
                    name: IdentifierLiteral {
                        value: "y".to_string(),
                    },
                    value: Expression::Int(IntegerLiteral { value: 10 }),
                }),
                Statement::Let(LetStatement {
                    name: IdentifierLiteral {
                        value: "foobar".to_string(),
                    },
                    value: Expression::Int(IntegerLiteral { value: 838383 }),
                }),
            ],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn return_statement() {
        let input = "
            return 5;
            return 10;
            return;
            ";

        let expectation = Program {
            statements: vec![
                Statement::Return(ReturnStatement {
                    return_value: Some(Expression::Int(IntegerLiteral { value: 5 })),
                }),
                Statement::Return(ReturnStatement {
                    return_value: Some(Expression::Int(IntegerLiteral { value: 10 })),
                }),
                Statement::Return(ReturnStatement { return_value: None }),
            ],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn indetifier_expression() {
        let input = "
            foobar;
            ";

        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::Terminating(
                Expression::Identifier(IdentifierLiteral {
                    value: "foobar".to_string(),
                }),
            ))],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn interger_literal() {
        let input = "
            5;
            ";

        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::Terminating(
                Expression::Int(IntegerLiteral { value: 5 }),
            ))],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn prefix_expression() {
        let input = "
            -15;
            !foobar;
            ";

        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::Terminating(Expression::Prefix(
                    PrefixExpression {
                        operator: PrefixOperator::Minus,
                        right: Box::new(Expression::Int(IntegerLiteral { value: 15 })),
                    },
                ))),
                Statement::Expression(ExpressionStatement::Terminating(Expression::Prefix(
                    PrefixExpression {
                        operator: PrefixOperator::Bang,
                        right: Box::new(Expression::Identifier(IdentifierLiteral {
                            value: "foobar".to_string(),
                        })),
                    },
                ))),
            ],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn infix_expression() {
        let input = "
            a + b / c;
            5 < 4 != 3 > 4;
            3 % 4;
            ";

        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::Terminating(Expression::Infix(
                    InfixExpression {
                        left: Box::new(Expression::Identifier(IdentifierLiteral {
                            value: "a".to_string(),
                        })),
                        operator: InfixOperator::Plus,
                        right: Box::new(Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Identifier(IdentifierLiteral {
                                value: "b".to_string(),
                            })),
                            operator: InfixOperator::Slash,
                            right: Box::new(Expression::Identifier(IdentifierLiteral {
                                value: "c".to_string(),
                            })),
                        })),
                    },
                ))),
                Statement::Expression(ExpressionStatement::Terminating(Expression::Infix(
                    InfixExpression {
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
                    },
                ))),
                Statement::Expression(ExpressionStatement::Terminating(Expression::Infix(
                    InfixExpression {
                        left: Box::new(Expression::Int(IntegerLiteral { value: 3 })),
                        operator: InfixOperator::Percent,
                        right: Box::new(Expression::Int(IntegerLiteral { value: 4 })),
                    },
                ))),
            ],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn parentheses() {
        let pairs = vec![
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        test_vs_code(pairs);
    }

    #[test]
    fn boolean_literal() {
        let input = "
            true;
            let x = false;
            ";

        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::Terminating(Expression::Boolean(
                    BooleanLiteral { value: true },
                ))),
                Statement::Let(LetStatement {
                    name: IdentifierLiteral {
                        value: "x".to_string(),
                    },
                    value: Expression::Boolean(BooleanLiteral { value: false }),
                }),
            ],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn if_expression() {
        let input = "
            if (x < y) { x };
            ";

        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::Terminating(
                Expression::If(IfExpression {
                    condition: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Identifier(IdentifierLiteral {
                            value: "x".to_string(),
                        })),
                        operator: InfixOperator::LessThan,
                        right: Box::new(Expression::Identifier(IdentifierLiteral {
                            value: "y".to_string(),
                        })),
                    })),
                    consequence: BlockExpression {
                        statements: vec![Statement::Expression(
                            ExpressionStatement::NonTerminating(Expression::Identifier(
                                IdentifierLiteral {
                                    value: "x".to_string(),
                                },
                            )),
                        )],
                    },
                    alternative: None,
                }),
            ))],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn if_else_expression() {
        let input = "
            if (x < y) { x } else { y };
            ";

        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::Terminating(
                Expression::If(IfExpression {
                    condition: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Identifier(IdentifierLiteral {
                            value: "x".to_string(),
                        })),
                        operator: InfixOperator::LessThan,
                        right: Box::new(Expression::Identifier(IdentifierLiteral {
                            value: "y".to_string(),
                        })),
                    })),
                    consequence: BlockExpression {
                        statements: vec![Statement::Expression(
                            ExpressionStatement::NonTerminating(Expression::Identifier(
                                IdentifierLiteral {
                                    value: "x".to_string(),
                                },
                            )),
                        )],
                    },
                    alternative: Some(BlockExpression {
                        statements: vec![Statement::Expression(
                            ExpressionStatement::NonTerminating(Expression::Identifier(
                                IdentifierLiteral {
                                    value: "y".to_string(),
                                },
                            )),
                        )],
                    }),
                }),
            ))],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn function_literal() {
        let input = "
            fn() {};
            fn(x, y) { x + y; };
            ";

        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::Terminating(Expression::Function(
                    FunctionLiteral {
                        parameters: vec![],
                        body: BlockExpression { statements: vec![] },
                    },
                ))),
                Statement::Expression(ExpressionStatement::Terminating(Expression::Function(
                    FunctionLiteral {
                        parameters: vec![
                            IdentifierLiteral {
                                value: "x".to_string(),
                            },
                            IdentifierLiteral {
                                value: "y".to_string(),
                            },
                        ],
                        body: BlockExpression {
                            statements: vec![Statement::Expression(
                                ExpressionStatement::Terminating(Expression::Infix(
                                    InfixExpression {
                                        left: Box::new(Expression::Identifier(IdentifierLiteral {
                                            value: "x".to_string(),
                                        })),
                                        operator: InfixOperator::Plus,
                                        right: Box::new(Expression::Identifier(
                                            IdentifierLiteral {
                                                value: "y".to_string(),
                                            },
                                        )),
                                    },
                                )),
                            )],
                        },
                    },
                ))),
            ],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::Terminating(
                Expression::Call(CallExpression {
                    left: Box::new(Expression::Identifier(IdentifierLiteral {
                        value: "add".to_string(),
                    })),
                    arguments: vec![
                        Expression::Int(IntegerLiteral { value: 1 }),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(IntegerLiteral { value: 2 })),
                            operator: InfixOperator::Asterisk,
                            right: Box::new(Expression::Int(IntegerLiteral { value: 3 })),
                        }),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(IntegerLiteral { value: 4 })),
                            operator: InfixOperator::Plus,
                            right: Box::new(Expression::Int(IntegerLiteral { value: 5 })),
                        }),
                    ],
                }),
            ))],
        };

        test_vs_expectation(input, expectation);
    }

    #[test]
    fn terminating_vs_non_terminating() {
        let input = "
            10
            10;
            fn () { 10 };
            fn () { 10; }
        ";
        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::NonTerminating(Expression::Int(
                    IntegerLiteral { value: 10 },
                ))),
                Statement::Expression(ExpressionStatement::Terminating(Expression::Int(
                    IntegerLiteral { value: 10 },
                ))),
                Statement::Expression(ExpressionStatement::Terminating(Expression::Function(
                    FunctionLiteral {
                        parameters: vec![],
                        body: BlockExpression {
                            statements: vec![Statement::Expression(
                                ExpressionStatement::NonTerminating(Expression::Int(
                                    IntegerLiteral { value: 10 },
                                )),
                            )],
                        },
                    },
                ))),
                Statement::Expression(ExpressionStatement::NonTerminating(Expression::Function(
                    FunctionLiteral {
                        parameters: vec![],
                        body: BlockExpression {
                            statements: vec![Statement::Expression(
                                ExpressionStatement::Terminating(Expression::Int(IntegerLiteral {
                                    value: 10,
                                })),
                            )],
                        },
                    },
                ))),
            ],
        };
        test_vs_expectation(input, expectation);
    }

    #[test]
    fn all_operator_precedence() {
        let pairs = vec![
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        test_vs_code(pairs);
    }

    #[test]
    fn none_literal() {
        let input = "
        none;
        let x = none;
        ";
        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::Terminating(Expression::NoneLiteral)),
                Statement::Let(LetStatement {
                    name: IdentifierLiteral {
                        value: "x".to_string(),
                    },
                    value: Expression::NoneLiteral,
                }),
            ],
        };
        test_vs_expectation(input, expectation);
    }

    #[test]
    fn string_literal() {
        let input = "
        \"hello world\";
        let x = \"hello world\";
        ";
        let expectation = Program {
            statements: vec![
                Statement::Expression(ExpressionStatement::Terminating(Expression::String(
                    StringLiteral {
                        value: "hello world".to_string(),
                    },
                ))),
                Statement::Let(LetStatement {
                    name: IdentifierLiteral {
                        value: "x".to_string(),
                    },
                    value: Expression::String(StringLiteral {
                        value: "hello world".to_string(),
                    }),
                }),
            ],
        };
        test_vs_expectation(input, expectation);
    }

    #[test]
    fn assign_statement() {
        let input = "
        x = 5;
        ";
        let expectation = Program {
            statements: vec![Statement::Assign(AssignStatement {
                name: IdentifierLiteral {
                    value: "x".to_string(),
                },
                value: Expression::Int(IntegerLiteral { value: 5 }),
            })],
        };
        test_vs_expectation(input, expectation);
    }

    #[test]
    fn array_literal() {
        let input = "
        [1, 2 * 2, 3 + 3]
        ";
        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::NonTerminating(
                Expression::Array(ArrayLiteral {
                    elements: vec![
                        Expression::Int(IntegerLiteral { value: 1 }),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(IntegerLiteral { value: 2 })),
                            operator: InfixOperator::Asterisk,
                            right: Box::new(Expression::Int(IntegerLiteral { value: 2 })),
                        }),
                        Expression::Infix(InfixExpression {
                            left: Box::new(Expression::Int(IntegerLiteral { value: 3 })),
                            operator: InfixOperator::Plus,
                            right: Box::new(Expression::Int(IntegerLiteral { value: 3 })),
                        }),
                    ],
                }),
            ))],
        };
        test_vs_expectation(input, expectation);
    }

    #[test]
    fn index_expression() {
        let input = "
        myArray[1 + 1]
        ";
        let expectation = Program {
            statements: vec![Statement::Expression(ExpressionStatement::NonTerminating(
                Expression::Index(IndexExpression {
                    left: Box::new(Expression::Identifier(IdentifierLiteral {
                        value: "myArray".to_string(),
                    })),
                    index: Box::new(Expression::Infix(InfixExpression {
                        left: Box::new(Expression::Int(IntegerLiteral { value: 1 })),
                        operator: InfixOperator::Plus,
                        right: Box::new(Expression::Int(IntegerLiteral { value: 1 })),
                    })),
                }),
            ))],
        };
        test_vs_expectation(input, expectation);
    }

    mod errors {

        use super::*;

        fn test_vs_error(pairs: Vec<(&str, Vec<ParserError>)>) {
            for (input, expectation) in pairs {
                let lexer = Lexer::new(input.to_string());
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();

                if parser.errors.len() < expectation.len() {
                    panic!(
                        "Expected {} errors, got {}. Program: {:?}",
                        expectation.len(),
                        parser.errors.len(),
                        program.statements
                    );
                }

                for i in 0..expectation.len() {
                    assert_eq!(parser.errors[i], expectation[i]);
                }
            }
        }

        #[test]
        fn let_statement() {
            let pairs = vec![
                (
                    "let x = 5",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::Semicolon)),
                        found: ParserExpectation::Token(Token::EOF),
                    }],
                ),
                (
                    "let x 5;",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::Assign)),
                        found: ParserExpectation::Token(Token::Int(5)),
                    }],
                ),
                (
                    "let 5 = 5;",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: ParserExpectation::Token(Token::Int(5)),
                    }],
                ),
            ];

            test_vs_error(pairs);
        }

        #[test]
        fn return_statement() {
            let pairs = vec![(
                "return 5",
                vec![ParserError::FoundOtherThanExpectedToken {
                    expected: Amount::One(ParserExpectation::Token(Token::Semicolon)),
                    found: ParserExpectation::Token(Token::EOF),
                }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn no_infix_found() {
            let pairs = vec![(
                "5 @ 5",
                vec![ParserError::NoPrefixParseFnFound {
                    token: Token::Illegal,
                }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn no_prefix_found() {
            let pairs = vec![(
                "&5",
                vec![ParserError::NoPrefixParseFnFound {
                    token: Token::Illegal,
                }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn non_matching_parens() {
            let pairs = vec![
                (
                    "((5 + 5);",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::RParen)),
                        found: ParserExpectation::Token(Token::Semicolon),
                    }],
                ),
                (
                    "5 + 5);",
                    vec![ParserError::NoPrefixParseFnFound {
                        token: Token::RParen,
                    }],
                ),
                (
                    "((5 + 5)",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::RParen)),
                        found: ParserExpectation::Token(Token::EOF),
                    }],
                ),
            ];

            test_vs_error(pairs);
        }

        #[test]
        fn block_expression() {
            let pairs = vec![(
                "{ 5",
                vec![ParserError::NoPrefixParseFnFound { token: Token::EOF }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn if_else_expression() {
            let pairs = vec![(
                "if (true) { return; } else",
                vec![ParserError::FoundOtherThanExpectedToken {
                    expected: Amount::One(ParserExpectation::Token(Token::LBrace)),
                    found: ParserExpectation::Token(Token::EOF),
                }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn function_literal() {
            let pairs = vec![
                (
                    "fn()",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::LBrace)),
                        found: ParserExpectation::Token(Token::EOF),
                    }],
                ),
                (
                    "fn(,x) { return; }",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: ParserExpectation::Token(Token::Comma),
                    }],
                ),
                (
                    "fn(x,) { return x; }",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: ParserExpectation::Token(Token::RParen),
                    }],
                ),
                (
                    "fn(x, y { return x+y; }",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::RParen)),
                        found: ParserExpectation::Token(Token::LBrace),
                    }],
                ),
            ];

            test_vs_error(pairs);
        }

        #[test]
        fn call_expression() {
            let pairs = vec![
                (
                    "add(1, 2, 3",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::RParen)),
                        found: ParserExpectation::Token(Token::EOF),
                    }],
                ),
                (
                    "add(1, 2,)",
                    vec![ParserError::NoPrefixParseFnFound {
                        token: Token::RParen,
                    }],
                ),
            ];

            test_vs_error(pairs);
        }

        #[test]
        fn array_literal() {
            let pairs = vec![
                (
                    "[1, 2, 3",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: Amount::One(ParserExpectation::Token(Token::RBracket)),
                        found: ParserExpectation::Token(Token::EOF),
                    }],
                ),
                (
                    "[1, 2,]",
                    vec![ParserError::NoPrefixParseFnFound {
                        token: Token::RBracket,
                    }],
                ),
            ];

            test_vs_error(pairs);
        }

        #[test]
        fn unterminated_index_expression() {
            let pairs = vec![(
                "foo[",
                vec![ParserError::NoPrefixParseFnFound { token: Token::EOF }],
            )];

            test_vs_error(pairs);
        }
    }
}
