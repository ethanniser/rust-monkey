use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::HashMap;
use std::fmt::Display;

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
            Token::LParen => Precedence::Call,
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
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, ParserError>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, ParserError>;

#[derive(Debug, PartialEq)]
pub enum Operator {
    Prefix,
    Infix,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Token(Token),
    Operator(Operator),
}

#[derive(Debug, PartialEq)]
pub enum NodeExpectation {
    One(Node),
    Many(Vec<Node>),
}

impl Display for NodeExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeExpectation::One(node) => write!(f, "{:?}", node),
            NodeExpectation::Many(nodes) => {
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
        expected: NodeExpectation,
        found: Node,
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
    pub prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    pub infix_parse_fns: HashMap<Token, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Token::UnknownIllegal,
            peek_token: Token::UnknownIllegal,
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(
            Token::Identifier("".to_string()),
            Parser::parse_identifier_literal,
        );
        parser.register_prefix(Token::Int(0), Parser::parse_interger_literal);
        parser.register_prefix(Token::Bang, Parser::parse_prefix_expression);
        parser.register_prefix(Token::Minus, Parser::parse_prefix_expression);
        parser.register_prefix(Token::True, Parser::parse_boolean_literal);
        parser.register_prefix(Token::False, Parser::parse_boolean_literal);
        parser.register_prefix(Token::LParen, Parser::parse_grouped_expression);
        parser.register_prefix(Token::If, Parser::parse_if_expression);
        parser.register_prefix(Token::Function, Parser::parse_function_literal);
        parser.register_prefix(Token::LBrace, Parser::parse_block_expression);
        parser.register_prefix(Token::None, Parser::parse_none_literal);
        parser.register_prefix(Token::String("".to_string()), Parser::parse_string_literal);
        parser.register_prefix(Token::UnterminatedString, Parser::parse_string_literal);

        parser.register_infix(Token::Plus, Parser::parse_infix_expression);
        parser.register_infix(Token::Minus, Parser::parse_infix_expression);
        parser.register_infix(Token::Asterisk, Parser::parse_infix_expression);
        parser.register_infix(Token::Slash, Parser::parse_infix_expression);
        parser.register_infix(Token::Eq, Parser::parse_infix_expression);
        parser.register_infix(Token::NotEq, Parser::parse_infix_expression);
        parser.register_infix(Token::Lt, Parser::parse_infix_expression);
        parser.register_infix(Token::Gt, Parser::parse_infix_expression);
        parser.register_infix(Token::LParen, Parser::parse_call_expression);

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
                expected: NodeExpectation::One(Node::Token(token)),
                found: Node::Token(self.peek_token.clone()),
            })
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.get_precedence()
    }

    fn cur_precedence(&self) -> Precedence {
        self.cur_token.get_precedence()
    }

    pub fn register_prefix(&mut self, token: Token, function: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, function);
    }

    pub fn register_infix(&mut self, token: Token, function: InfixParseFn) {
        self.infix_parse_fns.insert(token, function);
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
            Token::Identifier(ref name) => IdentifierLiteral {
                value: name.clone(),
            },
            other_token => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::One(Node::Token(Token::Identifier(String::new()))),
                    found: Node::Token(other_token),
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
            None => Err(ParserError::NoPrefixParseFnFound {
                token: self.cur_token.clone(),
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let operator = match self.cur_token.clone() {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
            other_token => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::One(Node::Operator(Operator::Prefix)),
                    found: Node::Token(other_token),
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
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::One(Node::Operator(Operator::Infix)),
                    found: Node::Token(other_token),
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

    fn parse_identifier_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::Identifier(ref identifier) => Ok(Expression::Identifier(IdentifierLiteral {
                value: identifier.clone(),
            })),
            _ => unreachable!("parse_identifier_literal called only ever be invoked when current token is Identifier"),
        }
    }

    fn parse_interger_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::Int(value) => Ok(Expression::Int(IntegerLiteral { value })),
            _ => unreachable!(
                "parse_interger_literal called only ever be invoked when current token is Int"
            ),
        }
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::True => Ok(Expression::Boolean(BooleanLiteral { value: true })),
            Token::False => Ok(Expression::Boolean(BooleanLiteral { value: false })),
            _ => unreachable!("parse_boolean_literal called only ever be invoked when current token is True or False"),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::UnterminatedString => Err(ParserError::FoundOtherThanExpectedToken {
                expected: NodeExpectation::One(Node::Token(Token::String(String::new()))),
                found: Node::Token(self.cur_token.clone()),
            }),
            Token::String(value) => Ok(Expression::String(StringLiteral { value })),
            _ => unreachable!(
                "parse_string_literal called only ever be invoked when current token is String"
            ),
        }
    }

    fn parse_none_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token.clone() {
            Token::None => Ok(Expression::NoneLiteral),
            _ => unreachable!(
                "parse_none_literal called only ever be invoked when current token is None"
            ),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen)?;

        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::LParen)?;

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen)?;
        self.expect_peek(Token::LBrace)?;

        let consequence = match self.parse_block_expression()? {
            Expression::Block(body) => Ok(body),
            other_expression => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::One(Node::Expression(Expression::Block(
                        BlockExpression {
                            statements: Vec::new(),
                        },
                    ))),
                    found: Node::Expression(other_expression),
                })
            }
        }?;

        if self.peek_token_is(Token::Else) {
            self.next_token();
            self.expect_peek(Token::LBrace)?;
            let alternative = match self.parse_block_expression()? {
                Expression::Block(body) => Ok(Some(body)),
                other_expression => {
                    return Err(ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::One(Node::Expression(Expression::Block(
                            BlockExpression {
                                statements: Vec::new(),
                            },
                        ))),
                        found: Node::Expression(other_expression),
                    })
                }
            }?;

            Ok(Expression::If(IfExpression {
                condition: Box::new(condition),
                consequence,
                alternative,
            }))
        } else {
            Ok(Expression::If(IfExpression {
                condition: Box::new(condition),
                consequence,
                alternative: None,
            }))
        }
    }

    fn parse_block_expression(&mut self) -> Result<Expression, ParserError> {
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

        Ok(Expression::Block(BlockExpression { statements }))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::LBrace)?;

        match self.parse_block_expression()? {
            Expression::Block(body) => {
                Ok(Expression::Function(FunctionLiteral { parameters, body }))
            }
            other_expression => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::One(Node::Expression(Expression::Block(
                        BlockExpression {
                            statements: Vec::new(),
                        },
                    ))),
                    found: Node::Expression(other_expression),
                })
            }
        }
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
                    expected: NodeExpectation::One(Node::Token(Token::Identifier(String::new()))),
                    found: Node::Token(other_token),
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
                        expected: NodeExpectation::One(Node::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: Node::Token(other_token),
                    })
                }
            };

            identifiers.push(IdentifierLiteral { value: identifier });
        }

        self.expect_peek(Token::RParen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let function = match function {
            Expression::Identifier(identifier_literal) => {
                CallableExpression::Identifier(identifier_literal)
            }
            Expression::Function(function_literal) => {
                CallableExpression::Function(function_literal)
            }
            other_expression => {
                return Err(ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::Many(vec![
                        Node::Token(Token::Identifier(String::new())),
                        Node::Token(Token::Function),
                    ]),
                    found: Node::Expression(other_expression),
                });
            }
        };
        let arguments = self.parse_call_arguments()?;

        Ok(Expression::Call(CallExpression {
            function,
            arguments,
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut arguments = Vec::new();

        if self.peek_token_is(Token::RParen) {
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

        self.expect_peek(Token::RParen)?;

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

        eprintln!("{:?}", parser.errors);
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

            eprintln!("{:?}", input_parser.errors);
            eprintln!("{:?}", expectation_parser.errors);
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
                    function: CallableExpression::Identifier(IdentifierLiteral {
                        value: "add".to_string(),
                    }),
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
                        expected: NodeExpectation::One(Node::Token(Token::Semicolon)),
                        found: Node::Token(Token::EOF),
                    }],
                ),
                (
                    "let x 5;",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::One(Node::Token(Token::Assign)),
                        found: Node::Token(Token::Int(5)),
                    }],
                ),
                (
                    "let 5 = 5;",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::One(Node::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: Node::Token(Token::Int(5)),
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
                    expected: NodeExpectation::One(Node::Token(Token::Semicolon)),
                    found: Node::Token(Token::EOF),
                }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn no_infix_found() {
            let pairs = vec![(
                "5 @ 5",
                vec![ParserError::NoPrefixParseFnFound {
                    token: Token::UnknownIllegal,
                }],
            )];

            test_vs_error(pairs);
        }

        #[test]
        fn no_prefix_found() {
            let pairs = vec![(
                "&5",
                vec![ParserError::NoPrefixParseFnFound {
                    token: Token::UnknownIllegal,
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
                        expected: NodeExpectation::One(Node::Token(Token::RParen)),
                        found: Node::Token(Token::Semicolon),
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
                        expected: NodeExpectation::One(Node::Token(Token::RParen)),
                        found: Node::Token(Token::EOF),
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
                    expected: NodeExpectation::One(Node::Token(Token::LBrace)),
                    found: Node::Token(Token::EOF),
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
                        expected: NodeExpectation::One(Node::Token(Token::LBrace)),
                        found: Node::Token(Token::EOF),
                    }],
                ),
                (
                    "fn(,x) { return; }",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::One(Node::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: Node::Token(Token::Comma),
                    }],
                ),
                (
                    "fn(x,) { return x; }",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::One(Node::Token(Token::Identifier(
                            String::new(),
                        ))),
                        found: Node::Token(Token::RParen),
                    }],
                ),
                (
                    "fn(x, y { return x+y; }",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::One(Node::Token(Token::RParen)),
                        found: Node::Token(Token::LBrace),
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
                        expected: NodeExpectation::One(Node::Token(Token::RParen)),
                        found: Node::Token(Token::EOF),
                    }],
                ),
                (
                    "add(1, 2,)",
                    vec![ParserError::NoPrefixParseFnFound {
                        token: Token::RParen,
                    }],
                ),
                (
                    "true()",
                    vec![ParserError::FoundOtherThanExpectedToken {
                        expected: NodeExpectation::Many(vec![
                            Node::Token(Token::Identifier(String::new())),
                            Node::Token(Token::Function),
                        ]),
                        found: Node::Expression(Expression::Boolean(BooleanLiteral {
                            value: true,
                        })),
                    }],
                ),
            ];

            test_vs_error(pairs);
        }

        #[test]
        fn unterminated_string() {
            let pairs = vec![(
                r#""hello"#,
                vec![ParserError::FoundOtherThanExpectedToken {
                    expected: NodeExpectation::One(Node::Token(Token::String(String::new()))),
                    found: Node::Token(Token::UnterminatedString),
                }],
            )];

            test_vs_error(pairs);
        }
    }
}
