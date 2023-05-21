use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::HashMap;

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

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, token: Token) -> bool {
        match token {
            Token::Identifier(_) => matches!(self.cur_token, Token::Identifier(_)),
            Token::Int(_) => matches!(self.cur_token, Token::Int(_)),
            _ => self.cur_token == token,
        }
    }

    fn peek_token_is(&self, token: Token) -> bool {
        match token {
            Token::Identifier(_) => matches!(self.peek_token, Token::Identifier(_)),
            Token::Int(_) => matches!(self.peek_token, Token::Int(_)),
            _ => self.peek_token == token,
        }
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

        Ok(Statement::ExpressionStatement(ExpressionStatement {
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token);

        match prefix {
            Some(prefix) => {
                let mut left_expression = prefix(self)?;

                // while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
                //     let infix = self.infix_parse_fns.get(&self.peek_token);

                //     match infix {
                //         Some(infix) => {
                //             self.next_token();
                //             left_expression = infix(self, left_expression)?;
                //         }
                //         None => return Ok(left_expression),
                //     }
                // }

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

        let expectation = [Statement::ExpressionStatement(ExpressionStatement {
            expression: Expression::Identifier(Identifier {
                value: "foobar".to_string(),
            }),
        })];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
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

        let expectation = [Statement::ExpressionStatement(ExpressionStatement {
            expression: Expression::Int(IntegerLiteral { value: 5 }),
        })];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        eprintln!("{:?}", parser.errors);
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
}
