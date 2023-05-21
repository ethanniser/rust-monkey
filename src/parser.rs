use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

pub struct ParserError {
    pub message: String,
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
            other_token => Err(ParserError {
                message: format!("unimplemented parser for token {other_token:?}",),
            }),
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
