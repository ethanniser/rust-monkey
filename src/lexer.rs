#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    pub ch: char,
}

use crate::token::Token;

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };

        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let mut flag = false;

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '\0' => Token::Eof,
            _ => {
                flag = true;
                if self.ch.is_alphabetic() {
                    let identifier = self.read_identifier();
                    Token::check_keyword(&identifier)
                } else if self.ch.is_digit(10) {
                    Token::Int(self.read_number())
                } else {
                    Token::Illegal
                }
            }
        };

        if !flag {
            self.read_char();
        }
        token
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.read_position)
                .expect("Failed to read char");
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input
                .chars()
                .nth(self.read_position)
                .expect("Failed to read char")
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_alphabetic() {
            self.read_char();
        }
        let x = self.input[position..self.position].to_string();
        x
    }

    fn read_number(&mut self) -> isize {
        let position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        self.input[position..self.position]
            .parse()
            .expect("Failed to parse number")
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

use std::iter::Iterator;

pub struct LexerIterator {
    lexer: Box<Lexer>,
    has_seen_eof: bool,
}

impl IntoIterator for Lexer {
    type Item = Token;
    type IntoIter = LexerIterator;

    fn into_iter(self) -> Self::IntoIter {
        LexerIterator {
            lexer: Box::new(self),
            has_seen_eof: false,
        }
    }
}

impl Iterator for LexerIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.has_seen_eof {
            return None;
        }

        let token = self.lexer.next_token();
        match token {
            Token::Eof => {
                self.has_seen_eof = true; // Remember that we've seen EOF
            }
            _ => {}
        }
        Some(token)
    }
}
