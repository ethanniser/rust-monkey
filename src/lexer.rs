#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    pub ch: char,
}

use crate::token::Token;

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
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
            '"' => match self.read_string() {
                Some(s) => Token::String(s),
                None => Token::UnterminatedString,
            },
            '\0' => Token::EOF,
            _ => {
                flag = true;
                if self.ch.is_alphabetic() {
                    let identifier = self.read_identifier();
                    Token::check_keyword(&identifier)
                } else if self.ch.is_ascii_digit() {
                    Token::Int(self.read_number())
                } else {
                    self.read_char();
                    Token::UnknownIllegal
                }
            }
        };

        if !flag {
            self.read_char();
        }
        token
    }

    fn read_string(&mut self) -> Option<String> {
        let position = self.position + 1;
        let mut escape_next = false;
        loop {
            self.read_char();
            if escape_next {
                escape_next = false;
            } else if self.ch == '\0' {
                break;
            } else if self.ch == '\\' {
                escape_next = true;
            } else if self.ch == '"' {
                break;
            }
        }
        if self.ch == '\0' {
            return None;
        }

        Some(self.input[position..self.position].iter().collect())
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = *self
                .input
                .get(self.read_position)
                .expect("Failed to read char");
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            *self
                .input
                .get(self.read_position)
                .expect("Failed to read char")
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_alphabetic() {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> isize {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[position..self.position]
            .iter()
            .collect::<String>()
            .parse()
            .expect("Failed to parse number")
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn lexer() {
        let input = r#"
                    let five = 5;
                    let ten = 10;

                    let add = fn(x, y) {
                      x + y;
                    };

                    let result = add(five, ten);
                    !-/*5;
                    5 < 10 > 5;

                    if (5 < 10) {
                      return true;
                    } else {
                      return false;
                    }

                    10 == 10;
                    10 != 9;

                    "foobar"
                    "foo bar"
                    "#
        .to_string();

        let expected_output = [
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LParen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();

            tokens.push(token.clone());

            if token == Token::EOF {
                break;
            }
        }

        for i in 0..tokens.len() {
            assert!(tokens[i] == expected_output[i]);
        }

        assert!(tokens.len() == expected_output.len());
    }

    #[test]
    fn illegal() {
        let input = "&5".to_string();
        let expected_output = [Token::UnknownIllegal, Token::Int(5), Token::EOF];
        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();

            tokens.push(token.clone());

            if token == Token::EOF {
                break;
            }
        }

        for i in 0..tokens.len() {
            assert!(tokens[i] == expected_output[i]);
        }

        assert!(tokens.len() == expected_output.len());
    }

    #[test]
    fn unclosed_string() {
        let input = r#""this is a string"#.to_string();
        let expected_output = [Token::UnterminatedString, Token::EOF];
        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();

            tokens.push(token.clone());

            if token == Token::EOF {
                break;
            }
        }

        for i in 0..tokens.len() {
            assert!(tokens[i] == expected_output[i]);
        }

        assert!(tokens.len() == expected_output.len());
    }

    #[test]
    fn escaped_string() {
        let input = r#""this is a string with \"quotes\"""#.to_string();
        let expected_output = [
            Token::String(r#"this is a string with \"quotes\""#.to_string()),
            Token::EOF,
        ];
        let mut lexer = Lexer::new(input);

        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();

            tokens.push(token.clone());

            if token == Token::EOF {
                break;
            }
        }

        eprintln!("{:?}", tokens);

        for i in 0..tokens.len() {
            assert!(tokens[i] == expected_output[i]);
        }

        assert!(tokens.len() == expected_output.len());
    }
}
