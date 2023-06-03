#[derive(Debug, Clone)]
pub enum Token {
    UnknownIllegal,
    UnterminatedString,
    EOF,
    Identifier(String),
    Int(isize),
    String(String),
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    None,
}

impl PartialEq for Token {
    #[allow(clippy::match_like_matches_macro)]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::UnknownIllegal, Token::UnknownIllegal) => true,
            (Token::UnterminatedString, Token::UnterminatedString) => true,
            (Token::EOF, Token::EOF) => true,
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            (Token::String(_), Token::String(_)) => true,
            (Token::Assign, Token::Assign) => true,
            (Token::Plus, Token::Plus) => true,
            (Token::Minus, Token::Minus) => true,
            (Token::Bang, Token::Bang) => true,
            (Token::Asterisk, Token::Asterisk) => true,
            (Token::Slash, Token::Slash) => true,
            (Token::Lt, Token::Lt) => true,
            (Token::Gt, Token::Gt) => true,
            (Token::Eq, Token::Eq) => true,
            (Token::NotEq, Token::NotEq) => true,
            (Token::Comma, Token::Comma) => true,
            (Token::Semicolon, Token::Semicolon) => true,
            (Token::LParen, Token::LParen) => true,
            (Token::RParen, Token::RParen) => true,
            (Token::LBrace, Token::LBrace) => true,
            (Token::RBrace, Token::RBrace) => true,
            (Token::Function, Token::Function) => true,
            (Token::Let, Token::Let) => true,
            (Token::True, Token::True) => true,
            (Token::False, Token::False) => true,
            (Token::If, Token::If) => true,
            (Token::Else, Token::Else) => true,
            (Token::Return, Token::Return) => true,
            (Token::None, Token::None) => true,
            (Token::LBracket, Token::LBracket) => true,
            (Token::RBracket, Token::RBracket) => true,
            _ => false,
        }
    }
}

impl Eq for Token {}

use std::hash::{Hash, Hasher};

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Token::UnknownIllegal => {
                1.hash(state);
            }
            Token::EOF => {
                2.hash(state);
            }
            Token::Identifier(_) => {
                3.hash(state);
            }
            Token::Int(_) => {
                4.hash(state);
            }
            Token::Assign => {
                5.hash(state);
            }
            Token::Plus => {
                6.hash(state);
            }
            Token::Minus => {
                7.hash(state);
            }
            Token::Bang => {
                8.hash(state);
            }
            Token::Asterisk => {
                9.hash(state);
            }
            Token::Slash => {
                10.hash(state);
            }
            Token::Lt => {
                11.hash(state);
            }
            Token::Gt => {
                12.hash(state);
            }
            Token::Eq => {
                13.hash(state);
            }
            Token::NotEq => {
                14.hash(state);
            }
            Token::Comma => {
                15.hash(state);
            }
            Token::Semicolon => {
                16.hash(state);
            }
            Token::LParen => {
                17.hash(state);
            }
            Token::RParen => {
                18.hash(state);
            }
            Token::LBrace => {
                19.hash(state);
            }
            Token::RBrace => {
                20.hash(state);
            }
            Token::Function => {
                21.hash(state);
            }
            Token::Let => {
                22.hash(state);
            }
            Token::True => {
                23.hash(state);
            }
            Token::False => {
                24.hash(state);
            }
            Token::If => {
                25.hash(state);
            }
            Token::Else => {
                26.hash(state);
            }
            Token::Return => {
                27.hash(state);
            }
            Token::None => {
                28.hash(state);
            }
            Token::String(_) => {
                29.hash(state);
            }
            Token::UnterminatedString => {
                30.hash(state);
            }
            Token::LBracket => {
                31.hash(state);
            }
            Token::RBracket => {
                32.hash(state);
            }
        }
    }
}

impl Token {
    pub fn check_keyword(identifier: &str) -> Token {
        match identifier {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "none" => Token::None,
            _ => Token::Identifier(identifier.to_string()),
        }
    }
}
