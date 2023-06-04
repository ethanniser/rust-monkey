#[derive(Debug, Clone)]
pub enum Token {
    Illegal,
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
    Percent,
    DoublePipe,
    DoubleAmpersand,
    Colon,
}

impl PartialEq for Token {
    #[allow(clippy::match_like_matches_macro)]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Illegal, Token::Illegal) => true,
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
            (Token::Percent, Token::Percent) => true,
            (Token::DoublePipe, Token::DoublePipe) => true,
            (Token::DoubleAmpersand, Token::DoubleAmpersand) => true,
            (Token::Colon, Token::Colon) => true,
            _ => false,
        }
    }
}

impl Eq for Token {}

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
