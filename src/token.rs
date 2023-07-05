use std::fmt;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    
    // Identifiers + literals
    IDENT,
    INT,
    
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    
    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    
    // Keywords
    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,

    // Booleans
    TRUE,
    FALSE,

    // Comparition
    EQ,
    NOT_EQ,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type: token_type,
            literal: literal,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}


pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        // General Keywords
        "let" => TokenType::LET,
        "fn" => TokenType::FUNCTION,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "return" => TokenType::RETURN,
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,

        "==" => TokenType::EQ,
        "!=" => TokenType::NOT_EQ,


        // Normal identifier
        _ => TokenType::IDENT,
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::ASTERISK => write!(f, "*"),
            TokenType::SLASH => write!(f, "/"),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::GT => write!(f, ">"),
            TokenType::LT => write!(f, "<"),
            TokenType::EQ => write!(f, "=="),
            TokenType::NOT_EQ => write!(f, "!="),
            _ => write!(f, "{:?}", self),
        }
    }
}