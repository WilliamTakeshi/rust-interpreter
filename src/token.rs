
#[derive(Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    
    // Identifiers + literals
    IDENT,
    INT,
    
    // Operators
    ASSIGN,
    PLUS,
    
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

        // Normal identifier
        _ => TokenType::IDENT,
    }
}