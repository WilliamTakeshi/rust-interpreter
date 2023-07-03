use crate::token::{Token, TokenType, lookup_ident};

// #[derive(Debug)]
pub struct Lexer {
    input: String,
    input_chars: Vec<char>, // Char Vector to allow getting chars at certain indices
    position: usize, // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: char, // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input: input.clone(),
            input_chars: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0'; // ASCII code for the "NUL"
        } else {
            self.ch = self.input_chars[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char()
        }
    }

    fn next_token(&mut self) -> Token {

        self.skip_whitespace();

        let tok: Token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let mut literal = ch.to_string();
                    literal.push(self.ch);
                    Token::new(TokenType::EQ, literal)
                } else {
                    Token::new(TokenType::ASSIGN, self.ch.to_string())
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let mut literal = ch.to_string();
                    literal.push(self.ch);
                    Token::new(TokenType::NOT_EQ, literal)
                } else {
                    Token::new(TokenType::BANG, self.ch.to_string())
                }
            },
            ';' => Token::new(TokenType::SEMICOLON, self.ch.to_string()),
            '(' => Token::new(TokenType::LPAREN, self.ch.to_string()),
            ')' => Token::new(TokenType::RPAREN, self.ch.to_string()),
            ',' => Token::new(TokenType::COMMA, self.ch.to_string()),
            '+' => Token::new(TokenType::PLUS, self.ch.to_string()),
            '{' => Token::new(TokenType::LBRACE, self.ch.to_string()),
            '}' => Token::new(TokenType::RBRACE, self.ch.to_string()),
            '-' => Token::new(TokenType::MINUS, self.ch.to_string()),
            '*' => Token::new(TokenType::ASTERISK, self.ch.to_string()),
            '/' => Token::new(TokenType::SLASH, self.ch.to_string()),
            '<' => Token::new(TokenType::LT, self.ch.to_string()),
            '>' => Token::new(TokenType::GT, self.ch.to_string()),
            '\0' => Token::new(TokenType::EOF, "".to_string()),
            _ => {
                // let mut tok = Token::new(TokenType::EOF, "".to_string());
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    return Token::new(lookup_ident(literal), literal.to_string())
                } else if is_digit(self.ch) {
                    return Token::new(TokenType::INT, self.read_number().to_string())
                } else {
                    Token::new(TokenType::ILLEGAL, self.ch.to_string())
                }
            },
        };
    
        self.read_char();
    
        tok
    }
    fn read_number(&mut self) -> &str {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        &self.input[position..self.position]
    }
    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input_chars[self.read_position]
        }
    }
}

fn is_letter(c: char) -> bool {
    // c.is_ascii_alphabetic() || c == '_'
    c.is_alphabetic() || c == '_'

}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::Lexer;
    use crate::token::{Token, TokenType};



    #[test]
    fn test_next_token() {
        let input: &str = "=+(){},;";

        let expected: Vec<_> = vec![
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l: Lexer = Lexer::new(input.to_string());

        for t in expected {
            let tok = l.next_token();
            assert_eq!(tok, t);
        }
    }

    #[test]
    fn test_complex_lexer() {
        let input ="let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);";
        let expected: Vec<_> = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l: Lexer = Lexer::new(input.to_string());

        for t in expected {
            let tok = l.next_token();
            assert_eq!(tok, t);
        }
    }

    #[test]
    fn test_new_tokens() {
        let input: &str = "!-/*5;
5 < 10 > 5;";

        let expected: Vec<_> = vec![
            Token::new(TokenType::BANG, "!".to_string()),
            Token::new(TokenType::MINUS, "-".to_string()),
            Token::new(TokenType::SLASH, "/".to_string()),
            Token::new(TokenType::ASTERISK, "*".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l: Lexer = Lexer::new(input.to_string());

        for t in expected {
            let tok = l.next_token();
            assert_eq!(tok, t);
        }
    }

    #[test]
    fn test_new_keywords() {
        let input: &str = "if (5 < 10) {
return true;
} else {
return false;
}";

        let expected: Vec<_> = vec![
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l: Lexer = Lexer::new(input.to_string());

        for t in expected {
            let tok = l.next_token();
            assert_eq!(tok, t);
        }
    }


    #[test]
    fn test_eq_neq() {
        let input: &str = "10 == 10;
        10 != 9;";

        let expected: Vec<_> = vec![
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::NOT_EQ, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l: Lexer = Lexer::new(input.to_string());

        for t in expected {
            let tok = l.next_token();
            assert_eq!(tok, t);
        }
    }


}
