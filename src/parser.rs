
use crate::ast;
use crate::lexer::{Lexer};
use crate::token::{Token, TokenType};
use std::mem;
struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}


impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lexer,
            cur_token: Token::new(TokenType::ILLEGAL, "".to_string()),
            peek_token: Token::new(TokenType::ILLEGAL, "".to_string()),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        // self.cur_token = self.peek_token;
        // self.peek_token = self.lexer.next_token();
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token());

    }

    fn parse_program(&self) -> ast::Program {
        todo!()
    }
}



#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer};
    use crate::ast;
    use super::Parser;

    #[test]
    fn test_let_statement() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";

        let lexer = Lexer::new(input.to_string());
        let parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);

        let expected_identifier = ["x", "y", "foobar"];

        for (index, expected_name) in expected_identifier.iter().enumerate() {
            assert_let_statement(&program.statements[index], expected_name);
        }

    }

    fn assert_let_statement(statement: &ast::Statement, expected_identifier: &str) {
        match statement {
            ast::Statement::Let(identifier, _) => {
                assert_eq!(*identifier, expected_identifier.to_string());
            }
            _ => assert!(false),
        };
    }
}