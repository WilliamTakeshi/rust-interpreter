use crate::ast::{self, Statement, Expr};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::mem;
struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<Result<(), String>>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lexer,
            cur_token: Token::new(TokenType::ILLEGAL, "".to_string()),
            peek_token: Token::new(TokenType::ILLEGAL, "".to_string()),
            errors: vec![],
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

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        while self.cur_token.token_type != TokenType::EOF {
            let statement_opt = self.parse_statement();
            match statement_opt {
                Some(statement) => program.statements.push(statement),
                None => (),
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            _ => return None,
        }        
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }


        let cur_token: Token = mem::replace(
            &mut self.cur_token,
            Token::new(TokenType::ILLEGAL, "".to_string()),
        );

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        // Parse Expression
        self.next_token();

        while self.cur_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        // let value = match self.parse_expression(&mut Precedence::LOWEST) {
        //     Some(expr) => expr,
        //     None => ast::Expr::None,
        // };
        
        Some(ast::Statement::Let(cur_token.literal.clone(), Expr::None))
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(&token_type) {
            self.next_token();
            true
        } else {
            self.errors.push(Err(format!(
                "Token {:?} expected, instead got {:?}!",
                token_type,
                self.peek_token.token_type
            )));
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast;
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statement() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }
        assert_eq!(parser.errors.len(), 0);

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
