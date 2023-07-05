use crate::ast::{self, Expr};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::mem;
struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<Result<(), String>>,
}

pub enum Precedence {
    LOWEST,
    EQUALS, // '==' or '!='
    LESSGREATER, // '<' or '>'
    SUM, // '+' or '-'
    PRODUCT, // '*' or '/'
    PREFIX, // '-x' or '!x'
    CALL, // 'func(x)'
}

fn get_token_precedence(token_type: TokenType) -> Precedence {
    match token_type {
        TokenType::EQ => Precedence::EQUALS,
        TokenType::NOT_EQ => Precedence::EQUALS,
        TokenType::LT => Precedence::LESSGREATER,
        TokenType::GT => Precedence::LESSGREATER,
        TokenType::PLUS => Precedence::SUM,
        TokenType::MINUS => Precedence::SUM,
        TokenType::SLASH => Precedence::PRODUCT,
        TokenType::ASTERISK => Precedence::PRODUCT,
        TokenType::LPAREN => Precedence::CALL,
        _ => Precedence::LOWEST,
    }
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
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expr_statement(),
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

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        // Parse Expression
        self.next_token();
        

        while self.cur_token.token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(ast::Statement::Return(Expr::None))
    }

    fn parse_expr_statement(&mut self) -> Option<ast::Statement> {
        let expr = self.parse_expr(&mut Precedence::LOWEST);

        // Check for the optional semicolon
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        match expr {
            Some(expr) => Some(ast::Statement::Expr(expr)),
            None => None,
        }
    }


    fn parse_prefix(&mut self, token_type: TokenType) -> ast::Expr {
        match token_type {
            TokenType::IDENT => self.parse_identifier(),
            // TokenType::INT => self.parse_integer_literal(),
            // TokenType::BANG | TokenType::MINUS => self.parse_prefix_expression(),
            // TokenType::TRUE | TokenType::FALSE => self.parse_boolean(),
            // TokenType::LPAREN => self.parse_grouped_expression(),
            // TokenType::IF => self.parse_if_expression(),
            // TokenType::FUNCTION => self.parse_function_literal(),
            // TokenType::STRING => self.parse_string_literal(),
            // TokenType::LBRACKET => self.parse_array_literal(),
            // TokenType::LBRACE => self.parse_hash_literal(),
            _ => ast::Expr::None,
        }
    }

    // fn parse_infix(
    //     &mut self,
    //     left: ast::Expr,
    //     token_type: TokenType,
    // ) -> Result<ast::Expr, ast::Expr> {
    //     match token_type {
    //         TokenType::LPAREN => self.parse_call_expression(left),
    //         // TokenType::LBRACKET => self.parse_index_expression(left),
    //         _ => self.parse_infix_expression(left),
    //     }
    // }

    fn parse_expr(&mut self, precedence: &mut Precedence) -> Option<ast::Expr> {
        let prefix_expr = self.parse_prefix(self.cur_token.token_type);


        match prefix_expr {
            ast::Expr::None => {
                self.errors.push(Err(format!(
                    "No Prefix found for: {:?}",
                    self.cur_token.token_type
                )));
                return None;
            }
            _ => Some(prefix_expr),
        }

        // while !self.peek_token_is(&TokenType::SEMICOLON)
        // 	// Check if the current precedence still doesn't overule the next one
        //     && precedence < &mut self.peek_precedence()
        //     // Check if INFIX operator (has a precedence)
        //     && get_token_precedence((*self.peek_token).token_type) != Precedence::LOWEST
        // {
        //     self.next_token();
        //     match self.parse_infix(left_expr, (*self.cur_token).token_type) {
        //         Ok(result) => {
        //             left_expr = result;
        //         }
        //         Err(err) => return Some(err),
        //     }
        // }

        // Some(left_expr)
    }

    fn parse_identifier(&mut self) -> ast::Expr {
        let cur_token = mem::replace(
            &mut self.cur_token,
            Token::new(TokenType::ILLEGAL, "".to_string()),
        );

        ast::Expr::Identifier(cur_token.literal.clone())
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
                token_type, self.peek_token.token_type
            )));
            false
        }
    }

}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::{self, Program};
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

    #[test]
    fn test_return_statement() {
        let input = "return 5;
return 10;
return 993322;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 3);

        for statement in program.statements.iter() {
            match statement {
                ast::Statement::Return(_) => (),
                _ => assert!(false),
            };
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }

        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 1);

        let statement: ast::Statement = program.statements.pop().expect("Program has not enough statements!");
        match statement {
            ast::Statement::Expr(expr) => match expr {
                ast::Expr::Identifier(identifier) => {
                    assert_eq!(identifier, "foobar".to_string());
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }
}
