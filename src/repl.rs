use std::io::{self};
use crate::{lexer::Lexer, token::TokenType};

const PROMPT: &str = ">> ";

pub fn start() {
    println!("Welcome to the Monkey REPL!");

    // REPL Loop
    loop {
        // Print prompt and flush to write it to console
        print!("{}", PROMPT);
        io::Write::flush(&mut io::stdout()).expect("flush failed!");

        // Scan Input line
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Err(error) => {
                println!("error: {}", error);
                return;
            }
            Ok(_) => (),
        }

        // Create Lexer
        let mut lexer = Lexer::new(buffer);

        let mut token = lexer.next_token();
        while token.token_type != TokenType::EOF {
            println!("{}, {}", token.token_type, token.literal);
            token = lexer.next_token();
        }

    }
}