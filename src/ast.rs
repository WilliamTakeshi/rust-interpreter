use crate::token::TokenType;
pub struct Program {
    pub statements: Vec<Statement>,
}


// fn token_literal(&self) -> String {
//     if p.statements.len() > 0 {
//         p.statements[0].token_literal
//     } else {
//         ""
//     }
// }


pub enum Statement {
    Let(Identifier, Expr),
    Return(Expr),
    BlockStatement(Vec<Statement>),
    Expr(Expr),
    None,
}

pub enum Expr {
    Bool(bool),
    Identifier(Identifier),
    IntegerLiteral(i32),
    String(String),
    ArrayLiteral(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>), // Left, Index
    HashLiteral(Vec<(Expr, Expr)>),
    Prefix(String, Box<Expr>),
    Infix(TokenType, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    FunctionLiteral(Vec<Identifier>, Box<Statement>),
    CallExpression {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    }, // Function can be either func_literal or identifier
    None,
}


pub type Identifier = String;