use crate::token::TokenType;
pub struct Program {
    pub statements: Vec<Statement>,
}

pub fn program_to_string(p: &Program) -> String {
    let mut output = String::new();
    for (index, statement) in p.statements.iter().enumerate() {
        output += &statement_to_string(&statement);
        if p.statements.len() - index != 1 {
            output += "\n";
        }
    }

    output
}

pub fn statement_to_string(statement: &Statement) -> String {
    match statement {
        Statement::Let(ident, expr) => format!("let {} = {};", ident, expression_to_string(expr)),
        Statement::Return(expr) => format!("(return {})", expression_to_string(expr)),
        Statement::Expr(expr) => format!("{}", expression_to_string(expr)),
        Statement::None => "None".to_string(),
        _ => "None".to_string(),
    }
}

pub fn expression_to_string(expression: &Expr) -> String {
    match expression {
        Expr::Bool(value) => format!("{}", value),
        Expr::Identifier(value) => format!("{}", value),
        Expr::IntegerLiteral(value) => format!("{}", value),
        Expr::String(value) => value.clone(),
        Expr::Prefix(op, boxed_expr) => format!("({}{})", op, expression_to_string(boxed_expr)),
        Expr::Infix(op, boxed_left, boxed_right) => format!(
            "({} {} {})",
            expression_to_string(boxed_left),
            op,
            expression_to_string(boxed_right)
        ),
        Expr::If(condition, if_block, else_block) => match else_block {
            Some(box_else) => format!(
                "if {} {} else {}",
                expression_to_string(condition),
                statement_to_string(if_block),
                statement_to_string(box_else)
            ),
            None => format!(
                "if {} {}",
                expression_to_string(condition),
                statement_to_string(if_block)
            ),
        },
        // Expr::FunctionLiteral(Vec<Identifier>, Box<Statement>),
        // Expr::ArrayLiteral(Vec<Expr>),
        // Expr::Index(Box<Expr>, Box<Expr>), // Left, Index
        // Expr::HashLiteral(Vec<(Expr, Expr)>),
        // Expr::CallExpression {
        //     function: Box<Expr>,
        //     arguments: Vec<Expr>,
        // }, // Function can be either func_literal or identifier
        _ => "".to_string()
    }
}


// fn token_literal(&self) -> String {
//     if p.statements.len() > 0 {
//         p.statements[0].token_literal
//     } else {
//         ""
//     }
// }


#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expr),
    Return(Expr),
    BlockStatement(Vec<Statement>),
    Expr(Expr),
    None,
}

#[derive(Debug)]
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


#[cfg(test)]
mod tests{
    use crate::ast::program_to_string;

    use super::{Program, Statement, Expr};


    #[test]
    fn test_string() {
        let _input = "let myVar = anotherVar;";
        let program = Program {
            statements: vec![
                Statement::Let("myVar".to_string(), Expr::Identifier("anotherVar".to_string()))
            ],
        };

        assert_eq!(program_to_string(&program), "let myVar = anotherVar;")
    }
}