use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Identifier(String),
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Call { function: Box<Expr>, arguments: Vec<Expr> },
}

#[derive(Debug)]
pub enum Stmt {
    Let {
        name: String,
        type_annotation: Option<String>,
        initializer: Box<Expr>,
    },
    Function {
        name: String,
        params: Vec<(String, String)>, // (name, type)
        return_type: Option<String>,
        body: Vec<Stmt>,
    },
    Return(Option<Expr>),
    Expression(Expr),
}
