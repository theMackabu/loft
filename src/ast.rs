use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Identifier(String),

    Assignment {
        target: String,
        value: Box<Expr>,
    },

    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },

    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },

    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },

    Block {
        statements: Vec<Stmt>,
        value: Option<Box<Expr>>,
        returns: bool,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Return(Option<Expr>),
    ExpressionStmt(Expr),
    ExpressionValue(Expr),

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
}
