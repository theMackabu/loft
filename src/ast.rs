use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Identifier(String),

    MemberAccess {
        object: Box<Expr>,
        member: String,
    },

    MemberAssignment {
        object: Box<Expr>,
        member: String,
        value: Box<Expr>,
    },

    Assignment {
        target: String,
        value: Box<Expr>,
    },

    Unary {
        operator: Token,
        operand: Box<Expr>,
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

#[derive(Debug, Clone)]
pub enum Type {
    Simple(String),
    Generic { name: String, type_params: Vec<Type> },
    TypeParam(String),
}

#[derive(Debug)]
pub enum Stmt {
    Return(Option<Expr>),
    ExpressionStmt(Expr),
    ExpressionValue(Expr),

    TypeAlias {
        name: String,
        type_params: Vec<String>,
        ty: Type,
    },

    Let {
        name: String,
        mutable: bool,
        type_annotation: Option<Type>,
        initializer: Box<Expr>,
    },

    Const {
        name: String,
        type_annotation: Option<Type>,
        initializer: Box<Expr>,
    },

    Function {
        name: String,
        params: Vec<(String, String)>, // (name, type)
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
}
