use crate::lexer::Token;

#[derive(Debug)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Identifier(String),
    Await(Box<Expr>),

    Ok(Box<Expr>),
    Err(Box<Expr>),
    Some(Box<Expr>),
    Try(Box<Expr>),

    Unit,
    None,

    MethodCall {
        object: Box<Expr>,
        method: String,
        arguments: Vec<Expr>,
    },

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
    Unit,
}

#[derive(Debug)]
pub enum EnumVariant {
    Simple(String),
    Tuple(String, Vec<Type>),
    Struct(String, Vec<(String, Type)>),
}

#[derive(Debug)]
pub enum Stmt {
    Return(Option<Expr>),
    ExpressionStmt(Expr),
    ExpressionValue(Expr),

    Struct {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        fields: Vec<(String, bool, Type)>,
    },

    TypeAlias {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        ty: Type,
    },

    Enum {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        variants: Vec<EnumVariant>,
    },

    Let {
        name: String,
        mutable: bool,
        type_annotation: Option<Type>,
        initializer: Box<Expr>,
    },

    Const {
        name: String,
        visibility: bool,
        type_annotation: Option<Type>,
        initializer: Box<Expr>,
    },

    Function {
        name: String,
        visibility: bool,
        is_async: bool,
        params: Vec<(String, String)>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
}
