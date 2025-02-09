use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum NumericType {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Path(Path),
    Simple(String),
    TypeParam(String),

    Slice { element_type: Box<Type> },
    Array { element_type: Box<Type>, size: usize },
    Generic { path: Path, type_params: Vec<Type> },
}

#[derive(Debug)]
pub enum EnumVariant {
    Simple(String),
    Tuple(String, Vec<Type>),
    Struct(String, Vec<(String, Type)>),
}

#[derive(Debug)]
pub enum UsePath {
    Simple(String),
    Nested(Vec<String>),
}

#[derive(Debug)]
pub enum Expr {
    Path(Path),
    Boolean(bool),
    String(String),
    Array(Vec<Expr>),
    Identifier(String),
    Await(Box<Expr>),

    Integer(i64, Option<NumericType>),
    Float(f64, Option<NumericType>),

    Ok(Box<Expr>),
    Err(Box<Expr>),
    Some(Box<Expr>),
    Try(Box<Expr>),

    Unit,
    None,

    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },

    Cast {
        expr: Box<Expr>,
        target_type: Type,
    },

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

    StructInit {
        struct_name: String,
        fields: Vec<(String, Expr, bool)>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Return(Option<Expr>),
    ExpressionStmt(Expr),
    ExpressionValue(Expr),

    Use {
        path: UsePath,
        alias: Option<String>,
        visibility: bool,
    },
    Module {
        name: String,
        visibility: bool,
        body: Vec<Stmt>,
    },

    Struct {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        fields: Vec<(String, bool, Type)>, // field_name, expr, is_shorthand
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
        initializer: Option<Box<Expr>>,
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
        params: Vec<(String, bool, Type)>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
}
