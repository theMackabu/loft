#![allow(dead_code)]

use crate::lexer::{Token, TokenInfo};

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
pub enum Pattern {
    Identifier { name: String, mutable: bool },
    Reference { mutable: bool, pattern: Box<Pattern> },
}

#[derive(Clone, Debug)]
pub enum MacroDelimiter {
    Paren,
    Bracket,
    Brace,
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub is_inner: bool,
    pub name: String,
    pub tokens: Vec<TokenInfo>,
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
    Function { params: Vec<Type>, return_type: Box<Type> },

    Reference { mutable: bool, inner: Box<Type> },
    Pointer { inner: Box<Type> },
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

    Closure {
        params: Vec<(String, Option<Type>)>,
        body: Box<Expr>,
        is_async: bool,
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

    MacroInvocation {
        name: String,
        delimiter: MacroDelimiter,
        tokens: Vec<TokenInfo>,
    },

    Assignment {
        target: String,
        value: Box<Expr>,
    },

    Reference {
        mutable: bool,
        operand: Box<Expr>,
    },

    Dereference {
        operand: Box<Expr>,
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
        is_async: bool,
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
        attributes: Vec<Attribute>,
    },

    Module {
        name: String,
        visibility: bool,
        body: Vec<Stmt>,
        attributes: Vec<Attribute>,
    },

    Struct {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        fields: Vec<(String, bool, Type)>, // field_name, expr, is_shorthand
        attributes: Vec<Attribute>,
    },

    Impl {
        target: Path,
        items: Vec<Stmt>,
        attributes: Vec<Attribute>,
    },

    TypeAlias {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        ty: Type,
        attributes: Vec<Attribute>,
    },

    Enum {
        name: String,
        visibility: bool,
        type_params: Vec<String>,
        variants: Vec<EnumVariant>,
        attributes: Vec<Attribute>,
    },

    Let {
        name: String,
        mutable: bool,
        type_annotation: Option<Type>,
        initializer: Option<Box<Expr>>,
        attributes: Vec<Attribute>,
    },

    Const {
        name: String,
        visibility: bool,
        type_annotation: Option<Type>,
        initializer: Box<Expr>,
        attributes: Vec<Attribute>,
    },

    Static {
        name: String,
        visibility: bool,
        type_annotation: Option<Type>,
        initializer: Box<Expr>,
        attributes: Vec<Attribute>,
    },

    MacroDefinition {
        visibility: bool,
        name: String,
        tokens: Vec<TokenInfo>,
        attributes: Vec<Attribute>,
    },

    Function {
        name: String,
        visibility: bool,
        is_async: bool,
        type_params: Vec<String>,
        params: Vec<(Pattern, Type)>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
        attributes: Vec<Attribute>,
    },
}
