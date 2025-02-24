#![allow(dead_code)]

use super::lexer::{Token, TokenInfo};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum NumericType {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,

    U8,
    U16,
    U32,
    U64,
    U128,
    USize,

    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier { name: String, mutable: bool },
    Reference { mutable: bool, pattern: Box<Pattern> },

    Wildcard,
    Path(Path),
    Literal(Box<Expr>),
    Or(Vec<Pattern>),
    Tuple(Vec<Pattern>),

    TupleStruct { path: Path, elements: Vec<Pattern> },
    Struct { path: Path, fields: Vec<(String, Pattern)>, rest: bool },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub params: Vec<(Pattern, Type)>,
    pub body: Vec<Stmt>,
    pub is_method: bool,
    pub is_static: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MacroDelimiter {
    Paren,
    Bracket,
    Brace,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Attribute {
    pub is_inner: bool,
    pub name: String,
    pub tokens: Vec<TokenInfo>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum WhileCondition {
    Expression(Box<Expr>),
    Let(Pattern, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub ident: String,
    pub generics: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Path(Path),
    Simple(String),
    Tuple(Vec<Type>),
    TypeParam(String),

    Slice { element_type: Box<Type> },
    Array { element_type: Box<Type>, size: usize },
    Generic { path: Path, type_params: Vec<Type> },
    Function { params: Vec<Type>, return_type: Box<Type> },

    Reference { mutable: bool, inner: Box<Type> },
    Pointer { inner: Box<Type> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumVariant {
    Simple(String),
    Tuple(String, Vec<Type>),
    Struct(String, HashMap<String, Type>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UsePath {
    Simple(String),
    Nested(Vec<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Path(Path),
    Boolean(bool),
    String(String),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Identifier(String),
    Await(Box<Expr>),
    Try(Box<Expr>),

    Integer(i64, Option<NumericType>),
    Float(f64, Option<NumericType>),

    Unit,
    None,

    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },

    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    },

    ArrayRepeat {
        value: Box<Expr>,
        count: Box<Expr>,
    },

    Cast {
        expr: Box<Expr>,
        target_type: Type,
    },

    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
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
        target: Box<Expr>,
        value: Box<Expr>,
    },

    CompoundAssignment {
        target: Box<Expr>,
        operator: Token,
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

    IfLet {
        pattern: Pattern,
        value: Box<Expr>,
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
        path: Box<Path>,
        fields: HashMap<String, (Expr, bool)>,
    },

    Loop {
        label: Option<String>,
        body: Box<Expr>,
    },

    While {
        label: Option<String>,
        condition: WhileCondition,
        body: Box<Expr>,
    },

    For {
        label: Option<String>,
        pattern: Pattern,
        iterable: Box<Expr>,
        body: Box<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Return(Option<Expr>),
    Continue(Option<String>),
    Break(Option<String>, Option<Expr>),

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
        is_external: bool,
        body: Vec<Stmt>,
        attributes: Vec<Attribute>,
    },

    Struct {
        path: Path,
        visibility: bool,
        type_params: Vec<String>,
        fields: HashMap<String, (Type, bool)>,
        attributes: Vec<Attribute>,
    },

    Trait {
        name: String,
        items: Vec<Stmt>,
        visibility: bool,
        attributes: Vec<Attribute>,
    },

    Impl {
        target: Path,
        items: Vec<Stmt>,
        attributes: Vec<Attribute>,
    },

    TraitImpl {
        trait_path: Path,
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
        pattern: Pattern,
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
