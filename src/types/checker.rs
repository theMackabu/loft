use crate::{
    parser::{ast::*, lexer::*},
    util::convert_type_annotation,
};

use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeError {
    UndefinedVariable { name: String, location: String },
    InvalidOperation { message: String, location: String },
    TypeMismatch { expected: String, found: String, location: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
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
    Bool,
    Char,
    Str,
    Unit,
    Unknown,
}

pub struct TypeChecker {
    stmt: Vec<Stmt>,
    variables: HashMap<String, Primitive>,
}

impl TypeChecker {
    pub fn new(ast: &Vec<Stmt>) -> Self {
        Self {
            stmt: ast.to_owned(),
            variables: HashMap::new(),
        }
    }

    pub fn check(&mut self) -> Result<(), TypeError> {
        let statements = self.stmt.to_owned();

        for stmt in &statements {
            self.check_statement(stmt)?;
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<Primitive, TypeError> {
        use Primitive::*;

        match expr {
            Expr::Unit => Ok(Unit),

            Expr::Boolean(_) => Ok(Bool),
            Expr::String(_) => Ok(Str),

            Expr::Integer(_, suffix) => match suffix {
                Some(NumericType::I8) => Ok(I8),
                Some(NumericType::I16) => Ok(I16),
                Some(NumericType::I32) => Ok(I32),
                Some(NumericType::I64) => Ok(I64),
                Some(NumericType::I128) => Ok(I128),

                Some(NumericType::U8) => Ok(U8),
                Some(NumericType::U16) => Ok(U16),
                Some(NumericType::U32) => Ok(U32),
                Some(NumericType::U64) => Ok(U64),
                Some(NumericType::U128) => Ok(U128),

                _ => Ok(I32),
            },

            Expr::Float(_, suffix) => match suffix {
                Some(NumericType::F32) => Ok(F32),
                Some(NumericType::F64) => Ok(F64),
                _ => Ok(F64),
            },

            Expr::Binary { left, operator, right } => self.check_binary_operation(left, operator, right),

            Expr::Identifier(name) => self.variables.get(name).cloned().ok_or(TypeError::UndefinedVariable {
                name: name.clone(),
                location: "".to_string(), // add proper location tracking
            }),

            _ => Ok(Unknown),
        }
    }

    fn check_binary_operation(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<Primitive, TypeError> {
        use Primitive::*;

        let left_type = self.check_expr(left)?;
        let right_type = self.check_expr(right)?;

        match operator {
            Token::Plus | Token::Minus | Token::Star | Token::Slash => match (&left_type, &right_type) {
                (I32, I32) => Ok(I32),
                (F64, F64) => Ok(F64),

                _ => Err(TypeError::TypeMismatch {
                    expected: format!("{:?}", left_type),
                    found: format!("{:?}", right_type),
                    location: "".to_string(), // add proper location tracking
                }),
            },

            Token::Equals | Token::NotEquals | Token::LeftAngle | Token::RightAngle | Token::LessEquals | Token::GreaterEquals => Ok(Bool),

            _ => Ok(Unknown),
        }
    }

    fn check_statement(&mut self, stmt: &Stmt) -> Result<(), TypeError> {
        match stmt {
            Stmt::Function { body, .. } => {
                for stmt in body {
                    self.check_statement(stmt)?;
                }
                Ok(())
            }

            Stmt::Let {
                pattern,
                type_annotation,
                initializer,
                ..
            } => {
                if let Pattern::Identifier { name, .. } = pattern {
                    if let Some(init) = initializer {
                        let expr_type = self.check_expr(init)?;

                        if let Some(annotation) = type_annotation {
                            let annotated_type = convert_type_annotation(annotation)?;

                            if annotated_type != expr_type {
                                return Err(TypeError::TypeMismatch {
                                    expected: annotated_type.type_name(),
                                    found: expr_type.type_name(),
                                    location: "".to_string(), // add location
                                });
                            }
                        }

                        self.variables.insert(name.clone(), expr_type);
                    }
                }
                Ok(())
            }

            // skip all other top-level declarations for now
            _ => Ok(()),
        }
    }
}
