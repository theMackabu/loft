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

#[derive(Clone, Debug)]
struct FunctionType {
    params: Vec<(Pattern, Type)>,
    return_type: Primitive,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pointer {
    Reference { mutable: bool },
    RawPointer,
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

    Array(Box<Primitive>, usize),
    Slice(Box<Primitive>),
    Tuple(Vec<Primitive>),
    Pointer(Box<Primitive>, Pointer),
}

pub struct TypeChecker {
    stmt: Vec<Stmt>,
    variables: HashMap<String, Primitive>,
    globals: HashMap<String, Primitive>,
    functions: HashMap<String, FunctionType>,
}

impl TypeChecker {
    pub fn new(ast: &Vec<Stmt>) -> Self {
        Self {
            stmt: ast.to_owned(),
            variables: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn check(&mut self) -> Result<(), TypeError> {
        let statements = self.stmt.to_owned();

        for stmt in &statements {
            self.collect_globals(stmt)?;
        }

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

            Expr::String(_) => Ok(Primitive::Pointer(Box::new(Primitive::Str), self::Pointer::Reference { mutable: false })),

            Expr::Tuple(elements) => {
                let mut types = Vec::new();
                for elem in elements {
                    types.push(self.check_expr(elem)?);
                }
                Ok(Tuple(types))
            }

            //             Expr::Array(elements) => {
            //                 if elements.is_empty() {
            //                     return Ok(Array(Box::new(Unknown)));
            //                 }
            //
            //                 let first_type = self.check_expr(&elements[0])?;
            //
            //                 for elem in elements.iter().skip(1) {
            //                     let elem_type = self.check_expr(elem)?;
            //                     if elem_type != first_type {
            //                         return Err(TypeError::TypeMismatch {
            //                             expected: first_type.type_name(),
            //                             found: elem_type.type_name(),
            //                             location: "array element".to_string(),
            //                         });
            //                     }
            //                 }
            //
            //                 Ok(Array(Box::new(first_type)))
            //             }
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

            Expr::Reference { mutable, operand } => {
                let inner_type = self.check_expr(operand)?;
                Ok(Primitive::Pointer(Box::new(inner_type), self::Pointer::Reference { mutable: *mutable }))
            }

            Expr::Dereference { operand } => {
                let operand_type = self.check_expr(operand)?;
                match operand_type {
                    Primitive::Pointer(inner_type, _) => Ok(*inner_type),
                    _ => Err(TypeError::InvalidOperation {
                        message: "Cannot dereference non-pointer type".to_string(),
                        location: "".to_string(),
                    }),
                }
            }

            Expr::Call { function, arguments } => {
                if let Expr::Identifier(name) = &**function {
                    // Look up function in scope
                    if let Some(func_type) = self.functions.get(name) {
                        // Check argument types match parameters
                        // For now, just return the function's return type
                        return Ok(func_type.return_type.clone());
                    }
                }
                self.check_expr(function)
            }

            Expr::Identifier(name) => {
                self.variables.get(name).or_else(|| self.globals.get(name)).cloned().ok_or(TypeError::UndefinedVariable {
                    name: name.clone(),
                    location: "".to_string(), // add proper location tracking
                })
            }

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
            Stmt::Const {
                name, type_annotation, initializer, ..
            } => {
                let expr_type = self.check_expr(initializer)?;
                if let Some(annotation) = type_annotation {
                    let annotated_type = convert_type_annotation(annotation)?;
                    if annotated_type != expr_type {
                        return Err(TypeError::TypeMismatch {
                            expected: annotated_type.type_name(),
                            found: expr_type.type_name(),
                            location: format!("in const declaration {}", name),
                        });
                    }
                }
                self.globals.insert(name.clone(), expr_type);
                Ok(())
            }

            Stmt::Static {
                name, type_annotation, initializer, ..
            } => {
                let expr_type = self.check_expr(initializer)?;
                if let Some(annotation) = type_annotation {
                    let annotated_type = convert_type_annotation(annotation)?;
                    if annotated_type != expr_type {
                        return Err(TypeError::TypeMismatch {
                            expected: annotated_type.type_name(),
                            found: expr_type.type_name(),
                            location: format!("in static declaration {}", name),
                        });
                    }
                }
                self.globals.insert(name.clone(), expr_type);
                Ok(())
            }

            Stmt::Function { name, params, return_type, body, .. } => {
                let return_type = if let Some(ty) = return_type { convert_type_annotation(ty)? } else { Primitive::Unit };

                self.functions.insert(
                    name.clone(),
                    FunctionType {
                        params: params.clone(),
                        return_type: return_type.clone(),
                    },
                );

                let outer_scope = self.variables.clone();
                self.variables.clear();

                for (param_pattern, param_type) in params {
                    if let Pattern::Identifier { name: param_name, .. } = param_pattern {
                        let param_type = convert_type_annotation(param_type)?;
                        self.variables.insert(param_name.clone(), param_type);
                    }
                }

                for stmt in &body[..body.len() - 1] {
                    if let Stmt::Return(Some(expr)) = stmt {
                        let actual_return_type = self.check_expr(expr)?;
                        if actual_return_type != return_type {
                            return Err(TypeError::TypeMismatch {
                                expected: return_type.type_name(),
                                found: actual_return_type.type_name(),
                                location: format!("in function {}", name),
                            });
                        }
                    }
                    self.check_statement(stmt)?;
                }

                if let Some(last_stmt) = body.last() {
                    match last_stmt {
                        Stmt::Return(Some(expr)) => {
                            let actual_return_type = self.check_expr(expr)?;
                            if actual_return_type != return_type {
                                return Err(TypeError::TypeMismatch {
                                    expected: return_type.type_name(),
                                    found: actual_return_type.type_name(),
                                    location: format!("in function {}", name),
                                });
                            }
                        }
                        Stmt::ExpressionValue(expr) | Stmt::ExpressionStmt(expr) => {
                            let actual_return_type = self.check_expr(expr)?;
                            if actual_return_type != return_type {
                                return Err(TypeError::TypeMismatch {
                                    expected: return_type.type_name(),
                                    found: actual_return_type.type_name(),
                                    location: format!("in function {}", name),
                                });
                            }
                        }
                        _ => self.check_statement(last_stmt)?,
                    }
                }

                self.variables = outer_scope;
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

    fn collect_globals(&mut self, stmt: &Stmt) -> Result<(), TypeError> {
        match stmt {
            Stmt::Const {
                name,
                type_annotation,
                initializer,
                ..  // handle visibility and attributes
            } => {
                let const_type = if let Some(ty) = type_annotation {
                    convert_type_annotation(ty)?
                } else {
                    self.check_expr(initializer)?
                };
                self.globals.insert(name.clone(), const_type);
                Ok(())
            }
            Stmt::Static {
                name,
                type_annotation,
                initializer,
                ..  // handle visibility and attributes
            } => {
                let static_type = if let Some(ty) = type_annotation {
                    convert_type_annotation(ty)?
                } else {
                    self.check_expr(initializer)?
                };
                self.globals.insert(name.clone(), static_type);
                Ok(())
            }
            _ => Ok(()),
        }
    }
}
