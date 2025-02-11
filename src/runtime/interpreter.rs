use super::scope::Environment as ScopeEnv;
use crate::parser::{ast::*, lexer::*};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),

    F32(f32),
    F64(f64),

    Str(&'static str),
    Boolean(bool),

    Unit,
    Tuple(Vec<Value>),
}

struct Environment {
    scopes: Vec<HashMap<String, Value>>,
    scope_resolver: ScopeEnv,
}

impl Environment {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            scope_resolver: ScopeEnv::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.scope_resolver.enter_scope();
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        self.scope_resolver.exit_scope();
    }

    fn set_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        // Check if variable is declared using scope resolver
        if self.scope_resolver.resolve(name).is_some() {
            if let Some(scope) = self.scopes.last_mut() {
                scope.insert(name.to_string(), value);
                Ok(())
            } else {
                Err("No active scope".to_string())
            }
        } else {
            Err(format!("Variable '{}' not declared", name))
        }
    }

    fn get_variable(&self, name: &str) -> Option<&Value> {
        // First check if variable exists in scope
        if self.scope_resolver.resolve(name).is_some() {
            for scope in self.scopes.iter().rev() {
                if let Some(value) = scope.get(name) {
                    return Some(value);
                }
            }
        }
        None
    }
}

pub struct Interpreter {
    env: Environment,
    ast: Vec<Stmt>,
}

impl Interpreter {
    pub fn new(ast: &Vec<Stmt>) -> Result<Self, String> {
        let mut interpreter = Self {
            env: Environment::new(),
            ast: ast.clone(),
        };

        interpreter.env.scope_resolver.resolve_program(ast)?;
        interpreter.declare_globals(ast)?;

        Ok(interpreter)
    }

    pub fn start_main(&mut self) -> Result<Value, String> {
        match self.find_function("main") {
            Some(main) => self.execute_statement(&main.to_owned()),
            None => Err("No main function found".to_string()),
        }
    }

    pub fn execute(&mut self, statements: &[Stmt]) -> Result<Value, String> {
        let mut last_value = Value::Unit;

        for stmt in statements {
            last_value = self.execute_statement(stmt)?;
        }

        Ok(last_value)
    }

    fn find_function(&self, name: &str) -> Option<&Stmt> {
        for stmt in &self.ast {
            if let Stmt::Function { name: func_name, .. } = stmt {
                if func_name == name {
                    return Some(stmt);
                }
            }
        }
        None
    }

    fn declare_globals(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for stmt in statements {
            match stmt {
                Stmt::Const { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    self.env.scope_resolver.declare_variable(name);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Static { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    self.env.scope_resolver.declare_variable(name);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Module { name, body, .. } => {
                    self.env.scope_resolver.declare_module(name)?;
                    self.env.enter_scope();
                    self.declare_globals(body)?;
                    self.env.exit_scope();
                }

                _ => {}
            }
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &Stmt) -> Result<Value, String> {
        match stmt {
            Stmt::Module { body, .. } => {
                self.env.enter_scope();
                let result = self.execute(body)?;
                self.env.exit_scope();
                Ok(result)
            }

            Stmt::Let { pattern, initializer, .. } => {
                let value = if let Some(init) = initializer { self.evaluate_expression(init)? } else { Value::Unit };

                if let Pattern::Identifier { name, .. } = pattern {
                    self.env.scope_resolver.declare_variable(name);
                    self.env.set_variable(name, value)?;
                }
                Ok(Value::Unit)
            }

            Stmt::ExpressionStmt(expr) => {
                self.evaluate_expression(expr)?;
                Ok(Value::Unit)
            }

            Stmt::Function { name, params, body, .. } => {
                self.env.scope_resolver.declare_function(name)?;
                self.env.enter_scope();

                // add type checking _ty
                for (pat, _) in params {
                    if let Pattern::Identifier { name, .. } = pat {
                        self.env.scope_resolver.declare_variable(name);
                    }
                }

                let result = if let Some(last_stmt) = body.last() {
                    match last_stmt {
                        Stmt::Return(Some(expr)) => {
                            for stmt in &body[..body.len() - 1] {
                                self.execute_statement(stmt)?;
                            }
                            self.evaluate_expression(expr)?
                        }
                        Stmt::ExpressionValue(expr) => {
                            for stmt in &body[..body.len() - 1] {
                                self.execute_statement(stmt)?;
                            }
                            self.evaluate_expression(expr)?
                        }
                        _ => {
                            for stmt in body {
                                self.execute_statement(stmt)?;
                            }
                            Value::Unit
                        }
                    }
                } else {
                    Value::Unit
                };

                self.env.exit_scope();
                Ok(result)
            }

            Stmt::Return(Some(expr)) => self.evaluate_expression(expr),

            Stmt::ExpressionValue(expr) => self.evaluate_expression(expr),

            _ => Ok(Value::Unit),
        }
    }

    fn evaluate_expression(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            // add type checking
            Expr::Integer(value, ty) => Ok(match ty.to_owned().unwrap_or(NumericType::I32) {
                NumericType::I8 => Value::I8(*value as i8),
                NumericType::I16 => Value::I16(*value as i16),
                NumericType::I32 => Value::I32(*value as i32),
                NumericType::I64 => Value::I64(*value),

                // implement numerical lowering
                NumericType::I128 => Value::I128(*value as i128),
                NumericType::ISize => Value::ISize(*value as isize),

                NumericType::U8 => Value::U8(*value as u8),
                NumericType::U16 => Value::U16(*value as u16),
                NumericType::U32 => Value::U32(*value as u32),
                NumericType::U64 => Value::U64(*value as u64),

                // implement numerical lowering
                NumericType::U128 => Value::U128(*value as u128),
                NumericType::USize => Value::USize(*value as usize),

                _ => unreachable!(), // cannot hit this
            }),

            Expr::Float(value, ty) => Ok(match ty.to_owned().unwrap_or(NumericType::F64) {
                NumericType::F32 => Value::F32(*value as f32),
                NumericType::F64 => Value::F64(*value),

                _ => unreachable!(), // cannot hit this
            }),

            Expr::String(value) => Ok(Value::Str(Box::leak(value.to_owned().into_boxed_str()))),

            Expr::Identifier(name) => self.env.get_variable(name).cloned().ok_or_else(|| format!("Undefined variable: {}", name)),

            Expr::Tuple(expressions) => {
                let mut values = Vec::new();
                for expr in expressions {
                    values.push(self.evaluate_expression(expr)?);
                }
                Ok(Value::Tuple(values))
            }

            Expr::Block { statements, value, .. } => {
                self.env.enter_scope();

                for stmt in statements {
                    self.execute_statement(stmt)?;
                }

                let result = if let Some(expr) = value { self.evaluate_expression(expr)? } else { Value::Unit };

                self.env.exit_scope();
                Ok(result)
            }

            Expr::Dereference { operand } => {
                let value = self.evaluate_expression(operand)?;
                // for now, just return the value directly since we're not fully implementing
                // the reference system in the interpreter
                Ok(value)
            }

            // same as above
            Expr::Reference { mutable: _, operand } => self.evaluate_expression(operand),

            Expr::Binary { left, operator, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;

                match (&left_val, operator, &right_val) {
                    // i8
                    (Value::I8(l), Token::Plus, Value::I8(r)) => Ok(Value::I8(l + r)),
                    (Value::I8(l), Token::Minus, Value::I8(r)) => Ok(Value::I8(l - r)),
                    (Value::I8(l), Token::Star, Value::I8(r)) => Ok(Value::I8(l * r)),
                    (Value::I8(l), Token::Slash, Value::I8(r)) => Ok(Value::I8(l / r)),

                    // u8
                    (Value::U8(l), Token::Plus, Value::U8(r)) => Ok(Value::U8(l + r)),
                    (Value::U8(l), Token::Minus, Value::U8(r)) => Ok(Value::U8(l - r)),
                    (Value::U8(l), Token::Star, Value::U8(r)) => Ok(Value::U8(l * r)),
                    (Value::U8(l), Token::Slash, Value::U8(r)) => Ok(Value::U8(l / r)),

                    // i16
                    (Value::I16(l), Token::Plus, Value::I16(r)) => Ok(Value::I16(l + r)),
                    (Value::I16(l), Token::Minus, Value::I16(r)) => Ok(Value::I16(l - r)),
                    (Value::I16(l), Token::Star, Value::I16(r)) => Ok(Value::I16(l * r)),
                    (Value::I16(l), Token::Slash, Value::I16(r)) => Ok(Value::I16(l / r)),

                    // u16
                    (Value::U16(l), Token::Plus, Value::U16(r)) => Ok(Value::U16(l + r)),
                    (Value::U16(l), Token::Minus, Value::U16(r)) => Ok(Value::U16(l - r)),
                    (Value::U16(l), Token::Star, Value::U16(r)) => Ok(Value::U16(l * r)),
                    (Value::U16(l), Token::Slash, Value::U16(r)) => Ok(Value::U16(l / r)),

                    // i32
                    (Value::I32(l), Token::Plus, Value::I32(r)) => Ok(Value::I32(l + r)),
                    (Value::I32(l), Token::Minus, Value::I32(r)) => Ok(Value::I32(l - r)),
                    (Value::I32(l), Token::Star, Value::I32(r)) => Ok(Value::I32(l * r)),
                    (Value::I32(l), Token::Slash, Value::I32(r)) => Ok(Value::I32(l / r)),

                    // u32
                    (Value::U32(l), Token::Plus, Value::U32(r)) => Ok(Value::U32(l + r)),
                    (Value::U32(l), Token::Minus, Value::U32(r)) => Ok(Value::U32(l - r)),
                    (Value::U32(l), Token::Star, Value::U32(r)) => Ok(Value::U32(l * r)),
                    (Value::U32(l), Token::Slash, Value::U32(r)) => Ok(Value::U32(l / r)),

                    // f32
                    (Value::F32(l), Token::Plus, Value::F32(r)) => Ok(Value::F32(l + r)),
                    (Value::F32(l), Token::Minus, Value::F32(r)) => Ok(Value::F32(l - r)),
                    (Value::F32(l), Token::Star, Value::F32(r)) => Ok(Value::F32(l * r)),
                    (Value::F32(l), Token::Slash, Value::F32(r)) => Ok(Value::F32(l / r)),

                    // i64
                    (Value::I64(l), Token::Plus, Value::I64(r)) => Ok(Value::I64(l + r)),
                    (Value::I64(l), Token::Minus, Value::I64(r)) => Ok(Value::I64(l - r)),
                    (Value::I64(l), Token::Star, Value::I64(r)) => Ok(Value::I64(l * r)),
                    (Value::I64(l), Token::Slash, Value::I64(r)) => Ok(Value::I64(l / r)),

                    // u64
                    (Value::U64(l), Token::Plus, Value::U64(r)) => Ok(Value::U64(l + r)),
                    (Value::U64(l), Token::Minus, Value::U64(r)) => Ok(Value::U64(l - r)),
                    (Value::U64(l), Token::Star, Value::U64(r)) => Ok(Value::U64(l * r)),
                    (Value::U64(l), Token::Slash, Value::U64(r)) => Ok(Value::U64(l / r)),

                    // f64
                    (Value::F64(l), Token::Plus, Value::F64(r)) => Ok(Value::F64(l + r)),
                    (Value::F64(l), Token::Minus, Value::F64(r)) => Ok(Value::F64(l - r)),
                    (Value::F64(l), Token::Star, Value::F64(r)) => Ok(Value::F64(l * r)),
                    (Value::F64(l), Token::Slash, Value::F64(r)) => Ok(Value::F64(l / r)),

                    // i128
                    (Value::I128(l), Token::Plus, Value::I128(r)) => Ok(Value::I128(l + r)),
                    (Value::I128(l), Token::Minus, Value::I128(r)) => Ok(Value::I128(l - r)),
                    (Value::I128(l), Token::Star, Value::I128(r)) => Ok(Value::I128(l * r)),
                    (Value::I128(l), Token::Slash, Value::I128(r)) => Ok(Value::I128(l / r)),

                    // u128
                    (Value::U128(l), Token::Plus, Value::U128(r)) => Ok(Value::U128(l + r)),
                    (Value::U128(l), Token::Minus, Value::U128(r)) => Ok(Value::U128(l - r)),
                    (Value::U128(l), Token::Star, Value::U128(r)) => Ok(Value::U128(l * r)),
                    (Value::U128(l), Token::Slash, Value::U128(r)) => Ok(Value::U128(l / r)),

                    // isize
                    (Value::ISize(l), Token::Plus, Value::ISize(r)) => Ok(Value::ISize(l + r)),
                    (Value::ISize(l), Token::Minus, Value::ISize(r)) => Ok(Value::ISize(l - r)),
                    (Value::ISize(l), Token::Star, Value::ISize(r)) => Ok(Value::ISize(l * r)),
                    (Value::ISize(l), Token::Slash, Value::ISize(r)) => Ok(Value::ISize(l / r)),

                    // usize
                    (Value::USize(l), Token::Plus, Value::USize(r)) => Ok(Value::USize(l + r)),
                    (Value::USize(l), Token::Minus, Value::USize(r)) => Ok(Value::USize(l - r)),
                    (Value::USize(l), Token::Star, Value::USize(r)) => Ok(Value::USize(l * r)),
                    (Value::USize(l), Token::Slash, Value::USize(r)) => Ok(Value::USize(l / r)),

                    // i8
                    (Value::I8(l), Token::LeftAngle, Value::I8(r)) => Ok(Value::Boolean(l < r)),
                    (Value::I8(l), Token::RightAngle, Value::I8(r)) => Ok(Value::Boolean(l > r)),
                    (Value::I8(l), Token::LessEquals, Value::I8(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::I8(l), Token::GreaterEquals, Value::I8(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::I8(l), Token::Equals, Value::I8(r)) => Ok(Value::Boolean(l == r)),
                    (Value::I8(l), Token::NotEquals, Value::I8(r)) => Ok(Value::Boolean(l != r)),

                    // u8
                    (Value::U8(l), Token::LeftAngle, Value::U8(r)) => Ok(Value::Boolean(l < r)),
                    (Value::U8(l), Token::RightAngle, Value::U8(r)) => Ok(Value::Boolean(l > r)),
                    (Value::U8(l), Token::LessEquals, Value::U8(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::U8(l), Token::GreaterEquals, Value::U8(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::U8(l), Token::Equals, Value::U8(r)) => Ok(Value::Boolean(l == r)),
                    (Value::U8(l), Token::NotEquals, Value::U8(r)) => Ok(Value::Boolean(l != r)),

                    // i16
                    (Value::I16(l), Token::LeftAngle, Value::I16(r)) => Ok(Value::Boolean(l < r)),
                    (Value::I16(l), Token::RightAngle, Value::I16(r)) => Ok(Value::Boolean(l > r)),
                    (Value::I16(l), Token::LessEquals, Value::I16(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::I16(l), Token::GreaterEquals, Value::I16(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::I16(l), Token::Equals, Value::I16(r)) => Ok(Value::Boolean(l == r)),
                    (Value::I16(l), Token::NotEquals, Value::I16(r)) => Ok(Value::Boolean(l != r)),

                    // u16
                    (Value::U16(l), Token::LeftAngle, Value::U16(r)) => Ok(Value::Boolean(l < r)),
                    (Value::U16(l), Token::RightAngle, Value::U16(r)) => Ok(Value::Boolean(l > r)),
                    (Value::U16(l), Token::LessEquals, Value::U16(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::U16(l), Token::GreaterEquals, Value::U16(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::U16(l), Token::Equals, Value::U16(r)) => Ok(Value::Boolean(l == r)),
                    (Value::U16(l), Token::NotEquals, Value::U16(r)) => Ok(Value::Boolean(l != r)),

                    // i32
                    (Value::I32(l), Token::LeftAngle, Value::I32(r)) => Ok(Value::Boolean(l < r)),
                    (Value::I32(l), Token::RightAngle, Value::I32(r)) => Ok(Value::Boolean(l > r)),
                    (Value::I32(l), Token::LessEquals, Value::I32(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::I32(l), Token::GreaterEquals, Value::I32(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::I32(l), Token::Equals, Value::I32(r)) => Ok(Value::Boolean(l == r)),
                    (Value::I32(l), Token::NotEquals, Value::I32(r)) => Ok(Value::Boolean(l != r)),

                    // u32
                    (Value::U32(l), Token::LeftAngle, Value::U32(r)) => Ok(Value::Boolean(l < r)),
                    (Value::U32(l), Token::RightAngle, Value::U32(r)) => Ok(Value::Boolean(l > r)),
                    (Value::U32(l), Token::LessEquals, Value::U32(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::U32(l), Token::GreaterEquals, Value::U32(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::U32(l), Token::Equals, Value::U32(r)) => Ok(Value::Boolean(l == r)),
                    (Value::U32(l), Token::NotEquals, Value::U32(r)) => Ok(Value::Boolean(l != r)),

                    // f32
                    (Value::F32(l), Token::LeftAngle, Value::F32(r)) => Ok(Value::Boolean(l < r)),
                    (Value::F32(l), Token::RightAngle, Value::F32(r)) => Ok(Value::Boolean(l > r)),
                    (Value::F32(l), Token::LessEquals, Value::F32(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::F32(l), Token::GreaterEquals, Value::F32(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::F32(l), Token::Equals, Value::F32(r)) => Ok(Value::Boolean(l == r)),
                    (Value::F32(l), Token::NotEquals, Value::F32(r)) => Ok(Value::Boolean(l != r)),

                    // i64
                    (Value::I64(l), Token::LeftAngle, Value::I64(r)) => Ok(Value::Boolean(l < r)),
                    (Value::I64(l), Token::RightAngle, Value::I64(r)) => Ok(Value::Boolean(l > r)),
                    (Value::I64(l), Token::LessEquals, Value::I64(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::I64(l), Token::GreaterEquals, Value::I64(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::I64(l), Token::Equals, Value::I64(r)) => Ok(Value::Boolean(l == r)),
                    (Value::I64(l), Token::NotEquals, Value::I64(r)) => Ok(Value::Boolean(l != r)),

                    // u64
                    (Value::U64(l), Token::LeftAngle, Value::U64(r)) => Ok(Value::Boolean(l < r)),
                    (Value::U64(l), Token::RightAngle, Value::U64(r)) => Ok(Value::Boolean(l > r)),
                    (Value::U64(l), Token::LessEquals, Value::U64(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::U64(l), Token::GreaterEquals, Value::U64(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::U64(l), Token::Equals, Value::U64(r)) => Ok(Value::Boolean(l == r)),
                    (Value::U64(l), Token::NotEquals, Value::U64(r)) => Ok(Value::Boolean(l != r)),

                    // f64
                    (Value::F64(l), Token::LeftAngle, Value::F64(r)) => Ok(Value::Boolean(l < r)),
                    (Value::F64(l), Token::RightAngle, Value::F64(r)) => Ok(Value::Boolean(l > r)),
                    (Value::F64(l), Token::LessEquals, Value::F64(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::F64(l), Token::GreaterEquals, Value::F64(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::F64(l), Token::Equals, Value::F64(r)) => Ok(Value::Boolean(l == r)),
                    (Value::F64(l), Token::NotEquals, Value::F64(r)) => Ok(Value::Boolean(l != r)),

                    // i128
                    (Value::I128(l), Token::LeftAngle, Value::I128(r)) => Ok(Value::Boolean(l < r)),
                    (Value::I128(l), Token::RightAngle, Value::I128(r)) => Ok(Value::Boolean(l > r)),
                    (Value::I128(l), Token::LessEquals, Value::I128(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::I128(l), Token::GreaterEquals, Value::I128(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::I128(l), Token::Equals, Value::I128(r)) => Ok(Value::Boolean(l == r)),
                    (Value::I128(l), Token::NotEquals, Value::I128(r)) => Ok(Value::Boolean(l != r)),

                    // u128
                    (Value::U128(l), Token::LeftAngle, Value::U128(r)) => Ok(Value::Boolean(l < r)),
                    (Value::U128(l), Token::RightAngle, Value::U128(r)) => Ok(Value::Boolean(l > r)),
                    (Value::U128(l), Token::LessEquals, Value::U128(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::U128(l), Token::GreaterEquals, Value::U128(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::U128(l), Token::Equals, Value::U128(r)) => Ok(Value::Boolean(l == r)),
                    (Value::U128(l), Token::NotEquals, Value::U128(r)) => Ok(Value::Boolean(l != r)),

                    // isize
                    (Value::ISize(l), Token::LeftAngle, Value::ISize(r)) => Ok(Value::Boolean(l < r)),
                    (Value::ISize(l), Token::RightAngle, Value::ISize(r)) => Ok(Value::Boolean(l > r)),
                    (Value::ISize(l), Token::LessEquals, Value::ISize(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::ISize(l), Token::GreaterEquals, Value::ISize(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::ISize(l), Token::Equals, Value::ISize(r)) => Ok(Value::Boolean(l == r)),
                    (Value::ISize(l), Token::NotEquals, Value::ISize(r)) => Ok(Value::Boolean(l != r)),

                    // usize
                    (Value::USize(l), Token::LeftAngle, Value::USize(r)) => Ok(Value::Boolean(l < r)),
                    (Value::USize(l), Token::RightAngle, Value::USize(r)) => Ok(Value::Boolean(l > r)),
                    (Value::USize(l), Token::LessEquals, Value::USize(r)) => Ok(Value::Boolean(l <= r)),
                    (Value::USize(l), Token::GreaterEquals, Value::USize(r)) => Ok(Value::Boolean(l >= r)),
                    (Value::USize(l), Token::Equals, Value::USize(r)) => Ok(Value::Boolean(l == r)),
                    (Value::USize(l), Token::NotEquals, Value::USize(r)) => Ok(Value::Boolean(l != r)),

                    _ => Err(format!("Invalid binary operation: {:?} {:?} {:?}", left_val, operator, right_val)),
                }
            }

            Expr::If { condition, then_branch, else_branch } => {
                let cond_value = self.evaluate_expression(condition)?;

                match cond_value {
                    Value::Boolean(true) => self.evaluate_expression(then_branch),
                    Value::Boolean(false) => {
                        if let Some(else_expr) = else_branch {
                            self.evaluate_expression(else_expr)
                        } else {
                            Ok(Value::Unit)
                        }
                    }
                    _ => Err("Condition must be boolean".to_string()),
                }
            }

            // !MODULE SYSTEM!
            // TEMPORARY
            Expr::Call { function, arguments } => {
                match &**function {
                    Expr::Path(path) => {
                        // handle import calls (like use std::io, io::println)
                        // handle path-based calls (like std::io::println)
                        // TEMPORARY
                        if path.segments.len() == 2 && path.segments[0].ident == "io" && path.segments[1].ident == "println" {
                            if let Some(arg) = arguments.first() {
                                let value = self.evaluate_expression(arg)?;
                                println!("io::println {value:?}"); // TEMPORARY - add display and args
                                return Ok(Value::Unit);
                            } else {
                                return Err("io::println requires an argument".to_string());
                            }
                        }
                        return Err(format!("Unknown path function: {:?}", path));
                    }

                    Expr::Identifier(name) => {
                        // Regular function calls
                        let evaluated_args: Result<Vec<Value>, String> = arguments.iter().map(|arg| self.evaluate_expression(arg)).collect();
                        let arg_values = evaluated_args?;

                        let (params, body) = if let Some(Stmt::Function { params, body, .. }) = self.find_function(name) {
                            (params.clone(), body.clone())
                        } else {
                            return Err(format!("Function '{}' not found", name));
                        };

                        let outer_scope = self.env.scopes.clone();
                        self.env.enter_scope();

                        for ((param, _), value) in params.iter().zip(arg_values) {
                            if let Pattern::Identifier { name, .. } = param {
                                self.env.scope_resolver.declare_variable(name);
                                self.env.set_variable(name, value.clone())?;
                            }
                        }

                        let result = self.execute(&body);

                        self.env.exit_scope();
                        self.env.scopes = outer_scope;

                        result
                    }
                    _ => Err(format!("Unsupported function call type: {:?}", function)),
                }
            }

            _ => Ok(Value::Unit),
        }
    }
}
