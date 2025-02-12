mod assign;
mod cast;

use super::{scope::Environment as ScopeEnv, value::Value};
use crate::parser::{ast::*, lexer::*};
use crate::{impl_binary_ops, impl_promote_to_type};
use std::{cell::RefCell, collections::HashMap};

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
        let main_func = self.find_function("main").ok_or("No main function found")?;
        let result = self.execute_statement(&main_func.to_owned())?;

        match result {
            Value::Enum { enum_type, variant, data } if enum_type == "Result" => match variant.as_str() {
                "Ok" => Ok(*data.unwrap_or_else(|| Box::new(Value::Unit))),
                "Err" => Err(data.map(|d| *d).unwrap_or(Value::Unit).to_string()),
                _ => Ok(Value::Enum { enum_type, variant, data }),
            },

            other => Ok(other),
        }
    }

    pub fn execute(&mut self, statements: &[Stmt]) -> Result<Value, String> {
        let mut last_value = Value::Unit;

        for stmt in statements {
            let result = self.execute_statement(stmt)?;
            if let Value::Return(val) = result {
                return Ok(*val);
            }
            last_value = result;
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
                    self.env.scope_resolver.declare_variable(name, false);
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Static { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    self.env.scope_resolver.declare_variable(name, false);
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
            Stmt::ExpressionValue(expr) => self.evaluate_expression(expr),

            Stmt::ExpressionStmt(expr) => {
                self.evaluate_expression(expr)?;
                Ok(Value::Unit)
            }

            Stmt::Return(expr) => match expr {
                Some(e) => {
                    let val = self.evaluate_expression(e)?;
                    Ok(Value::Return(Box::new(val)))
                }
                None => Ok(Value::Return(Box::new(Value::Unit))),
            },

            Stmt::Module { body, .. } => {
                self.env.enter_scope();
                let result = self.execute(body)?;
                self.env.exit_scope();
                Ok(result)
            }

            Stmt::Let {
                pattern,
                initializer,
                type_annotation,
                ..
            } => {
                let value = if let Some(init) = initializer {
                    let init_value = self.evaluate_expression(init)?;

                    if let Some(target_type) = type_annotation {
                        self.perform_cast(init_value, target_type)?
                    } else {
                        init_value
                    }
                } else {
                    Value::Unit
                };

                if let Pattern::Identifier { name, mutable } = pattern {
                    self.env.scope_resolver.declare_variable(name, *mutable);
                    self.env.set_variable(name, value)?;
                }

                Ok(Value::Unit)
            }

            Stmt::Function { name, params, body, return_type, .. } => {
                if name == "main" {
                    if !params.is_empty() {
                        return Err("main function cannot have parameters".into());
                    }

                    if let Some(ret_type) = return_type {
                        match ret_type {
                            Type::Path(path)
                                if matches! { path.segments.as_slice(),
                                    [seg] if seg.ident == "i32" && seg.generics.is_empty()
                                } => {}

                            Type::Path(path)
                                if matches! { path.segments.as_slice(),
                                    [seg] if seg.ident == "Result" && seg.generics.len() == 2
                                } => {}

                            _ => return Err("main function can only return i32, Result<T, E>, or nothing".into()),
                        }
                    }
                }

                self.env.scope_resolver.declare_function(name)?;
                self.env.enter_scope();

                for (pat, _) in params {
                    if let Pattern::Identifier { name, mutable } = pat {
                        self.env.scope_resolver.declare_variable(name, *mutable);
                    }
                }

                let result = match self.execute(body)? {
                    Value::Return(val) => *val,
                    other => other,
                };

                self.env.exit_scope();
                Ok(result)
            }

            _ => Ok(Value::Unit),
        }
    }

    fn evaluate_expression(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Path(path) => {
                if path.segments.len() == 2 {
                    let enum_type = &path.segments[0].ident;
                    let variant = &path.segments[1].ident;
                    Ok(Value::Enum {
                        enum_type: enum_type.clone(),
                        variant: variant.clone(),
                        data: None,
                    })
                } else {
                    Err(format!("Invalid path expression: {:?}", path))
                }
            }

            Expr::Cast { expr, target_type } => {
                let value = self.evaluate_expression(expr)?;
                self.perform_cast(value, target_type)
            }

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
                    let result = self.execute_statement(stmt)?;
                    if let Value::Return(_) = result {
                        self.env.exit_scope();
                        return Ok(result);
                    }
                }

                let result = if let Some(expr) = value { self.evaluate_expression(expr)? } else { Value::Unit };

                self.env.exit_scope();
                Ok(result)
            }

            Expr::Dereference { operand } => {
                let value = self.evaluate_expression(operand)?;

                if value.is_ref_mut() {
                    match value.ref_mut_val() {
                        Ok(inner_value) => Ok(inner_value.clone()),
                        Err(err) => return Err(format!("Dereference error: {}", err)),
                    }
                } else {
                    match value.ref_val() {
                        Ok(inner_value) => Ok(inner_value.clone()),
                        Err(err) => return Err(format!("Dereference error: {}", err)),
                    }
                }
            }

            Expr::Reference { mutable, operand } => {
                let value = self.evaluate_expression(operand.as_ref())?;
                Ok(Value::Reference {
                    data: Box::new(RefCell::new(value)),
                    mutable: *mutable,
                })
            }

            Expr::Binary { left, operator, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;

                if let Ok(promoted) = impl_promote_to_type! {
                    (&right_val, &left_val),
                    (I16, i16),
                    (U16, u16),
                    (I32, i32),
                    (U32, u32),
                    (I64, i64),
                    (U64, u64),
                    (ISize, isize),
                    (USize, usize)
                } {
                    return impl_binary_ops! {
                        (&left_val, operator, &promoted),
                        I8, U8, I16, U16, I32, U32, F32,
                        I64, U64, F64, I128, U128,
                        ISize, USize
                    };
                }

                if let Ok(promoted) = impl_promote_to_type! {
                    (&left_val, &right_val),
                    (I16, i16),
                    (U16, u16),
                    (I32, i32),
                    (U32, u32),
                    (I64, i64),
                    (U64, u64),
                    (ISize, isize),
                    (USize, usize)
                } {
                    return impl_binary_ops! {
                        (&promoted, operator, &right_val),
                        I8, U8, I16, U16, I32, U32, F32,
                        I64, U64, F64, I128, U128,
                        ISize, USize
                    };
                }
                Err(format!("Cannot perform operation between {:?} and {:?}", left_val, right_val))
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

            Expr::Match { value, arms } => {
                let match_value = self.evaluate_expression(value)?;

                for arm in arms {
                    if self.pattern_matches(&arm.pattern, &match_value)? {
                        if let Some(guard) = &arm.guard {
                            if !self.evaluate_guard(guard)? {
                                continue;
                            }
                        }
                        return self.evaluate_expression(&arm.body);
                    }
                }

                Err("No matching pattern found".to_string())
            }

            Expr::Assignment { target, value } => {
                let value = self.evaluate_expression(value)?;

                if let Some(symbol_info) = self.env.scope_resolver.resolve(target) {
                    if let Some(current_value) = self.env.get_variable(target) {
                        if symbol_info.mutable || matches!(current_value, Value::Unit) {
                            self.env.set_variable(target, value)?;
                            Ok(Value::Unit)
                        } else {
                            Err(format!("Cannot assign to immutable variable '{}'", target))
                        }
                    } else {
                        Err(format!("Variable '{}' not found", target))
                    }
                } else {
                    Err(format!("Variable '{}' not found", target))
                }
            }

            Expr::CompoundAssignment { target, operator, value } => match target.as_ref() {
                Expr::Identifier(name) => {
                    if let Some(symbol_info) = self.env.scope_resolver.resolve(name) {
                        if !symbol_info.mutable {
                            return Err(format!("Cannot modify immutable variable '{}'", name));
                        }

                        let left_val = self.env.get_variable(name).ok_or_else(|| format!("Variable '{}' not found", name))?.clone();
                        let right_val = self.evaluate_expression(value)?;
                        let result = self.evaluate_compound_assignment(&left_val, operator, &right_val)?;

                        self.env.set_variable(name, result)?;
                        Ok(Value::Unit)
                    } else {
                        Err(format!("Variable '{}' not found", name))
                    }
                }
                _ => Err("Invalid assignment target".to_string()),
            },

            // !MODULE SYSTEM!
            // TEMPORARY
            Expr::Call { function, arguments } => {
                match &**function {
                    Expr::Path(path) if path.segments.len() == 2 => {
                        // handle import calls (like use std::io, io::println)
                        // handle path-based calls (like std::io::println)
                        // TEMPORARY

                        if path.segments[0].ident == "io" && path.segments[1].ident == "println" {
                            if let Some(arg) = arguments.first() {
                                let value = self.evaluate_expression(arg)?;
                                println!("{value}"); // TEMPORARY - add display and args
                                return Ok(Value::Unit);
                            } else {
                                return Err("io::println requires an argument".to_string());
                            }
                        }

                        let enum_type = &path.segments[0].ident;
                        let variant = &path.segments[1].ident;
                        if arguments.len() == 1 {
                            let value = self.evaluate_expression(&arguments[0])?;
                            Ok(Value::Enum {
                                enum_type: enum_type.clone(),
                                variant: variant.clone(),
                                data: Some(Box::new(value)),
                            })
                        } else {
                            Err("Enum variant with data must have exactly one argument".to_string())
                        }
                    }

                    Expr::Identifier(name) => {
                        if name == "main" {
                            return Err("main function cannot be called directly".to_string());
                        }

                        let evaluated_args: Result<Vec<Value>, String> = arguments.iter().map(|arg| self.evaluate_expression(arg)).collect();
                        let arg_values = evaluated_args?;

                        let (params, body) = if let Some(Stmt::Function { params, body, .. }) = self.find_function(name) {
                            (params.clone(), body.clone())
                        } else {
                            return Err(format!("Function '{}' not found", name));
                        };

                        if arg_values.len() != params.len() {
                            return Err(format!("Function '{}' expects {} arguments but got {}", name, params.len(), arg_values.len()));
                        }

                        let outer_scope = self.env.scopes.clone();
                        self.env.enter_scope();

                        for ((param, _type), value) in params.iter().zip(arg_values) {
                            if let Pattern::Identifier { name, mutable } = param {
                                self.env.scope_resolver.declare_variable(name, *mutable);
                                self.env.set_variable(name, value)?;
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

    fn pattern_matches(&mut self, pattern: &Pattern, value: &Value) -> Result<bool, String> {
        match (pattern, value) {
            (Pattern::Literal(expr), value) => {
                let pattern_value = self.evaluate_expression(expr)?;
                Ok(&pattern_value == value)
            }

            (Pattern::Path(path), Value::Enum { variant, data, enum_type }) => {
                if path.segments.len() == 2 {
                    Ok(path.segments[0].ident == *enum_type && path.segments[1].ident == *variant && data.is_none())
                } else {
                    Ok(false)
                }
            }

            (Pattern::TupleStruct { path, elements }, Value::Enum { variant, data, enum_type }) => {
                if path.segments.len() == 2 && path.segments[0].ident == *enum_type && path.segments[1].ident == *variant {
                    match (elements.len(), data) {
                        (1, Some(inner_value)) => self.pattern_matches(&elements[0], inner_value),
                        (0, None) => Ok(true),
                        _ => Ok(false),
                    }
                } else {
                    Ok(false)
                }
            }

            (Pattern::Tuple(elements), Value::Enum { data: Some(inner_value), .. }) => {
                if elements.len() == 1 {
                    self.pattern_matches(&elements[0], inner_value)
                } else {
                    Ok(false)
                }
            }

            (Pattern::Identifier { name, mutable }, value) => {
                self.env.scope_resolver.declare_variable(name, *mutable);
                self.env.set_variable(name, value.clone())?;
                Ok(true)
            }

            (Pattern::Wildcard, _) => Ok(true),

            _ => Ok(false),
        }
    }

    fn evaluate_guard(&mut self, guard: &Expr) -> Result<bool, String> {
        match self.evaluate_expression(guard)? {
            Value::Boolean(b) => Ok(b),
            _ => Err("Guard expression must evaluate to a boolean".to_string()),
        }
    }
}
