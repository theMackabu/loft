use super::scope::Environment as ScopeEnv;
use crate::parser::{ast::*, lexer::*};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),

    Str(&'static str),
    Boolean(bool),

    Unit,
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
}

impl Interpreter {
    pub fn new(ast: &Vec<Stmt>) -> Result<Self, String> {
        super::scope::resolve_program(ast)?;

        let mut interpreter = Self { env: Environment::new() };

        interpreter.declare_globals(ast)?;

        Ok(interpreter)
    }
    pub fn execute(&mut self, statements: &[Stmt]) -> Result<Value, String> {
        let mut last_value = Value::Unit;

        for stmt in statements {
            last_value = self.execute_statement(stmt)?;
        }

        Ok(last_value)
    }

    fn declare_globals(&mut self, statements: &[Stmt]) -> Result<(), String> {
        for stmt in statements {
            match stmt {
                Stmt::Const { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Static { name, initializer, .. } => {
                    let value = self.evaluate_expression(initializer)?;
                    if let Some(scope) = self.env.scopes.first_mut() {
                        scope.insert(name.clone(), value);
                    }
                }

                Stmt::Module { name, body, .. } => {
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
            Stmt::Let { pattern, initializer, .. } => {
                let value = if let Some(init) = initializer { self.evaluate_expression(init)? } else { Value::Unit };

                if let Pattern::Identifier { name, .. } = pattern {
                    self.env.scope_resolver.declare_variable(name);
                    self.env.set_variable(name, value)?;
                }
                Ok(Value::Unit)
            }

            Stmt::Function { name, params, body, .. } => {
                self.env.scope_resolver.declare_function(name)?;
                self.env.enter_scope();

                // Declare parameters in new scope
                for (pat, _) in params {
                    if let Pattern::Identifier { name, .. } = pat {
                        self.env.scope_resolver.declare_variable(name);
                    }
                }

                let result = self.execute(body)?;
                self.env.exit_scope();
                Ok(result)
            }

            Stmt::Module { body, .. } => {
                self.env.enter_scope();
                let result = self.execute(body)?;
                self.env.exit_scope();
                Ok(result)
            }

            Stmt::ExpressionStmt(expr) => self.evaluate_expression(expr),

            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    self.evaluate_expression(e)
                } else {
                    Ok(Value::Unit)
                }
            }

            _ => Ok(Value::Unit),
        }
    }

    fn evaluate_expression(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Integer(value, _) => Ok(Value::Integer(*value)),

            Expr::String(value) => Ok(Value::Str(Box::leak(value.to_owned().into_boxed_str()))),

            Expr::Identifier(name) => self.env.get_variable(name).cloned().ok_or_else(|| format!("Undefined variable: {}", name)),

            Expr::Block { statements, value, .. } => {
                self.env.enter_scope();

                for stmt in statements {
                    self.execute_statement(stmt)?;
                }

                let result = if let Some(expr) = value { self.evaluate_expression(expr)? } else { Value::Unit };

                self.env.exit_scope();
                Ok(result)
            }

            Expr::Binary { left, operator, right } => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;

                match (left_val, operator, right_val) {
                    (Value::Integer(l), Token::LeftAngle, Value::Integer(r)) => Ok(Value::Boolean(l < r)),
                    _ => Err("Invalid binary operation".to_string()),
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

            _ => Ok(Value::Unit),
        }
    }
}
