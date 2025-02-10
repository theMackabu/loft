use crate::parser::ast::{Expr, Pattern, Stmt};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Variable,
    Function,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub struct Environment {
    scopes: Vec<HashMap<String, SymbolInfo>>,
}

impl Environment {
    pub fn new() -> Self { Environment { scopes: vec![HashMap::new()] } }

    pub fn enter_scope(&mut self) { self.scopes.push(HashMap::new()); }

    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn declare_variable(&mut self, name: &str) {
        if let Some(current) = self.scopes.last_mut() {
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Variable,
                },
            );
        }
    }

    pub fn declare_function(&mut self, name: &str) -> Result<(), String> {
        if let Some(current) = self.scopes.last_mut() {
            if let Some(existing) = current.get(name) {
                if existing.kind == DeclKind::Function {
                    return Err(format!("Function `{}` is already declared in this scope", name));
                }
            }
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Function,
                },
            );
            Ok(())
        } else {
            Err("No active scope found".to_owned())
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }
}

pub fn resolve_program(statements: &[Stmt]) -> Result<(), String> {
    let mut env = Environment::new();
    for stmt in statements {
        resolve_stmt(stmt, &mut env)?;
    }
    Ok(())
}

fn resolve_stmt(stmt: &Stmt, env: &mut Environment) -> Result<(), String> {
    use crate::parser::ast::Stmt::*;

    match stmt {
        Let { pattern, initializer, .. } => {
            if let Some(init_expr) = initializer {
                resolve_expr(init_expr, env)?;
            }
            if let Some(name) = extract_identifier(pattern) {
                env.declare_variable(&name);
            }
            Ok(())
        }

        Function { name, params, body, .. } => {
            env.declare_function(name)?;
            env.enter_scope();

            for (pat, _ty) in params {
                if let Some(param_name) = extract_identifier(pat) {
                    env.declare_variable(&param_name);
                }
            }

            for s in body {
                resolve_stmt(s, env)?;
            }

            env.exit_scope();
            Ok(())
        }

        Module { body, .. } => {
            env.enter_scope();
            for s in body {
                resolve_stmt(s, env)?;
            }
            env.exit_scope();
            Ok(())
        }
        ExpressionStmt(expr) | ExpressionValue(expr) => resolve_expr(expr, env),

        // more statement types
        _ => Ok(()),
    }
}

fn resolve_expr(expr: &Expr, env: &mut Environment) -> Result<(), String> {
    use crate::parser::ast::Expr;

    match expr {
        Expr::Block { statements, value, .. } => {
            env.enter_scope();
            for s in statements {
                resolve_stmt(s, env)?;
            }
            if let Some(inner) = value {
                resolve_expr(inner, env)?;
            }
            env.exit_scope();
            Ok(())
        }
        Expr::Call { function, arguments } => {
            resolve_expr(function, env)?;
            for arg in arguments {
                resolve_expr(arg, env)?;
            }
            Ok(())
        }
        Expr::If {
            condition, then_branch, else_branch, ..
        } => {
            resolve_expr(condition, env)?;
            resolve_expr(then_branch, env)?;
            if let Some(else_expr) = else_branch {
                resolve_expr(else_expr, env)?;
            }
            Ok(())
        }
        //  additional expression variants
        _ => Ok(()),
    }
}

fn extract_identifier(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Identifier { name, .. } => Some(name.clone()),
        Pattern::Reference { pattern, .. } => extract_identifier(pattern),
        _ => None,
    }
}
