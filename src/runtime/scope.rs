use crate::parser::ast::{Expr, Stmt};
use crate::util::extract_identifier_info;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Variable,
    Function,
    Module,
    Reference { mutable: bool },
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub kind: DeclKind,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Scope {
    scopes: Vec<HashMap<String, SymbolInfo>>,
}

impl Scope {
    pub fn new() -> Self { Scope { scopes: vec![HashMap::new()] } }

    pub fn enter_scope(&mut self) { self.scopes.push(HashMap::new()); }

    pub fn exit_scope(&mut self) { self.scopes.pop(); }

    pub fn resolve_program(&self, statements: &[Stmt]) -> Result<(), String> {
        let mut env = Self::new();

        for stmt in statements {
            resolve_stmt(stmt, &mut env)?;
        }

        Ok(())
    }

    pub fn declare_variable(&mut self, name: &str, mutable: bool) {
        if let Some(current) = self.scopes.last_mut() {
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Variable,
                    mutable,
                },
            );
        }
    }

    pub fn declare_variable_in_scope(&mut self, name: &str, mutable: bool, scope_index: usize) -> Result<(), String> {
        if let Some(scope) = self.scopes.get_mut(scope_index) {
            if scope.contains_key(name) {
                return Err(format!("Variable '{}' already declared in this scope", name));
            }

            scope.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Variable,
                    mutable,
                },
            );
            Ok(())
        } else {
            Err(format!("Scope {} not found", scope_index))
        }
    }

    pub fn declare_reference(&mut self, name: &str, mutable: bool) {
        if let Some(current) = self.scopes.last_mut() {
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    name: name.to_owned(),
                    kind: DeclKind::Reference { mutable },
                    mutable,
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
                    mutable: false,
                    name: name.to_owned(),
                    kind: DeclKind::Function,
                },
            );
            Ok(())
        } else {
            Err("No active scope found".to_owned())
        }
    }

    pub fn declare_module(&mut self, name: &str) -> Result<(), String> {
        if let Some(current) = self.scopes.last_mut() {
            if current.contains_key(name) {
                return Err(format!("Module `{}` is already declared in this scope", name));
            }
            current.insert(
                name.to_owned(),
                SymbolInfo {
                    mutable: false,
                    name: name.to_owned(),
                    kind: DeclKind::Module,
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

fn resolve_stmt(stmt: &Stmt, env: &mut Scope) -> Result<(), String> {
    use crate::parser::ast::Stmt::*;

    match stmt {
        Let { pattern, initializer, .. } => {
            if let Some(init_expr) = initializer {
                resolve_expr(init_expr, env)?;
            }
            if let Some((name, mutable)) = extract_identifier_info(pattern) {
                env.declare_variable(&name, mutable);
            }
            Ok(())
        }

        Function { name, params, body, .. } => {
            env.declare_function(name)?;
            env.enter_scope();

            // add type checking _ty
            for (pat, _ty) in params {
                if let Some((param_name, mutable)) = extract_identifier_info(pat) {
                    env.declare_variable(&param_name, mutable);
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

fn resolve_expr(expr: &Expr, env: &mut Scope) -> Result<(), String> {
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
