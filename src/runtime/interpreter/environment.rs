use super::*;
use crate::runtime::scope::Scope;
use std::{cell::RefCell, rc::Rc};

/// Manages the runtime environment with scoped variable storage.
///
/// The `Environment` struct handles nested scopes, variable resolution, and
/// updates during execution. It ensures variables are properly scoped and
/// accessible in the current context.
pub struct Environment {
    pub scopes: Vec<HashMap<String, Value>>,
    pub scope_resolver: Scope,
    pub next_ref_id: usize,
}

impl Environment {
    /// Creates a new environment with a global scope.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            scope_resolver: Scope::new(),
            next_ref_id: 0,
        }
    }

    /// Adds a new block scope to the environment.
    ///
    /// Pushes an empty scope onto the stack and updates the scope resolver.
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.scope_resolver.enter_scope();
    }

    /// Removes the current block scope.
    ///
    /// Pops the most recent scope and updates the scope resolver.
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
        self.scope_resolver.exit_scope();
    }

    pub fn get_variable_ref(&self, name: &str) -> Option<&Value> {
        if self.scope_resolver.resolve(name).is_none() {
            return None;
        }
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    pub fn get_variable_source(&self, name: &str) -> Option<(String, usize)> { self.find_variable(name).map(|(scope_index, _)| (name.to_string(), scope_index)) }

    pub fn resolve_effective_origin(&self, name: &str, origin: &Option<(String, usize)>) -> (String, usize) {
        let current_scope = self.get_current_scope();
        if name == "self" {
            if let Some((outer_name, outer_scope)) = origin {
                return (outer_name.to_owned(), *outer_scope);
            }
        }
        (name.to_owned(), current_scope)
    }

    /// Retrieves the value of a variable from the environment.
    ///
    /// Searches all active scopes for the variable. Returns `None` if the variable
    /// is not found or not declared.
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.get_variable_ref(name).and_then(|value| match value.inner() {
            ValueType::Reference { source_name, source_scope, .. } => {
                if let (Some(source_name), Some(source_scope)) = (source_name, source_scope) {
                    if let Some(scope) = self.scopes.get(source_scope) {
                        scope.get(&source_name).or(Some(value))
                    } else {
                        Some(value)
                    }
                } else {
                    Some(value)
                }
            }
            _ => Some(value),
        })
    }

    /// Sets the value of a declared variable in the current scope.
    ///
    /// Ensures the variable exists before setting its value. Returns an error if
    /// the variable is not declared or if no active scope exists.
    pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        if let Some(symbol_info) = self.scope_resolver.resolve(name) {
            let final_value = if symbol_info.mutable {
                self.make_deeply_mutable(value)
            } else {
                self.make_deeply_immutable(value)
            };

            if let Some(scope) = self.scopes.last_mut() {
                scope.insert(name.to_string(), final_value);
                Ok(())
            } else {
                Err("No active scope".to_string())
            }
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }

    /// Sets a variable in a specific scope.
    ///
    /// Creates or updates a variable in the specified scope. Returns an error if
    /// the scope index is invalid.
    pub fn set_scoped_variable(&mut self, name: &str, value: Value, scope_index: usize, mutable: bool) -> Result<(), String> {
        if let Some(scope) = self.scopes.get_mut(scope_index) {
            if self.scope_resolver.resolve(name).is_none() {
                self.scope_resolver.declare_variable_in_scope(name, mutable, scope_index)?;
            } else {
                let symbol_info = self.scope_resolver.resolve(name).ok_or_else(|| format!("Variable '{}' not found", name))?;
                if !symbol_info.mutable {
                    return Err(format!("Cannot assign to immutable variable '{}'", name));
                }
            }
            scope.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(format!("Scope {} not found", scope_index))
        }
    }

    /// Updates the value of an existing variable.
    ///
    /// Searches for the variable in indexed scopes and updates its value if
    /// found. Returns an error if the variable does not exist.
    pub fn update_scoped_variable(&mut self, name: &str, value: Value, scope_index: usize) -> Result<(), String> {
        if let Some(scope) = self.scopes.get_mut(scope_index) {
            scope.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(format!("Scope {} not found", scope_index))
        }
    }

    /// update.
    pub fn find_variable(&self, name: &str) -> Option<(usize, &Value)> {
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(value) = scope.get(name) {
                return Some((index, value));
            }
        }
        None
    }

    /// Generates a unique temporary reference name.
    pub fn generate_temp_reference_name(&mut self) -> String {
        let name = format!("__ref_{}", self.next_ref_id);
        self.next_ref_id += 1;
        name
    }

    /// Returns the current scope (e.g., the index of the current scope).
    pub fn get_current_scope(&self) -> usize { self.scopes.len() - 1 }

    pub fn make_deeply_mutable(&self, value: Value) -> Value {
        let inner = match value.inner() {
            ValueType::Struct { name, fields } => {
                let mut mutable_fields = HashMap::new();
                for (field_name, field_ref) in fields {
                    let mutable_value = self.make_deeply_mutable(field_ref.borrow().clone());
                    mutable_fields.insert(field_name, Rc::new(RefCell::new(mutable_value)));
                }
                ValueType::Struct { name, fields: mutable_fields }
            }

            ValueType::Reference { data, source_name, source_scope } => ValueType::Reference {
                data: data.map(|d| self.make_deeply_mutable(d)),
                source_name,
                source_scope,
            },

            other => other,
        };

        Box::new(ValueEnum::Mutable(inner))
    }

    pub fn make_deeply_immutable(&self, value: Value) -> Value {
        let inner = match value.inner() {
            ValueType::Struct { name, fields } => {
                let mut immutable_fields = HashMap::new();
                for (field_name, field_ref) in fields {
                    let immutable_value = self.make_deeply_immutable(field_ref.borrow().clone());
                    immutable_fields.insert(field_name, Rc::new(RefCell::new(immutable_value)));
                }
                ValueType::Struct { name, fields: immutable_fields }
            }

            ValueType::Reference { data, source_name, source_scope } => ValueType::Reference {
                data: data.map(|d| self.make_deeply_immutable(d)),
                source_name,
                source_scope,
            },

            other => other,
        };

        Box::new(ValueEnum::Immutable(inner))
    }
}
