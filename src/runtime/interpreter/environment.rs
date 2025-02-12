use super::*;
use crate::runtime::scope::Scope;

/// Manages the runtime environment with scoped variable storage.
///
/// The `Environment` struct handles nested scopes, variable resolution, and
/// updates during execution. It ensures variables are properly scoped and
/// accessible in the current context.
pub struct Environment {
    pub scopes: Vec<HashMap<String, Value>>,
    pub scope_resolver: Scope,
}

impl Environment {
    /// Creates a new environment with a global scope.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            scope_resolver: Scope::new(),
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

    /// Retrieves the value of a variable from the environment.
    ///
    /// Searches all active scopes for the variable. Returns `None` if the variable
    /// is not found or not declared.
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        if self.scope_resolver.resolve(name).is_none() {
            return None;
        }

        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    /// Sets the value of a declared variable in the current scope.
    ///
    /// Ensures the variable exists before setting its value. Returns an error if
    /// the variable is not declared or if no active scope exists.
    pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        match self.scope_resolver.resolve(name) {
            Some(_) => {
                if let Some(scope) = self.scopes.last_mut() {
                    scope.insert(name.to_string(), value);
                    Ok(())
                } else {
                    Err("No active scope".to_string())
                }
            }
            None => Err(format!("Variable '{}' not declared", name)),
        }
    }

    /// Updates the value of an existing variable.
    ///
    /// Searches for the variable in all active scopes and updates its value if
    /// found. Returns an error if the variable does not exist.
    pub fn update_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        if let Some(scope) = self.scopes.iter_mut().rev().find(|scope| scope.contains_key(name)) {
            scope.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(format!("Variable '{}' not found in any scope", name))
        }
    }
}
