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

    pub fn get_variable_ref(&self, name: &str) -> Option<&Value> {
        if self.scope_resolver.resolve(name).is_none() {
            return None;
        }
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    /// Retrieves the value of a variable from the environment.
    ///
    /// Searches all active scopes for the variable. Returns `None` if the variable
    /// is not found or not declared.
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.get_variable_ref(name).map(|value| {
            if let Value::Reference { source_name, source_scope, .. } = value {
                if let Some(scope) = self.scopes.get(*source_scope) {
                    scope.get(source_name).unwrap_or(value)
                } else {
                    value
                }
            } else {
                value
            }
        })
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
    /// Searches for the variable in indexed scopes and updates its value if
    /// found. Returns an error if the variable does not exist.
    pub fn update_scoped_variable(&mut self, name: &str, value: Value, scope_index: usize) -> Result<(), String> {
        println!("Updating variable '{}' in scope {} with value {:?}", name, scope_index, value);
        println!("Current scopes: {:?}", self.scopes);

        if let Some(scope) = self.scopes.get_mut(scope_index) {
            scope.insert(name.to_string(), value);
            println!("After update, scopes: {:?}", self.scopes);
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
}
