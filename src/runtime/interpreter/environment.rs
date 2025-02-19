use super::*;
use crate::runtime::scope::Scope;

/// Manages the runtime environment with scoped variable storage.
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

    /// Returns the index of the current scope.
    pub fn get_current_scope(&self) -> usize { self.scopes.len() - 1 }

    /// Adds a new block scope to the environment.
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.scope_resolver.enter_scope();
    }

    /// Removes the current block scope.
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
        self.scope_resolver.exit_scope();
    }

    /// Retrieves the value of a variable from the environment.
    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        if self.scope_resolver.resolve(name).is_none() {
            return None;
        }
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    /// Sets the value of a declared variable in the current scope.
    pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), String> {
        if let Some(symbol_info) = self.scope_resolver.resolve(name) {
            if symbol_info.mutable {
                self.make_deeply_mutable(value.clone())
            } else {
                self.make_deeply_immutable(value.clone())
            };

            if let Some(scope) = self.scopes.last_mut() {
                if let Some(existing) = scope.get(name) {
                    let new_val = value.borrow().clone();
                    *existing.borrow_mut() = new_val;
                } else {
                    scope.insert(name.to_string(), value);
                }
                Ok(())
            } else {
                Err("No active scope".to_string())
            }
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }

    /// Sets the value of a declared variable without changing mutability in the current scope.
    pub fn set_variable_raw(&mut self, name: &str, value: Value) -> Result<(), String> {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(existing) = scope.get(name) {
                *existing.borrow_mut() = value.borrow().clone();
            } else {
                scope.insert(name.to_string(), value);
            }
            Ok(())
        } else {
            Err("No active scope".to_string())
        }
    }

    /// Sets a variable in a specific scope.
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

            if let Some(existing) = scope.get(name) {
                *existing.borrow_mut() = value.borrow().clone();
            } else {
                scope.insert(name.to_string(), value);
            }
            Ok(())
        } else {
            Err(format!("Scope {} not found", scope_index))
        }
    }

    /// Updates the value of an existing variable.
    pub fn update_scoped_variable(&mut self, name: &str, value: Value, scope_index: usize) -> Result<(), String> {
        if let Some(scope) = self.scopes.get_mut(scope_index) {
            if let Some(existing) = scope.get(name) {
                *existing.borrow_mut() = value.borrow().clone();
            } else {
                scope.insert(name.to_string(), value);
            }
            Ok(())
        } else {
            Err(format!("Scope {} not found", scope_index))
        }
    }

    /// Searches for a variable within all active scopes.
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

    /// Registers a global variant or type in the environment.
    pub fn register_global_variant(&mut self, variant: &str, enum_type: &str) -> Result<(), String> {
        let type_def = self.find_variable(enum_type).ok_or_else(|| format!("Type '{}' not found", enum_type))?.1.clone();

        let global_value = {
            let borrowed = type_def.borrow();

            match borrowed.inner() {
                ValueType::EnumDef { variants, .. } => {
                    let variant_def = variants
                        .iter()
                        .find(|v| match v {
                            EnumVariant::Simple(name) | EnumVariant::Tuple(name, _) | EnumVariant::Struct(name, _) => name == variant,
                        })
                        .ok_or_else(|| format!("Variant '{}' not found in enum '{}'", variant, enum_type))?;

                    match variant_def {
                        EnumVariant::Simple(_) => val!(ValueType::Enum {
                            enum_type: enum_type.to_string(),
                            variant: variant.to_string(),
                            data: None
                        }),

                        EnumVariant::Tuple(_, field_types) => val!(ValueType::EnumConstructor {
                            enum_name: enum_type.to_string(),
                            variant_name: variant.to_string(),
                            fields: field_types.clone()
                        }),

                        EnumVariant::Struct(_, fields) => val!(ValueType::EnumStructConstructor {
                            enum_name: enum_type.to_string(),
                            variant_name: variant.to_string(),
                            fields: fields.clone()
                        }),
                    }
                }

                ValueType::StructDef { .. } if variant == enum_type => type_def.clone(),

                _ => return Err(format!("'{}' is not an enum or struct type", enum_type)),
            }
        };

        if self.scopes.is_empty() {
            self.enter_scope();
        }

        self.scopes.first_mut().ok_or("No global scope found")?.insert(variant.to_string(), global_value);

        Ok(())
    }
}
