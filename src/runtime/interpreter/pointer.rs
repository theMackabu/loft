use super::*;

use crate::runtime::value::{Value, ValueEnum, ValueType};
use std::collections::{HashMap, HashSet};
use std::{cell::RefCell, rc::Rc};

impl Environment {
    /// Recursively converts a value and its inner fields to mutable.
    /// Uses cycle detection to avoid processing self-referential values.
    pub fn make_deeply_mutable(&self, value: Value) -> Value {
        let mut visited: HashSet<*const RefCell<ValueEnum>> = HashSet::new();
        self.make_deeply_mutable_internal(value, &mut visited)
    }

    /// Recursively converts a value and its inner fields to immutable.
    /// Uses cycle detection to avoid processing self-referential values.
    pub fn make_deeply_immutable(&self, value: Value) -> Value {
        let mut visited: HashSet<*const RefCell<ValueEnum>> = HashSet::new();
        self.make_deeply_immutable_internal(value, &mut visited)
    }

    /// Internal helper that performs recursion with cycle detection.
    fn make_deeply_mutable_internal(&self, value: Value, visited: &mut HashSet<*const RefCell<ValueEnum>>) -> Value {
        let ptr = Rc::as_ptr(&value);

        if visited.contains(&ptr) {
            return value;
        }

        visited.insert(ptr);

        let new_inner = {
            let borrowed = value.borrow();
            match borrowed.inner() {
                ValueType::Struct { name, fields } => {
                    let mut mutable_fields = HashMap::new();
                    for (field_name, field_value) in fields {
                        let mutable_value = self.make_deeply_mutable_internal(field_value.clone(), visited);
                        mutable_fields.insert(field_name.clone(), mutable_value);
                    }

                    ValueType::Struct {
                        name: name.clone(),
                        fields: mutable_fields,
                    }
                }

                ValueType::Reference {
                    source_name,
                    source_scope,
                    original_ptr,
                    _undropped,
                } => ValueType::Reference {
                    source_name: source_name.clone(),
                    source_scope,
                    original_ptr,
                    _undropped: _undropped.clone(),
                },

                other => other.clone(),
            }
        };

        {
            let mut borrowed = value.borrow_mut();
            *borrowed = ValueEnum::Mutable(new_inner);
        }

        visited.remove(&ptr);
        return value;
    }

    fn make_deeply_immutable_internal(&self, value: Value, visited: &mut HashSet<*const RefCell<ValueEnum>>) -> Value {
        let ptr = Rc::as_ptr(&value);

        if visited.contains(&ptr) {
            return value;
        }

        visited.insert(ptr);

        let new_inner = {
            let borrowed = value.borrow();
            match borrowed.inner() {
                ValueType::Struct { name, fields } => {
                    let mut immutable_fields = HashMap::new();
                    for (field_name, field_value) in fields {
                        let immutable_value = self.make_deeply_immutable_internal(field_value.clone(), visited);
                        immutable_fields.insert(field_name.clone(), immutable_value);
                    }

                    ValueType::Struct {
                        name: name.clone(),
                        fields: immutable_fields,
                    }
                }

                ValueType::Reference {
                    source_name,
                    source_scope,
                    original_ptr,
                    _undropped,
                } => ValueType::Reference {
                    source_name: source_name.clone(),
                    source_scope,
                    original_ptr,
                    _undropped: _undropped.clone(),
                },

                other => other.clone(),
            }
        };

        {
            let mut borrowed = value.borrow_mut();
            *borrowed = ValueEnum::Immutable(new_inner);
        }

        visited.remove(&ptr);
        return value;
    }
}
