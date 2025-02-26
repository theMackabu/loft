use std::{cell::RefCell, rc::Rc};

use crate::{
    parser::ast::Pattern,
    runtime::scope::BindingKind::{self, *},
    runtime::value::{Value, ValueType},
};

pub fn is_self_parameter(pat: &Pattern) -> bool {
    match pat {
        Pattern::Identifier { name, .. } => name == "self",
        Pattern::Reference { pattern, .. } => is_self_parameter(pattern),
        _ => false,
    }
}

pub fn extract_identifier_info(pattern: &Pattern) -> Vec<(String, BindingKind)> {
    match pattern {
        Pattern::Identifier { name, mutable } => vec![(name.clone(), Variable(*mutable))],

        Pattern::Reference { pattern, mutable } => {
            if let Pattern::Identifier { name, .. } = &**pattern {
                vec![(name.clone(), Reference(*mutable))]
            } else {
                extract_identifier_info(pattern)
            }
        }

        Pattern::BindingPattern { name, mutable, subpattern } => {
            let mut ids = vec![(name.clone(), Variable(*mutable))];
            ids.extend(extract_identifier_info(subpattern));
            ids
        }

        Pattern::Tuple(elements) => {
            let mut ids = Vec::new();
            for element in elements {
                ids.extend(extract_identifier_info(element));
            }
            ids
        }

        Pattern::TupleStruct { elements, .. } => {
            let mut ids = Vec::new();
            for element in elements {
                ids.extend(extract_identifier_info(element));
            }
            ids
        }

        Pattern::Struct { fields, .. } => {
            let mut ids = Vec::new();
            for (_, field_pattern) in fields {
                ids.extend(extract_identifier_info(field_pattern));
            }
            ids
        }

        Pattern::Or(patterns) => {
            if patterns.is_empty() {
                return vec![];
            }

            let first_ids = extract_identifier_info(&patterns[0]);

            for (i, pattern) in patterns.iter().enumerate().skip(1) {
                let branch_ids = extract_identifier_info(pattern);
                if branch_ids != first_ids {
                    panic!(
                        "All branches in an or pattern must bind the same identifiers in the same order. Branch {} mismatch: expected {:?}, got {:?}.",
                        i, first_ids, branch_ids
                    );
                }
            }

            first_ids
        }

        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Path(_) => Vec::new(),
    }
}

pub fn unwrap_value(val: &Value) -> Value {
    let mut current = val.clone();

    loop {
        let borrowed = current.borrow();
        match &borrowed.inner() {
            ValueType::Reference { _undropped, .. } => {
                let next = _undropped.clone();
                drop(borrowed);
                current = next;
            }
            _ => break,
        }
    }

    Rc::new(RefCell::new(current.borrow().clone().into_immutable()))
}
