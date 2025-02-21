use super::*;

impl<'st> Interpreter<'st> {
    pub fn match_pattern(&mut self, pattern: &Pattern, value: &Value, declare_variables: bool) -> Result<(), String> {
        if !self.pattern_matches(pattern, value)? {
            return Err("Pattern does not match value".to_string());
        }

        if declare_variables {
            self.bind_pattern_variables(pattern, value)?;
        }

        Ok(())
    }

    fn bind_pattern_variables(&mut self, pattern: &Pattern, value: &Value) -> Result<(), String> {
        match pattern {
            Pattern::Identifier { name, mutable } => {
                self.env.scope_resolver.declare_variable(name, *mutable);
                self.env.set_variable(name, value.clone())?;
            }

            Pattern::Reference { mutable, pattern } => {
                let ref_value = match value.borrow().inner() {
                    ValueType::Reference { .. } => value.clone(),
                    _ => {
                        let reference = ValueType::Reference {
                            original_ptr: value.as_ptr(),
                            source_name: None,
                            source_scope: None,
                            _undropped: value.clone(),
                        };
                        if *mutable { val!(mut reference) } else { val!(reference) }
                    }
                };
                self.bind_pattern_variables(pattern, &ref_value)?;
            }

            Pattern::Tuple(patterns) => {
                if let ValueType::Tuple(values) | ValueType::Enum { data: Some(values), .. } = value.borrow().inner() {
                    for (pattern, value) in patterns.iter().zip(values.iter()) {
                        self.bind_pattern_variables(pattern, value)?;
                    }
                }
            }

            Pattern::TupleStruct { elements, .. } => {
                if let ValueType::Enum { data: Some(values), .. } = value.borrow().inner() {
                    for (pattern, value) in elements.iter().zip(values.iter()) {
                        self.bind_pattern_variables(pattern, value)?;
                    }
                }
            }

            Pattern::Or(patterns) => {
                // Bind using first matching pattern
                for pattern in patterns {
                    if self.pattern_matches(pattern, value)? {
                        return self.bind_pattern_variables(pattern, value);
                    }
                }
            }

            // These patterns don't bind variables
            Pattern::Path(_) | Pattern::Wildcard | Pattern::Literal(_) | Pattern::Struct { .. } => {}
        }
        Ok(())
    }
}
