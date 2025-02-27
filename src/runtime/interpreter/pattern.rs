use super::*;

impl<'st> Interpreter {
    pub fn pattern_matches(&mut self, pattern: &Pattern, value: &Value) -> Result<bool, String> {
        self.bind_pattern_variables(pattern, None, value)?;
        self.pattern_matches_inner(pattern, value)
    }

    pub fn declare_pattern(&mut self, pattern: &Pattern, param_type: Option<&Type>, value: &Value, declare_variables: bool) -> Result<(), String> {
        if !self.pattern_matches_inner(pattern, value)? {
            return Err("Pattern does not match value".to_string());
        }

        if declare_variables {
            self.bind_pattern_variables(pattern, param_type, value)?;
        }

        Ok(())
    }

    fn pattern_matches_inner(&mut self, pattern: &Pattern, value: &Value) -> Result<bool, String> {
        match (pattern, value) {
            (Pattern::Literal(expr), value) => {
                let pattern_value = self.evaluate_expression(expr)?;

                let pattern_inner = {
                    let borrowed = pattern_value.borrow();
                    borrowed.inner()
                };

                let value_inner = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                Ok(pattern_inner == value_inner)
            }

            (Pattern::Path(path), value) => {
                if path.segments.len() != 2 {
                    return Ok(false);
                }

                let enum_name = &path.segments[0].ident;
                let variant_name = &path.segments[1].ident;

                if self.env.get_variable(enum_name).is_none() {
                    return Err(format!("Enum type '{}' not found in pattern matching", enum_name));
                }

                let enum_def = self.env.get_variable(enum_name).unwrap();
                let valid_variant = match enum_def.borrow().inner() {
                    ValueType::EnumDef { variants, .. } => variants.iter().any(|v| match v {
                        EnumVariant::Simple(name) => name == variant_name,
                        EnumVariant::Tuple(name, _) => name == variant_name,
                        EnumVariant::Struct(name, _) => name == variant_name,
                    }),
                    _ => return Err(format!("'{}' is not an enum type", enum_name)),
                };

                if !valid_variant {
                    return Err(format!("Enum '{}' does not have variant '{}'", enum_name, variant_name));
                }

                let inner_value = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                if let ValueType::Enum { variant, data, enum_type } = inner_value {
                    Ok(*enum_name == enum_type && *variant_name == variant && data.as_ref().map_or(true, |v| v.is_empty()))
                } else {
                    Ok(false)
                }
            }

            // simplify pattern matching in ast
            (Pattern::TupleStruct { path, elements }, value) => {
                let value_inner = value.borrow().inner();
                if let ValueType::Enum { variant, data, enum_type } = value_inner {
                    if path.segments.len() == 2 {
                        if path.segments[0].ident == *enum_type && path.segments[1].ident == *variant {
                            match data {
                                Some(values) if elements.len() == values.len() => {
                                    for (element, val) in elements.iter().zip(values.iter()) {
                                        if !self.pattern_matches(element, val)? {
                                            return Ok(false);
                                        }
                                    }
                                    Ok(true)
                                }
                                None if elements.is_empty() => Ok(true),
                                _ => Ok(false),
                            }
                        } else {
                            Ok(false)
                        }
                    } else if path.segments.len() == 1 {
                        if path.segments[0].ident == variant {
                            match data {
                                Some(values) if elements.len() == values.len() => {
                                    for (element, val) in elements.iter().zip(values.iter()) {
                                        if !self.pattern_matches(element, val)? {
                                            return Ok(false);
                                        }
                                    }
                                    Ok(true)
                                }
                                None if elements.is_empty() => Ok(true),
                                _ => Ok(false),
                            }
                        } else {
                            Ok(false)
                        }
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }

            (Pattern::Tuple(elements), value) => {
                let inner_value = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match inner_value {
                    ValueType::Tuple(values) => {
                        if elements.len() == values.len() {
                            for (element, field_val) in elements.iter().zip(values.iter()) {
                                if !self.pattern_matches(element, field_val)? {
                                    return Ok(false);
                                }
                            }
                            Ok(true)
                        } else {
                            Ok(false)
                        }
                    }
                    ValueType::Enum { data: Some(values), .. } => {
                        if elements.len() == values.len() {
                            for (element, field_val) in elements.iter().zip(values.iter()) {
                                if !self.pattern_matches(element, field_val)? {
                                    return Ok(false);
                                }
                            }
                            Ok(true)
                        } else {
                            Ok(false)
                        }
                    }
                    _ => Ok(false),
                }
            }

            (Pattern::Struct { path, fields, rest }, value) => {
                let value_inner = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match value_inner {
                    ValueType::Struct { name, fields: value_fields, .. } => {
                        if path.segments.last().unwrap().ident != *name {
                            return Ok(false);
                        }

                        for (field_name, field_pattern) in fields {
                            if let Some(field_value) = value_fields.get(field_name) {
                                if !self.pattern_matches(field_pattern, field_value)? {
                                    return Ok(false);
                                }
                            } else if !rest {
                                return Ok(false);
                            }
                        }

                        Ok(true)
                    }
                    ValueType::Reference { original_ptr, .. } => {
                        if original_ptr.is_null() {
                            return Ok(false);
                        }

                        unsafe {
                            let rc = Rc::from_raw(original_ptr);
                            let result = self.pattern_matches(pattern, &rc.clone());
                            std::mem::forget(rc);
                            result
                        }
                    }
                    _ => Ok(false),
                }
            }

            (Pattern::Reference { mutable, pattern }, value) => {
                let inner_value = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match inner_value {
                    ValueType::Reference { original_ptr, .. } => {
                        if *mutable && !value.borrow().is_mutable() {
                            return Ok(false);
                        }
                        if original_ptr.is_null() {
                            Ok(false)
                        } else {
                            unsafe { self.pattern_matches(pattern, &Rc::from_raw(original_ptr)) }
                        }
                    }
                    _ => self.pattern_matches(pattern, value),
                }
            }

            (Pattern::Or(patterns), value) => {
                for pattern in patterns {
                    if self.pattern_matches(pattern, value)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }

            (Pattern::BindingPattern { subpattern, .. }, value) => self.pattern_matches(subpattern, value),

            (Pattern::Identifier { .. }, _) => Ok(true),

            (Pattern::Wildcard, _) => Ok(true),
        }
    }

    fn bind_pattern_variables(&mut self, pattern: &Pattern, param_type: Option<&Type>, value: &Value) -> Result<(), String> {
        let mut stack = vec![(pattern, param_type, value.clone())];

        while let Some((current_pattern, p_type, current_value)) = stack.pop() {
            match current_pattern {
                Pattern::Identifier { name, mutable } => {
                    if let Some(tt) = p_type {
                        match tt {
                            Type::Reference { mutable: ref_mutable, .. } => {
                                let borrowed_value = current_value.borrow();
                                if *ref_mutable && !borrowed_value.is_mutable() {
                                    return Err(format!("Cannot pass immutable reference as mutable for variable '{}'", name));
                                }
                                self.env.scope_resolver.declare_reference(name, *ref_mutable);
                            }
                            _ => {
                                self.env.scope_resolver.declare_variable(name, *mutable);
                            }
                        }
                    } else {
                        self.env.scope_resolver.declare_variable(name, *mutable);
                    }

                    self.env.set_variable(name, current_value)?;
                }

                Pattern::Reference { mutable, pattern } => {
                    let ref_value = match current_value.borrow().inner() {
                        ValueType::Reference { .. } => current_value.clone(),
                        _ => {
                            let reference = ValueType::Reference {
                                original_ptr: current_value.as_ptr(),
                                source_name: None,
                                source_scope: None,
                                _undropped: current_value.clone(),
                            };
                            if *mutable { val!(mut reference) } else { val!(reference) }
                        }
                    };

                    stack.push((pattern, p_type, ref_value));
                }

                Pattern::Tuple(patterns) => {
                    if let ValueType::Tuple(values) | ValueType::Enum { data: Some(values), .. } = current_value.borrow().inner() {
                        for (sub_pattern, sub_value) in patterns.iter().zip(values.iter()) {
                            stack.push((sub_pattern, p_type, sub_value.clone()));
                        }
                    }
                }

                Pattern::TupleStruct { elements, .. } => {
                    if let ValueType::Enum { data: Some(values), .. } = current_value.borrow().inner() {
                        for (sub_pattern, sub_value) in elements.iter().zip(values.iter()) {
                            stack.push((sub_pattern, p_type, sub_value.clone()));
                        }
                    }
                }

                Pattern::Or(patterns) => {
                    for sub_pattern in patterns {
                        if self.pattern_matches(sub_pattern, &current_value)? {
                            stack.push((sub_pattern, p_type, current_value.clone()));
                            break;
                        }
                    }
                }

                Pattern::BindingPattern { name, mutable, subpattern } => {
                    self.env.scope_resolver.declare_variable(name, *mutable);
                    self.env.set_variable(name, current_value.clone())?;
                    stack.push((subpattern, p_type, current_value.clone()));
                }

                Pattern::Struct { fields, .. } => {
                    if let ValueType::Struct { fields: value_fields, .. } = current_value.borrow().inner() {
                        for (field_name, field_pattern) in fields {
                            if let Some(field_value) = value_fields.get(field_name) {
                                stack.push((field_pattern, p_type, field_value.clone()));
                            }
                        }
                    }
                }

                Pattern::Path(_) | Pattern::Wildcard | Pattern::Literal(_) => {}
            }
        }

        Ok(())
    }
}
