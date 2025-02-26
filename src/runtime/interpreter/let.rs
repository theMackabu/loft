use super::*;

impl<'st> Interpreter<'st> {
    #[inline]
    pub(crate) fn parse_let_statement(&mut self, attr: &Stmt) -> Result<Value, String> {
        unbind! { Stmt::Let { pattern, initializer, type_annotation, .. } = attr }

        let value = if let Some(init) = initializer {
            let init_value = self.evaluate_expression(init)?;
            if let Some(target_type) = type_annotation {
                self.perform_cast(init_value, target_type)?
            } else {
                init_value
            }
        } else {
            ValueEnum::unit()
        };

        match pattern {
            Pattern::Identifier { name, mutable } => {
                let declared_mutability = *mutable;
                let is_initialized = initializer.is_some();
                let value_inner = { value.borrow().inner() };

                if let ValueType::Reference { source_name, .. } = value_inner {
                    if source_name.is_none() {
                        self.env.scope_resolver.declare_variable(name, declared_mutability);

                        if is_initialized {
                            self.env.scope_resolver.mark_as_initialized(name)?;
                        }

                        let mut value_ref = value.borrow_mut();
                        *value_ref = if declared_mutability {
                            value_ref.clone().into_mutable()
                        } else {
                            value_ref.clone().into_immutable()
                        };
                    } else {
                        self.env.scope_resolver.declare_variable(name, value.borrow().is_mutable());
                        self.env.scope_resolver.mark_as_initialized(name)?;
                    }
                } else {
                    self.env.scope_resolver.declare_variable(name, declared_mutability);

                    if is_initialized {
                        self.env.scope_resolver.mark_as_initialized(name)?;
                    }

                    let mut value_ref = value.borrow_mut();
                    *value_ref = if declared_mutability {
                        value_ref.clone().into_mutable()
                    } else {
                        value_ref.clone().into_immutable()
                    };
                }

                self.env.set_variable_raw(name, value)?;
            }

            Pattern::Tuple(patterns) => {
                let value_inner = value.borrow().inner();

                match value_inner {
                    ValueType::Tuple(elements) => {
                        if patterns.len() != elements.len() {
                            return Err(format!("Expected tuple with {} elements but got {} elements", patterns.len(), elements.len()));
                        }

                        for (i, pattern) in patterns.iter().enumerate() {
                            match pattern {
                                Pattern::Identifier { name, mutable } => {
                                    let element_value = elements[i].clone();
                                    let is_ref = matches!(element_value.borrow().inner(), ValueType::Reference { .. });
                                    let declared_mutability = if is_ref { element_value.borrow().is_mutable() } else { *mutable };

                                    self.env.scope_resolver.declare_variable(name, declared_mutability);
                                    self.env.scope_resolver.mark_as_initialized(name)?;

                                    if !is_ref {
                                        let mut element_ref = element_value.borrow_mut();
                                        *element_ref = if declared_mutability {
                                            element_ref.clone().into_mutable()
                                        } else {
                                            element_ref.clone().into_immutable()
                                        };
                                    }

                                    self.env.set_variable_raw(name, element_value)?;
                                }
                                _ => return Err("Nested destructuring patterns are not supported yet".to_string()),
                            }
                        }
                    }
                    _ => return Err("Cannot destructure non-tuple value".to_string()),
                }
            }

            Pattern::Struct { path, fields, rest } => {
                let value_inner = value.borrow().inner();
                match value_inner {
                    ValueType::Struct {
                        name: struct_name,
                        fields: ref struct_fields,
                    } => {
                        let expected_name = &path.segments[0].ident;
                        if &struct_name != expected_name {
                            return Err(format!("Expected struct '{}' but got '{}'", expected_name, struct_name));
                        }

                        for (field_name, field_pattern) in fields {
                            match struct_fields.get(field_name) {
                                Some(field_value) => match field_pattern {
                                    Pattern::Identifier { name, mutable } => {
                                        self.env.scope_resolver.declare_variable(name, *mutable);
                                        self.env.scope_resolver.mark_as_initialized(name)?;
                                        let mut field_value_ref = field_value.borrow_mut();
                                        *field_value_ref = if *mutable {
                                            field_value_ref.clone().into_mutable()
                                        } else {
                                            field_value_ref.clone().into_immutable()
                                        };
                                        self.env.set_variable_raw(name, field_value.clone())?;
                                    }
                                    _ => {
                                        return Err("Nested destructuring patterns for structs are not supported yet".to_string());
                                    }
                                },
                                None => return Err(format!("Field '{}' not found in struct '{}'", field_name, struct_name)),
                            }
                        }

                        if !*rest && struct_fields.len() != fields.len() {
                            return Err("Struct pattern does not cover all fields".to_string());
                        }
                    }
                    _ => return Err("Cannot destructure non-struct value".to_string()),
                }
            }

            _ => return Err("Unsupported pattern in let binding".to_string()),
        }

        Ok(ValueEnum::unit())
    }
}
