use super::*;

impl<'st> Interpreter<'st> {
    pub fn handle_enum_def(&mut self, name: &str, variants: Vec<EnumVariant>) -> Result<(), String> {
        self.env.scope_resolver.declare_variable(name, true);

        let enum_def = val!(ValueType::EnumDef {
            name: name.to_string(),
            variants: variants.clone(),
            methods: HashMap::new(),
        });

        self.env.set_variable(name, enum_def)?;

        for variant in variants {
            match variant {
                EnumVariant::Simple(variant_name) => {
                    let constructor = val!(ValueType::EnumConstructor {
                        enum_name: name.to_string(),
                        variant_name: variant_name.clone(),
                        fields: Vec::new(),
                    });

                    let full_path = format!("{}::{}", name, variant_name);
                    self.env.scope_resolver.declare_variable(&full_path, false);
                    self.env.set_variable(&full_path, constructor)?;
                }

                EnumVariant::Tuple(variant_name, field_types) => {
                    let constructor = val!(ValueType::EnumConstructor {
                        enum_name: name.to_string(),
                        variant_name: variant_name.clone(),
                        fields: field_types,
                    });

                    let full_path = format!("{}::{}", name, variant_name);
                    self.env.scope_resolver.declare_variable(&full_path, false);
                    self.env.set_variable(&full_path, constructor)?;
                }

                EnumVariant::Struct(variant_name, fields) => {
                    let field_types = fields.into_iter().map(|(name, ty)| (name, ty)).collect();

                    let constructor = val!(ValueType::EnumStructConstructor {
                        enum_name: name.to_string(),
                        variant_name: variant_name.clone(),
                        fields: field_types,
                    });

                    let full_path = format!("{}::{}", name, variant_name);
                    self.env.scope_resolver.declare_variable(&full_path, false);
                    self.env.set_variable(&full_path, constructor)?;
                }
            }
        }

        Ok(())
    }

    pub fn handle_enum_method_call(&mut self, enum_value: Value, enum_type: &String, method: &str, args: &[Expr]) -> Result<Value, String> {
        let enum_def = match self.env.get_variable(enum_type) {
            Some(value) => match value.borrow().inner() {
                ValueType::EnumDef { methods, .. } => methods,
                _ => return Err(format!("Type '{}' is not an enum definition", enum_type)),
            },
            None => return Err(format!("Enum definition '{}' not found", enum_type)),
        };

        let function = enum_def.get(method).ok_or_else(|| format!("Method '{}' not found on enum '{}'", method, enum_type))?.clone();

        self.env.enter_scope();
        let mut params_to_process = Vec::new();

        if !function.is_static {
            if let Some((pattern, param_type)) = function.params.first() {
                match pattern {
                    Pattern::Identifier { name: param_name, mutable } if param_name == "self" => {
                        params_to_process.push(("self".to_owned(), enum_value.clone(), *mutable, param_type.clone()));
                    }
                    Pattern::Reference { mutable, pattern } => {
                        if let Pattern::Identifier { name: param_name, .. } = pattern.as_ref() {
                            if param_name != "self" {
                                return Err("First parameter must be a form of 'self'".to_string());
                            }
                            params_to_process.push(("self".to_owned(), enum_value.clone(), *mutable, param_type.clone()));
                        }
                    }
                    _ => return Err("First parameter must be a form of 'self'".to_string()),
                }
            }
        }

        let arg_offset = if function.is_static { 0 } else { 1 };
        for (i, arg) in args.iter().enumerate() {
            if let Some((Pattern::Identifier { name: param_name, mutable }, param_type)) = function.params.get(i + arg_offset) {
                let arg_value = self.evaluate_expression(arg)?;
                params_to_process.push((param_name.clone(), arg_value, *mutable, param_type.clone()));
            }
        }

        for (param_name, value, mutable, param_type) in params_to_process {
            if param_name == "self" && Rc::ptr_eq(&enum_value, &value) {
                self.env.scope_resolver.declare_variable(&param_name, mutable);
                self.env.set_variable_raw(&param_name, enum_value.clone())?;
                continue;
            }

            match param_type {
                Type::Reference { mutable: ref_mutable, .. } => {
                    self.env.scope_resolver.declare_reference(&param_name, ref_mutable);
                    if ref_mutable && !value.borrow().is_mutable() {
                        return Err("Cannot pass immutable value as mutable reference".to_string());
                    }
                }
                _ => self.env.scope_resolver.declare_variable(&param_name, mutable),
            }
            self.env.set_variable(&param_name, value)?;
        }

        let result = self.execute(&function.body)?;
        self.env.exit_scope();

        Ok(result)
    }
}
