use super::*;

impl Interpreter {
    pub fn extract_field_chain(&self, expr: &Expr) -> Result<(String, Vec<String>), String> {
        match expr {
            Expr::Identifier(name) => Ok((name.clone(), vec![])),
            Expr::MemberAccess { object, member } => {
                let (base, mut chain) = self.extract_field_chain(object)?;
                chain.push(member.clone());
                Ok((base, chain))
            }
            _ => Err("Invalid expression in member assignment target".to_string()),
        }
    }

    pub fn handle_struct_def(&mut self, name: &str, fields: HashMap<String, (Type, bool)>) -> Result<(), String> {
        self.env.scope_resolver.declare_variable(name, false);

        let struct_def = val!(ValueType::StructDef {
            name: name.to_string(),
            fields,
            methods: HashMap::new(),
        });

        self.env.set_variable(name, struct_def)
    }

    pub fn handle_impl_block(&mut self, target: &Path, items: &[Stmt]) -> Result<(), String> {
        let type_name = if target.segments.len() == 1 {
            &target.segments[0].ident
        } else {
            return Err(format!("Invalid impl target path: {:?}", target));
        };

        let (name, fields, mut methods) = match self.env.get_variable(type_name).as_ref() {
            Some(value) => match value.inner() {
                ValueType::StructDef { name, fields, methods } => (name.clone(), fields.clone(), methods.clone()),
                _ => return Err(format!("Value '{}' is not a struct definition", type_name)),
            },
            None => return Err(format!("Type '{}' not found", type_name)),
        };

        for method in items {
            if let Stmt::Function { name: method_name, params, body, .. } = method {
                let is_static = params.is_empty() || {
                    let first_param = &params[0].0;
                    !matches!(first_param, Pattern::Identifier { name, .. } if name == "self")
                        && !matches!(first_param, Pattern::Reference { pattern, .. } if matches!(pattern.as_ref(), Pattern::Identifier { name, .. } if name == "self"))
                };

                methods.insert(
                    method_name.clone(),
                    Function {
                        params: params.clone(),
                        body: body.clone(),
                        is_method: true,
                        is_static,
                    },
                );
            }
        }

        let updated_struct_def = val!(ValueType::StructDef { name, fields, methods });
        self.env.set_variable(type_name, updated_struct_def)
    }

    pub fn evaluate_struct_init(&mut self, name: &str, fields: HashMap<String, (Expr, bool)>) -> Result<Value, String> {
        let def_fields = match self.env.find_variable(name) {
            Some((_, value)) => match value.inner() {
                ValueType::StructDef { fields: def_fields, .. } => def_fields,
                _ => return Err(format!("Value '{}' is not a struct definition", name)),
            },
            None => return Err(format!("Struct definition '{}' not found", name)),
        };

        let mut field_values: HashMap<String, Value> = HashMap::new();

        for (field_name, (expr, is_shorthand)) in fields {
            if !def_fields.contains_key(&field_name) {
                return Err(format!("Field '{}' not found in struct '{}'", field_name, name));
            }

            let value = if is_shorthand {
                if let Some(value) = self.env.get_variable(&field_name) {
                    value.clone()
                } else {
                    return Err(format!("Variable '{}' not found for shorthand initialization", field_name));
                }
            } else {
                self.evaluate_expression(&expr)?
            };

            field_values.insert(field_name, value);
        }

        if let Some(missing) = def_fields.keys().find(|key| !field_values.contains_key(*key)) {
            return Err(format!("Missing field '{}' in struct initialization", missing));
        }

        Ok(val!(ValueType::Struct {
            name: name.to_owned(),
            fields: field_values,
        }))
    }

    pub fn evaluate_method_call(&mut self, object: Value, method: &str, args: &[Expr]) -> Result<Value, String> {
        match object.inner() {
            ValueType::Struct { name, fields } => {
                let origin_info = if let Some((source_name, source_scope)) = self.env.get_variable_source(&name) {
                    Some((source_name, source_scope))
                } else {
                    None
                };
                self.handle_struct_method_call(&name, &fields, origin_info, method, args)
            }

            ValueType::Reference {
                data: Some(boxed_value),
                source_name,
                source_scope,
                ..
            } => {
                if !object.is_mutable() {
                    return Err("Cannot call method on non-mutable reference".to_string());
                }

                match boxed_value.inner() {
                    ValueType::Struct { name, fields } => {
                        let origin_info = match (source_name, source_scope) {
                            (Some(name), Some(scope)) => Some((name.clone(), scope)),
                            _ => self.env.get_variable_source(&name).map(|(name, scope)| (name.clone(), scope)),
                        };

                        self.handle_struct_method_call(&name, &fields, origin_info, method, args)
                    }
                    _ => Err("Cannot call method on non-struct reference".to_string()),
                }
            }

            _ => Err("Cannot call method on non-struct value".to_string()),
        }
    }

    pub fn handle_struct_method_call(&mut self, name: &String, fields: &HashMap<String, Value>, origin: Option<(String, usize)>, method: &str, args: &[Expr]) -> Result<Value, String> {
        let struct_def = match self.env.get_variable(name) {
            Some(value) => match value.inner() {
                ValueType::StructDef { methods, .. } => methods,
                _ => return Err(format!("Type '{}' is not a struct definition", name)),
            },
            None => return Err(format!("Struct definition '{}' not found", name)),
        };

        let function = struct_def.get(method).ok_or_else(|| format!("Method '{}' not found on struct '{}'", method, name))?.clone();

        self.env.enter_scope();

        let mut params_to_process = Vec::new();
        let mut original_location = None;

        if !function.is_static {
            let base_self_value = val!(mut ValueType::Struct {
                name: name.clone(),
                fields: fields.clone(),
            });

            original_location = origin.clone().map(|(name, scope)| (name, scope));

            let self_value = if let Some((source_name, scope_index)) = origin.clone() {
                val!(mut ValueType::Reference {
                    source_name: Some(source_name.to_string()),
                    source_scope: Some(scope_index),
                    data: Some(base_self_value)
                })
            } else {
                base_self_value
            };

            if let Some((pattern, param_type)) = function.params.first() {
                match pattern {
                    Pattern::Identifier { name: param_name, mutable } if param_name == "self" => {
                        let final_self = if *mutable { self.env.make_deeply_mutable(self_value) } else { self_value };
                        params_to_process.push(("self".to_string(), final_self, *mutable, param_type.clone()));
                    }

                    Pattern::Reference { mutable, pattern } => {
                        if let Pattern::Identifier { name: param_name, .. } = pattern.as_ref() {
                            if param_name != "self" {
                                return Err("First parameter must be a form of 'self'".to_string());
                            }
                            let ref_value = if *mutable { self.env.make_deeply_mutable(self_value) } else { self_value };
                            params_to_process.push(("self".to_string(), ref_value, *mutable, param_type.clone()));
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
            match param_type {
                Type::Reference { mutable: ref_mutable, .. } => {
                    self.env.scope_resolver.declare_reference(&param_name, ref_mutable);
                    if ref_mutable && !value.is_mutable() {
                        return Err("Cannot pass immutable value as mutable reference".to_string());
                    }
                }
                _ => {
                    self.env.scope_resolver.declare_variable(&param_name, mutable);
                }
            }

            let final_value = if mutable { value.into_mutable() } else { value.into_immutable() };
            self.env.set_variable(&param_name, Box::new(final_value))?;
        }

        println!("Before method execution - origin: {:?}", origin.clone());
        let result = self.execute(&function.body);
        println!("After method execution");

        if let Some((source_name, scope_index)) = original_location.clone() {
            println!("Attempting update - source: {}, scope: {}", source_name, scope_index);
            if let Some(modified_self) = self.env.get_variable_owned("self") {
                println!("Modified self value: {:?}", modified_self);

                let final_value = match modified_self.inner() {
                    ValueType::Reference { data: Some(inner), .. } => inner,
                    _ => modified_self,
                };

                let final_value = self.env.make_deeply_mutable(final_value);
                println!("Final value to update: {:?}", final_value);
                match self.env.update_scoped_variable(&source_name, final_value, scope_index) {
                    Ok(_) => println!("Update successful"),
                    Err(e) => println!("Update failed: {}", e),
                }
            }
        }

        if let Some((source_name, scope_index)) = original_location {
            if let Some(scope) = self.env.scopes.get(scope_index) {
                if let Some(value) = scope.get(&source_name) {
                    println!("Variable after update: {:?}", value);
                }
            }
        }

        self.env.exit_scope();
        result
    }
}
