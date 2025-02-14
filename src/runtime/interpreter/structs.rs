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

    pub fn update_struct_field(&self, value: &mut Value, chain: &[String], new_value: Value) -> Result<(), String> {
        if chain.is_empty() {
            return Err("Field chain is empty; cannot update".to_string());
        }

        if !value.is_mutable() {
            return Err("Cannot modify immutable value".to_string());
        }

        match value.inner_mut() {
            ValueType::Struct { fields, .. } => {
                let field_name = &chain[0];
                if chain.len() == 1 {
                    if let Some(existing) = fields.get_mut(field_name) {
                        *existing = new_value;
                        Ok(())
                    } else {
                        Err(format!("Field '{}' not found", field_name))
                    }
                } else {
                    if let Some(inner_value) = fields.get_mut(field_name) {
                        self.update_struct_field(inner_value, &chain[1..], new_value)
                    } else {
                        Err(format!("Field '{}' not found", field_name))
                    }
                }
            }
            _ => Err("Target value is not a struct".to_string()),
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
                let is_static = params.is_empty()
                    || !matches! {
                        &params[0].0,
                        Pattern::Identifier { name, .. } if name == "self"
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
            ValueType::Struct { name, fields } => self.handle_struct_method_call(&name, &fields, None, method, args),

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
                        let origin_info = source_name.as_ref().zip(source_scope).map(|(name, scope)| (name, scope));

                        self.handle_struct_method_call(&name, &fields, origin_info, method, args)
                    }
                    _ => Err("Cannot call method on non-struct reference".to_string()),
                }
            }

            _ => Err("Cannot call method on non-struct value".to_string()),
        }
    }

    pub fn handle_struct_method_call(&mut self, name: &String, fields: &HashMap<String, Value>, origin: Option<(&String, usize)>, method: &str, args: &[Expr]) -> Result<Value, String> {
        let methods = match self.env.get_variable(name) {
            Some(value) => match value.inner() {
                ValueType::StructDef { methods, .. } => methods.clone(),
                _ => return Err(format!("Value '{}' is not a struct definition", name)),
            },
            None => return Err(format!("Struct definition '{}' not found", name)),
        };

        let function = methods.get(method).cloned().ok_or_else(|| format!("Method '{}' not found on struct '{}'", method, name))?;

        self.env.enter_scope();

        let self_value = val!(ValueType::Struct {
            name: name.to_owned(),
            fields: fields.clone(),
        });

        // Bind the self parameter
        if let Some((self_param, _ty)) = function.params.get(0) {
            match self_param {
                Pattern::Identifier { name: self_name, mutable } => {
                    self.env.scope_resolver.declare_variable(self_name, *mutable);
                    self.env.set_variable(self_name, self_value.clone())?;
                }
                Pattern::Reference { mutable: ref_mut, pattern } => {
                    if let Pattern::Identifier { name: self_name, .. } = &**pattern {
                        let binding_mutability = *ref_mut;
                        self.env.scope_resolver.declare_variable(self_name, binding_mutability);
                        self.env.set_variable(self_name, self_value.clone())?;
                    } else {
                        return Err("Invalid pattern for self parameter".to_string());
                    }
                }
                _ => return Err("Invalid pattern for self parameter".to_string()),
            }
        } else {
            return Err("Method does not have a self parameter.".to_string());
        }

        // Evaluate and bind remaining arguments
        let evaluated_args: Vec<Value> = args.iter().map(|arg| self.evaluate_expression(arg)).collect::<Result<_, _>>()?;

        for (i, value) in evaluated_args.into_iter().enumerate() {
            if let Some((param_pattern, _)) = function.params.get(i + 1) {
                match param_pattern {
                    Pattern::Identifier { name, mutable } => {
                        self.env.scope_resolver.declare_variable(name, *mutable);
                        self.env.set_variable(name, value)?;
                    }
                    Pattern::Reference { mutable: ref_mut, pattern } => {
                        if let Pattern::Identifier { name, .. } = &**pattern {
                            let binding_mutability = *ref_mut;
                            self.env.scope_resolver.declare_variable(name, binding_mutability);
                            self.env.set_variable(name, value)?;
                        } else {
                            return Err("Invalid parameter pattern encountered.".to_string());
                        }
                    }
                    _ => return Err("Invalid parameter pattern encountered.".to_string()),
                }
            }
        }

        let result = self.execute(&function.body)?;

        let updated_self = if let Some((self_param, _)) = function.params.get(0) {
            match self_param {
                Pattern::Identifier { name, .. } => self.env.get_variable(name).cloned().ok_or_else(|| format!("Could not retrieve updated self value for '{name}'"))?,
                Pattern::Reference { pattern, .. } => {
                    if let Pattern::Identifier { name, .. } = &**pattern {
                        self.env.get_variable(name).cloned().ok_or_else(|| format!("Could not retrieve updated self value for '{name}'"))?
                    } else {
                        self_value.clone()
                    }
                }
                _ => self_value.clone(),
            }
        } else {
            self_value.clone()
        };

        let mut combined_self = updated_self.clone();
        let mut update_chain = vec![];

        if let Some((origin_name, scope_index)) = origin {
            let mut current_name = origin_name.to_string();
            let mut current_scope = scope_index;

            while let Some((_, value)) = self.env.find_variable(&current_name) {
                update_chain.push((current_name.clone(), current_scope));

                match value.inner() {
                    ValueType::Reference {
                        source_name: Some(parent_name),
                        source_scope: Some(parent_scope),
                        ..
                    } => {
                        current_name = parent_name.clone();
                        current_scope = parent_scope;
                    }
                    _ => break,
                }
            }

            for (update_name, update_scope) in update_chain.iter().rev() {
                if let Some(symbol_info) = self.env.scope_resolver.resolve(update_name) {
                    if !symbol_info.mutable {
                        return Err(format!("Cannot assign to immutable variable '{}'", update_name));
                    }

                    self.merge_nested_fields(&mut combined_self, update_name)?;
                    self.env.update_scoped_variable(update_name, combined_self.clone(), *update_scope)?;
                }
            }
        }

        self.env.exit_scope();
        Ok(result)
    }

    fn merge_nested_fields(&self, parent: &mut Value, parent_origin: &str) -> Result<(), String> {
        match parent.inner_mut() {
            ValueType::Struct { fields, .. } => {
                for (field_name, field_val) in fields.iter_mut() {
                    let composite_origin = format!("{}.{}", parent_origin, field_name);
                    let mut current_origin = composite_origin.clone();

                    while let Some((_, updated_val)) = self.env.find_variable(&current_origin) {
                        match updated_val.inner() {
                            ValueType::Reference { source_name: Some(next_origin), .. } => {
                                current_origin = next_origin.clone();
                            }
                            _ => {
                                *field_val = updated_val.clone();
                                break;
                            }
                        }
                    }

                    if matches!(field_val.inner(), ValueType::Struct { .. }) {
                        self.merge_nested_fields(field_val, &composite_origin)?;
                    }
                }
                Ok(())
            }
            _ => Err("Cannot merge fields of non-struct value".to_string()),
        }
    }
}
