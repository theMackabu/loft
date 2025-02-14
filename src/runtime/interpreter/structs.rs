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
        match value {
            Value::Struct { fields, .. } => {
                let field_name = &chain[0];
                if chain.len() == 1 {
                    if let Some((_, field_val)) = fields.iter_mut().find(|(name, _)| name == field_name) {
                        *field_val = new_value;
                        Ok(())
                    } else {
                        Err(format!("Field '{}' not found", field_name))
                    }
                } else {
                    if let Some((_, field_val)) = fields.iter_mut().find(|(name, _)| name == field_name) {
                        match field_val {
                            Value::Struct { .. } => self.update_struct_field(field_val, &chain[1..], new_value),
                            _ => Err(format!("Field '{}' is not a struct", field_name)),
                        }
                    } else {
                        Err(format!("Field '{}' not found", field_name))
                    }
                }
            }
            _ => Err("Target value is not a struct".to_string()),
        }
    }

    pub fn handle_struct_def(&mut self, name: &str, fields: &[(String, bool, Type)]) -> Result<(), String> {
        self.env.scope_resolver.declare_variable(name, false);

        let struct_def = Value::StructDef {
            name: name.to_string(),
            fields: fields.iter().map(|(name, _, ty)| (name.clone(), ty.clone())).collect(),
            methods: HashMap::new(),
        };

        self.env.set_variable(name, struct_def)
    }

    pub fn handle_impl_block(&mut self, target: &Path, items: &[Stmt]) -> Result<(), String> {
        let type_name = if target.segments.len() == 1 {
            &target.segments[0].ident
        } else {
            return Err(format!("Invalid impl target path: {:?}", target));
        };

        let (name, fields, mut methods) = match self.env.get_variable(type_name) {
            Some(Value::StructDef { name, fields, methods }) => (name.clone(), fields.clone(), methods.clone()),
            _ => return Err(format!("Type '{}' not found", type_name)),
        };

        for method in items {
            if let Stmt::Function { name: method_name, params, body, .. } = method {
                let is_static = params.is_empty()
                    || !matches! {
                        &params[0].0, Pattern::Identifier { name, .. } if name == "self"
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

        let updated_struct_def = Value::StructDef { name, fields, methods };
        self.env.set_variable(type_name, updated_struct_def)
    }

    pub fn evaluate_struct_init(&mut self, name: &str, fields: &[(String, Expr, bool)]) -> Result<Value, String> {
        let def_field_names = match self.env.find_variable(name) {
            Some((_, Value::StructDef { fields: def_fields, .. })) => def_fields.iter().map(|(n, _)| n.to_string()).collect::<Vec<_>>(),
            _ => return Err(format!("Struct definition '{}' not found", name)),
        };

        let mut field_values = Vec::new();

        for (field_name, expr, is_shorthand) in fields {
            if !def_field_names.iter().any(|n| n.starts_with(field_name)) {
                return Err(format!("Field '{}' not found in struct '{}'", field_name, name));
            }

            let value = if *is_shorthand {
                if let Some(value) = self.env.get_variable(field_name) {
                    value.clone()
                } else {
                    return Err(format!("Variable '{}' not found for shorthand initialization", field_name));
                }
            } else {
                self.evaluate_expression(expr)?
            };

            field_values.push((field_name.clone(), value));
        }

        for def_name in def_field_names {
            if !field_values.iter().any(|(name, _)| def_name == *name) {
                return Err(format!("Missing field '{}' in struct initialization", def_name));
            }
        }

        Ok(Value::Struct {
            name: name.to_string(),
            fields: field_values,
        })
    }

    pub fn evaluate_method_call(&mut self, object: &Value, method: &str, args: &[Expr]) -> Result<Value, String> {
        match object {
            Value::Struct { name, fields } => self.handle_struct_method_call(name, fields, None, method, args),

            Value::Reference {
                data: Some(boxed_value),
                source_name,
                source_scope,
                mutable,
                ..
            } => match &**boxed_value {
                Value::Struct { name, fields } => {
                    if !mutable {
                        return Err("Cannot call method on non-mutable reference".to_string());
                    }
                    let origin_info = (source_name.as_ref().ok_or("Missing source variable name")?, source_scope.ok_or("Missing source scope")?);
                    self.handle_struct_method_call(name, fields, Some(origin_info), method, args)
                }
                _ => Err("Cannot call method on non-struct reference".to_string()),
            },
            _ => Err("Cannot call method on non-struct value".to_string()),
        }
    }

    fn handle_struct_method_call(&mut self, name: &String, fields: &Vec<(String, Value)>, origin: Option<(&String, usize)>, method: &str, args: &[Expr]) -> Result<Value, String> {
        let methods = match self.env.get_variable(name) {
            Some(Value::StructDef { methods, .. }) => methods.clone(),
            _ => return Err(format!("Struct definition '{}' not found", name)),
        };

        let function = match methods.get(method) {
            Some(f) => f.clone(),
            None => return Err(format!("Method '{}' not found on struct '{}'", method, name)),
        };

        self.env.enter_scope();

        let self_value = Value::Struct {
            name: name.to_owned(),
            fields: fields.clone(),
        };

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

        if let Some((origin_name, scope_index)) = origin {
            // fix using Vec<String> for origin
            if let Some(symbol_info) = self.env.scope_resolver.resolve(origin_name) {
                if !symbol_info.mutable {
                    return Err(format!("Cannot assign to immutable variable '{}'", origin_name));
                }
                self.merge_nested_fields(&mut combined_self, origin_name)?;
                self.env.update_scoped_variable(origin_name, combined_self.clone(), scope_index)?;
            }
        }

        self.env.exit_scope();
        Ok(result)
    }

    fn merge_nested_fields(&self, parent: &mut Value, parent_origin: &String) -> Result<(), String> {
        if let Value::Struct { fields, .. } = parent {
            for (field_name, field_val) in fields.iter_mut() {
                // fix using Vec<String> for origin
                let composite_origin = format!("{}.{}", parent_origin, field_name);
                if let Some((_, updated_val)) = self.env.find_variable(&composite_origin) {
                    *field_val = updated_val.clone();
                }
            }
        }
        Ok(())
    }
}
