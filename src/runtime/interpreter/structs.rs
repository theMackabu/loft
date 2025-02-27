use super::{methods::Method, *};
use std::{cell::RefCell, rc::Rc};

impl<'st> Interpreter {
    pub fn extract_field_chain(&self, expr: &Expr) -> Result<(String, Vec<String>), String> {
        match expr {
            Expr::Identifier(name) => Ok((name.clone(), vec![])),

            Expr::Dereference { operand } => self.extract_field_chain(operand),

            Expr::MemberAccess { object, member } => {
                let (base, mut chain) = self.extract_field_chain(object)?;
                chain.push(member.clone());
                Ok((base, chain))
            }

            _ => Err("Invalid expression in member access".to_string()),
        }
    }

    pub fn handle_struct_def(&mut self, path: &Path, fields: HashMap<String, (Type, bool)>) -> Result<(), String> {
        let name = path.segments.last().ok_or_else(|| "Empty path provided".to_string())?.ident.clone();
        self.env.scope_resolver.declare_variable(&name, true);

        let struct_def = val!(ValueType::StructDef {
            name: name.clone(),
            fields: fields.clone(),
            methods: HashMap::new(),
        });

        self.env.set_variable(&name, struct_def)
    }

    pub fn handle_impl_block(&mut self, target: &Path, items: &[Stmt]) -> Result<(), String> {
        let type_name = target.segments.last().ok_or_else(|| format!("Invalid impl target path: {target}"))?.ident.clone();

        if let Some(value) = self.env.get_variable(&type_name) {
            let mut cell = value.borrow_mut();

            match cell.inner_mut() {
                ValueType::StructDef { methods, .. } => {
                    for method_stmt in items {
                        if let Stmt::Function {
                            name: method_name,
                            params,
                            body,
                            is_const,
                            type_params,
                            ..
                        } = method_stmt
                        {
                            let is_static = params.is_empty() || {
                                let first_param = &params[0].0;
                                !matches!(first_param, Pattern::Identifier { name, .. } if name == "self")
                                    && !matches!(first_param, Pattern::Reference { pattern, .. }
                                        if matches!(pattern.as_ref(), Pattern::Identifier { name, .. } if name == "self"))
                            };

                            methods.insert(
                                method_name.clone(),
                                Function {
                                    params: params.clone(),
                                    body: body.clone(),
                                    is_method: true,
                                    type_params: type_params.clone(),
                                    is_const: *is_const,
                                    is_static,
                                },
                            );
                        }
                    }
                }

                ValueType::EnumDef { methods, .. } => {
                    for method_stmt in items {
                        if let Stmt::Function {
                            name: method_name,
                            params,
                            body,
                            is_const,
                            type_params,
                            ..
                        } = method_stmt
                        {
                            let is_static = params.is_empty() || {
                                let first_param = &params[0].0;
                                !matches!(first_param, Pattern::Identifier { name, .. } if name == "self")
                                    && !matches!(first_param, Pattern::Reference { pattern, .. }
                                        if matches!(pattern.as_ref(), Pattern::Identifier { name, .. } if name == "self"))
                            };

                            methods.insert(
                                method_name.clone(),
                                Function {
                                    params: params.clone(),
                                    body: body.clone(),
                                    is_method: true,
                                    type_params: type_params.clone(),
                                    is_const: *is_const,
                                    is_static,
                                },
                            );
                        }
                    }
                }

                _ => return Err(format!("Value '{}' is not a struct or enum definition", type_name)),
            }
        } else {
            return Err(format!("Type '{}' not found", type_name));
        }
        Ok(())
    }

    pub fn evaluate_struct_init(&mut self, path: &Path, fields: HashMap<String, (Expr, bool)>) -> Result<Value, String> {
        if path.segments.is_empty() {
            return Err("Struct initialization has an empty path".to_owned());
        }

        let name = if path.segments.len() == 2 {
            format!("{}::{}", path.segments[0].ident, path.segments[1].ident)
        } else {
            path.segments.last().unwrap().ident.clone()
        };

        let def_fields = match self.env.find_variable(&name) {
            Some((_, value)) => match value.borrow().inner() {
                ValueType::StructDef { fields: def_fields, .. } => def_fields,
                ValueType::EnumStructConstructor { fields: def_fields, .. } => def_fields,
                _ => return Err(format!("Value '{}' is not a struct definition", name)),
            },
            None => return Err(format!("Struct definition '{}' not found", name)),
        };

        if def_fields.keys().all(|k| k.parse::<usize>().is_ok()) {
            return Err(format!("Tuple struct '{}' cannot be initialized with named fields, use '{}'(...) syntax instead", name, name));
        }

        let mut field_values: HashMap<String, Value> = HashMap::new();

        for (field_name, (expr, is_shorthand)) in fields {
            if !def_fields.contains_key(&field_name) {
                return Err(format!("Field '{}' not found in struct '{}'", field_name, name));
            }

            let value = if is_shorthand {
                if let Some(var_value) = self.env.get_variable(&field_name) {
                    var_value.clone()
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

    pub(crate) fn handle_type_method_call(&mut self, handle: Method, type_name: &String) -> Result<Value, String> {
        let Method { object, args, call } = handle;

        let type_def = match self.env.get_variable(type_name) {
            Some(value) => match value.borrow().inner() {
                ValueType::StructDef { methods, .. } => methods,
                ValueType::EnumDef { methods, .. } => methods,
                _ => return Err(format!("Type '{}' is not a struct or enum definition", type_name)),
            },
            None => return Err(format!("Type definition '{}' not found", type_name)),
        };

        let function = type_def.get(call).ok_or_else(|| format!("Method '{call}' not found on type '{type_name}'"))?.clone();

        self.env.enter_scope();
        let mut params_to_process = Vec::new();

        if !function.is_static {
            if let Some((pattern, param_type)) = function.params.first() {
                match pattern {
                    Pattern::Identifier { name: param_name, mutable } if param_name == "self" => {
                        params_to_process.push(("self".to_owned(), object.clone(), *mutable, param_type.clone()));
                    }
                    Pattern::Reference { mutable, pattern } => {
                        if let Pattern::Identifier { name: param_name, .. } = pattern.as_ref() {
                            if param_name != "self" {
                                return Err("First parameter must be a form of 'self'".to_string());
                            }
                            params_to_process.push(("self".to_owned(), object.clone(), *mutable, param_type.clone()));
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
            if param_name == "self" && Rc::ptr_eq(&object, &value) {
                self.env.scope_resolver.declare_variable(&param_name, mutable);
                self.env.set_variable_raw(&param_name, object.clone())?;
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
