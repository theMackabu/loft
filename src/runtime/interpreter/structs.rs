use super::*;
use crate::runtime::value::ValueExt;
use std::{cell::RefCell, rc::Rc};

impl Interpreter {
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
            Some(value) => match value.borrow().inner() {
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
            Some((_, value)) => match value.borrow().inner() {
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

    pub fn evaluate_method_call(&mut self, object_expr: &Expr, object: Value, method: &str, args: &[Expr]) -> Result<Value, String> {
        let field_chain = match self.extract_field_chain(object_expr) {
            Ok((base, chain)) if !chain.is_empty() => Some((base, chain)),
            _ => None,
        };

        match object.borrow().inner() {
            ValueType::Struct { name, fields } => {
                let origin_info = self.env.get_variable_source(&name);
                self.handle_struct_method_call(&name, &fields, origin_info, method, args, field_chain)
            }
            ValueType::Reference {
                data: Some(boxed_value),
                source_name,
                source_scope,
                ..
            } => {
                if !object.borrow().is_mutable() {
                    return Err("Cannot call method on non-mutable reference".to_string());
                }
                match boxed_value.borrow().inner() {
                    ValueType::Struct { name, fields } => {
                        let origin_info = match (source_name, source_scope) {
                            (Some(name), Some(scope)) => Some((name.clone(), scope)),
                            _ => self.env.get_variable_source(&name).map(|(n, s)| (n.clone(), s)),
                        };
                        self.handle_struct_method_call(&name, &fields, origin_info, method, args, field_chain)
                    }
                    _ => Err("Cannot call method on non-struct reference".to_string()),
                }
            }
            _ => Err("Cannot call method on non-struct value".to_string()),
        }
    }

    pub fn handle_struct_method_call(
        &mut self, name: &String, fields: &HashMap<String, Value>, origin: Option<(String, usize)>, method: &str, args: &[Expr], field_chain: Option<(String, Vec<String>)>,
    ) -> Result<Value, String> {
        let struct_def = match self.env.get_variable(name) {
            Some(value) => match value.borrow().inner() {
                ValueType::StructDef { methods, .. } => methods,
                _ => return Err(format!("Type '{}' is not a struct definition", name)),
            },
            None => return Err(format!("Struct definition '{}' not found", name)),
        };

        let function = struct_def.get(method).ok_or_else(|| format!("Method '{}' not found on struct '{}'", method, name))?.clone();

        self.env.enter_scope();
        let mut params_to_process = Vec::new();

        let effective_origin = self.env.resolve_effective_origin("self", &origin);

        let live_self_value = if let Some((base_name, chain)) = &field_chain {
            if let Some((_, base_value)) = self.env.find_variable(base_name) {
                base_value.get_struct_field(chain).ok_or_else(|| format!("Failed to get field with chain {:?}", chain))?
            } else {
                return Err(format!("Base variable '{}' not found", base_name));
            }
        } else if !effective_origin.0.is_empty() {
            self.env
                .get_variable(&effective_origin.0)
                .ok_or_else(|| format!("Variable '{}' not found", effective_origin.0))?
                .clone()
        } else {
            val!(ValueType::Struct {
                name: name.clone(),
                fields: fields.iter().map(|(k, v)| (k.clone(), Rc::clone(v))).collect(),
            })
        };

        let self_value = if !function.is_static {
            if origin.is_some() {
                live_self_value
            } else {
                val!(ValueType::Reference {
                    source_name: None,
                    source_scope: None,
                    data: Some(live_self_value)
                })
            }
        } else {
            live_self_value
        };

        if !function.is_static {
            if let Some((pattern, param_type)) = function.params.first() {
                match pattern {
                    Pattern::Identifier { name: param_name, mutable } if param_name == "self" => {
                        params_to_process.push(("self".to_owned(), self_value, *mutable, param_type.clone()));
                    }
                    Pattern::Reference { mutable, pattern } => {
                        if let Pattern::Identifier { name: param_name, .. } = pattern.as_ref() {
                            if param_name != "self" {
                                return Err("First parameter must be a form of 'self'".to_string());
                            }
                            params_to_process.push(("self".to_owned(), self_value, *mutable, param_type.clone()));
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
