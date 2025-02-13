use super::*;

impl Interpreter {
    pub fn handle_struct_def(&mut self, name: &str, fields: &[(String, bool, Type)], methods: &[Stmt]) -> Result<(), String> {
        let mut struct_methods = HashMap::new();

        for method in methods {
            if let Stmt::Function { name: method_name, params, body, .. } = method {
                struct_methods.insert(
                    method_name.clone(),
                    Function {
                        params: params.clone(),
                        body: body.clone(),
                        is_method: true,
                    },
                );
            }
        }

        let struct_def = Value::StructDef {
            name: name.to_string(),
            fields: fields.iter().map(|(name, _, ty)| (name.clone(), ty.clone())).collect(),
            methods: struct_methods,
        };

        self.env.set_variable(name, struct_def)?;
        Ok(())
    }

    pub fn evaluate_struct_init(&mut self, name: &str, fields: &[(String, Expr, bool)]) -> Result<Value, String> {
        let mut field_values = Vec::new();

        for (field_name, expr, is_shorthand) in fields {
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

        Ok(Value::Struct {
            name: name.to_string(),
            fields: field_values,
        })
    }

    pub fn evaluate_method_call(&mut self, object: &Value, method: &str, args: &[Expr]) -> Result<Value, String> {
        let (struct_name, methods) = match object {
            Value::Struct { name, fields: _ } => match self.env.get_variable(name) {
                Some(Value::StructDef { methods, .. }) => (name.clone(), methods.clone()),
                _ => return Err(format!("Struct definition '{}' not found", name)),
            },
            Value::StructDef { name, methods, .. } => (name.clone(), methods.clone()),
            _ => return Err("Cannot call method on non-struct value".to_string()),
        };

        let function = match methods.get(method) {
            Some(f) => f.clone(),
            None => return Err(format!("Method '{}' not found on struct '{}'", method, struct_name)),
        };

        self.env.enter_scope();

        if let Value::Struct { .. } = object {
            self.env.set_variable("self", object.clone())?;
        }

        let evaluated_args: Vec<Value> = args.iter().map(|arg| self.evaluate_expression(arg)).collect::<Result<_, _>>()?;

        for (i, value) in evaluated_args.into_iter().enumerate() {
            if let Some((param_pattern, _)) = function.params.get(i) {
                if let Pattern::Identifier { name, .. } = param_pattern {
                    self.env.set_variable(name, value)?;
                }
            }
        }

        let result = self.execute(&function.body)?;
        self.env.exit_scope();

        Ok(result)
    }
}
