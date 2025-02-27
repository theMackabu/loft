use super::*;

impl<'st> Interpreter {
    pub fn evaluate_const_functions(&mut self) -> Result<(), String> {
        let mut const_functions = Vec::new();

        for stmt in &self.program {
            if let Stmt::Function {
                name,
                is_const: true,
                params,
                body,
                return_type,
                type_params,
                ..
            } = stmt
            {
                let function_data = Rc::new(FunctionData {
                    params: params.clone(),
                    body: body.clone(),
                    type_params: type_params.clone(),
                    name: Some(name.clone()),
                    return_type: return_type.clone(),
                    captures: None,
                    is_method: false,
                    is_const: true,
                    visibility: true,
                });

                self.env.scope_resolver.declare_const(&name, false);
                self.env.set_variable(&name, val!(ValueType::Function(function_data)))?;
                const_functions.push((name.clone(), params.clone(), body.clone()));
            }
        }

        for (name, params, body) in const_functions {
            if params.is_empty() {
                self.validate_const_function(&body)?;
                let result = self.call_function(self.env.get_variable(&name).unwrap().clone(), Vec::new())?;

                self.env.scope_resolver.declare_const(&name, false);
                self.env.set_variable(&name, result)?;
            }
        }

        Ok(())
    }

    pub fn validate_const_function(&self, body: &[Stmt]) -> Result<(), String> {
        for stmt in body {
            match stmt {
                Stmt::Let { initializer, .. } => {
                    if let Some(init) = initializer {
                        self.validate_const_expression(init)?;
                    }
                }

                Stmt::Return(Some(expr)) => {
                    self.validate_const_expression(expr)?;
                }

                Stmt::ExpressionStmt(expr) | Stmt::ExpressionValue(expr) => {
                    self.validate_const_expression(expr)?;
                }

                _ => return Err("This statement is not allowed in a const function".to_string()),
            }
        }

        Ok(())
    }

    pub fn validate_const_expression(&self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Binary { left, right, .. } => {
                self.validate_const_expression(left)?;
                self.validate_const_expression(right)?;
            }

            Expr::Boolean(_) | Expr::Integer(_, _) | Expr::Float(_, _) | Expr::String(_) | Expr::Unit => {}

            Expr::Identifier(name) => {
                if let Some(_) = self.env.get_variable(name) {
                    if !self.env.is_const(name) {
                        return Err(format!("Identifier `{name}` is not a constant"));
                    }
                } else {
                    return Err(format!("Identifier `{name}` is not defined"));
                }
            }

            Expr::If { condition, then_branch, else_branch } => {
                self.validate_const_expression(condition)?;
                self.validate_const_expression(then_branch)?;

                if let Some(else_expr) = else_branch {
                    self.validate_const_expression(else_expr)?;
                }
            }

            Expr::Match { value, arms } => {
                self.validate_const_expression(value)?;
                for arm in arms {
                    self.validate_const_expression(&arm.body)?;
                }
            }

            Expr::Call { function, arguments } => {
                if let Expr::Identifier(name) = &**function {
                    if let Some(value) = self.env.get_variable(name) {
                        if !matches!(value.borrow().inner(), ValueType::Function(ref func) if func.is_const) {
                            return Err(format!("Function `{name}` is not a constant function"));
                        }
                    } else {
                        return Err(format!("Function `{name}` is not defined"));
                    }
                } else {
                    return Err("Only direct function calls are allowed in constant expressions".into());
                }

                for arg in arguments {
                    self.validate_const_expression(arg)?;
                }
            }

            _ => return Err("Expression is not allowed in a const function".into()),
        }

        Ok(())
    }
}
