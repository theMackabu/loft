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
                const_functions.push((name.clone(), params.clone(), body.clone(), return_type.clone(), type_params.clone()));
            }
        }

        for (name, params, body, return_type, type_params) in const_functions {
            self.validate_const_function(&body)?;

            let function_data = Rc::new(FunctionData {
                params: params.clone(),
                body,
                type_params,
                name: Some(name.clone()),
                return_type,
                captures: None,
                is_method: false,
                is_const: true,
                visibility: true,
            });

            self.env.set_variable(&name, val!(ValueType::Function(function_data)))?;

            if params.is_empty() {
                let result = self.call_function(self.env.get_variable(&name).unwrap().clone(), Vec::new())?;
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
                    // check if it's a const value
                }
            }

            _ => return Err("Expression is not allowed in a const function".into()),
        }
        Ok(())
    }
}
