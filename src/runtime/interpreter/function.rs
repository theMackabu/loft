use super::*;

impl<'st> Interpreter {
    pub fn handle_function_declaration(&mut self, name: &str, params: Vec<(Pattern, Type)>, body: Vec<Stmt>, return_type: &Option<Type>, visibility: bool) -> Result<(), String> {
        let function_data = Rc::new(FunctionData {
            params,
            body,

            captures: None,
            is_method: false,
            visibility,

            name: Some(name.to_string()),
            return_type: return_type.clone(),
        });

        self.env.set_variable(&name, val!(ValueType::Function(function_data)))?;

        Ok(())
    }

    pub fn execute_program(&mut self) -> Result<Value, String> {
        for stmt in self.program.clone() {
            if let Stmt::Function {
                name,
                params,
                body,
                return_type,
                visibility,
                ..
            } = stmt
            {
                self.handle_function_declaration(&name, params.to_vec(), body.to_vec(), &return_type, visibility)?;
            } else {
                self.execute_statement(&stmt)?;
            }
        }

        if let Some(main_func) = self.env.get_variable("main") {
            let result = self.call_function(main_func.clone(), Vec::new())?;

            let result_inner = {
                let borrowed = result.borrow();
                borrowed.inner()
            };

            match result_inner {
                // improve
                ValueType::Enum { enum_type, variant, data } if enum_type == "Result" => match variant.as_str() {
                    "Ok" => match data {
                        Some(values) if !values.is_empty() => Ok(values[0].clone()),
                        _ => Ok(val!(ValueType::Unit)),
                    },

                    "Err" => match data {
                        Some(values) if !values.is_empty() => Err(values[0].borrow().to_string()),
                        _ => Err("Unknown error".to_string()),
                    },

                    _ => Ok(val!(ValueType::Enum {
                        enum_type: enum_type.clone(),
                        variant: variant.clone(),
                        data: data.clone(),
                    })),
                },

                _ => Ok(result),
            }
        } else {
            Ok(ValueEnum::unit())
        }
    }

    pub fn call_function(&mut self, func_value: Value, arguments: Vec<Value>) -> Result<Value, String> {
        let mut current_func = func_value;
        let mut current_args = arguments;

        loop {
            let function_data = match current_func.borrow().inner() {
                ValueType::Function(data) => data.clone(),
                _ => return Err("Not a function".to_string()),
            };

            if function_data.name.clone().unwrap_or("".into()) == "main" && function_data.params.len() > 0 {
                return Err("main() cannot have parameters".into());
            }

            if current_args.len() != function_data.params.len() {
                return Err(format!(
                    "Function '{}' expects {} arguments but got {}",
                    function_data.name.as_deref().unwrap_or("anonymous"),
                    function_data.params.len(),
                    current_args.len()
                ));
            }

            self.env.enter_function_scope();

            if let Some(captures) = &function_data.captures {
                for (name, val) in captures {
                    self.env.set_variable_raw(name, val.clone())?;
                }
            }

            for ((pattern, param_type), arg) in function_data.params.iter().zip(&current_args) {
                self.declare_pattern(pattern, Some(param_type), arg, true)?;
            }

            let mut continue_with_tail_call = false;
            let mut result = val!(ValueType::Unit);

            for stmt in function_data.body.clone() {
                result = self.execute_statement(&stmt)?;

                let tail_call_info = {
                    if let ValueType::TailCall { function, arguments } = result.borrow().inner() {
                        Some((function.clone(), arguments.clone()))
                    } else {
                        None
                    }
                };

                if let Some((function, arguments)) = tail_call_info {
                    current_func = function;
                    current_args = arguments;
                    continue_with_tail_call = true;
                    break;
                }

                let return_value = { if let ValueType::Return(val) = result.borrow().inner() { Some(val.clone()) } else { None } };

                if let Some(val) = return_value {
                    result = val;
                    self.env.exit_function_scope();
                    return Ok(result);
                }
            }

            self.env.exit_function_scope();

            if continue_with_tail_call {
                continue;
            } else {
                return Ok(result);
            }
        }
    }
}
