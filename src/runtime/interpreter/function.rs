use super::*;

struct CallFrame {
    ip: usize,
    function_data: FunctionData,
    pending_result: Option<Value>,
}

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
        let mut call_stack: Vec<CallFrame> = Vec::new();

        let function_data = match func_value.borrow().inner().clone() {
            ValueType::Function(data) => (*data).clone(),
            _ => return Err("Not a function".to_string()),
        };

        if function_data.name.clone().unwrap_or_default() == "main" && !function_data.params.is_empty() {
            return Err("main() cannot have parameters".into());
        }

        if arguments.len() != function_data.params.len() {
            return Err(format!(
                "Function '{}' expects {} arguments but got {}",
                function_data.name.as_deref().unwrap_or("anonymous"),
                function_data.params.len(),
                arguments.len()
            ));
        }

        // make dry
        self.env.enter_function_scope();
        if let Some(captures) = &function_data.captures {
            for (name, val) in captures {
                self.env.set_variable_raw(name, val.clone())?;
            }
        }
        for ((pattern, param_type), arg) in function_data.params.iter().zip(&arguments) {
            self.declare_pattern(pattern, Some(param_type), arg, true)?;
        }

        call_stack.push(CallFrame {
            ip: 0,
            function_data: function_data.clone(),
            pending_result: None,
        });

        let mut final_result = ValueEnum::unit();

        while let Some(frame) = call_stack.last_mut() {
            if frame.ip >= frame.function_data.body.len() {
                let result = frame.pending_result.clone().unwrap_or_else(|| ValueEnum::unit());
                call_stack.pop();
                self.env.exit_function_scope();

                if let Some(caller) = call_stack.last_mut() {
                    caller.pending_result = Some(result);
                } else {
                    final_result = result;
                }
                continue;
            }

            let stmt = frame.function_data.body[frame.ip].clone();
            frame.ip += 1;

            let res = self.execute_statement(&stmt)?;
            let inner = res.borrow().inner().clone();

            match inner {
                ValueType::TailCall { function, arguments } => {
                    let new_func_data = match function.borrow().inner().clone() {
                        ValueType::Function(data) => (*data).clone(),
                        _ => return Err("Tail value is not a function".into()),
                    };

                    if arguments.len() != new_func_data.params.len() {
                        return Err(format!(
                            "Function '{}' expects {} arguments but got {}",
                            new_func_data.name.as_deref().unwrap_or("anonymous"),
                            new_func_data.params.len(),
                            arguments.len()
                        ));
                    }

                    // make dry
                    self.env.enter_function_scope();
                    if let Some(captures) = &new_func_data.captures {
                        for (name, val) in captures {
                            self.env.set_variable_raw(name, val.clone())?;
                        }
                    }

                    for ((pattern, param_type), arg) in new_func_data.params.iter().zip(&arguments) {
                        self.declare_pattern(pattern, Some(param_type), arg, true)?;
                    }

                    call_stack.push(CallFrame {
                        function_data: new_func_data,
                        ip: 0,
                        pending_result: None,
                    });
                }

                ValueType::Return(val) => {
                    frame.pending_result = Some(val.clone());
                    frame.ip = frame.function_data.body.len();
                }

                ValueType::Break(_, break_value) => {
                    frame.pending_result = Some(break_value.unwrap_or(ValueEnum::unit()));
                    frame.ip = frame.function_data.body.len();
                }

                _ => {
                    frame.pending_result = Some(res.clone());
                }
            }
        }

        Ok(final_result)
    }
}
