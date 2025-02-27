use super::*;

pub struct CallFrame {
    ip: usize,
    function_data: FunctionData,
    pending_result: Option<Value>,
}

enum Bounce {
    Continue,
    Result(Value),
    Error(String),
}

impl<'st> Interpreter {
    pub fn handle_function_declaration(
        &mut self, name: &str, params: Vec<(Pattern, Type)>, body: Vec<Stmt>, return_type: &Option<Type>, visibility: bool, is_const: bool, type_params: Vec<GenericParam>,
    ) -> Result<(), String> {
        let function_data = Rc::new(FunctionData {
            params,
            body,
            type_params,

            captures: None,
            is_method: false,
            is_const,
            visibility,

            name: Some(name.to_string()),
            return_type: return_type.clone(),
        });

        self.env.set_variable(&name, val!(ValueType::Function(function_data)))?;

        Ok(())
    }

    pub fn execute_program(&mut self) -> Result<Value, String> {
        self.evaluate_const_functions()?;

        for stmt in self.program.clone() {
            if let Stmt::Function {
                name,
                params,
                body,
                return_type,
                visibility,
                is_const,
                type_params,
                ..
            } = stmt
            {
                if !is_const {
                    self.handle_function_declaration(&name, params.to_vec(), body.to_vec(), &return_type, visibility, is_const, type_params)?;
                }
            } else {
                self.execute_statement(&stmt)?;
            }
        }

        if let Some(main_func) = self.env.get_variable("main") {
            let mut result = self.call_function(main_func.clone(), Vec::new())?;

            loop {
                let inner = {
                    let borrowed = result.borrow();
                    borrowed.inner().clone()
                };

                if let ValueType::TailCall { function, arguments } = inner {
                    result = self.call_function(function.clone(), arguments)?;
                } else {
                    break;
                }
            }

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
        let previous_call_stack = std::mem::take(&mut self.current_call_stack);
        let bounce = self.call_function_bounce(func_value, arguments)?;
        let result = self.trampoline(bounce)?;

        self.current_call_stack = previous_call_stack;

        Ok(result)
    }

    fn trampoline(&mut self, mut bounce: Bounce) -> Result<Value, String> {
        loop {
            match bounce {
                Bounce::Continue => bounce = self.execute_function_step(),
                Bounce::Result(value) => return Ok(value),
                Bounce::Error(err) => return Err(err),
            }
        }
    }

    fn call_function_bounce(&mut self, func_value: Value, arguments: Vec<Value>) -> Result<Bounce, String> {
        let function_data = match func_value.borrow().inner().clone() {
            ValueType::Function(data) => (*data).clone(),
            _ => return Err("Not a function".to_string()),
        };

        self.fnc_name = function_data.name.clone();

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

        self.env.enter_function_scope();
        if let Some(captures) = &function_data.captures {
            for (name, val) in captures {
                self.env.set_variable_raw(name, val.clone())?;
            }
        }
        for ((pattern, param_type), arg) in function_data.params.iter().zip(&arguments) {
            self.declare_pattern(pattern, Some(param_type), arg, true)?;
        }

        let initial_frame = CallFrame {
            ip: 0,
            function_data: function_data.clone(),
            pending_result: None,
        };

        self.current_call_stack.push(initial_frame);

        Ok(Bounce::Continue)
    }

    fn execute_function_step(&mut self) -> Bounce {
        if self.current_call_stack.is_empty() {
            self.fnc_name = None;
            return Bounce::Result(ValueEnum::unit());
        }

        let frame_index = self.current_call_stack.len() - 1;
        if frame_index >= self.current_call_stack.len() {
            return Bounce::Result(val!(ValueType::Unit));
        }

        if self.current_call_stack[frame_index].ip >= self.current_call_stack[frame_index].function_data.body.len() {
            let result = self.current_call_stack[frame_index].pending_result.clone().unwrap_or_else(|| ValueEnum::unit());

            self.current_call_stack.pop();
            self.env.exit_function_scope();

            if !self.current_call_stack.is_empty() {
                let caller_index = self.current_call_stack.len() - 1;
                self.current_call_stack[caller_index].pending_result = Some(result);
                return Bounce::Continue;
            } else {
                self.fnc_name = None;
                return Bounce::Result(result);
            }
        }

        let body_len = self.current_call_stack[frame_index].function_data.body.len();
        let is_tail = body_len > 0 && self.current_call_stack[frame_index].ip == body_len - 1;

        let curr_ip = self.current_call_stack[frame_index].ip;
        if curr_ip >= body_len {
            return Bounce::Result(val!(ValueType::Unit));
        }

        let stmt = self.current_call_stack[frame_index].function_data.body[curr_ip].clone();
        self.current_call_stack[frame_index].ip += 1;

        if is_tail {
            if let Stmt::ExpressionStmt(expr_box) = stmt.clone() {
                if let Expr::Call { function, arguments } = expr_box {
                    let is_self_call = match &self.current_call_stack[frame_index].function_data.name {
                        Some(current_name) => {
                            if let Expr::Identifier(called_name) = &*function {
                                current_name == called_name
                            } else {
                                false
                            }
                        }
                        None => false,
                    };

                    if is_self_call {
                        let func_val = match self.evaluate_expression(&function) {
                            Ok(val) => val,
                            Err(err) => return Bounce::Error(err),
                        };

                        let mut arg_values = Vec::with_capacity(arguments.len());

                        for arg in arguments {
                            match self.evaluate_expression(&arg) {
                                Ok(val) => arg_values.push(val),
                                Err(err) => return Bounce::Error(err),
                            }
                        }

                        if !self.current_call_stack.is_empty() {
                            self.current_call_stack.pop();
                        }
                        self.env.exit_function_scope();

                        match self.call_function_bounce(func_val, arg_values) {
                            Ok(_) => return Bounce::Continue,
                            Err(err) => return Bounce::Error(err),
                        }
                    }
                }
            }
        }

        let res = match self.execute_statement(&stmt) {
            Ok(value) => value,
            Err(err) => return Bounce::Error(err),
        };

        let inner = res.borrow().inner().clone();

        match inner {
            ValueType::TailCall { function, arguments } => {
                let new_func_data = match function.borrow().inner().clone() {
                    ValueType::Function(data) => (*data).clone(),
                    _ => return Bounce::Error("Not a function".to_string()),
                };

                if arguments.len() != new_func_data.params.len() {
                    return Bounce::Error("Argument count mismatch".to_string());
                }

                if !self.current_call_stack.is_empty() {
                    self.current_call_stack.pop();
                }
                self.env.exit_function_scope();

                self.env.enter_function_scope();
                if let Some(captures) = &new_func_data.captures {
                    for (name, val) in captures {
                        if let Err(err) = self.env.set_variable_raw(name, val.clone()) {
                            return Bounce::Error(err);
                        }
                    }
                }

                for ((pattern, param_type), arg) in new_func_data.params.iter().zip(&arguments) {
                    if let Err(err) = self.declare_pattern(pattern, Some(param_type), arg, true) {
                        return Bounce::Error(err);
                    }
                }

                self.current_call_stack.push(CallFrame {
                    ip: 0,
                    function_data: new_func_data,
                    pending_result: None,
                });
            }

            ValueType::Return(val) => {
                if !self.current_call_stack.is_empty() {
                    let current_index = self.current_call_stack.len() - 1;
                    self.current_call_stack[current_index].pending_result = Some(val.clone());
                    self.current_call_stack[current_index].ip = self.current_call_stack[current_index].function_data.body.len();
                }
            }

            ValueType::Break(_, break_value) => {
                if !self.current_call_stack.is_empty() {
                    let current_index = self.current_call_stack.len() - 1;
                    self.current_call_stack[current_index].pending_result = Some(break_value.unwrap_or(ValueEnum::unit()));
                    self.current_call_stack[current_index].ip = self.current_call_stack[current_index].function_data.body.len();
                }
            }

            _ => {
                if !self.current_call_stack.is_empty() {
                    let current_index = self.current_call_stack.len() - 1;
                    self.current_call_stack[current_index].pending_result = Some(res.clone());
                }
            }
        }

        Bounce::Continue
    }
}
