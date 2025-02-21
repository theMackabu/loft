use super::*;

impl<'st> Interpreter<'st> {
    pub fn handle_str_method_call(&mut self, object: Value, method: &str, args: &[Expr], value: String) -> Result<Value, String> {
        match method {
            "len" => {
                if !args.is_empty() {
                    return Err("len method does not take any arguments".to_string());
                }
                let len = value.len();
                Ok(val!(ValueType::USize(len)))
            }

            "push" => {
                if args.len() != 1 {
                    return Err("push method takes exactly one argument".to_string());
                }

                let arg_value = self.evaluate_expression(&args[0])?;
                if let ValueType::Str(push_value) = arg_value.borrow().inner() {
                    if push_value.len() != 1 {
                        return Err("push method requires a single character".to_string());
                    }

                    let mut object_mut = object.borrow_mut();
                    if let ValueEnum::Mutable(ValueType::Str(ref mut string)) = *object_mut {
                        string.push_str(&push_value);
                        Ok(object.clone())
                    } else {
                        Err("Cannot call push on an immutable string".to_string())
                    }
                } else {
                    Err("push method requires a string argument".to_string())
                }
            }

            "pop" => {
                if !args.is_empty() {
                    return Err("pop method does not take any arguments".to_string());
                }

                let mut object_mut = object.borrow_mut();
                if let ValueEnum::Mutable(ValueType::Str(ref mut string)) = *object_mut {
                    if let Some(popped_char) = string.pop() {
                        Ok(val!(ValueType::Str(popped_char.to_string())))
                    } else {
                        Err("Cannot pop from an empty string".to_string())
                    }
                } else {
                    Err("Cannot call pop on an immutable string".to_string())
                }
            }

            _ => Err(format!("Unknown method `{}` for string", method)),
        }
    }
}
