use super::*;

impl<'st> Interpreter<'st> {
    pub(crate) fn handle_str_method_call(&mut self, handle: Method, value: String) -> Result<Value, String> {
        let Method { object, args, call } = handle;

        match call {
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
                let actual_value = match arg_value.borrow().inner() {
                    ValueType::Reference {
                        source_name: _,
                        source_scope: _,
                        original_ptr: _,
                        _undropped: ref value,
                    } => value.borrow().inner(),
                    other => other,
                };

                if let ValueType::Str(push_value) = actual_value {
                    let mut object_mut = object.borrow_mut();
                    match *object_mut {
                        ValueEnum::Mutable(ValueType::Str(ref mut string)) => {
                            string.push_str(&push_value);
                            Ok(object.clone())
                        }

                        ValueEnum::Mutable(ValueType::Reference { _undropped: ref value, .. }) => {
                            let mut value_ref = value.borrow_mut();
                            if let ValueEnum::Mutable(ValueType::Str(ref mut string)) = *value_ref {
                                string.push_str(&push_value);
                                Ok(object.clone())
                            } else {
                                Err("Cannot call push on an immutable string".to_string())
                            }
                        }

                        _ => Err("Cannot call push on an immutable string".to_string()),
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

            _ => Err(format!("Unknown method `{call}` for string")),
        }
    }
}
