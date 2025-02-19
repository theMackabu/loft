use super::*;
use std::{cell::RefCell, rc::Rc};

impl<'st> Interpreter<'st> {
    pub fn evaluate_method_call(&mut self, object: Value, method: &str, args: &[Expr]) -> Result<Value, String> {
        match method {
            "clone" => {
                if !args.is_empty() {
                    return Err("clone method does not take any arguments".to_string());
                }
                return Ok(object.borrow().deep_clone());
            }

            "to_string" => {
                if !args.is_empty() {
                    return Err("to_string method does not take any arguments".to_string());
                }
                return Ok(val!(ValueType::Str(object.borrow().to_string())));
            }

            _ => {}
        }

        let value_info = self.get_effective_value(&object)?;
        self.dispatch_method_call(value_info, method, args)
    }

    fn get_effective_value(&self, object: &Value) -> Result<(ValueType, Value), String> {
        let object_inner = object.borrow().inner().clone();

        if let ValueType::Reference { original_ptr, .. } = object_inner {
            if original_ptr.is_null() {
                return Err("Reference contains null pointer".to_string());
            }

            unsafe {
                let cell_ref = &*original_ptr;
                let inner = cell_ref.borrow().inner().clone();
                Ok((inner, object.clone()))
            }
        } else {
            Ok((object_inner, object.clone()))
        }
    }

    fn dispatch_method_call(&mut self, (value_type, object): (ValueType, Value), method: &str, args: &[Expr]) -> Result<Value, String> {
        match value_type {
            // object, method, etc, args, name (make into struct)
            ValueType::Array { ref ty, ref el, len } => self.handle_array_method_call(object, method, args, ty, el, len),

            ValueType::Slice { ref ty, ref el } => self.handle_slice_method_call(object, method, args, ty, el),

            ValueType::Struct { name, .. } => self.handle_struct_method_call(object, &name, method, args),

            _ => {
                let borrowed = object.borrow();
                let type_name = borrowed.kind();

                let ref_prefix = match (borrowed.is_ref(), borrowed.is_mutable()) {
                    (true, true) => "&mut ",
                    (true, false) => "& ",
                    (false, true) => "mut ",
                    (false, false) => "",
                };

                Err(format!("Cannot call {method} on {ref_prefix}{type_name}"))
            }
        }
    }
}
