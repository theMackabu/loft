mod array;
mod iter;
mod str;

use super::*;
use std::{cell::RefCell, rc::Rc};

pub(crate) struct Method<'st> {
    pub(crate) object: Value,
    pub(crate) call: &'st str,
    pub(crate) args: Vec<Expr>,
}

impl<'st> Method<'st> {
    fn new(object: Value, call: &'st str, args: &[Expr]) -> Self { Self { object, call, args: args.to_vec() } }
}

impl<'st> Interpreter {
    pub fn evaluate_method_call(&mut self, object: Value, method: &str, args: &[Expr]) -> Result<Value, String> {
        match method {
            "clone" => {
                if !args.is_empty() {
                    return Err("clone method does not take any arguments".to_string());
                }

                return Ok(object.borrow().deep_clone());
            }

            "is_empty" => {
                if !args.is_empty() {
                    return Err("is_empty method does not take any arguments".to_string());
                }

                let is_empty = object.borrow().inner().is_empty();
                return Ok(val!(ValueType::Boolean(is_empty)));
            }

            "type_name" => {
                if !args.is_empty() {
                    return Err("type_name method does not take any arguments".to_string());
                }

                return Ok(ValueEnum::new_str(object.borrow().kind()));
            }

            "as_ptr" => {
                if !args.is_empty() {
                    return Err("as_ptr method does not take any arguments".to_string());
                }

                return Ok(val!(ValueType::Pointer(object.as_ptr())));
            }

            "deref" => {
                if !args.is_empty() {
                    return Err("deref method does not take any arguments".to_string());
                }

                return Ok(self.resolve_value(&object)?);
            }

            "as_str" => {
                if !args.is_empty() {
                    return Err("as_str method does not take any arguments".to_string());
                }

                return Ok(ValueEnum::new_str(object.borrow().to_string()));
            }

            "as_ref" => {
                if !args.is_empty() {
                    return Err("as_ref method does not take any arguments".to_string());
                }

                let ptr = object.as_ptr();

                return Ok(val!(ValueType::Reference {
                    original_ptr: ptr,
                    source_name: None,
                    source_scope: None,
                    _undropped: object.clone(),
                }));
            }

            "as_mut" => {
                if !args.is_empty() {
                    return Err("as_mut method does not take any arguments".to_string());
                }

                let ptr = object.as_ptr();

                return Ok(val!(mut ValueType::Reference {
                    original_ptr: ptr,
                    source_name: None,
                    source_scope: None,
                    _undropped: object.clone(),
                }));
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
        let handle = Method::new(object, method, args);

        match value_type {
            // add numerical methods
            ValueType::Str(val) => self.handle_str_method_call(handle, val),

            ValueType::Array { ref ty, ref el, len } => self.handle_array_method_call(handle, ty, el, len),
            ValueType::Slice { ref ty, ref el } => self.handle_slice_method_call(handle, ty, el),

            ValueType::Struct { name, .. } => self.handle_type_method_call(handle, &name),
            ValueType::Enum { enum_type, .. } => self.handle_type_method_call(handle, &enum_type),

            range @ ValueType::Range { .. } => self.handle_range_method_call(handle, range),
            mut iter @ ValueType::Iterator { .. } => self.handle_iter_method_call(handle, &mut iter),

            _ => {
                let borrowed = handle.object.borrow();
                let type_name = borrowed.kind();

                let ref_prefix = match (borrowed.is_ref(), borrowed.is_mutable()) {
                    (true, true) => "&mut ",
                    (true, false) => "&",
                    (false, true) => "mut ",
                    (false, false) => "",
                };

                Err(format!("Cannot call {method} on {ref_prefix}{type_name}"))
            }
        }
    }

    fn resolve_value(&self, object: &Value) -> Result<Value, String> {
        let mut current_value = object.clone();

        loop {
            let object_inner = current_value.borrow().inner().clone();

            match object_inner {
                ValueType::Reference { original_ptr, .. } => {
                    if original_ptr.is_null() {
                        return Err("Reference contains null pointer".to_string());
                    }

                    unsafe {
                        let cell_ref = &*original_ptr;
                        current_value = Rc::from(cell_ref.clone());
                    }
                }

                ValueType::Pointer(ptr) => {
                    if ptr.is_null() {
                        return Err("Pointer is null".to_string());
                    }

                    unsafe {
                        let cell_ref = &*ptr;
                        current_value = Rc::from(cell_ref.clone());
                    }
                }

                _ => return Ok(current_value),
            }
        }
    }
}
