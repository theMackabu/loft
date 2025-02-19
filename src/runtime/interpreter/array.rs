use super::*;

impl Interpreter {
    pub fn handle_array_method_call(&mut self, method: &str, args: &[Expr], element_type: &ValueType, elements: &[Value], length: usize) -> Result<Value, String> {
        match method {
            "len" => Ok(val!(ValueType::USize(length))),

            "get" => {
                if args.len() != 1 {
                    return Err("Method get expects exactly one argument".to_string());
                }

                let idx_value = self.evaluate_expression(&args[0])?;
                let idx = match idx_value.borrow().inner() {
                    ValueType::USize(i) => i,
                    ValueType::I32(i) if i >= 0 => i as usize,
                    // ...handle other types
                    _ => return Err("Array index must be a non-negative integer".to_string()),
                };

                if idx >= length {
                    Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "None".to_string(),
                        data: None
                    }))
                } else {
                    Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "Some".to_string(),
                        data: Some(vec![elements[idx].clone()])
                    }))
                }
            }

            // check mutability
            "to_slice" => Ok(val!(ValueType::Slice {
                ty: Box::new(element_type.clone()),
                el: elements.to_vec(),
            })),

            _ => Err(format!("Unknown method '{}' on array type", method)),
        }
    }

    pub fn handle_slice_method_call(&mut self, slice_value: Value, method: &str, args: &[Expr], element_type: &ValueType, elements: &[Value]) -> Result<Value, String> {
        match method {
            "len" => Ok(val!(ValueType::USize(elements.len()))),

            "push" => {
                {
                    let borrowed = slice_value.borrow();
                    if let ValueType::Slice { .. } = borrowed.inner() {
                        if !borrowed.is_mutable() {
                            return Err("Cannot call push on immutable slice".to_string());
                        }
                    }
                }

                if args.is_empty() {
                    return Err("push method requires at least one argument".to_string());
                }

                let new_element = self.evaluate_expression(&args[0])?;

                if Rc::ptr_eq(&new_element, &slice_value) {
                    return Err("Cannot push a slice into itself".to_string());
                }

                let slice_element_type = {
                    let slice_ref = slice_value.borrow();
                    if let ValueType::Slice { ty, .. } = slice_ref.inner() {
                        ty.clone()
                    } else {
                        return Err("Not a slice".to_string());
                    }
                };

                {
                    let new_elem_inner = new_element.borrow();
                    if !self.types_are_compatible(&new_elem_inner.inner(), &slice_element_type) {
                        return Err("Type mismatch: cannot push element to slice".to_string());
                    }
                }

                {
                    let mut slice_ref = slice_value.borrow_mut();
                    if let ValueType::Slice { el: ref mut elements, .. } = slice_ref.inner_mut() {
                        elements.push(new_element);
                    }
                }

                Ok(slice_value)
            }

            "pop" => {
                let is_mutable = {
                    let borrowed = slice_value.borrow();
                    if let ValueType::Slice { .. } = borrowed.inner() {
                        borrowed.is_mutable()
                    } else {
                        false
                    }
                };

                if !is_mutable {
                    return Err("Cannot call push on immutable slice".to_string());
                }

                let popped = {
                    let mut slice_ref = slice_value.borrow_mut();
                    if let ValueType::Slice { ref mut el, .. } = slice_ref.inner_mut() {
                        if el.is_empty() {
                            None
                        } else {
                            Some(el.pop().unwrap())
                        }
                    } else {
                        None
                    }
                };

                match popped {
                    Some(value) => Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "Some".to_string(),
                        data: Some(vec![value])
                    })),
                    None => Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "None".to_string(),
                        data: None
                    })),
                }
            }

            "get" => {
                if args.len() != 1 {
                    return Err("Method get expects exactly one argument".to_string());
                }

                let idx_value = self.evaluate_expression(&args[0])?;
                let idx = match idx_value.borrow().inner() {
                    ValueType::USize(i) => i,
                    ValueType::I32(i) if i >= 0 => i as usize,
                    // ...handle other types
                    _ => return Err("Slice index must be a non-negative integer".to_string()),
                };

                if idx >= elements.len() {
                    Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "None".to_string(),
                        data: None
                    }))
                } else {
                    Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "Some".to_string(),
                        data: Some(vec![elements[idx].clone()])
                    }))
                }
            }

            "slice" => {
                if args.len() != 2 {
                    return Err("Method slice expects exactly two arguments".to_string());
                }

                let start_value = self.evaluate_expression(&args[0])?;
                let end_value = self.evaluate_expression(&args[1])?;

                let start = match start_value.borrow().inner() {
                    ValueType::USize(i) => i,
                    ValueType::I32(i) if i >= 0 => i as usize,
                    // ...handle other types
                    _ => return Err("Slice start index must be a non-negative integer".to_string()),
                };

                let end = match end_value.borrow().inner() {
                    ValueType::USize(i) => i,
                    ValueType::I32(i) if i >= 0 => i as usize,
                    // ...handle other types
                    _ => return Err("Slice end index must be a non-negative integer".to_string()),
                };

                if start > end || end > elements.len() {
                    return Err(format!("Invalid slice range: {}..{} (slice length: {})", start, end, elements.len()));
                }

                let new_elements = elements[start..end].to_vec();

                Ok(val!(ValueType::Slice {
                    ty: Box::new(element_type.clone()),
                    el: new_elements,
                }))
            }

            // add more methods: concat, filter, map, iter, etc.
            _ => Err(format!("Unknown method '{}' on slice type", method)),
        }
    }

    fn types_are_compatible(&self, actual: &ValueType, expected: &ValueType) -> bool {
        match (actual, expected) {
            (ValueType::I8(_), ValueType::I8(_)) => true,
            (ValueType::I16(_), ValueType::I16(_)) => true,
            (ValueType::I32(_), ValueType::I32(_)) => true,
            (ValueType::I64(_), ValueType::I64(_)) => true,
            (ValueType::I128(_), ValueType::I128(_)) => true,
            (ValueType::ISize(_), ValueType::ISize(_)) => true,

            (ValueType::U8(_), ValueType::U8(_)) => true,
            (ValueType::U16(_), ValueType::U16(_)) => true,
            (ValueType::U32(_), ValueType::U32(_)) => true,
            (ValueType::U64(_), ValueType::U64(_)) => true,
            (ValueType::U128(_), ValueType::U128(_)) => true,
            (ValueType::USize(_), ValueType::USize(_)) => true,

            (ValueType::F32(_), ValueType::F32(_)) => true,
            (ValueType::F64(_), ValueType::F64(_)) => true,

            (ValueType::Boolean(_), ValueType::Boolean(_)) => true,
            (ValueType::Str(_), ValueType::Str(_)) => true,
            (ValueType::Unit, ValueType::Unit) => true,

            _ => false,
        }
    }
}
