use super::*;

impl<'st> Interpreter {
    pub(crate) fn handle_array_method_call(&mut self, handle: Method, element_type: &ValueType, elements: &[Value], length: usize) -> Result<Value, String> {
        let Method { object, args, call } = handle;

        match call {
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

            "to_slice" => {
                if !args.is_empty() {
                    return Err("to_slice method does not take any arguments".to_string());
                }

                let slice = ValueType::Slice {
                    ty: Box::new(element_type.clone()),
                    el: elements.to_vec(),
                };

                let is_mutable = object.borrow().is_mutable();

                Ok(if is_mutable { val!(mut slice) } else { val!(slice) })
            }

            "iter" => {
                if !args.is_empty() {
                    return Err("into_iter method does not take any arguments".to_string());
                }

                let iter = val!(ValueType::Iterator {
                    current: val!(ValueType::I64(0)),
                    end: val!(ValueType::I64(length as i64)),
                    inclusive: false,
                    exhausted: length == 0,
                    collection: object,
                    kind: "array".to_string(),
                });

                return Ok(iter);
            }

            _ => Err(format!("Unknown method '{call}' on array type")),
        }
    }

    pub(crate) fn handle_slice_method_call(&mut self, handle: Method, element_type: &ValueType, elements: &[Value]) -> Result<Value, String> {
        let Method { object, args, call } = handle;

        match call {
            "len" => Ok(val!(ValueType::USize(elements.len()))),

            "push" => {
                {
                    let borrowed = object.borrow();
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

                if Rc::ptr_eq(&new_element, &object) {
                    return Err("Cannot push a slice into itself".to_string());
                }

                let slice_element_type = {
                    let slice_ref = object.borrow();
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
                    let mut slice_ref = object.borrow_mut();
                    if let ValueType::Slice { el, .. } = slice_ref.inner_mut() {
                        el.push(new_element);
                    }
                }

                Ok(object)
            }

            "pop" => {
                let (is_empty, is_mutable) = {
                    let borrowed = object.borrow();
                    if let ValueType::Slice { ref el, .. } = borrowed.inner() {
                        (el.is_empty(), borrowed.is_mutable())
                    } else {
                        return Err("Expected a slice value".to_string());
                    }
                };

                if is_empty {
                    return Ok(val!(ValueType::Enum {
                        enum_type: "Option".to_string(),
                        variant: "None".to_string(),
                        data: None
                    }));
                }

                if !is_mutable {
                    return Err("Cannot pop from immutable slice".to_string());
                }

                let popped = {
                    let mut slice_ref = object.borrow_mut();
                    if let ValueType::Slice { el, .. } = slice_ref.inner_mut() {
                        el.pop().unwrap()
                    } else {
                        unreachable!("Already verified this is a slice")
                    }
                };

                Ok(val!(ValueType::Enum {
                    enum_type: "Option".to_string(),
                    variant: "Some".to_string(),
                    data: Some(vec![popped])
                }))
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

            "iter" => {
                if !args.is_empty() {
                    return Err("iter method does not take any arguments".to_string());
                }

                let iter = val!(ValueType::Iterator {
                    current: val!(ValueType::I64(0)),
                    end: val!(ValueType::I64(elements.len() as i64)),
                    inclusive: false,
                    exhausted: elements.is_empty(),
                    collection: object,
                    kind: "slice".to_string(),
                });

                return Ok(iter);
            }

            // add more methods: concat, filter, map, etc.
            _ => Err(format!("Unknown method '{call}' on slice type")),
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
