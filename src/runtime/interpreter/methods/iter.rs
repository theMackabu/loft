use super::*;

impl<'st> Interpreter {
    pub(crate) fn handle_range_method_call(&mut self, handle: Method, range: ValueType) -> Result<Value, String> {
        let Method { object, args, call } = handle;
        unbind! { ValueType::Range { start, end, inclusive } = range }

        match call {
            "iter" => {
                if !args.is_empty() {
                    return Err("iter method does not take any arguments".to_string());
                }

                let iter = val!(ValueType::Iterator {
                    current: start.clone(),
                    end: end.clone(),
                    inclusive,
                    exhausted: false,
                    collection: object,
                    kind: "range".to_string(),
                });

                return Ok(iter);
            }

            _ => Err(format!("Method '{call}' not found on Range type")),
        }
    }

    pub(crate) fn handle_iter_method_call(&mut self, handle: Method, iter: &mut ValueType) -> Result<Value, String> {
        let Method { args, call, .. } = handle;

        unbind! {
            &mut ValueType::Iterator {
                ref mut current,
                ref mut exhausted,
                ref inclusive,
                ref collection,
                ref end,
                ref kind,
            } = iter
        }

        match call {
            "next" => {
                if !args.is_empty() {
                    return Err("next method does not take any arguments".to_string());
                }

                if *exhausted {
                    return Ok(val!(ValueType::Unit));
                }

                match kind.as_str() {
                    "range" => {
                        let current_val = current.clone();

                        let curr_num = extract_number(&current_val)?;
                        let end_num = extract_number(&end)?;

                        let next_val = match current_val.borrow().inner() {
                            ValueType::I64(n) => val!(ValueType::I64(n + 1)),
                            ValueType::I32(n) => val!(ValueType::I32(n + 1)),
                            ValueType::I16(n) => val!(ValueType::I16(n + 1)),
                            ValueType::I8(n) => val!(ValueType::I8(n + 1)),
                            ValueType::U64(n) => val!(ValueType::U64(n + 1)),
                            ValueType::U32(n) => val!(ValueType::U32(n + 1)),
                            ValueType::U16(n) => val!(ValueType::U16(n + 1)),
                            ValueType::U8(n) => val!(ValueType::U8(n + 1)),

                            _ => return Err("Iterator contains non-numeric type".to_string()),
                        };

                        *current = next_val;
                        if *inclusive {
                            if curr_num > end_num {
                                *exhausted = true;
                            }
                        } else {
                            if curr_num >= end_num {
                                *exhausted = true;
                            }
                        }

                        return Ok(current_val);
                    }

                    "array" | "slice" => {
                        let index = match current.borrow().inner() {
                            ValueType::I64(idx) => idx as usize,
                            _ => return Err("Iterator index is not an integer".to_string()),
                        };

                        let element = match collection.borrow().inner() {
                            ValueType::Array { ref el, .. } | ValueType::Slice { ref el, .. } => {
                                if index < el.len() {
                                    el[index].clone()
                                } else {
                                    return Err("Iterator index out of bounds".to_string());
                                }
                            }
                            _ => return Err("Iterator collection type mismatch".to_string()),
                        };

                        *current = val!(ValueType::I64((index + 1) as i64));

                        let end_idx = match end.borrow().inner() {
                            ValueType::I64(idx) => idx,
                            _ => return Err("Iterator end is not an integer".to_string()),
                        };

                        if (index + 1) as i64 >= end_idx {
                            *exhausted = true;
                        }

                        return Ok(element);
                    }

                    _ => return Err(format!("Unknown iterator kind: {}", kind)),
                }
            }

            "has_next" => {
                if !args.is_empty() {
                    return Err("has_next method does not take any arguments".to_string());
                }

                if *exhausted {
                    return Ok(val!(ValueType::Boolean(false)));
                }

                match kind.as_str() {
                    "range" => {
                        let curr_num = extract_number(&current)?;
                        let end_num = extract_number(&end)?;

                        let has_next = if *inclusive { curr_num <= end_num } else { curr_num < end_num };

                        return Ok(val!(ValueType::Boolean(has_next)));
                    }

                    "array" | "slice" => {
                        let index = match current.borrow().inner() {
                            ValueType::I64(idx) => idx as usize,
                            _ => return Err("Iterator index is not an integer".to_string()),
                        };

                        let end_idx = match end.borrow().inner() {
                            ValueType::I64(idx) => idx as usize,
                            _ => return Err("Iterator end is not an integer".to_string()),
                        };

                        return Ok(val!(ValueType::Boolean(index < end_idx)));
                    }

                    _ => return Err(format!("Unknown iterator kind: {}", kind)),
                }
            }

            _ => Err(format!("Method '{call}' not found on Iterator type")),
        }
    }
}

fn extract_number(value: &Value) -> Result<i64, String> {
    match value.borrow().inner() {
        ValueType::I8(n) => Ok(n as i64),
        ValueType::I16(n) => Ok(n as i64),
        ValueType::I32(n) => Ok(n as i64),
        ValueType::I64(n) => Ok(n),
        ValueType::ISize(n) => Ok(n as i64),
        ValueType::U8(n) => Ok(n as i64),
        ValueType::U16(n) => Ok(n as i64),
        ValueType::U32(n) => Ok(n as i64),

        ValueType::U64(n) => {
            if n > i64::MAX as u64 {
                return Err(format!("Value {} is too large for range iteration", n));
            }
            Ok(n as i64)
        }

        ValueType::USize(n) => {
            if n > i64::MAX as usize {
                return Err(format!("Value {} is too large for range iteration", n));
            }
            Ok(n as i64)
        }

        _ => Err(format!("Cannot use non-numeric type in range iteration: {}", value.borrow().kind())),
    }
}
