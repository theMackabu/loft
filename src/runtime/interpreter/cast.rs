use super::*;
use std::mem;

impl Interpreter {
    pub fn perform_cast(&mut self, value: Value, target_type: &Type) -> Result<Value, String> {
        match target_type {
            // it cannot cast references like: let big_num: &i64 = 50;
            Type::Reference { mutable, inner } => {
                let value_inner = {
                    let borrowed = value.borrow();
                    borrowed.inner()
                };

                match value_inner {
                    ValueType::Reference {
                        source_name,
                        source_scope,
                        original_ptr,
                        _undropped,
                    } => {
                        if original_ptr.is_null() {
                            return Err("Reference contains null pointer".to_string());
                        }

                        if let (Some(src_name), Some(src_scope)) = (source_name.clone(), source_scope) {
                            let scope = self.env.scopes.get(src_scope).ok_or_else(|| format!("Reference scope {} not found", src_scope))?;
                            let inner_value = scope.get(&src_name).ok_or_else(|| format!("Reference source '{}' not found", src_name))?;
                            let casted_inner = self.perform_cast(inner_value.clone(), inner)?;
                            self.env.update_scoped_variable(&src_name, casted_inner.clone(), src_scope)?;

                            let reference = ValueType::Reference {
                                source_name: Some(src_name),
                                source_scope: Some(src_scope),
                                original_ptr: Rc::as_ptr(&casted_inner),
                                _undropped: casted_inner.clone(),
                            };

                            Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                        } else {
                            unsafe {
                                let original_rc = Rc::from_raw(original_ptr);
                                let original_rc_clone = original_rc.clone();
                                mem::forget(original_rc);

                                let casted_inner = self.perform_cast(original_rc_clone, inner)?;

                                let reference = ValueType::Reference {
                                    source_name: None,
                                    source_scope: None,
                                    original_ptr: Rc::as_ptr(&casted_inner),
                                    _undropped: casted_inner.clone(),
                                };

                                Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                            }
                        }
                    }

                    _ => {
                        let casted = self.perform_cast(value, inner)?;
                        let temp_name = self.env.generate_temp_reference_name();
                        let current_scope = self.env.get_current_scope();
                        self.env.update_scoped_variable(&temp_name, casted.clone(), current_scope)?;

                        let reference = ValueType::Reference {
                            source_name: Some(temp_name),
                            source_scope: Some(current_scope),
                            original_ptr: Rc::as_ptr(&casted),
                            _undropped: casted.clone(),
                        };

                        Ok(if *mutable { val!(mut reference) } else { val!(reference) })
                    }
                }
            }

            Type::Path(path) if path.segments.len() == 1 => {
                let new_value = {
                    let inner_val = {
                        let borrowed = value.borrow();
                        borrowed.inner()
                    };
                    match inner_val {
                        ValueType::Reference {
                            source_name,
                            source_scope,
                            original_ptr,
                            _undropped,
                        } => {
                            if original_ptr.is_null() {
                                return Err("Reference contains null pointer".to_string());
                            }

                            if let (Some(src_name), Some(src_scope)) = (source_name.clone(), source_scope) {
                                if let Some(scope) = self.env.scopes.get(src_scope) {
                                    if let Some(val) = scope.get(&src_name) {
                                        val.clone()
                                    } else {
                                        return Err(format!("Reference source '{}' not found", src_name));
                                    }
                                } else {
                                    return Err(format!("Reference scope {} not found", src_scope));
                                }
                            } else {
                                unsafe {
                                    let original_rc = Rc::from_raw(original_ptr);
                                    let original_rc_clone = original_rc.clone();
                                    mem::forget(original_rc);
                                    original_rc_clone
                                }
                            }
                        }

                        _ => value.clone(),
                    }
                };

                let type_name = &path.segments[0].ident;
                match type_name.as_str() {
                    "bool" => self.cast_to_bool::<bool>(new_value),

                    "i8" => self.cast_to_int::<i8>(new_value),
                    "i16" => self.cast_to_int::<i16>(new_value),
                    "i32" => self.cast_to_int::<i32>(new_value),
                    "i64" => self.cast_to_int::<i64>(new_value),
                    "i128" => self.cast_to_int::<i128>(new_value),
                    "isize" => self.cast_to_int::<isize>(new_value),

                    "u8" => self.cast_to_uint::<u8>(new_value),
                    "u16" => self.cast_to_uint::<u16>(new_value),
                    "u32" => self.cast_to_uint::<u32>(new_value),
                    "u64" => self.cast_to_uint::<u64>(new_value),
                    "u128" => self.cast_to_uint::<u128>(new_value),
                    "usize" => self.cast_to_uint::<usize>(new_value),

                    "f32" => self.cast_to_float::<f32>(new_value),
                    "f64" => self.cast_to_float::<f64>(new_value),

                    _ => Err(format!("Unsupported cast to type: {}", type_name)),
                }
            }

            _ => Err("Invalid cast target type".to_string()),
        }
    }

    fn cast_to_bool<T>(&self, value: Value) -> Result<Value, String>
    where
        T: TryFrom<bool> + 'static,
    {
        let inner_value = {
            let borrowed = value.borrow();
            borrowed.inner()
        };

        match inner_value {
            ValueType::Boolean(b) => Ok(val!(ValueType::Boolean(b))),

            ValueType::I8(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::I16(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::I32(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::I64(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::I128(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::ISize(v) => Ok(val!(ValueType::Boolean(v != 0))),

            ValueType::U8(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::U16(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::U32(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::U64(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::U128(v) => Ok(val!(ValueType::Boolean(v != 0))),
            ValueType::USize(v) => Ok(val!(ValueType::Boolean(v != 0))),

            ValueType::F32(v) => Ok(val!(ValueType::Boolean(v != 0.0))),
            ValueType::F64(v) => Ok(val!(ValueType::Boolean(v != 0.0))),

            ValueType::Str(ref s) => Ok(val!(ValueType::Boolean(!s.is_empty()))),

            _ => Err("Cannot cast value to bool".to_string()),
        }
    }

    fn cast_to_int<T>(&self, value: Value) -> Result<Value, String>
    where
        T: TryFrom<i64> + TryFrom<u64> + 'static,
    {
        let inner_value = {
            let borrowed = value.borrow();
            borrowed.inner()
        };

        match inner_value {
            ValueType::I8(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::I16(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::I32(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::I64(v) => self.numeric_to_int::<T>(v),
            ValueType::I128(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::ISize(v) => self.numeric_to_int::<T>(v as i64),

            ValueType::U8(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::U16(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::U32(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::U64(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::U128(v) => self.numeric_to_int::<T>(v as i64),
            ValueType::USize(v) => self.numeric_to_int::<T>(v as i64),

            ValueType::F32(v) => self.float_to_int::<T>(v as f64),
            ValueType::F64(v) => self.float_to_int::<T>(v),

            _ => Err("Cannot cast non-numeric value to integer".to_string()),
        }
    }

    fn cast_to_uint<T>(&self, value: Value) -> Result<Value, String>
    where
        T: TryFrom<u64> + 'static,
    {
        let inner_value = {
            let borrowed = value.borrow();
            borrowed.inner()
        };

        match inner_value {
            ValueType::I8(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            ValueType::I16(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            ValueType::I32(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            ValueType::I64(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            ValueType::I128(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            ValueType::ISize(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),

            ValueType::U8(v) => self.numeric_to_uint::<T>(v as u64),
            ValueType::U16(v) => self.numeric_to_uint::<T>(v as u64),
            ValueType::U32(v) => self.numeric_to_uint::<T>(v as u64),
            ValueType::U64(v) => self.numeric_to_uint::<T>(v),
            ValueType::U128(v) => self.numeric_to_uint::<T>(v as u64),
            ValueType::USize(v) => self.numeric_to_uint::<T>(v as u64),

            ValueType::F32(v) if v >= 0.0 => self.float_to_uint::<T>(v as f64),
            ValueType::F64(v) if v >= 0.0 => self.float_to_uint::<T>(v),

            _ => Err("Cannot cast negative or non-numeric value to unsigned integer".to_string()),
        }
    }

    fn cast_to_float<T>(&self, value: Value) -> Result<Value, String>
    where
        T: 'static,
    {
        let inner_value = {
            let borrowed = value.borrow();
            borrowed.inner()
        };

        match (inner_value, std::any::TypeId::of::<T>()) {
            (ValueType::F32(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v))),
            (ValueType::F32(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::F64(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::F64(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v))),

            (ValueType::I8(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::I8(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::I16(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::I16(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::I32(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::I32(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::I64(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::I64(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::I128(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::I128(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::ISize(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::ISize(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),

            (ValueType::U8(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::U8(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::U16(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::U16(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::U32(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::U32(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::U64(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::U64(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::U128(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::U128(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),
            (ValueType::USize(v), t) if t == std::any::TypeId::of::<f32>() => Ok(val!(ValueType::F32(v as f32))),
            (ValueType::USize(v), t) if t == std::any::TypeId::of::<f64>() => Ok(val!(ValueType::F64(v as f64))),

            _ => Err("Invalid float cast".to_string()),
        }
    }

    fn numeric_to_int<T>(&self, value: i64) -> Result<Value, String>
    where
        T: TryFrom<i64> + 'static,
    {
        match std::any::TypeId::of::<T>() {
            t if t == std::any::TypeId::of::<i8>() => Ok(val!(ValueType::I8(value as i8))),
            t if t == std::any::TypeId::of::<i16>() => Ok(val!(ValueType::I16(value as i16))),
            t if t == std::any::TypeId::of::<i32>() => Ok(val!(ValueType::I32(value as i32))),
            t if t == std::any::TypeId::of::<i64>() => Ok(val!(ValueType::I64(value))),
            t if t == std::any::TypeId::of::<i128>() => Ok(val!(ValueType::I128(value as i128))),
            t if t == std::any::TypeId::of::<isize>() => Ok(val!(ValueType::ISize(value as isize))),
            _ => Err("Invalid integer cast".to_string()),
        }
    }

    fn numeric_to_uint<T>(&self, value: u64) -> Result<Value, String>
    where
        T: TryFrom<u64> + 'static,
    {
        match std::any::TypeId::of::<T>() {
            t if t == std::any::TypeId::of::<u8>() => Ok(val!(ValueType::U8(value as u8))),
            t if t == std::any::TypeId::of::<u16>() => Ok(val!(ValueType::U16(value as u16))),
            t if t == std::any::TypeId::of::<u32>() => Ok(val!(ValueType::U32(value as u32))),
            t if t == std::any::TypeId::of::<u64>() => Ok(val!(ValueType::U64(value))),
            t if t == std::any::TypeId::of::<u128>() => Ok(val!(ValueType::U128(value as u128))),
            t if t == std::any::TypeId::of::<usize>() => Ok(val!(ValueType::USize(value as usize))),
            _ => Err("Invalid unsigned integer cast".to_string()),
        }
    }

    fn float_to_int<T>(&self, value: f64) -> Result<Value, String>
    where
        T: TryFrom<i64> + 'static,
    {
        if value.is_nan() || value.is_infinite() {
            return Err("Cannot cast NaN or infinite float to integer".to_string());
        }

        self.numeric_to_int::<T>(value.round() as i64)
    }

    fn float_to_uint<T>(&self, value: f64) -> Result<Value, String>
    where
        T: TryFrom<u64> + 'static,
    {
        if value.is_nan() || value.is_infinite() {
            return Err("Cannot cast NaN or infinite float to unsigned integer".to_string());
        }

        if value < 0.0 {
            return Err("Cannot cast negative float to unsigned integer".to_string());
        }

        self.numeric_to_uint::<T>(value.round() as u64)
    }
}
