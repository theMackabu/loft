use super::*;

impl Interpreter {
    pub fn perform_cast(&self, value: Value, target_type: &Type) -> Result<Value, String> {
        match target_type {
            Type::Reference { mutable, inner } => match value {
                Value::Reference(cell, _) => {
                    let inner_value = cell.borrow().clone();
                    let casted_inner = self.perform_cast(inner_value, inner)?;
                    Ok(Value::Reference(Box::new(RefCell::new(casted_inner)), *mutable))
                }
                other => {
                    let casted = self.perform_cast(other, inner)?;
                    Ok(Value::Reference(Box::new(RefCell::new(casted)), *mutable))
                }
            },

            Type::Path(path) if path.segments.len() == 1 => {
                let value = match value {
                    Value::Reference(cell, _) => cell.borrow().clone(),
                    other => other,
                };

                let type_name = &path.segments[0].ident;

                match type_name.as_str() {
                    "i8" => self.cast_to_int::<i8>(value),
                    "i16" => self.cast_to_int::<i16>(value),
                    "i32" => self.cast_to_int::<i32>(value),
                    "i64" => self.cast_to_int::<i64>(value),
                    "i128" => self.cast_to_int::<i128>(value),
                    "isize" => self.cast_to_int::<isize>(value),

                    "u8" => self.cast_to_uint::<u8>(value),
                    "u16" => self.cast_to_uint::<u16>(value),
                    "u32" => self.cast_to_uint::<u32>(value),
                    "u64" => self.cast_to_uint::<u64>(value),
                    "u128" => self.cast_to_uint::<u128>(value),
                    "usize" => self.cast_to_uint::<usize>(value),

                    "f32" => self.cast_to_float::<f32>(value),
                    "f64" => self.cast_to_float::<f64>(value),

                    _ => Err(format!("Unsupported cast to type: {}", type_name)),
                }
            }
            _ => Err("Invalid cast target type".to_string()),
        }
    }

    fn cast_to_int<T>(&self, value: Value) -> Result<Value, String>
    where
        T: TryFrom<i64> + TryFrom<u64> + 'static,
    {
        match value {
            Value::I8(v) => self.numeric_to_int::<T>(v as i64),
            Value::I16(v) => self.numeric_to_int::<T>(v as i64),
            Value::I32(v) => self.numeric_to_int::<T>(v as i64),
            Value::I64(v) => self.numeric_to_int::<T>(v),
            Value::I128(v) => self.numeric_to_int::<T>(v as i64),
            Value::ISize(v) => self.numeric_to_int::<T>(v as i64),

            Value::U8(v) => self.numeric_to_int::<T>(v as i64),
            Value::U16(v) => self.numeric_to_int::<T>(v as i64),
            Value::U32(v) => self.numeric_to_int::<T>(v as i64),
            Value::U64(v) => self.numeric_to_int::<T>(v as i64),
            Value::U128(v) => self.numeric_to_int::<T>(v as i64),
            Value::USize(v) => self.numeric_to_int::<T>(v as i64),

            Value::F32(v) => self.float_to_int::<T>(v as f64),
            Value::F64(v) => self.float_to_int::<T>(v),

            _ => Err("Cannot cast non-numeric value to integer".to_string()),
        }
    }

    fn cast_to_uint<T>(&self, value: Value) -> Result<Value, String>
    where
        T: TryFrom<u64> + 'static,
    {
        match value {
            Value::I8(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            Value::I16(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            Value::I32(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            Value::I64(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            Value::I128(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),
            Value::ISize(v) if v >= 0 => self.numeric_to_uint::<T>(v as u64),

            Value::U8(v) => self.numeric_to_uint::<T>(v as u64),
            Value::U16(v) => self.numeric_to_uint::<T>(v as u64),
            Value::U32(v) => self.numeric_to_uint::<T>(v as u64),
            Value::U64(v) => self.numeric_to_uint::<T>(v),
            Value::U128(v) => self.numeric_to_uint::<T>(v as u64),
            Value::USize(v) => self.numeric_to_uint::<T>(v as u64),

            Value::F32(v) if v >= 0.0 => self.float_to_uint::<T>(v as f64),
            Value::F64(v) if v >= 0.0 => self.float_to_uint::<T>(v),

            _ => Err("Cannot cast negative or non-numeric value to unsigned integer".to_string()),
        }
    }

    fn cast_to_float<T>(&self, value: Value) -> Result<Value, String>
    where
        T: 'static,
    {
        match (value, std::any::TypeId::of::<T>()) {
            (Value::F32(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v)),
            (Value::F32(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::F64(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::F64(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v)),

            (Value::I8(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::I8(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::I16(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::I16(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::I32(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::I32(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::I64(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::I64(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::I128(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::I128(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::ISize(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::ISize(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),

            (Value::U8(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::U8(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::U16(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::U16(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::U32(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::U32(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::U64(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::U64(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::U128(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::U128(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),
            (Value::USize(v), t) if t == std::any::TypeId::of::<f32>() => Ok(Value::F32(v as f32)),
            (Value::USize(v), t) if t == std::any::TypeId::of::<f64>() => Ok(Value::F64(v as f64)),

            _ => Err("Invalid float cast".to_string()),
        }
    }

    fn numeric_to_int<T>(&self, value: i64) -> Result<Value, String>
    where
        T: TryFrom<i64> + 'static,
    {
        match std::any::TypeId::of::<T>() {
            t if t == std::any::TypeId::of::<i8>() => Ok(Value::I8(value as i8)),
            t if t == std::any::TypeId::of::<i16>() => Ok(Value::I16(value as i16)),
            t if t == std::any::TypeId::of::<i32>() => Ok(Value::I32(value as i32)),
            t if t == std::any::TypeId::of::<i64>() => Ok(Value::I64(value)),
            t if t == std::any::TypeId::of::<i128>() => Ok(Value::I128(value as i128)),
            t if t == std::any::TypeId::of::<isize>() => Ok(Value::ISize(value as isize)),
            _ => Err("Invalid integer cast".to_string()),
        }
    }

    fn numeric_to_uint<T>(&self, value: u64) -> Result<Value, String>
    where
        T: TryFrom<u64> + 'static,
    {
        match std::any::TypeId::of::<T>() {
            t if t == std::any::TypeId::of::<u8>() => Ok(Value::U8(value as u8)),
            t if t == std::any::TypeId::of::<u16>() => Ok(Value::U16(value as u16)),
            t if t == std::any::TypeId::of::<u32>() => Ok(Value::U32(value as u32)),
            t if t == std::any::TypeId::of::<u64>() => Ok(Value::U64(value)),
            t if t == std::any::TypeId::of::<u128>() => Ok(Value::U128(value as u128)),
            t if t == std::any::TypeId::of::<usize>() => Ok(Value::USize(value as usize)),
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
