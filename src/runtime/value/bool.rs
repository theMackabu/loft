use super::*;

impl ValueType {
    pub fn is_empty(&self) -> bool {
        match self {
            ValueType::Unit => true,
            ValueType::Boolean(val) => !*val,
            ValueType::Str(val) => val.is_empty(),
            ValueType::Pointer(ptr) => ptr.is_null(),
            ValueType::Array { el, .. } => el.is_empty(),
            ValueType::Slice { el, .. } => el.is_empty(),
            ValueType::Tuple(values) => values.is_empty(),
            ValueType::Struct { fields, .. } => fields.is_empty(),
            ValueType::Enum { data, .. } => data.as_ref().map_or(true, |d| d.is_empty()),
            ValueType::Range { start, end } => start.borrow().inner().is_empty() && end.borrow().inner().is_empty(),

            ValueType::Reference { original_ptr, _undropped, .. } => {
                if (*original_ptr).is_null() {
                    true
                } else {
                    unsafe { &*(*original_ptr) }.borrow().inner().is_empty()
                }
            }

            _ => false,
        }
    }

    pub fn to_bool(&self) -> Result<bool, String> {
        match self {
            ValueType::Boolean(b) => Ok(*b),

            ValueType::Array { el, .. } => Ok(!el.is_empty()),
            ValueType::Slice { el, .. } => Ok(!el.is_empty()),
            ValueType::Tuple(values) => Ok(!values.is_empty()),

            ValueType::I8(v) => Ok(*v != 0),
            ValueType::I16(v) => Ok(*v != 0),
            ValueType::I32(v) => Ok(*v != 0),
            ValueType::I64(v) => Ok(*v != 0),
            ValueType::I128(v) => Ok(*v != 0),
            ValueType::ISize(v) => Ok(*v != 0),

            ValueType::U8(v) => Ok(*v != 0),
            ValueType::U16(v) => Ok(*v != 0),
            ValueType::U32(v) => Ok(*v != 0),
            ValueType::U64(v) => Ok(*v != 0),
            ValueType::U128(v) => Ok(*v != 0),
            ValueType::USize(v) => Ok(*v != 0),

            ValueType::F32(v) => Ok(*v != 0.0),
            ValueType::F64(v) => Ok(*v != 0.0),

            ValueType::Str(s) => Ok(!s.is_empty()),
            ValueType::Unit => Ok(false),

            _ => Err(format!("Cannot convert value to boolean")),
        }
    }
}
