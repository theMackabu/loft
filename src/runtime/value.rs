use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),

    F32(f32),
    F64(f64),

    Str(&'static str),
    Boolean(bool),

    Tuple(Vec<Value>),
    Return(Box<Value>),
    Reference(Box<RefCell<Value>>, bool),

    Enum { enum_type: String, variant: String, data: Option<Box<Value>> },

    Unit,
}

impl Value {
    pub fn is_mutable_ref(&self) -> bool {
        match self {
            Value::Reference(_, is_mutable) => *is_mutable,
            _ => false,
        }
    }

    pub fn get_ref_value(&self) -> Result<Ref<Value>, String> {
        match self {
            Value::Reference(ref_cell, _) => Ok(ref_cell.borrow()),
            _ => Err("Not a reference".to_string()),
        }
    }

    pub fn get_mut_ref_value(&self) -> Result<RefMut<Value>, String> {
        match self {
            Value::Reference(ref_cell, true) => Ok(ref_cell.borrow_mut()),
            Value::Reference(_, false) => Err("Cannot get mutable reference to immutable value".to_string()),
            _ => Err("Not a reference".to_string()),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I8(v) => write!(f, "{}", v),
            Value::I16(v) => write!(f, "{}", v),
            Value::I32(v) => write!(f, "{}", v),
            Value::I64(v) => write!(f, "{}", v),
            Value::I128(v) => write!(f, "{}", v),
            Value::ISize(v) => write!(f, "{}", v),

            Value::U8(v) => write!(f, "{}", v),
            Value::U16(v) => write!(f, "{}", v),
            Value::U32(v) => write!(f, "{}", v),
            Value::U64(v) => write!(f, "{}", v),
            Value::U128(v) => write!(f, "{}", v),
            Value::USize(v) => write!(f, "{}", v),

            Value::F32(v) => write!(f, "{}", v),
            Value::F64(v) => write!(f, "{}", v),

            Value::Str(v) => write!(f, "{}", v),
            Value::Boolean(v) => write!(f, "{}", v),

            Value::Tuple(values) => {
                write!(f, "(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, ")")
            }

            Value::Enum { enum_type, variant, data } => {
                write!(f, "{}::{}", enum_type, variant)?;
                if let Some(value) = data {
                    // if data has strings, quote
                    write!(f, "({})", value)
                } else {
                    Ok(())
                }
            }

            Value::Reference(v, _) => write!(f, "{}", v.borrow()),
            Value::Return(v) => write!(f, "{}", v),
            Value::Unit => write!(f, "()"),
        }
    }
}
