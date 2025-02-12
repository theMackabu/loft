use std::fmt;

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

    Enum {
        enum_type: String,
        variant: String,
        data: Option<Box<Value>>,
    },

    Reference {
        mutable: bool,
        source_name: Option<String>,
        source_scope: Option<usize>,
        data: Option<Box<Value>>,
    },

    Unit,
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
                    match **value {
                        Value::Str(v) => write!(f, "\"{}\"", v),
                        _ => write!(f, "({})", value),
                    }
                } else {
                    Ok(())
                }
            }

            // update to show real value
            Value::Reference { source_name, .. } => write!(f, "ref({})", source_name.clone().expect("HANDLE THIS")),
            Value::Return(v) => write!(f, "{}", v),
            Value::Unit => write!(f, "()"),
        }
    }
}
