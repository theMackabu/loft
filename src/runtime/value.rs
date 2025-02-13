use crate::parser::ast::{Function, Type};
use std::{collections::HashMap, fmt};

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

    Str(String),
    Boolean(bool),

    Tuple(Vec<Value>),
    Return(Box<Value>),

    Struct {
        name: String,
        fields: Vec<(String, Value)>,
    },

    StructDef {
        name: String,
        fields: Vec<(String, Type)>,
        methods: HashMap<String, Function>,
    },

    Enum {
        enum_type: String,
        variant: String,
        data: Option<Vec<Value>>,
    },

    Reference {
        mutable: bool,
        source_name: Option<String>,
        source_scope: Option<usize>,
        data: Option<Box<Value>>,
    },

    StaticMethod {
        struct_name: String,
        method: String,
        function: Function,
    },

    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I8(v) => write!(f, "{v}"),
            Value::I16(v) => write!(f, "{v}"),
            Value::I32(v) => write!(f, "{v}"),
            Value::I64(v) => write!(f, "{v}"),
            Value::I128(v) => write!(f, "{v}"),
            Value::ISize(v) => write!(f, "{v}"),

            Value::U8(v) => write!(f, "{v}"),
            Value::U16(v) => write!(f, "{v}"),
            Value::U32(v) => write!(f, "{v}"),
            Value::U64(v) => write!(f, "{v}"),
            Value::U128(v) => write!(f, "{v}"),
            Value::USize(v) => write!(f, "{v}"),

            Value::Unit => write!(f, "()"),

            Value::Str(v) => write!(f, "{}", v),

            Value::Boolean(v) => write!(f, "{}", v),

            Value::Return(v) => write!(f, "{v}"),

            Value::StructDef { name, .. } => write!(f, "<struct {name}>"),

            Value::StaticMethod { struct_name, method, .. } => write!(f, "{}::{}", struct_name, method),

            Value::F32(v) => {
                if v.fract() == 0.0 {
                    write!(f, "{v:.1}")
                } else {
                    write!(f, "{v}")
                }
            }

            Value::F64(v) => {
                if v.fract() == 0.0 {
                    write!(f, "{v:.1}")
                } else {
                    write!(f, "{v}")
                }
            }

            Value::Tuple(values) => {
                write!(f, "(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match value {
                        Value::Str(s) => write!(f, "\"{s}\"")?,
                        _ => write!(f, "{value}")?,
                    }
                }
                write!(f, ")")
            }

            Value::Reference { source_name, data, mutable, .. } => {
                if let Some(value) = data {
                    write!(f, "{value}")
                } else if let Some(name) = source_name {
                    if *mutable {
                        write!(f, "&mut {name}")
                    } else {
                        write!(f, "&{name}")
                    }
                } else {
                    write!(f, "&<unnamed>")
                }
            }

            Value::Struct { name, fields } => {
                write!(f, "{name} {{ ")?;
                for (i, (field_name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match value {
                        Value::Str(s) => write!(f, "{field_name}: \"{s}\"")?,
                        _ => write!(f, "{field_name}: {value}")?,
                    }
                }
                write!(f, " }}")
            }

            Value::Enum { enum_type, variant, data } => {
                write!(f, "{enum_type}::{variant}")?;
                if let Some(values) = data {
                    write!(f, "(")?;
                    for (i, value) in values.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        match value {
                            Value::Str(s) => write!(f, "\"{s}\"")?,
                            _ => write!(f, "{value}")?,
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }
        }
    }
}
