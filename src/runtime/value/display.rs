use super::*;

impl fmt::Display for ValueEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value_type = self.inner();

        match value_type {
            ValueType::I8(v) => write!(f, "{v}"),

            ValueType::I16(v) => write!(f, "{v}"),

            ValueType::I32(v) => write!(f, "{v}"),

            ValueType::I64(v) => write!(f, "{v}"),

            ValueType::I128(v) => write!(f, "{v}"),

            ValueType::ISize(v) => write!(f, "{v}"),

            ValueType::U8(v) => write!(f, "{v}"),

            ValueType::U16(v) => write!(f, "{v}"),

            ValueType::U32(v) => write!(f, "{v}"),

            ValueType::U64(v) => write!(f, "{v}"),

            ValueType::U128(v) => write!(f, "{v}"),

            ValueType::USize(v) => write!(f, "{v}"),

            ValueType::F32(v) => write!(f, "{v}"),

            ValueType::F64(v) => write!(f, "{v}"),

            ValueType::Unit => write!(f, "()"),

            ValueType::Str(v) => write!(f, "{}", v),

            ValueType::Boolean(v) => write!(f, "{}", v),

            ValueType::Return(v) => write!(f, "{}", v.borrow()),

            ValueType::Pointer(ptr) => write!(f, "{:p}", ptr),

            ValueType::StructDef { name, .. } => write!(f, "<struct {name}>"),

            ValueType::StaticMethod { struct_name, method, .. } => write!(f, "{}::{}", struct_name, method),

            ValueType::Tuple(values) => {
                write!(f, "(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    // add quotes
                    write!(f, "{}", value.borrow())?;
                }
                write!(f, ")")
            }

            ValueType::FieldRef { base, chain, .. } => {
                if self.is_mutable() {
                    write!(f, "&mut ")?;
                }
                write!(f, "{}", base.borrow())?;
                for field in chain {
                    write!(f, ".{field}")?;
                }
                Ok(())
            }

            ValueType::Reference { source_name, original_ptr, .. } => {
                if !original_ptr.is_null() {
                    unsafe { write!(f, "{}", (*original_ptr).borrow()) }
                } else if let Some(name) = source_name {
                    if self.is_mutable() {
                        write!(f, "&mut {name}")
                    } else {
                        write!(f, "&{name}")
                    }
                } else {
                    write!(f, "&<unnamed>")
                }
            }

            ValueType::Struct { name, fields } => {
                write!(f, "{name} {{ ")?;
                for (i, (field_name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    // add quotes
                    write!(f, "{}: {}", field_name, value.borrow())?;
                }
                write!(f, " }}")
            }

            ValueType::Enum { enum_type, variant, data } => {
                write!(f, "{enum_type}::{variant}")?;
                if let Some(values) = data {
                    write!(f, "(")?;
                    for (i, value) in values.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        // add quotes
                        write!(f, "{}", value.borrow())?;
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }

            ValueType::Array { el, .. } => {
                write!(f, "[")?;
                for (i, value) in el.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    // add quotes
                    write!(f, "{}", value.borrow())?;
                }
                write!(f, "]")
            }

            ValueType::Slice { el, .. } => {
                write!(f, "[")?;
                for (i, value) in el.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    // add quotes
                    write!(f, "{}", value.borrow())?;
                }
                write!(f, "]")
            }
        }
    }
}
