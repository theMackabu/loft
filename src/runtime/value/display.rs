use super::*;

fn is_string_value(value: &Value) -> bool {
    let borrowed = value.borrow();
    match borrowed.inner() {
        ValueType::Str(_) => true,
        ValueType::Reference { original_ptr, .. } if !original_ptr.is_null() => unsafe {
            let ref_borrowed = (*original_ptr).borrow();
            matches!(ref_borrowed.inner(), ValueType::Str(_))
        },
        _ => false,
    }
}

impl fmt::Display for ValueEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value_type = self.inner();

        match value_type {
            ValueType::Function(func) => write!(f, "{:?}", func.name),

            ValueType::Str(v) => write!(f, "{}", String::from_utf8_lossy(&v)),

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

            ValueType::Unbounded => write!(f, ".."),

            ValueType::Boolean(v) => write!(f, "{}", v),

            ValueType::Return(v) => write!(f, "{}", v.borrow()),

            ValueType::Continue(_) => write!(f, "continue"),

            ValueType::Break(_, _) => write!(f, "break"),

            ValueType::Pointer(ptr) => write!(f, "{:p}", ptr),

            ValueType::StructDef { name, .. } => write!(f, "<struct {name}>"),

            ValueType::EnumDef { name, .. } => write!(f, "<enum {name}>"),

            ValueType::Range { start, end, inclusive } => write!(f, "{}..{2}{}", start.borrow(), end.borrow(), if inclusive { "=" } else { "" }),

            ValueType::Iterator { current, end, inclusive, kind, .. } => write!(f, "<{kind} {}..{2}{}>", current.borrow(), end.borrow(), if inclusive { "=" } else { "" }),

            ValueType::StaticMethod { struct_name, method, .. } => write!(f, "{}::{}", struct_name, method),

            ValueType::EnumConstructor { enum_name, variant_name, .. } => write!(f, "<constructor {}::{}>", enum_name, variant_name),

            ValueType::EnumStructConstructor { enum_name, variant_name, .. } => write!(f, "<struct-constructor {}::{}>", enum_name, variant_name),

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
                    if self.is_mutable() { write!(f, "&mut {name}") } else { write!(f, "&{name}") }
                } else {
                    write!(f, "&<unnamed>")
                }
            }

            ValueType::Tuple(values) => {
                write!(f, "(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if is_string_value(value) {
                        write!(f, "\"{}\"", value.borrow())?;
                    } else {
                        write!(f, "{}", value.borrow())?;
                    }
                }
                write!(f, ")")
            }

            ValueType::Array { el, .. } => {
                write!(f, "[")?;
                for (i, value) in el.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if is_string_value(value) {
                        write!(f, "\"{}\"", value.borrow())?;
                    } else {
                        write!(f, "{}", value.borrow())?;
                    }
                }
                write!(f, "]")
            }

            ValueType::Slice { el, .. } => {
                write!(f, "[")?;
                for (i, value) in el.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if is_string_value(value) {
                        write!(f, "\"{}\"", value.borrow())?;
                    } else {
                        write!(f, "{}", value.borrow())?;
                    }
                }
                write!(f, "]")
            }

            ValueType::Enum { variant, data, .. } => {
                write!(f, "{variant}")?;
                if let Some(values) = data {
                    write!(f, "(")?;
                    for (i, value) in values.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        if is_string_value(value) {
                            write!(f, "\"{}\"", value.borrow())?;
                        } else {
                            write!(f, "{}", value.borrow())?;
                        }
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }

            ValueType::Struct { name, fields } => {
                if fields.keys().all(|k| k.parse::<usize>().is_ok()) {
                    let mut sorted_fields: Vec<_> = fields.iter().collect();
                    sorted_fields.sort_by_key(|(k, _)| k.parse::<usize>().unwrap());

                    write!(f, "{name}(")?;
                    for (i, (_, value)) in sorted_fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        if is_string_value(value) {
                            write!(f, "\"{}\"", value.borrow())?;
                        } else {
                            write!(f, "{}", value.borrow())?;
                        }
                    }
                    write!(f, ")")
                } else {
                    write!(f, "{name} {{ ")?;
                    for (i, (field_name, value)) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        if is_string_value(value) {
                            write!(f, "{}: \"{}\"", field_name, value.borrow())?;
                        } else {
                            write!(f, "{}: {}", field_name, value.borrow())?;
                        }
                    }
                    write!(f, " }}")
                }
            }
        }
    }
}
