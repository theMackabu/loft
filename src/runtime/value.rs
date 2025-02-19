use crate::parser::ast::{Function, Type};
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

pub type Value = Rc<RefCell<ValueEnum>>;

#[derive(Clone, Debug, PartialEq)]
pub enum ValueEnum {
    Immutable(ValueType),
    Mutable(ValueType),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueType {
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
    Return(Value),

    Struct {
        name: String,
        fields: HashMap<String, Value>,
    },

    StructDef {
        name: String,
        fields: HashMap<String, (Type, bool)>,
        methods: HashMap<String, Function>,
    },

    Enum {
        enum_type: String,
        variant: String,
        data: Option<Vec<Value>>,
    },

    FieldRef {
        base: Value,
        chain: Vec<String>,
    },

    Reference {
        source_name: Option<String>,
        source_scope: Option<usize>,
        original_ptr: *const RefCell<ValueEnum>,
        _undropped: Rc<RefCell<ValueEnum>>,
    },

    StaticMethod {
        struct_name: String,
        method: String,
        function: Function,
    },

    Unit,
}

pub trait ValueExt {
    fn into_return(self) -> Value;
}

impl ValueExt for Value {
    fn into_return(self) -> Value {
        {
            let mut value = self.borrow_mut();
            let (inner_val, is_mutable) = match &*value {
                ValueEnum::Mutable(v) => (v.clone(), true),
                ValueEnum::Immutable(v) => (v.clone(), false),
            };

            *value = if is_mutable {
                ValueEnum::Mutable(ValueType::Return(Value::new(RefCell::new(ValueEnum::Mutable(inner_val)))))
            } else {
                ValueEnum::Immutable(ValueType::Return(Value::new(RefCell::new(ValueEnum::Immutable(inner_val)))))
            };
        }

        return self;
    }
}

impl ValueEnum {
    pub fn unit() -> Value { Rc::new(RefCell::new(ValueEnum::Immutable(ValueType::Unit))) }

    pub fn is_mutable(&self) -> bool { matches!(self, ValueEnum::Mutable(_)) }

    pub fn set_mutable(&mut self, mutable: bool) {
        *self = match self {
            ValueEnum::Immutable(inner) if mutable => ValueEnum::Mutable(inner.clone()),
            ValueEnum::Mutable(inner) if !mutable => ValueEnum::Immutable(inner.clone()),
            _ => return,
        };
    }

    pub fn into_mutable(self) -> ValueEnum {
        match self {
            ValueEnum::Immutable(inner) => ValueEnum::Mutable(inner),
            already_mutable => already_mutable,
        }
    }

    pub fn into_immutable(self) -> ValueEnum {
        match self {
            ValueEnum::Mutable(inner) => ValueEnum::Immutable(inner),
            already_immutable => already_immutable,
        }
    }

    pub fn inner(&self) -> ValueType {
        match self {
            ValueEnum::Mutable(inner) | ValueEnum::Immutable(inner) => inner.clone(),
        }
    }

    pub fn inner_mut(&mut self) -> &mut ValueType {
        match self {
            ValueEnum::Mutable(inner) => inner,
            ValueEnum::Immutable(_) => panic!("Called inner_mut on immutable value"),
        }
    }

    pub fn get_source_info(&self) -> Option<(String, usize)> {
        match &self.inner() {
            ValueType::Reference {
                source_name: Some(name),
                source_scope: Some(scope),
                ..
            } => Some((name.clone(), *scope)),
            _ => None,
        }
    }
}

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
        }
    }
}
