mod clone;
mod display;

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

    Array {
        ty: Box<ValueType>,
        el: Vec<Value>,
        len: usize,
    },

    Slice {
        ty: Box<ValueType>,
        el: Vec<Value>,
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

    pub fn is_ref(&self) -> bool { matches!(self.inner(), ValueType::Reference { .. }) }

    pub fn deep_clone(&self) -> Value {
        match self {
            ValueEnum::Immutable(inner) => crate::val!(inner.deep_clone()),
            ValueEnum::Mutable(inner) => crate::val!(mut inner.deep_clone()),
        }
    }

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

    pub fn kind(&self) -> String {
        match self.inner() {
            ValueType::I8(_) => String::from("i8"),
            ValueType::I16(_) => String::from("i16"),
            ValueType::I32(_) => String::from("i32"),
            ValueType::I64(_) => String::from("i64"),
            ValueType::I128(_) => String::from("i128"),
            ValueType::ISize(_) => String::from("isize"),

            ValueType::U8(_) => String::from("u8"),
            ValueType::U16(_) => String::from("u16"),
            ValueType::U32(_) => String::from("u32"),
            ValueType::U64(_) => String::from("u64"),
            ValueType::U128(_) => String::from("u128"),
            ValueType::USize(_) => String::from("usize"),

            ValueType::F32(_) => String::from("f32"),
            ValueType::F64(_) => String::from("f64"),

            ValueType::Str(_) => String::from("str"),
            ValueType::Boolean(_) => String::from("bool"),
            ValueType::Tuple(_) => String::from("tuple"),
            ValueType::Return(_) => String::from("return"),

            ValueType::Struct { name, .. } => name,
            ValueType::StructDef { name, .. } => name,
            ValueType::Enum { enum_type, .. } => enum_type,

            ValueType::FieldRef { .. } => String::from("field_ref"),
            ValueType::Reference { .. } => String::from("reference"),
            ValueType::StaticMethod { .. } => String::from("static_method"),

            ValueType::Array { .. } => String::from("array"),
            ValueType::Slice { .. } => String::from("slice"),

            ValueType::Unit => String::from("unit"),
        }
    }
}
