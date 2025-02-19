use super::*;

impl ValueType {
    pub fn deep_clone(&self) -> ValueType {
        match self {
            ValueType::I8(v) => ValueType::I8(*v),
            ValueType::I16(v) => ValueType::I16(*v),
            ValueType::I32(v) => ValueType::I32(*v),
            ValueType::I64(v) => ValueType::I64(*v),
            ValueType::I128(v) => ValueType::I128(*v),
            ValueType::ISize(v) => ValueType::ISize(*v),

            ValueType::U8(v) => ValueType::U8(*v),
            ValueType::U16(v) => ValueType::U16(*v),
            ValueType::U32(v) => ValueType::U32(*v),
            ValueType::U64(v) => ValueType::U64(*v),
            ValueType::U128(v) => ValueType::U128(*v),
            ValueType::USize(v) => ValueType::USize(*v),

            ValueType::F32(v) => ValueType::F32(*v),
            ValueType::F64(v) => ValueType::F64(*v),

            ValueType::Str(v) => ValueType::Str(v.clone()),

            ValueType::Pointer(v) => ValueType::Pointer(v.clone()),

            ValueType::Boolean(v) => ValueType::Boolean(*v),

            ValueType::Tuple(values) => ValueType::Tuple(values.iter().map(|v| v.borrow().deep_clone()).collect()),

            ValueType::Return(v) => ValueType::Return(v.borrow().deep_clone()),

            ValueType::Struct { name, fields } => {
                let cloned_fields = fields.iter().map(|(k, v)| (k.clone(), v.borrow().deep_clone())).collect();
                ValueType::Struct {
                    name: name.clone(),
                    fields: cloned_fields,
                }
            }

            ValueType::StructDef { name, fields, methods } => ValueType::StructDef {
                name: name.clone(),
                fields: fields.clone(),
                methods: methods.clone(),
            },

            ValueType::Enum { enum_type, variant, data } => ValueType::Enum {
                enum_type: enum_type.clone(),
                variant: variant.clone(),
                data: data.as_ref().map(|values| values.iter().map(|v| v.borrow().deep_clone()).collect()),
            },

            ValueType::FieldRef { base, chain } => ValueType::FieldRef {
                base: base.borrow().deep_clone(),
                chain: chain.clone(),
            },

            ValueType::Reference { original_ptr, _undropped, .. } => {
                if original_ptr.is_null() {
                    ValueType::Unit
                } else {
                    _undropped.borrow().inner()
                }
            }

            ValueType::StaticMethod { struct_name, method, function } => ValueType::StaticMethod {
                struct_name: struct_name.clone(),
                method: method.clone(),
                function: function.clone(),
            },

            ValueType::Array { ty, el, len } => ValueType::Array {
                ty: Box::new(*ty.clone()),
                el: el.iter().map(|v| v.borrow().deep_clone()).collect(),
                len: *len,
            },

            ValueType::Slice { ty, el } => ValueType::Slice {
                ty: Box::new(*ty.clone()),
                el: el.iter().map(|v| v.borrow().deep_clone()).collect(),
            },

            ValueType::Unit => ValueType::Unit,
        }
    }
}
