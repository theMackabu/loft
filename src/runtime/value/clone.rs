use super::*;

impl ValueType {
    pub fn deep_clone(&self) -> ValueType {
        match self {
            ValueType::Function(func) => ValueType::Function(func.clone()),

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

            ValueType::Continue(v) => ValueType::Continue(v.clone()),

            ValueType::Break(b, v) => ValueType::Break(b.clone(), v.clone()),

            ValueType::TailCall { function, arguments } => ValueType::TailCall {
                function: function.clone(),
                arguments: arguments.iter().map(|arg| arg.borrow().deep_clone()).collect(),
            },

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

            ValueType::EnumDef { name, variants, methods } => ValueType::EnumDef {
                name: name.clone(),
                variants: variants.clone(),
                methods: methods.clone(),
            },

            ValueType::EnumConstructor { enum_name, variant_name, fields } => ValueType::EnumConstructor {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields.clone(),
            },

            ValueType::EnumStructConstructor { enum_name, variant_name, fields } => ValueType::EnumStructConstructor {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: fields.clone(),
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

            ValueType::Range { start, end, inclusive } => ValueType::Range {
                start: start.borrow().deep_clone(),
                end: end.borrow().deep_clone(),
                inclusive: inclusive.clone(),
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

            ValueType::Iterator {
                current,
                end,
                inclusive,
                exhausted,
                collection,
                kind,
            } => ValueType::Iterator {
                current: current.borrow().deep_clone(),
                end: end.borrow().deep_clone(),
                collection: collection.borrow().deep_clone(),
                inclusive: inclusive.clone(),
                exhausted: exhausted.clone(),
                kind: kind.clone(),
            },

            ValueType::Unbounded => ValueType::Unbounded,
            ValueType::Unit => ValueType::Unit,
        }
    }
}
