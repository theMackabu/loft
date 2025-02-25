use super::runtime::value::{Value, ValueEnum, ValueType};

use std::cmp::Ordering;
use std::rc::Rc;

pub fn tuple_equals(left: &Vec<Value>, right: &Vec<Value>) -> Result<bool, String> {
    if left.len() != right.len() {
        return Ok(false);
    }
    for (l, r) in left.iter().zip(right.iter()) {
        if !value_equals(l, r)? {
            return Ok(false);
        }
    }
    Ok(true)
}

pub fn tuple_cmp(left: &Vec<Value>, right: &Vec<Value>) -> Result<Ordering, String> {
    for (l, r) in left.iter().zip(right.iter()) {
        let ord = compare_values(l, r)?;
        if ord != Ordering::Equal {
            return Ok(ord);
        }
    }
    Ok(left.len().cmp(&right.len()))
}

pub fn value_equals(lhs: &Value, rhs: &Value) -> Result<bool, String> {
    if Rc::ptr_eq(lhs, rhs) {
        return Ok(true);
    }

    let lhs_val = lhs.borrow();
    let rhs_val = rhs.borrow();

    match (&*lhs_val, &*rhs_val) {
        // integer types
        (ValueEnum::Immutable(ValueType::I8(a)), ValueEnum::Immutable(ValueType::I8(b)))
        | (ValueEnum::Mutable(ValueType::I8(a)), ValueEnum::Immutable(ValueType::I8(b)))
        | (ValueEnum::Immutable(ValueType::I8(a)), ValueEnum::Mutable(ValueType::I8(b)))
        | (ValueEnum::Mutable(ValueType::I8(a)), ValueEnum::Mutable(ValueType::I8(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::I16(a)), ValueEnum::Immutable(ValueType::I16(b)))
        | (ValueEnum::Mutable(ValueType::I16(a)), ValueEnum::Immutable(ValueType::I16(b)))
        | (ValueEnum::Immutable(ValueType::I16(a)), ValueEnum::Mutable(ValueType::I16(b)))
        | (ValueEnum::Mutable(ValueType::I16(a)), ValueEnum::Mutable(ValueType::I16(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::I32(a)), ValueEnum::Immutable(ValueType::I32(b)))
        | (ValueEnum::Mutable(ValueType::I32(a)), ValueEnum::Immutable(ValueType::I32(b)))
        | (ValueEnum::Immutable(ValueType::I32(a)), ValueEnum::Mutable(ValueType::I32(b)))
        | (ValueEnum::Mutable(ValueType::I32(a)), ValueEnum::Mutable(ValueType::I32(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::I64(a)), ValueEnum::Immutable(ValueType::I64(b)))
        | (ValueEnum::Mutable(ValueType::I64(a)), ValueEnum::Immutable(ValueType::I64(b)))
        | (ValueEnum::Immutable(ValueType::I64(a)), ValueEnum::Mutable(ValueType::I64(b)))
        | (ValueEnum::Mutable(ValueType::I64(a)), ValueEnum::Mutable(ValueType::I64(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::I128(a)), ValueEnum::Immutable(ValueType::I128(b)))
        | (ValueEnum::Mutable(ValueType::I128(a)), ValueEnum::Immutable(ValueType::I128(b)))
        | (ValueEnum::Immutable(ValueType::I128(a)), ValueEnum::Mutable(ValueType::I128(b)))
        | (ValueEnum::Mutable(ValueType::I128(a)), ValueEnum::Mutable(ValueType::I128(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::ISize(a)), ValueEnum::Immutable(ValueType::ISize(b)))
        | (ValueEnum::Mutable(ValueType::ISize(a)), ValueEnum::Immutable(ValueType::ISize(b)))
        | (ValueEnum::Immutable(ValueType::ISize(a)), ValueEnum::Mutable(ValueType::ISize(b)))
        | (ValueEnum::Mutable(ValueType::ISize(a)), ValueEnum::Mutable(ValueType::ISize(b))) => Ok(a == b),

        // unsigned integer types
        (ValueEnum::Immutable(ValueType::U8(a)), ValueEnum::Immutable(ValueType::U8(b)))
        | (ValueEnum::Mutable(ValueType::U8(a)), ValueEnum::Immutable(ValueType::U8(b)))
        | (ValueEnum::Immutable(ValueType::U8(a)), ValueEnum::Mutable(ValueType::U8(b)))
        | (ValueEnum::Mutable(ValueType::U8(a)), ValueEnum::Mutable(ValueType::U8(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::U16(a)), ValueEnum::Immutable(ValueType::U16(b)))
        | (ValueEnum::Mutable(ValueType::U16(a)), ValueEnum::Immutable(ValueType::U16(b)))
        | (ValueEnum::Immutable(ValueType::U16(a)), ValueEnum::Mutable(ValueType::U16(b)))
        | (ValueEnum::Mutable(ValueType::U16(a)), ValueEnum::Mutable(ValueType::U16(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::U32(a)), ValueEnum::Immutable(ValueType::U32(b)))
        | (ValueEnum::Mutable(ValueType::U32(a)), ValueEnum::Immutable(ValueType::U32(b)))
        | (ValueEnum::Immutable(ValueType::U32(a)), ValueEnum::Mutable(ValueType::U32(b)))
        | (ValueEnum::Mutable(ValueType::U32(a)), ValueEnum::Mutable(ValueType::U32(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::U64(a)), ValueEnum::Immutable(ValueType::U64(b)))
        | (ValueEnum::Mutable(ValueType::U64(a)), ValueEnum::Immutable(ValueType::U64(b)))
        | (ValueEnum::Immutable(ValueType::U64(a)), ValueEnum::Mutable(ValueType::U64(b)))
        | (ValueEnum::Mutable(ValueType::U64(a)), ValueEnum::Mutable(ValueType::U64(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::U128(a)), ValueEnum::Immutable(ValueType::U128(b)))
        | (ValueEnum::Mutable(ValueType::U128(a)), ValueEnum::Immutable(ValueType::U128(b)))
        | (ValueEnum::Immutable(ValueType::U128(a)), ValueEnum::Mutable(ValueType::U128(b)))
        | (ValueEnum::Mutable(ValueType::U128(a)), ValueEnum::Mutable(ValueType::U128(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::USize(a)), ValueEnum::Immutable(ValueType::USize(b)))
        | (ValueEnum::Mutable(ValueType::USize(a)), ValueEnum::Immutable(ValueType::USize(b)))
        | (ValueEnum::Immutable(ValueType::USize(a)), ValueEnum::Mutable(ValueType::USize(b)))
        | (ValueEnum::Mutable(ValueType::USize(a)), ValueEnum::Mutable(ValueType::USize(b))) => Ok(a == b),

        // float types
        (ValueEnum::Immutable(ValueType::F32(a)), ValueEnum::Immutable(ValueType::F32(b)))
        | (ValueEnum::Mutable(ValueType::F32(a)), ValueEnum::Immutable(ValueType::F32(b)))
        | (ValueEnum::Immutable(ValueType::F32(a)), ValueEnum::Mutable(ValueType::F32(b)))
        | (ValueEnum::Mutable(ValueType::F32(a)), ValueEnum::Mutable(ValueType::F32(b))) => Ok(a == b),

        (ValueEnum::Immutable(ValueType::F64(a)), ValueEnum::Immutable(ValueType::F64(b)))
        | (ValueEnum::Mutable(ValueType::F64(a)), ValueEnum::Immutable(ValueType::F64(b)))
        | (ValueEnum::Immutable(ValueType::F64(a)), ValueEnum::Mutable(ValueType::F64(b)))
        | (ValueEnum::Mutable(ValueType::F64(a)), ValueEnum::Mutable(ValueType::F64(b))) => Ok(a == b),

        // boolean type
        (ValueEnum::Immutable(ValueType::Boolean(a)), ValueEnum::Immutable(ValueType::Boolean(b)))
        | (ValueEnum::Mutable(ValueType::Boolean(a)), ValueEnum::Immutable(ValueType::Boolean(b)))
        | (ValueEnum::Immutable(ValueType::Boolean(a)), ValueEnum::Mutable(ValueType::Boolean(b)))
        | (ValueEnum::Mutable(ValueType::Boolean(a)), ValueEnum::Mutable(ValueType::Boolean(b))) => Ok(a == b),

        // string type
        (ValueEnum::Immutable(ValueType::Str(a)), ValueEnum::Immutable(ValueType::Str(b)))
        | (ValueEnum::Mutable(ValueType::Str(a)), ValueEnum::Immutable(ValueType::Str(b)))
        | (ValueEnum::Immutable(ValueType::Str(a)), ValueEnum::Mutable(ValueType::Str(b)))
        | (ValueEnum::Mutable(ValueType::Str(a)), ValueEnum::Mutable(ValueType::Str(b))) => Ok(a == b),

        // unit type
        (ValueEnum::Immutable(ValueType::Unit), ValueEnum::Immutable(ValueType::Unit))
        | (ValueEnum::Mutable(ValueType::Unit), ValueEnum::Immutable(ValueType::Unit))
        | (ValueEnum::Immutable(ValueType::Unit), ValueEnum::Mutable(ValueType::Unit))
        | (ValueEnum::Mutable(ValueType::Unit), ValueEnum::Mutable(ValueType::Unit)) => Ok(true),

        // tuple type
        (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b))) => tuple_equals(a, b),

        // array and slice types
        (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. })) => tuple_equals(a, b),

        (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. })) => tuple_equals(a, b),

        (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. })) => tuple_equals(a, b),

        (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. })) => tuple_equals(a, b),

        // references
        (ValueEnum::Immutable(ValueType::Reference { _undropped: a, .. }), _) => value_equals(a, rhs),

        (_, ValueEnum::Immutable(ValueType::Reference { _undropped: b, .. })) => value_equals(lhs, b),

        (ValueEnum::Mutable(ValueType::Reference { _undropped: a, .. }), _) => value_equals(a, rhs),

        (_, ValueEnum::Mutable(ValueType::Reference { _undropped: b, .. })) => value_equals(lhs, b),

        // struct equality
        (ValueEnum::Immutable(ValueType::Struct { name: name_a, fields: fields_a }), ValueEnum::Immutable(ValueType::Struct { name: name_b, fields: fields_b }))
        | (ValueEnum::Mutable(ValueType::Struct { name: name_a, fields: fields_a }), ValueEnum::Immutable(ValueType::Struct { name: name_b, fields: fields_b }))
        | (ValueEnum::Immutable(ValueType::Struct { name: name_a, fields: fields_a }), ValueEnum::Mutable(ValueType::Struct { name: name_b, fields: fields_b }))
        | (ValueEnum::Mutable(ValueType::Struct { name: name_a, fields: fields_a }), ValueEnum::Mutable(ValueType::Struct { name: name_b, fields: fields_b })) => {
            if name_a != name_b || fields_a.len() != fields_b.len() {
                return Ok(false);
            }

            for (key, val_a) in fields_a {
                if let Some(val_b) = fields_b.get(key) {
                    if !value_equals(val_a, val_b)? {
                        return Ok(false);
                    }
                } else {
                    return Ok(false);
                }
            }
            Ok(true)
        }

        // enum equality
        (
            ValueEnum::Immutable(ValueType::Enum {
                enum_type: type_a,
                variant: var_a,
                data: data_a,
            }),
            ValueEnum::Immutable(ValueType::Enum {
                enum_type: type_b,
                variant: var_b,
                data: data_b,
            }),
        )
        | (
            ValueEnum::Mutable(ValueType::Enum {
                enum_type: type_a,
                variant: var_a,
                data: data_a,
            }),
            ValueEnum::Immutable(ValueType::Enum {
                enum_type: type_b,
                variant: var_b,
                data: data_b,
            }),
        )
        | (
            ValueEnum::Immutable(ValueType::Enum {
                enum_type: type_a,
                variant: var_a,
                data: data_a,
            }),
            ValueEnum::Mutable(ValueType::Enum {
                enum_type: type_b,
                variant: var_b,
                data: data_b,
            }),
        )
        | (
            ValueEnum::Mutable(ValueType::Enum {
                enum_type: type_a,
                variant: var_a,
                data: data_a,
            }),
            ValueEnum::Mutable(ValueType::Enum {
                enum_type: type_b,
                variant: var_b,
                data: data_b,
            }),
        ) => {
            if type_a != type_b || var_a != var_b {
                return Ok(false);
            }

            match (data_a, data_b) {
                (Some(vals_a), Some(vals_b)) => tuple_equals(vals_a, vals_b),
                (None, None) => Ok(true),
                _ => Ok(false),
            }
        }

        _ => Err(format!("Equality not supported between {:?} and {:?}", lhs_val, rhs_val)),
    }
}

pub fn compare_values(lhs: &Value, rhs: &Value) -> Result<Ordering, String> {
    let lhs_val = lhs.borrow();
    let rhs_val = rhs.borrow();

    match (&*lhs_val, &*rhs_val) {
        // integer types
        (ValueEnum::Immutable(ValueType::I8(a)), ValueEnum::Immutable(ValueType::I8(b)))
        | (ValueEnum::Mutable(ValueType::I8(a)), ValueEnum::Immutable(ValueType::I8(b)))
        | (ValueEnum::Immutable(ValueType::I8(a)), ValueEnum::Mutable(ValueType::I8(b)))
        | (ValueEnum::Mutable(ValueType::I8(a)), ValueEnum::Mutable(ValueType::I8(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::I16(a)), ValueEnum::Immutable(ValueType::I16(b)))
        | (ValueEnum::Mutable(ValueType::I16(a)), ValueEnum::Immutable(ValueType::I16(b)))
        | (ValueEnum::Immutable(ValueType::I16(a)), ValueEnum::Mutable(ValueType::I16(b)))
        | (ValueEnum::Mutable(ValueType::I16(a)), ValueEnum::Mutable(ValueType::I16(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::I32(a)), ValueEnum::Immutable(ValueType::I32(b)))
        | (ValueEnum::Mutable(ValueType::I32(a)), ValueEnum::Immutable(ValueType::I32(b)))
        | (ValueEnum::Immutable(ValueType::I32(a)), ValueEnum::Mutable(ValueType::I32(b)))
        | (ValueEnum::Mutable(ValueType::I32(a)), ValueEnum::Mutable(ValueType::I32(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::I64(a)), ValueEnum::Immutable(ValueType::I64(b)))
        | (ValueEnum::Mutable(ValueType::I64(a)), ValueEnum::Immutable(ValueType::I64(b)))
        | (ValueEnum::Immutable(ValueType::I64(a)), ValueEnum::Mutable(ValueType::I64(b)))
        | (ValueEnum::Mutable(ValueType::I64(a)), ValueEnum::Mutable(ValueType::I64(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::I128(a)), ValueEnum::Immutable(ValueType::I128(b)))
        | (ValueEnum::Mutable(ValueType::I128(a)), ValueEnum::Immutable(ValueType::I128(b)))
        | (ValueEnum::Immutable(ValueType::I128(a)), ValueEnum::Mutable(ValueType::I128(b)))
        | (ValueEnum::Mutable(ValueType::I128(a)), ValueEnum::Mutable(ValueType::I128(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::ISize(a)), ValueEnum::Immutable(ValueType::ISize(b)))
        | (ValueEnum::Mutable(ValueType::ISize(a)), ValueEnum::Immutable(ValueType::ISize(b)))
        | (ValueEnum::Immutable(ValueType::ISize(a)), ValueEnum::Mutable(ValueType::ISize(b)))
        | (ValueEnum::Mutable(ValueType::ISize(a)), ValueEnum::Mutable(ValueType::ISize(b))) => Ok(a.cmp(b)),

        // unsigned integer types
        (ValueEnum::Immutable(ValueType::U8(a)), ValueEnum::Immutable(ValueType::U8(b)))
        | (ValueEnum::Mutable(ValueType::U8(a)), ValueEnum::Immutable(ValueType::U8(b)))
        | (ValueEnum::Immutable(ValueType::U8(a)), ValueEnum::Mutable(ValueType::U8(b)))
        | (ValueEnum::Mutable(ValueType::U8(a)), ValueEnum::Mutable(ValueType::U8(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::U16(a)), ValueEnum::Immutable(ValueType::U16(b)))
        | (ValueEnum::Mutable(ValueType::U16(a)), ValueEnum::Immutable(ValueType::U16(b)))
        | (ValueEnum::Immutable(ValueType::U16(a)), ValueEnum::Mutable(ValueType::U16(b)))
        | (ValueEnum::Mutable(ValueType::U16(a)), ValueEnum::Mutable(ValueType::U16(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::U32(a)), ValueEnum::Immutable(ValueType::U32(b)))
        | (ValueEnum::Mutable(ValueType::U32(a)), ValueEnum::Immutable(ValueType::U32(b)))
        | (ValueEnum::Immutable(ValueType::U32(a)), ValueEnum::Mutable(ValueType::U32(b)))
        | (ValueEnum::Mutable(ValueType::U32(a)), ValueEnum::Mutable(ValueType::U32(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::U64(a)), ValueEnum::Immutable(ValueType::U64(b)))
        | (ValueEnum::Mutable(ValueType::U64(a)), ValueEnum::Immutable(ValueType::U64(b)))
        | (ValueEnum::Immutable(ValueType::U64(a)), ValueEnum::Mutable(ValueType::U64(b)))
        | (ValueEnum::Mutable(ValueType::U64(a)), ValueEnum::Mutable(ValueType::U64(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::U128(a)), ValueEnum::Immutable(ValueType::U128(b)))
        | (ValueEnum::Mutable(ValueType::U128(a)), ValueEnum::Immutable(ValueType::U128(b)))
        | (ValueEnum::Immutable(ValueType::U128(a)), ValueEnum::Mutable(ValueType::U128(b)))
        | (ValueEnum::Mutable(ValueType::U128(a)), ValueEnum::Mutable(ValueType::U128(b))) => Ok(a.cmp(b)),

        (ValueEnum::Immutable(ValueType::USize(a)), ValueEnum::Immutable(ValueType::USize(b)))
        | (ValueEnum::Mutable(ValueType::USize(a)), ValueEnum::Immutable(ValueType::USize(b)))
        | (ValueEnum::Immutable(ValueType::USize(a)), ValueEnum::Mutable(ValueType::USize(b)))
        | (ValueEnum::Mutable(ValueType::USize(a)), ValueEnum::Mutable(ValueType::USize(b))) => Ok(a.cmp(b)),

        // float types
        (ValueEnum::Immutable(ValueType::F32(a)), ValueEnum::Immutable(ValueType::F32(b)))
        | (ValueEnum::Mutable(ValueType::F32(a)), ValueEnum::Immutable(ValueType::F32(b)))
        | (ValueEnum::Immutable(ValueType::F32(a)), ValueEnum::Mutable(ValueType::F32(b)))
        | (ValueEnum::Mutable(ValueType::F32(a)), ValueEnum::Mutable(ValueType::F32(b))) => a.partial_cmp(b).ok_or_else(|| "Cannot compare NaN values".to_string()),

        (ValueEnum::Immutable(ValueType::F64(a)), ValueEnum::Immutable(ValueType::F64(b)))
        | (ValueEnum::Mutable(ValueType::F64(a)), ValueEnum::Immutable(ValueType::F64(b)))
        | (ValueEnum::Immutable(ValueType::F64(a)), ValueEnum::Mutable(ValueType::F64(b)))
        | (ValueEnum::Mutable(ValueType::F64(a)), ValueEnum::Mutable(ValueType::F64(b))) => a.partial_cmp(b).ok_or_else(|| "Cannot compare NaN values".to_string()),

        // boolean type
        (ValueEnum::Immutable(ValueType::Boolean(a)), ValueEnum::Immutable(ValueType::Boolean(b)))
        | (ValueEnum::Mutable(ValueType::Boolean(a)), ValueEnum::Immutable(ValueType::Boolean(b)))
        | (ValueEnum::Immutable(ValueType::Boolean(a)), ValueEnum::Mutable(ValueType::Boolean(b)))
        | (ValueEnum::Mutable(ValueType::Boolean(a)), ValueEnum::Mutable(ValueType::Boolean(b))) => Ok(a.cmp(b)),

        // string type
        (ValueEnum::Immutable(ValueType::Str(a)), ValueEnum::Immutable(ValueType::Str(b)))
        | (ValueEnum::Mutable(ValueType::Str(a)), ValueEnum::Immutable(ValueType::Str(b)))
        | (ValueEnum::Immutable(ValueType::Str(a)), ValueEnum::Mutable(ValueType::Str(b)))
        | (ValueEnum::Mutable(ValueType::Str(a)), ValueEnum::Mutable(ValueType::Str(b))) => Ok(a.cmp(b)),

        // tuple type
        (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b))) => tuple_cmp(a, b),

        // array and slice types
        (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. })) => tuple_cmp(a, b),

        (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. })) => tuple_cmp(a, b),

        (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Immutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Slice { el: a, .. }), ValueEnum::Mutable(ValueType::Array { el: b, .. })) => tuple_cmp(a, b),

        (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Immutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Immutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. }))
        | (ValueEnum::Mutable(ValueType::Array { el: a, .. }), ValueEnum::Mutable(ValueType::Slice { el: b, .. })) => tuple_cmp(a, b),

        // references
        (ValueEnum::Immutable(ValueType::Reference { _undropped: a, .. }), _) => compare_values(a, rhs),

        (_, ValueEnum::Immutable(ValueType::Reference { _undropped: b, .. })) => compare_values(lhs, b),

        (ValueEnum::Mutable(ValueType::Reference { _undropped: a, .. }), _) => compare_values(a, rhs),

        (_, ValueEnum::Mutable(ValueType::Reference { _undropped: b, .. })) => compare_values(lhs, b),

        _ => Err(format!("Comparison not supported between {:?} and {:?}", lhs_val, rhs_val)),
    }
}
