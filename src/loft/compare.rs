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
        (ValueEnum::Immutable(ValueType::I8(a)), ValueEnum::Immutable(ValueType::I8(b))) => Ok(a == b),
        // add more arms for other primitive types (I16, U8, F64, Str, etc.)

        // for tuple comparisons, allow mixing mutable/immutable wrappers.
        (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b))) => tuple_equals(a, b),

        _ => Err(format!("Equality not supported between {:?} and {:?}", lhs_val, rhs_val)),
    }
}

pub fn compare_values(lhs: &Value, rhs: &Value) -> Result<Ordering, String> {
    let lhs_val = lhs.borrow();
    let rhs_val = rhs.borrow();

    match (&*lhs_val, &*rhs_val) {
        (ValueEnum::Immutable(ValueType::I8(a)), ValueEnum::Immutable(ValueType::I8(b))) => Ok(a.cmp(b)),
        // add additional arms for numeric, boolean, string, etc.

        // for tuple comparisons, allow mixing mutable/immutable wrappers.
        (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Immutable(ValueType::Tuple(b)))
        | (ValueEnum::Immutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b)))
        | (ValueEnum::Mutable(ValueType::Tuple(a)), ValueEnum::Mutable(ValueType::Tuple(b))) => tuple_cmp(a, b),

        _ => Err(format!("Comparison not supported between {:?} and {:?}", lhs_val, rhs_val)),
    }
}
