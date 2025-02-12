#[macro_export]
macro_rules! impl_compound_assignment {
    ($left:expr, $right:expr, $op:expr, {
        $(($type:ident, $rust_type:ty, $method:ident)),* $(,)?
    }) => {
        match ($left, $right) {
            $((Value::$type(l), Value::$type(r)) => Ok(Value::$type(l.$method(*r))),)*

            $((Value::Reference(ref_cell, _), Value::$type(r)) => {
                if let Value::$type(l) = &*ref_cell.borrow() {
                    Ok(Value::$type(l.$method(*r)))
                } else {
                    Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                }
            },)*

            $((Value::$type(l), Value::Reference(ref_cell, _)) => {
                if let Value::$type(r) = &*ref_cell.borrow() {
                    Ok(Value::$type(l.$method(*r)))
                } else {
                    Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                }
            },)*

            (Value::Reference(left_ref, _), Value::Reference(right_ref, _)) => {
                let left_val = left_ref.borrow();
                let right_val = right_ref.borrow();
                match (&*left_val, &*right_val) {
                    $((Value::$type(l), Value::$type(r)) => Ok(Value::$type(l.$method(*r))),)*
                    _ => Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                }
            },

            _ => Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
        }
    }
}

#[macro_export]
macro_rules! impl_binary_ops {
    (($left_val:expr, $operator:expr, $right_val:expr), $($type:ident),*) => {
        match ($left_val, $operator, $right_val) {
            $(
                (Value::$type(l), Token::Plus, Value::$type(r)) => Ok(Value::$type(l + r)),
                (Value::$type(l), Token::Minus, Value::$type(r)) => Ok(Value::$type(l - r)),
                (Value::$type(l), Token::Star, Value::$type(r)) => Ok(Value::$type(l * r)),
                (Value::$type(l), Token::Slash, Value::$type(r)) => Ok(Value::$type(l / r)),
            )*

            $(
                (Value::$type(l), Token::LeftAngle, Value::$type(r)) => Ok(Value::Boolean(l < r)),
                (Value::$type(l), Token::RightAngle, Value::$type(r)) => Ok(Value::Boolean(l > r)),
                (Value::$type(l), Token::LessEquals, Value::$type(r)) => Ok(Value::Boolean(l <= r)),
                (Value::$type(l), Token::GreaterEquals, Value::$type(r)) => Ok(Value::Boolean(l >= r)),
                (Value::$type(l), Token::Equals, Value::$type(r)) => Ok(Value::Boolean(l == r)),
                (Value::$type(l), Token::NotEquals, Value::$type(r)) => Ok(Value::Boolean(l != r)),
            )*

            _ => Err(format!("Invalid binary operation: {:?} {:?} {:?}", $left_val, $operator, $right_val)),
        }
    }
}

#[macro_export]
macro_rules! impl_promote_to_type {
    (($value:expr, $target:expr), $(($Value:ident, $type:ident)),*) => {
        match ($value, $target) {
            $(
                (Value::I8(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::U8(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::I16(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::U16(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::I32(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::U32(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::ISize(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
                (Value::USize(x), Value::$Value(_)) => Ok(Value::$Value((*x) as $type)),
            )*

            (Value::I8(x), Value::F32(_)) => Ok(Value::F32(*x as f32)),
            (Value::U8(x), Value::F32(_)) => Ok(Value::F32(*x as f32)),
            (Value::I16(x), Value::F32(_)) => Ok(Value::F32(*x as f32)),
            (Value::I32(x), Value::F32(_)) => Ok(Value::F32(*x as f32)),
            (Value::ISize(x), Value::F32(_)) => Ok(Value::F32(*x as f32)),
            (Value::USize(x), Value::F32(_)) => Ok(Value::F32(*x as f32)),

            (Value::I8(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),
            (Value::U8(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),
            (Value::I16(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),
            (Value::I32(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),
            (Value::F32(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),
            (Value::ISize(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),
            (Value::USize(x), Value::F64(_)) => Ok(Value::F64(*x as f64)),

            _ => Err(format!("Cannot promote {:?} to type of {:?}", $value, $target)),
        }
    }
}
