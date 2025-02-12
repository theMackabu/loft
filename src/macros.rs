#[macro_export]
macro_rules! impl_compound_assignment {
    ($env:expr, $left:expr, $right:expr, $op:expr, {
        $(($type:ident, $rust_type:ty, $method:ident)),* $(,)?
    }) => {
        match ($left, $right) {
            $((Value::$type(l), Value::$type(r)) => Ok(Value::$type(l.$method(*r))),)*

            $((Value::Reference { source_name, source_scope, mutable }, Value::$type(r)) => {
                if !mutable {
                    return Err(format!("Cannot modify immutable reference"));
                }
                if let Some(scope) = $env.scopes.get(*source_scope) {
                    if let Some(Value::$type(l)) = scope.get(source_name) {
                        Ok(Value::$type(l.$method(*r)))
                    } else {
                        Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                    }
                } else {
                    Err(format!("Reference scope not found"))
                }
            },)*

            $((Value::$type(l), Value::Reference { source_name, source_scope, .. }) => {
                if let Some(scope) = $env.scopes.get(*source_scope) {
                    if let Some(Value::$type(r)) = scope.get(source_name) {
                        Ok(Value::$type(l.$method(*r)))
                    } else {
                        Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                    }
                } else {
                    Err(format!("Reference scope not found"))
                }
            },)*

            (Value::Reference { source_name: left_name, source_scope: left_scope, mutable: left_mutable },
             Value::Reference { source_name: right_name, source_scope: right_scope, .. }) => {
                if !left_mutable {
                    return Err(format!("Cannot modify immutable reference"));
                }
                if let (Some(left_scope), Some(right_scope)) = (
                    $env.scopes.get(*left_scope),
                    $env.scopes.get(*right_scope)
                ) {
                    match (left_scope.get(left_name), right_scope.get(right_name)) {
                        $((Some(Value::$type(l)), Some(Value::$type(r))) => {
                            Ok(Value::$type(l.$method(*r)))
                        },)*
                        _ => Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                    }
                } else {
                    Err(format!("Reference scope not found"))
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
