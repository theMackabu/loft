#[macro_export]
macro_rules! val {
    (mut $inner:expr) => {
        Rc::new(RefCell::new(ValueEnum::Mutable($inner)))
    };
    ($inner:expr) => {
        Rc::new(RefCell::new(ValueEnum::Immutable($inner)))
    };
}

#[macro_export]
macro_rules! impl_compound_assignment {
    ($env:expr, $left:expr, $right:expr, $op:expr, {
        $(($type:ident, $rust_type:ty, $method:ident)),* $(,)?
    }) => {
        match ($left.inner(), $right.inner()) {
            $((ValueType::$type(l), ValueType::$type(r)) => Ok(val!(ValueType::$type(l.$method(r)))),)*

            $((ValueType::Reference { source_name, source_scope, .. }, ValueType::$type(r)) => {
                if !$left.is_mutable() {
                    return Err(format!("Cannot modify immutable reference"));
                }
                if let Some(scope) = $env.scopes.get(source_scope.expect("HANDLE THIS")) {
                    if let Some(value) = scope.get(&source_name.clone().expect("HANDLE THIS")) {
                        if let ValueType::$type(l) = value.inner() {
                            Ok(val!(ValueType::$type(l.$method(r))))
                        } else {
                            Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                        }
                    } else {
                        Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                    }
                } else {
                    Err(format!("Reference scope not found"))
                }
            },)*

            $((ValueType::$type(l), ValueType::Reference { source_name, source_scope, .. }) => {
                if let Some(scope) = $env.scopes.get(source_scope.expect("HANDLE THIS")) {
                    if let Some(value) = scope.get(&source_name.clone().expect("HANDLE THIS")) {
                        if let ValueType::$type(r) = value.inner() {
                            Ok(val!(ValueType::$type(l.$method(r))))
                        } else {
                            Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                        }
                    } else {
                        Err(format!("Cannot perform {:?} operation between {} and {}", $op, $left, $right))
                    }
                } else {
                    Err(format!("Reference scope not found"))
                }
            },)*

            (ValueType::Reference { source_name: left_name, source_scope: left_scope, .. },
             ValueType::Reference { source_name: right_name, source_scope: right_scope, .. }) => {
                if !$left.is_mutable() {
                    return Err(format!("Cannot modify immutable reference"));
                }
                if let (Some(left_scope), Some(right_scope)) = (
                    $env.scopes.get(left_scope.expect("HANDLE THIS")),
                    $env.scopes.get(right_scope.expect("HANDLE THIS"))
                ) {
                    match (
                        left_scope.get(&left_name.clone().expect("HANDLE THIS")).map(|v| v.inner()),
                        right_scope.get(&right_name.clone().expect("HANDLE THIS")).map(|v| v.inner())
                    ) {
                        $((Some(ValueType::$type(l)), Some(ValueType::$type(r))) => {
                            Ok(val!(ValueType::$type(l.$method(r))))
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
        match ($left_val.inner(), $operator, $right_val.inner()) {
            $(
                (ValueType::$type(l), Token::Plus, ValueType::$type(r)) => Ok(val!(ValueType::$type(l + r))),
                (ValueType::$type(l), Token::Minus, ValueType::$type(r)) => Ok(val!(ValueType::$type(l - r))),
                (ValueType::$type(l), Token::Star, ValueType::$type(r)) => Ok(val!(ValueType::$type(l * r))),
                (ValueType::$type(l), Token::Slash, ValueType::$type(r)) => Ok(val!(ValueType::$type(l / r))),
            )*

            $(
            (ValueType::$type(l), Token::LeftAngle, ValueType::$type(r)) => Ok(val!(ValueType::Boolean(l < r))),
            (ValueType::$type(l), Token::RightAngle, ValueType::$type(r)) => Ok(val!(ValueType::Boolean(l > r))),
            (ValueType::$type(l), Token::LessEquals, ValueType::$type(r)) => Ok(val!(ValueType::Boolean(l <= r))),
            (ValueType::$type(l), Token::GreaterEquals, ValueType::$type(r)) => Ok(val!(ValueType::Boolean(l >= r))),
            (ValueType::$type(l), Token::Equals, ValueType::$type(r)) => Ok(val!(ValueType::Boolean(l == r))),
            (ValueType::$type(l), Token::NotEquals, ValueType::$type(r)) => Ok(val!(ValueType::Boolean(l != r))),
            )*

            _ => Err(format!("Invalid binary operation: {:?} {:?} {:?}", $left_val, $operator, $right_val)),
        }
    }
}

#[macro_export]
macro_rules! impl_promote_to_type {
    (($value:expr, $target:expr), $(($Value:ident, $type:ident)),*) => {
        match ($value.inner(), $target.inner()) {
            $(
                (ValueType::I8(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::U8(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::I16(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::U16(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::I32(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::U32(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::ISize(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
                (ValueType::USize(x), ValueType::$Value(_)) => Ok(val!(ValueType::$Value(x as $type))),
            )*

            (ValueType::I8(x), ValueType::F32(_)) => Ok(val!(ValueType::F32(x as f32))),
            (ValueType::U8(x), ValueType::F32(_)) => Ok(val!(ValueType::F32(x as f32))),
            (ValueType::I16(x), ValueType::F32(_)) => Ok(val!(ValueType::F32(x as f32))),
            (ValueType::I32(x), ValueType::F32(_)) => Ok(val!(ValueType::F32(x as f32))),
            (ValueType::ISize(x), ValueType::F32(_)) => Ok(val!(ValueType::F32(x as f32))),
            (ValueType::USize(x), ValueType::F32(_)) => Ok(val!(ValueType::F32(x as f32))),

            (ValueType::I8(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),
            (ValueType::U8(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),
            (ValueType::I16(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),
            (ValueType::I32(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),
            (ValueType::F32(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),
            (ValueType::ISize(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),
            (ValueType::USize(x), ValueType::F64(_)) => Ok(val!(ValueType::F64(x as f64))),

            _ => Err(format!("Cannot promote {:?} to type of {:?}", $value, $target)),
        }
    }
}
