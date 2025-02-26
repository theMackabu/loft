#[macro_export]
macro_rules! unbind {
    { $pattern:pat = $expr:expr } => {
        let $pattern = $expr else {
            unreachable!();
        };
    };
}

#[macro_export]
macro_rules! inner_val {
    ($value:ident) => {
        let $value = {
            let borrowed = $value.borrow();
            borrowed.inner()
        };
    };
}

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
macro_rules! impl_binary_ops {
    (
        ($left_val:expr, $operator:expr, $right_val:expr),
        [$($num:ident),*],
        [$($int:ident),*],
        [$($else:ident),*]
    ) => {
        {
            let left = unwrap_value(&$left_val);
            let right = unwrap_value(&$right_val);

            let left_borrowed = left.borrow();
            let right_borrowed = right.borrow();

            match (left_borrowed.inner(), $operator, right_borrowed.inner()) {
                (ValueType::Boolean(l), Token::And, ValueType::Boolean(r)) => Ok(val!(ValueType::Boolean(l && r))),
                (ValueType::Boolean(l), Token::Or, ValueType::Boolean(r)) => Ok(val!(ValueType::Boolean(l || r))),

                $(
                    (ValueType::$num(l), Token::Plus, ValueType::$num(r)) => Ok(val!(ValueType::$num(l + r))),
                    (ValueType::$num(l), Token::Minus, ValueType::$num(r)) => Ok(val!(ValueType::$num(l - r))),
                    (ValueType::$num(l), Token::Star, ValueType::$num(r)) => Ok(val!(ValueType::$num(l * r))),
                    (ValueType::$num(l), Token::Slash, ValueType::$num(r)) => Ok(val!(ValueType::$num(l / r))),
                    (ValueType::$num(l), Token::Rem, ValueType::$num(r)) => Ok(val!(ValueType::$num(l % r))),
                )*

                $(
                    (ValueType::$else(l), Token::LeftAngle, ValueType::$else(r)) => Ok(val!(ValueType::Boolean(l < r))),
                    (ValueType::$else(l), Token::RightAngle, ValueType::$else(r)) => Ok(val!(ValueType::Boolean(l > r))),
                    (ValueType::$else(l), Token::LessEquals, ValueType::$else(r)) => Ok(val!(ValueType::Boolean(l <= r))),
                    (ValueType::$else(l), Token::GreaterEquals, ValueType::$else(r)) => Ok(val!(ValueType::Boolean(l >= r))),
                    (ValueType::$else(l), Token::Equals, ValueType::$else(r)) => Ok(val!(ValueType::Boolean(l == r))),
                    (ValueType::$else(l), Token::NotEquals, ValueType::$else(r)) => Ok(val!(ValueType::Boolean(l != r))),
                )*

                $(
                    (ValueType::$int(l), Token::BitAnd, ValueType::$int(r)) => Ok(val!(ValueType::$int(l & r))),
                    (ValueType::$int(l), Token::BitOr,  ValueType::$int(r)) => Ok(val!(ValueType::$int(l | r))),
                    (ValueType::$int(l), Token::BitXor, ValueType::$int(r)) => Ok(val!(ValueType::$int(l ^ r))),
                    (ValueType::$int(l), Token::Shl, ValueType::$int(r)) => Ok(val!(ValueType::$int(l << r))),
                    (ValueType::$int(l), Token::Shr, ValueType::$int(r)) => Ok(val!(ValueType::$int(l >> r))),
                )*

                (_, Token::Assign, _)
                | (_, Token::PlusEquals, _)
                | (_, Token::MinusEquals, _)
                | (_, Token::StarEquals, _)
                | (_, Token::SlashEquals, _)
                | (_, Token::RemAssign, _)
                | (_, Token::BitAndAssign, _)
                | (_, Token::BitOrAssign, _)
                | (_, Token::BitXorAssign, _)
                | (_, Token::ShlAssign, _)
                | (_, Token::ShrAssign, _)
                    => Err(format!("Operator `{}` is not supported in binary operations", $operator)),

                _ => Err(format!("Invalid binary operation: {left_borrowed} {} {right_borrowed}", $operator)),
            }
        }
    }
}

#[macro_export]
macro_rules! impl_promote_to_type {
    (($value:expr, $target:expr), $(($Value:ident, $type:ident)),*) => {
        {
            let value_ref = $value.borrow();
            let target_ref = $target.borrow();

            match (value_ref.inner(), target_ref.inner()) {
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

                _ => Err(format!("Cannot promote {:?} to type of {:?}", value_ref.inner(), target_ref.inner())),
            }
        }
    }
}

#[macro_export]
macro_rules! impl_compound_assignment {
    ($env:expr, $left:expr, $right:expr, $op:expr, {
        $(($type:ident, $rust_type:ty, $method:ident)),* $(,)?
    }) => {
        {
            let left_inner = unwrap_assignment(&$env, &$left).borrow().inner();
            let right_inner = unwrap_assignment(&$env, &$right).borrow().inner();

            match (left_inner, right_inner) {
                $(
                    (ValueType::$type(l), ValueType::$type(r)) => {
                        Ok(val!(ValueType::$type(l.$method(r))))
                    },
                )*

                $(
                    (
                        ValueType::Reference { source_name, source_scope, .. },
                        ValueType::$type(r)
                    ) => {
                        if !$left.borrow().is_mutable() {
                            return Err(format!("Cannot modify immutable reference"));
                        }
                        if let Some(scope) = $env.scopes.get(source_scope.expect("HANDLE THIS")) {
                            if let Some(value) = scope.get(&source_name.clone().expect("HANDLE THIS")) {
                                if let ValueType::$type(l) = value.borrow().inner() {
                                    Ok(val!(ValueType::$type(l.$method(r))))
                                } else {
                                    Err(format!(
                                        "Cannot perform {:?} operation between {:?} and {:?}",
                                        $op, $left, $right
                                    ))
                                }
                            } else {
                                Err(format!(
                                    "Cannot perform {:?} operation between {:?} and {:?}",
                                    $op, $left, $right
                                ))
                            }
                        } else {
                            Err(format!("Reference scope not found"))
                        }
                    },
                )*

                $(
                    (
                        ValueType::$type(l),
                        ValueType::Reference { source_name, source_scope, .. }
                    ) => {
                        if let Some(scope) = $env.scopes.get(source_scope.expect("HANDLE THIS")) {
                            if let Some(value) = scope.get(&source_name.clone().expect("HANDLE THIS")) {
                                if let ValueType::$type(r) = value.borrow().inner() {
                                    Ok(val!(ValueType::$type(l.$method(r))))
                                } else {
                                    Err(format!(
                                        "Cannot perform {:?} operation between {:?} and {:?}",
                                        $op, $left, $right
                                    ))
                                }
                            } else {
                                Err(format!(
                                    "Cannot perform {:?} operation between {:?} and {:?}",
                                    $op, $left, $right
                                ))
                            }
                        } else {
                            Err(format!("Reference scope not found"))
                        }
                    },
                )*

                (
                    ValueType::Reference {
                        source_name: left_name,
                        source_scope: left_scope,
                        ..
                    },
                    ValueType::Reference {
                        source_name: right_name,
                        source_scope: right_scope,
                        ..
                    }
                ) => {
                    if !$left.borrow().is_mutable() {
                        return Err(format!("Cannot modify immutable reference"));
                    }
                    if let (Some(left_sc), Some(right_sc)) = (
                        $env.scopes.get(left_scope.expect("HANDLE THIS")),
                        $env.scopes.get(right_scope.expect("HANDLE THIS"))
                    ) {
                        match (
                            left_sc.get(&left_name.clone().expect("HANDLE THIS"))
                                .map(|v| v.borrow().inner()),
                            right_sc.get(&right_name.clone().expect("HANDLE THIS"))
                                .map(|v| v.borrow().inner())
                        ) {
                            $(
                                (Some(ValueType::$type(l)), Some(ValueType::$type(r))) => {
                                    Ok(val!(ValueType::$type(l.$method(r))))
                                },
                            )*
                            _ => Err(format!(
                                "Cannot perform {:?} operation between {:?} and {:?}",
                                $op, $left, $right
                            ))
                        }
                    } else {
                        Err(format!("Reference scope not found"))
                    }
                },
                _ => Err(format!(
                    "Cannot perform {:?} operation between {:?} and {:?}",
                    $op, $left, $right
                ))
            }
        }
    }
}
