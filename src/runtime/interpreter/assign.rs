use super::*;
use crate::impl_compound_assignment;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

fn unwrap_assignment<'a>(env: &'a Environment, mut val: &'a Value) -> &'a Value {
    while let ValueType::Reference {
        source_name: Some(ref s_name),
        source_scope: Some(scope_idx),
        ..
    } = val.borrow().inner()
    {
        if let Some(scope) = env.scopes.get(scope_idx) {
            if let Some(next) = scope.get(s_name) {
                val = next;
                continue;
            }
        }
        break;
    }
    val
}

impl<'st> Interpreter<'st> {
    pub fn evaluate_compound_assignment(&self, left: &Value, operator: &Token, right: &Value) -> Result<Value, String> {
        match operator {
            Token::PlusEquals => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, wrapping_add),
                (I16, i16, wrapping_add),
                (I32, i32, wrapping_add),
                (I64, i64, wrapping_add),
                (I128, i128, wrapping_add),
                (ISize, isize, wrapping_add),
                (U8, u8, wrapping_add),
                (U16, u16, wrapping_add),
                (U32, u32, wrapping_add),
                (U64, u64, wrapping_add),
                (U128, u128, wrapping_add),
                (USize, usize, wrapping_add),
                (F32, f32, add),
                (F64, f64, add),
            }),

            Token::MinusEquals => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, wrapping_sub),
                (I16, i16, wrapping_sub),
                (I32, i32, wrapping_sub),
                (I64, i64, wrapping_sub),
                (I128, i128, wrapping_sub),
                (ISize, isize, wrapping_sub),
                (U8, u8, wrapping_sub),
                (U16, u16, wrapping_sub),
                (U32, u32, wrapping_sub),
                (U64, u64, wrapping_sub),
                (U128, u128, wrapping_sub),
                (USize, usize, wrapping_sub),
                (F32, f32, sub),
                (F64, f64, sub),
            }),

            Token::StarEquals => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, wrapping_mul),
                (I16, i16, wrapping_mul),
                (I32, i32, wrapping_mul),
                (I64, i64, wrapping_mul),
                (I128, i128, wrapping_mul),
                (ISize, isize, wrapping_mul),
                (U8, u8, wrapping_mul),
                (U16, u16, wrapping_mul),
                (U32, u32, wrapping_mul),
                (U64, u64, wrapping_mul),
                (U128, u128, wrapping_mul),
                (USize, usize, wrapping_mul),
                (F32, f32, mul),
                (F64, f64, mul),
            }),

            Token::SlashEquals => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, wrapping_div),
                (I16, i16, wrapping_div),
                (I32, i32, wrapping_div),
                (I64, i64, wrapping_div),
                (I128, i128, wrapping_div),
                (ISize, isize, wrapping_div),
                (U8, u8, wrapping_div),
                (U16, u16, wrapping_div),
                (U32, u32, wrapping_div),
                (U64, u64, wrapping_div),
                (U128, u128, wrapping_div),
                (USize, usize, wrapping_div),
                (F32, f32, div),
                (F64, f64, div),
            }),

            Token::RemAssign => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, wrapping_rem),
                (I16, i16, wrapping_rem),
                (I32, i32, wrapping_rem),
                (I64, i64, wrapping_rem),
                (I128, i128, wrapping_rem),
                (ISize, isize, wrapping_rem),
                (U8, u8, wrapping_rem),
                (U16, u16, wrapping_rem),
                (U32, u32, wrapping_rem),
                (U64, u64, wrapping_rem),
                (U128, u128, wrapping_rem),
                (USize, usize, wrapping_rem),
                (F32, f32, rem),
                (F64, f64, rem),
            }),

            Token::BitAndAssign => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, bitand),
                (I16, i16, bitand),
                (I32, i32, bitand),
                (I64, i64, bitand),
                (I128, i128, bitand),
                (ISize, isize, bitand),
                (U8, u8, bitand),
                (U16, u16, bitand),
                (U32, u32, bitand),
                (U64, u64, bitand),
                (U128, u128, bitand),
                (USize, usize, bitand),
            }),

            Token::BitOrAssign => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, bitor),
                (I16, i16, bitor),
                (I32, i32, bitor),
                (I64, i64, bitor),
                (I128, i128, bitor),
                (ISize, isize, bitor),
                (U8, u8, bitor),
                (U16, u16, bitor),
                (U32, u32, bitor),
                (U64, u64, bitor),
                (U128, u128, bitor),
                (USize, usize, bitor),
            }),

            Token::BitXorAssign => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, bitxor),
                (I16, i16, bitxor),
                (I32, i32, bitxor),
                (I64, i64, bitxor),
                (I128, i128, bitxor),
                (ISize, isize, bitxor),
                (U8, u8, bitxor),
                (U16, u16, bitxor),
                (U32, u32, bitxor),
                (U64, u64, bitxor),
                (U128, u128, bitxor),
                (USize, usize, bitxor),
            }),

            Token::ShlAssign => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, shl),
                (I16, i16, shl),
                (I32, i32, shl),
                (I64, i64, shl),
                (I128, i128, shl),
                (ISize, isize, shl),
                (U8, u8, shl),
                (U16, u16, shl),
                (U32, u32, shl),
                (U64, u64, shl),
                (U128, u128, shl),
                (USize, usize, shl),
            }),

            Token::ShrAssign => impl_compound_assignment!(self.env, left, right, operator, {
                (I8, i8, shr),
                (I16, i16, shr),
                (I32, i32, shr),
                (I64, i64, shr),
                (I128, i128, shr),
                (ISize, isize, shr),
                (U8, u8, shr),
                (U16, u16, shr),
                (U32, u32, shr),
                (U64, u64, shr),
                (U128, u128, shr),
                (USize, usize, shr),
            }),

            _ => Err(format!("Unsupported compound assignment operator: {:?}", operator)),
        }
    }
}
