use std::{cell::RefCell, rc::Rc};

use crate::{
    parser::ast::Pattern,
    runtime::value::{Value, ValueType}, // parser::ast::Type,
                                        // types::checker::{Pointer, Primitive, TypeError},
};

// use macros_rs::fmt::fmtstr;

pub fn is_self_parameter(pat: &Pattern) -> bool {
    match pat {
        Pattern::Identifier { name, .. } => name == "self",
        Pattern::Reference { pattern, .. } => is_self_parameter(pattern),
        _ => false,
    }
}

pub fn extract_identifier_info(pattern: &Pattern) -> Option<(String, bool)> {
    match pattern {
        Pattern::Identifier { name, mutable } => Some((name.clone(), *mutable)),
        Pattern::Reference { pattern, .. } => extract_identifier_info(pattern),
        _ => None,
    }
}

pub fn unwrap_value(val: &Value) -> Value {
    let mut current = val.clone();

    loop {
        let borrowed = current.borrow();
        match &borrowed.inner() {
            ValueType::Reference { _undropped, .. } => {
                let next = _undropped.clone();
                drop(borrowed);
                current = next;
            }
            _ => break,
        }
    }

    Rc::new(RefCell::new(current.borrow().clone().into_immutable()))
}

// pub fn convert_type_annotation(ty: &Type) -> Result<Primitive, TypeError> {
//     match ty {
//         Type::Simple(name) => convert_type_name(name),
//
//         Type::Path(path) => {
//             // todo: implement proper path resolution
//             // 1. resolve imports/use statements
//             // 2. handle qualified paths (e.g., std::string::string)
//             // 3. check type aliases
//             // 4. handle generic type parameters
//             let last_segment = path.segments.last().ok_or_else(|| TypeError::InvalidOperation {
//                 message: "Empty path in type annotation".to_string(),
//                 location: "".to_string(), // add location
//             })?;
//
//             convert_type_name(&last_segment.ident)
//         }
//
//         Type::Array { element_type, size } => Ok(Primitive::Array(Box::new(convert_type_annotation(element_type)?), *size)),
//
//         Type::Slice { element_type } => Ok(Primitive::Slice(Box::new(convert_type_annotation(element_type)?))),
//
//         Type::Tuple(types) => {
//             let mut tuple_types = Vec::new();
//             for ty in types {
//                 tuple_types.push(convert_type_annotation(ty)?);
//             }
//             Ok(Primitive::Tuple(tuple_types))
//         }
//
//         Type::Reference { mutable, inner } => {
//             let inner_type = convert_type_annotation(inner)?;
//             Ok(Primitive::Pointer(Box::new(inner_type), Pointer::Reference { mutable: *mutable }))
//         }
//         Type::Pointer { inner } => {
//             let inner_type = convert_type_annotation(inner)?;
//             Ok(Primitive::Pointer(Box::new(inner_type), Pointer::RawPointer))
//         }
//
//         Type::Unit => Ok(Primitive::Unit),
//
//         _ => Err(TypeError::InvalidOperation {
//             message: format!("Unsupported type annotation: {:?}", ty),
//             location: "".to_string(), // add location
//         }),
//     }
// }
//
// fn convert_type_name(name: &str) -> Result<Primitive, TypeError> {
//     match name {
//         "i8" => Ok(Primitive::I8),
//         "i16" => Ok(Primitive::I16),
//         "i32" => Ok(Primitive::I32),
//         "i64" => Ok(Primitive::I64),
//         "i128" => Ok(Primitive::I128),
//         "isize" => Ok(Primitive::ISize),
//
//         "u8" => Ok(Primitive::U8),
//         "u16" => Ok(Primitive::U16),
//         "u32" => Ok(Primitive::U32),
//         "u64" => Ok(Primitive::U64),
//         "u128" => Ok(Primitive::U128),
//         "usize" => Ok(Primitive::USize),
//
//         "f32" => Ok(Primitive::F32),
//         "f64" => Ok(Primitive::F64),
//
//         "bool" => Ok(Primitive::Bool),
//         "char" => Ok(Primitive::Char),
//         "str" => Ok(Primitive::Str),
//
//         _ => Err(TypeError::InvalidOperation {
//             message: format!("Unknown type: {}", name),
//             location: "".to_string(), // add location
//         }),
//     }
// }
//
// impl Primitive {
//     pub fn type_name(&self) -> String {
//         match self {
//             Primitive::I8 => "i8",
//             Primitive::I16 => "i16",
//             Primitive::I32 => "i32",
//             Primitive::I64 => "i64",
//             Primitive::I128 => "i128",
//             Primitive::ISize => "isize",
//
//             Primitive::U8 => "u8",
//             Primitive::U16 => "u16",
//             Primitive::U32 => "u32",
//             Primitive::U64 => "u64",
//             Primitive::U128 => "u128",
//             Primitive::USize => "usize",
//
//             Primitive::F32 => "f32",
//             Primitive::F64 => "f64",
//
//             Primitive::Bool => "bool",
//             Primitive::Char => "char",
//             Primitive::Str => "str",
//
//             Primitive::Array(elem_type, size) => fmtstr!("[{}; {}]", elem_type.type_name(), size),
//             Primitive::Slice(elem_type) => fmtstr!("[{}]", elem_type.type_name()),
//
//             Primitive::Tuple(types) => {
//                 if types.is_empty() {
//                     "()"
//                 } else {
//                     let types_str: Vec<String> = types.iter().map(|t| t.type_name()).collect();
//                     fmtstr!("({})", types_str.join(", "))
//                 }
//             }
//
//             Primitive::Pointer(inner_type, pointer_type) => match pointer_type {
//                 Pointer::Reference { mutable } => {
//                     if *mutable {
//                         fmtstr!("&mut {}", inner_type.type_name())
//                     } else {
//                         fmtstr!("&{}", inner_type.type_name())
//                     }
//                 }
//                 Pointer::RawPointer => fmtstr!("*{}", inner_type.type_name()),
//             },
//
//             Primitive::Unit => "()",
//             Primitive::Unknown => "<unknown>",
//         }
//         .to_owned()
//     }
// }
