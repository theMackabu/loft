use crate::{
    parser::ast::{Pattern, Type},
    types::checker::{Primitive, TypeError},
};

pub fn is_self_parameter(pat: &Pattern) -> bool {
    match pat {
        Pattern::Identifier { name, .. } => name == "self",
        Pattern::Reference { pattern, .. } => is_self_parameter(pattern),
        _ => false,
    }
}

pub fn convert_type_annotation(ty: &Type) -> Result<Primitive, TypeError> {
    match ty {
        Type::Simple(name) => convert_type_name(name),

        Type::Path(path) => {
            // todo: implement proper path resolution
            // 1. resolve imports/use statements
            // 2. handle qualified paths (e.g., std::string::string)
            // 3. check type aliases
            // 4. handle generic type parameters
            let last_segment = path.segments.last().ok_or_else(|| TypeError::InvalidOperation {
                message: "Empty path in type annotation".to_string(),
                location: "".to_string(), // add location
            })?;

            convert_type_name(&last_segment.ident)
        }

        Type::Unit => Ok(Primitive::Unit),

        _ => Err(TypeError::InvalidOperation {
            message: format!("Unsupported type annotation: {:?}", ty),
            location: "".to_string(), // add location
        }),
    }
}

fn convert_type_name(name: &str) -> Result<Primitive, TypeError> {
    match name {
        "i8" => Ok(Primitive::I8),
        "i16" => Ok(Primitive::I16),
        "i32" => Ok(Primitive::I32),
        "i64" => Ok(Primitive::I64),
        "i128" => Ok(Primitive::I128),
        "isize" => Ok(Primitive::ISize),

        "u8" => Ok(Primitive::U8),
        "u16" => Ok(Primitive::U16),
        "u32" => Ok(Primitive::U32),
        "u64" => Ok(Primitive::U64),
        "u128" => Ok(Primitive::U128),
        "usize" => Ok(Primitive::USize),

        "f32" => Ok(Primitive::F32),
        "f64" => Ok(Primitive::F64),

        "bool" => Ok(Primitive::Bool),
        "char" => Ok(Primitive::Char),
        "str" => Ok(Primitive::Str),

        _ => Err(TypeError::InvalidOperation {
            message: format!("Unknown type: {}", name),
            location: "".to_string(), // add location
        }),
    }
}

impl Primitive {
    pub fn type_name(&self) -> String {
        match self {
            Primitive::I8 => "i8",
            Primitive::I16 => "i16",
            Primitive::I32 => "i32",
            Primitive::I64 => "i64",
            Primitive::I128 => "i128",
            Primitive::ISize => "isize",

            Primitive::U8 => "u8",
            Primitive::U16 => "u16",
            Primitive::U32 => "u32",
            Primitive::U64 => "u64",
            Primitive::U128 => "u128",
            Primitive::USize => "usize",

            Primitive::F32 => "f32",
            Primitive::F64 => "f64",

            Primitive::Bool => "bool",
            Primitive::Char => "char",
            Primitive::Str => "str",

            Primitive::Unit => "()",
            Primitive::Unknown => "<unknown>",
        }
        .to_owned()
    }
}
