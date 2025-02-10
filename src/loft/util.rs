use crate::parser::ast::Pattern;

pub fn is_self_parameter(pat: &Pattern) -> bool {
    match pat {
        Pattern::Identifier { name, .. } => name == "self",
        Pattern::Reference { pattern, .. } => is_self_parameter(pattern),
        _ => false,
    }
}
