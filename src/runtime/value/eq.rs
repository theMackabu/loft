use super::*;

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueType::Function(f1), ValueType::Function(f2)) => Rc::ptr_eq(f1, f2),
            (ValueType::Unit, ValueType::Unit) => true,
            _ => false,
        }
    }
}
