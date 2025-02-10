use crate::parser::ast::*;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new(ast: Vec<Stmt>) -> Self { Self }

    pub fn check(&self) -> Result<(), ()> { Ok(()) }

    pub fn strip(&self) -> Vec<Stmt> { Vec::new() }
}
