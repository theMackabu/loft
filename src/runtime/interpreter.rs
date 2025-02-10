use crate::parser::ast::*;

pub struct Interpreter;

impl Interpreter {
    pub fn new(ast: Vec<Stmt>) -> Self { Self }

    pub fn execute(&self) -> Result<(), ()> { Ok(()) }
}
