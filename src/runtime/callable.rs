use std::fmt::{self, Debug, Display};
use std::{cell::RefCell, rc::Rc};

use super::interpreter::{environment::Environment, Interpreter};
use crate::parser::ast::{Attribute, Pattern, Stmt, Type};
use crate::runtime::value::Value;

pub trait Callable: Debug + Display {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, String>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct UserFunction {
    pub name: String,
    pub visibility: bool,
    pub is_async: bool,
    pub type_params: Vec<String>,
    pub params: Vec<(Pattern, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Stmt>,
    pub attributes: Vec<Attribute>,
    pub closure: Rc<RefCell<Environment>>,
}

impl Display for UserFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "<fn {}>", self.name) }
}

impl Callable for UserFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
        let current_depth = interpreter.env.scopes.len();
        interpreter.env = interpreter.env.new_enclosed(self.closure.clone());

        if args.len() != self.params.len() {
            return Err(format!("Function '{}' expected {} arguments, got {}", self.name, self.params.len(), args.len()));
        }

        for ((pattern, _ty), arg) in self.params.iter().zip(args.into_iter()) {
            match pattern {
                Pattern::Identifier { name, mutable } => {
                    interpreter.env.scope_resolver.declare_variable(name, *mutable);
                    interpreter.env.set_variable(name, arg)?;
                }
                _ => {
                    return Err("Unsupported function parameter pattern.".into());
                }
            }
        }

        let result = interpreter.execute(&self.body);

        while interpreter.env.scopes.len() > current_depth {
            interpreter.env.exit_scope();
        }

        result
    }
}

pub fn make_user_function(stmt_fn: &Stmt, env: Rc<RefCell<Environment>>) -> Result<UserFunction, String> {
    if let Stmt::Function {
        name,
        visibility,
        is_async,
        type_params,
        params,
        return_type,
        body,
        attributes,
    } = stmt_fn
    {
        Ok(UserFunction {
            closure: env,
            name: name.clone(),
            visibility: *visibility,
            is_async: *is_async,

            params: params.clone(),
            body: body.clone(),

            type_params: type_params.clone(),
            return_type: return_type.clone(),
            attributes: attributes.clone(),
        })
    } else {
        Err("Expected a function declaration (`Stmt::Function`)".into())
    }
}
