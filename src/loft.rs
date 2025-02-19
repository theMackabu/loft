use loft::{
    error::{Error, Result},
    // types::checker::TypeChecker,
    parser::{lexer::Lexer, Parser},
    runtime::{interpreter::Interpreter, value::ValueType},
};

use std::{fs, process::exit};

// !TEMPORARY PRELUDE IMPL
const PRELUDE: &'static str = r#"
pub enum Result<T, E> {
    Ok(T),
    Err(E),
}

pub enum Option<T> {
    None,
    Some(T),
}

impl Option<T> {
    fn is_some(&self) -> bool {
        match *self {
            Option::Some(_) => true,
            _ => false
        }
    }
    
    fn unwrap(self) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => core::panic("called `Option::unwrap()` on a `none` value")
        }
    }
}
"#;

fn run() -> Result {
    let filename = std::env::args().nth(1).ok_or(Error::MissingArgument)?;
    let input = fs::read_to_string(&filename)?;

    // !TEMPORARY PRELUDE IMPL
    let input = format!("{}\n{}", PRELUDE, input);

    let ast = Parser::new(Lexer::new(input)).parse_program().map_err(|err| Error::ParseError(err.to_string()))?;

    // println!("{ast:#?}");

    let mut runtime = Interpreter::new(&ast).map_err(|err| Error::RuntimeError(err.to_string()))?;
    let result = runtime.start_main().map_err(|err| Error::RuntimeError(err.to_string()))?;

    let result_inner = {
        let borrowed = result.borrow();
        borrowed.inner()
    };

    match result_inner {
        ValueType::I32(code) => exit(code),
        ValueType::Unit => Ok(()),
        err => Err(Error::UnexpectedReturnValue(err)),
    }
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        exit(1);
    }
}
