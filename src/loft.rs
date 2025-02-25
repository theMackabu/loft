use loft::{
    error::{Error, Result},
    // types::checker::TypeChecker,
    parser::{Parser, lexer::Lexer},
    runtime::{interpreter::Interpreter, value::ValueType},
};

use std::{fs, process::exit};

// !TEMPORARY PRELUDE IMPL
const PRELUDE: &'static str = include_str!("std/prelude.lo");

fn run() -> Result {
    let filename = std::env::args().nth(1).ok_or(Error::MissingArgument)?;
    let mut input = fs::read_to_string(&filename)?;

    if input.starts_with("#!") {
        if let Some(newline_pos) = input.find('\n') {
            input = input[newline_pos + 1..].to_string();
        } else {
            input = String::new();
        }
    }

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
