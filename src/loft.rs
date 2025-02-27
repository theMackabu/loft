use loft::{
    error::{Error, Result},
    parser::{Parser, lexer::Lexer},
    runtime::{interpreter::Interpreter, value::ValueType},
};

use std::{fs, process::exit, sync::Once};

// !TEMPORARY PRELUDE IMPL
const PRELUDE: &'static str = include_str!("std/prelude.lo");
static PRELUDE_INITIALIZED: Once = Once::new();

fn run() -> Result<()> {
    let filename = std::env::args().nth(1).ok_or(Error::MissingArgument)?;
    let mut input = fs::read_to_string(&filename)?;

    if input.starts_with("#!") {
        if let Some(newline_pos) = input.find('\n') {
            input = input[newline_pos + 1..].to_string();
        } else {
            input = String::new();
        }
    }

    let ast = Parser::new(Lexer::new(input)).parse_program().map_err(|err| Error::ParseError(err.to_string()))?;
    let mut runtime = Interpreter::new(&ast);

    PRELUDE_INITIALIZED.call_once(|| {
        let prelude_ast = Parser::new(Lexer::new(PRELUDE.to_string()))
            .parse_program()
            .unwrap_or_else(|e| panic!("Failed to parse prelude: {}", e));

        for stmt in &prelude_ast {
            runtime.execute_statement(stmt).unwrap_or_else(|e| panic!("Failed to execute prelude: {}", e));
        }
    });

    runtime.import_prelude().map_err(|err| Error::RuntimeError(err))?;
    runtime.declare_globals(ast).map_err(|err| Error::RuntimeError(err))?;
    runtime.env.scope_resolver.resolve_program(&runtime.program).map_err(|err| Error::RuntimeError(err))?;

    // stacker::maybe_grow(16 * 1024 * 1024, 128 * 1024 * 1024, || {
    let result = runtime.execute_program().map_err(|err| Error::RuntimeError(err.to_string()))?;
    let borrowed = result.borrow().clone();

    match borrowed.inner() {
        ValueType::Unit => Ok(()),
        ValueType::I32(code) => exit(code),

        _ => Err(Error::UnexpectedReturnValue(borrowed)),
    }
    // })
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        exit(1);
    }
}
