use loft::{
    parser::{lexer::Lexer, Parser},
    runtime::interpreter::Interpreter,
    // types::checker::TypeChecker,
};

use std::process::ExitCode;
use std::{error::Error, fs};

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = std::env::args().collect::<Vec<String>>();
    let input = fs::read_to_string(&args[1])?;
    let lexer = Lexer::new(input);

    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => {
            // let mut types = TypeChecker::new(&ast);
            let mut runtime = Interpreter::new(&ast)?;

            // if let Err(err) = types.check() {
            //     println!("Type error: {err:?}");
            //     return Ok(ExitCode::FAILURE);
            // }

            match runtime.start_main() {
                Ok(value) => println!("Evaluated with {value:?}"),
                Err(err) => println!("Runtime error: {err}"),
            }
        }
        Err(err) => println!("Parse error: {err}"),
    }

    Ok(ExitCode::SUCCESS)
}
