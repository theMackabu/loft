mod ast;
mod lexer;
mod parser;
mod scoping;
mod traits;
mod util;

use lexer::Lexer;
use parser::Parser;
use scoping::resolve_program;

use std::process::ExitCode;
use std::{error::Error, fs};

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let input = fs::read_to_string("test.rt")?;
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => {
            if let Err(e) = resolve_program(&ast) {
                println!("Scope resolution error: {}", e);
            } else {
                println!("{:#?}", ast);
            }
        }
        Err(err) => println!("Parse error: {err}"),
    }

    Ok(ExitCode::SUCCESS)
}
