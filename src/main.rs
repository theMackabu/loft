mod ast;
mod error;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

use std::process::ExitCode;
use std::{error::Error, fs};

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let input = fs::read_to_string("test.rt")?;
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => println!("Parse error: {:?}", e),
    }

    Ok(ExitCode::SUCCESS)
}
