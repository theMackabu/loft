mod ast;
mod lexer;
mod parser;
mod traits;
mod util;

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
        Err(err) => println!("Parse error: {err}"),
    }

    Ok(ExitCode::SUCCESS)
}
