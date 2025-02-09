mod ast;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let input = r#"
        let x: i32 = 42;
        let y = x + 10;
        let z = "hello world";
    "#
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    match parser.parse_program() {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => println!("Parse error: {:?}", e),
    }
}
