# Loft

<a href="https://github.com/theMackabu/loft/releases">
  <img alt="GitHub Release" src="https://img.shields.io/github/v/release/themackabu/loft?include_prereleases&logo=rust&label=latest%20alpha">
</a>

Loft is a new programming language designed to provide a Rust-like development experience with a focus on simplicity and expressiveness. This project is currently in alpha stage.

> [!WARNING]
>
> **IMPORTANT:** Loft is currently in very early development (v0.0.1-alpha) and should be considered experimental. Many features are incomplete, unstable, or subject to significant changes. The language is not yet suitable for production use.
>
> - The syntax and semantics may change without notice
> - The standard library is minimal and constantly evolving
> - Error messages might be unhelpful or confusing
> - Performance optimization has not been a priority yet
> - Documentation is sparse and may be outdated
> - Memory safety guarantees are still being implemented
>
> Use at your own risk, and please report any issues or bugs you encounter to help improve the language!

## Overview

Loft combines the safety and expressiveness of Rust with a more approachable learning curve. Key features include:

- Strong static typing
- Pattern matching
- First-class macros
- Support for structs and enums
- Ownership and borrowing semantics
- Safe memory management without a garbage collector

## Implementation Architecture

Loft is currently implemented as an interpreted language, though future versions may include compilation targets. The implementation consists of several key components:

### Interpreter

The language uses a token-based interpreter with the following pipeline:

1. **Lexing**: Source code is tokenized by the lexer (`src/parser/lexer.rs`), which converts raw text into tokens.
2. **Parsing**: The parser (`src/parser/mod.rs`) transforms tokens into an abstract syntax tree (AST).
3. **Runtime Interpretation**: The interpreter (`src/runtime/interpreter.rs`) walks through the AST and executes each node.
4. **Value Representation**: Values are represented at runtime through the `ValueType` enum (`src/runtime/value.rs`).

The interpreter design is modular, with separate components handling:

- Expressions and statements
- Control flow (conditionals, loops)
- Pattern matching
- Macro expansion
- Struct and enum operations
- References and borrowing

### Typing System

The type system is currently dynamic at runtime but with strong type checking. A more comprehensive static type checking system is in development in `src/types/checker.rs`, but is not fully implemented or integrated yet.

### Macros

The macro system is a significant work in progress, with temporary implementations visible in `src/runtime/interpreter/macro/`. Macros are parsed and expanded during runtime with a declarative pattern-matching approach.

### Memory Management

The memory model uses reference counting (via Rust's `Rc<RefCell<>>`) to manage object lifetimes. The borrowing and ownership semantics are partially implemented but not yet fully aligned with Rust's guarantees.

## Current Development Focus

The project is actively working on:

1. **Expanding the standard library** - Currently minimal, as seen in `src/std/prelude.lo`
2. **Improving the type checker** - Moving toward static analysis
3. **Refactoring the macro system** - Making it more robust and compatible with Rust's approach
4. **Enhancing error reporting** - Providing more helpful diagnostics
5. **Implementing missing language features** - Closures, traits, and more complete pattern matching

## Examples

### Hello World

```rust
fn main() {
    println!("Hello, world!");
}
```

### FizzBuzz

```rust
fn main() {
    for x in 1..=30 {
        match (x % 3, x % 5) {
            (0, 0) => println!("FizzBuzz"),
            (0, _) => println!("Fizz"),
            (_, 0) => println!("Buzz"),
            _ => println!("{x}"),
        }
    }
}
```

### Structs and Methods

```rust
struct Triangle {
    base: f64,
    height: f64,
    info: Info
}

struct Info {
    id: u8,
    name: &str,
}

impl Triangle {
    fn new(base: f64, height: f64, name: &str, id: u8) -> Self {
        Self { base, height, info: Info { name, id } }
    }

    fn display(&self) {
        println!("Triangle: base={}, height={}, id={}, name={}",
                self.base, self.height, self.info.id, self.info.name);
    }
}
```

## Installation

### From Source

```bash
git clone https://github.com/themackabu/loft
cd loft
cargo install --path .
```

Or using the provided Maidfile:

```bash
maid install
```

## Language Features

### Types

Loft supports a variety of primitive types:

- Integers: `i8`, `i16`, `i32`, `i64`, `i128`, `isize`
- Unsigned integers: `u8`, `u16`, `u32`, `u64`, `u128`, `usize`
- Floating point: `f32`, `f64`
- Boolean: `bool`
- Strings: `str`
- Unit type: `()`

### Pattern Matching

```rust
let result = match value {
    Ok(n) => n,
    Err(_) => -1,
};
```

### Macros

```rust
macro_rules! log {
    (error, $($arg:tt)*) => {
        println!("\x1b[31;40m[ERROR] {}\x1b[0m", format!($($arg)*))
    };
    (info: $($arg:tt)*) => {
        println!("\x1b[94m[INFO] {}\x1b[0m", format!($($arg)*))
    };
}
```

### Ownership and References

```rust
fn test_assignment(val: &mut i32) -> &mut i32 {
   *val += 3;
   return val;
}

fn main() {
   let mut value = 5;
   let val2 = test_assignment(&mut value);
   *val2 += 10;
}
```

## Running Loft Programs

### Direct Execution

```bash
loft your_program.lo
```

### With the Maidfile

```bash
maid run your_program.lo
```

### As a Script (with shebang)

```rust
#!/usr/bin/env loft
fn main() {
   println!("Running as a script!")
}
```

Make it executable and run:

```bash
chmod +x script.lo
./script.lo
```

## Testing

Run test files:

```bash
maid test array # run tests/array.lo
maid test-all # run all tests
```

## Project Structure

- `src/` - Core language implementation
  - `loft/` - Core language components
  - `parser/` - Lexer and parser
  - `runtime/` - Interpreter and runtime
  - `types/` - Type system (work in progress)
  - `std/` - Standard library (minimal)
- `tests/` - Test programs

## Contributing

Due to the early stage of development, the API and language design are highly volatile. However, contributions are welcome in the form of:

- Bug reports and test cases
- Feature suggestions
- Documentation improvements
- Performance optimizations
