#[path = "../types/mod.rs"]
pub mod types;

#[path = "../parser/mod.rs"]
pub mod parser;

#[path = "../runtime/mod.rs"]
pub mod runtime;

#[path = "../macros.rs"]
pub mod macros;

#[path = "../error.rs"]
pub mod error;

pub(crate) mod models;
pub(crate) mod util;

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {{
        let light_blue = "\x1b[94m";
        let reset = "\x1b[0m";
        eprintln!("{}[DEBUG] {}{}", light_blue, format_args!($($arg)*), reset);
    }};
}

#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => {{
        let yellow = "\x1b[38;5;11m";
        let reset = "\x1b[0m";
        eprintln!("{}[WARN] {}{}", yellow, format_args!($($arg)*), reset);
    }};
}

#[macro_export]
macro_rules! trace {
    ($($arg:tt)*) => {{
        let pink = "\x1b[38;5;13m";
        let reset = "\x1b[0m";
        eprintln!("{}[TRACE] {}{}", pink, format_args!($($arg)*), reset);
    }};
}
