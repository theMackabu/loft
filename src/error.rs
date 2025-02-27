use std::{
    error::Error as StdError,
    fmt::{Display, Formatter, Result as FmtResult},
    io::Error as IoError,
    result::Result as StdResult,
};

use crate::runtime::value::ValueEnum;

#[derive(Debug)]
pub enum Error {
    MissingArgument,
    IoError(std::io::Error),
    ParseError(String),
    RuntimeError(String),
    UnexpectedReturnValue(ValueEnum),
}

pub type Result<T> = StdResult<T, Error>;

impl StdError for Error {}

impl From<IoError> for Error {
    fn from(e: IoError) -> Self { Error::IoError(e) }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let file = "not_implemented_yet.lo";
        let line = 0;
        let column = 0;

        let thread_name = std::thread::current().name().unwrap_or("unnamed").to_string();

        match self {
            // todo: improve
            Error::ParseError(err) => f.write_str(err),

            Error::MissingArgument => f.write_str("missing file argument..."),

            Error::IoError(err) => write!(f, "io error: {err}"),

            Error::RuntimeError(err) => write!(f, "thread '{thread_name}' panicked at {file}:{line}:{column}\n{err}"),

            Error::UnexpectedReturnValue(err) => write!(f, "thread '{thread_name}' panicked at {file}:{line}:{column}\nunexpected return value: '{err}'"),
        }
    }
}
