use std::{
    error::Error as StdError,
    fmt::{Display, Formatter, Result as FmtResult},
    io::Error as IoError,
    result::Result as StdResult,
};

use crate::runtime::value::ValueType;

#[derive(Debug)]
pub enum Error {
    MissingArgument,
    IoError(std::io::Error),
    ParseError(String),
    RuntimeError(String),
    UnexpectedReturnValue(ValueType),
}

pub type Result = StdResult<(), Error>;

impl StdError for Error {}

impl From<IoError> for Error {
    fn from(e: IoError) -> Self { Error::IoError(e) }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Error::MissingArgument => write!(f, "Missing file argument"),
            Error::IoError(e) => write!(f, "I/O error: {}", e),
            Error::ParseError(err) => write!(f, "Parse error: {}", err),
            Error::RuntimeError(err) => write!(f, "runtime 'main' errored at '{err}', file:line:pos"),
            Error::UnexpectedReturnValue(err) => write!(f, "Unexpected return value from program entry point: {err:?}"),
        }
    }
}
