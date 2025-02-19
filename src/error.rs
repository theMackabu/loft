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
        let file = file!();
        let line = line!();
        let column = column!();

        let thread_name = std::thread::current().name().unwrap_or("unnamed").to_string();

        match self {
            Error::IoError(err) => write!(f, "thread '{thread_name}' panicked at 'io error: {err}', {file}:{line}:{column}"),
            Error::ParseError(err) => write!(f, "thread '{thread_name}' panicked at 'parse error: {err}', {file}:{line}:{column}"),
            Error::MissingArgument => write!(f, "thread '{thread_name}' panicked at 'missing file argument', {file}:{line}:{column}"),
            Error::RuntimeError(err) => write!(f, "thread '{thread_name}' panicked at '{err}', {file}:{line}:{column}"),
            Error::UnexpectedReturnValue(err) => write!(f, "thread '{thread_name}' panicked at 'unexpected return value: {err:?}', {file}:{line}:{column}"),
        }
    }
}

/*
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

    IoError { file: String, line: usize, column: usize, source: std::io::Error },

    ParseError { file: String, line: usize, column: usize, message: String },

    RuntimeError { file: String, line: usize, column: usize, message: String },

    UnexpectedReturnValue { file: String, line: usize, column: usize, value: ValueType },
}

pub type Result = StdResult<(), Error>;

impl StdError for Error {}

impl From<IoError> for Error {
    fn from(err: IoError) -> Self {
        Error::IoError {
            file: file!().to_string(),
            line: line!() as usize,
            column: column!() as usize,
            source: err,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let thread_name = std::thread::current().name().unwrap_or("unnamed").to_string();

        match self {
            Error::MissingArgument => write!(f, "missing file argument"),

            Error::IoError { file, line, column, source } => {
                write!(f, "thread '{thread_name}' panicked at 'io error: {source}', {file}:{line}:{column}")
            }

            Error::ParseError { file, line, column, message } => {
                write!(f, "thread '{thread_name}' panicked at 'parse error: {message}', {file}:{line}:{column}")
            }

            Error::RuntimeError { file, line, column, message } => {
                write!(f, "thread '{thread_name}' panicked at '{message}', {file}:{line}:{column}")
            }

            Error::UnexpectedReturnValue { file, line, column, value } => {
                write!(f, "thread '{thread_name}' panicked at 'unexpected return value: {value:?}', {file}:{line}:{column}")
            }
        }
    }
}
*/
