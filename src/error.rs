//! Errors

use std::path::PathBuf;
#[derive(Debug, Fail)]
pub enum CommandError {
    #[fail(display = "An argument that was expected is missing")]
    MissingArgument,
}

/// Errors issued while parsing ontologies.
#[derive(Debug, Fail)]
pub enum ParserError {
    #[fail(display = "Do not know how to parse file with path: {:?}", path)]
    FormatNotSupported{ path: PathBuf }
}
