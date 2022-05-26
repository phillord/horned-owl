//! Errors for the Horned-OWL library
use thiserror::Error;

/// Error for the Horned library
#[derive(Debug, Error)]
pub enum HornedError {
    /// An IO Error
    #[error("IO Error: {0}")]
    IOError(#[from] std::io::Error),

    /// An error found during the parsing of an underlying format
    #[error("Parsing Error: {0}")]
    ParserError(#[from] Box<dyn std::error::Error>),

    /// Data has been given that would we cannot make sense or would
    /// result in invalid OWL
    #[error("Validity Error: {0}")]
    ValidityError(String, Option<Box<dyn std::error::Error>>),

    /// A command has been given that is invalid
    #[error("Command Error: {0}")]
    CommandError(String),
}

macro_rules! invalid {
    ($($arg:tt)*) => {
        HornedError::ValidityError(format!($($arg)*), None)
    }
}
pub(crate) use invalid;

impl From<quick_xml::Error> for HornedError {
    fn from(e: quick_xml::Error) -> Self {
        Self::ParserError(e.into())
    }
}

impl From<rio_xml::RdfXmlError> for HornedError {
    fn from(e: rio_xml::RdfXmlError) -> Self {
        Self::ParserError(e.into())
    }
}
