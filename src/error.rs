//! Errors for the Horned-OWL library
use std::fmt::{Display, Formatter};

use thiserror::Error;

#[derive(Debug)]
pub enum Location {
    BytePosition(usize),
    Unknown
}

impl From<usize> for Location{
    fn from(u: usize) -> Self {
        Location::BytePosition(u)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BytePosition(u) => write!(f, "Byte Position: {u}"),
            Self::Unknown => write!(f, "Unknown")
        }
    }
}

/// Error for the Horned library
#[derive(Debug, Error)]
pub enum HornedError {
    /// An IO Error
    #[error("IO Error: {0}")]
    IOError(#[from] std::io::Error),

    /// An error found during the parsing of an underlying format
    #[error("Parsing Error: {0}")]
    ParserError(Box<dyn std::error::Error>, Location),

    /// Data has been given that would we cannot make sense or would
    /// result in invalid OWL
    #[error("Validity Error: {0} at {1}")]
    ValidityError(String, Location),

    /// A command has been given that is invalid
    #[error("Command Error: {0}")]
    CommandError(String),
}

macro_rules! invalid {
    ($($arg:tt)*) => {
        HornedError::ValidityError(format!($($arg)*), crate::error::Location::Unknown)
    }
}

pub(crate) use invalid;



impl HornedError {
    pub fn invalid_at<S: Into<String>, L: Into<Location>>(s:S, l:L) -> HornedError {
        HornedError::ValidityError(s.into(), l.into())
    }
    pub fn invalid<S: Into<String>>(s:S) -> HornedError {
        HornedError::ValidityError(s.into(), Location::Unknown)
    }
}

impl From<oxiri::IriParseError> for HornedError {
    fn from(e: oxiri::IriParseError) -> Self {
        Self::ParserError(e.into(), Location::Unknown)
    }
}

impl From<rio_xml::RdfXmlError> for HornedError {
    fn from(e: rio_xml::RdfXmlError) -> Self {
        Self::ParserError(e.into(), Location::Unknown)
    }
}

impl From<quick_xml::Error> for HornedError {
    fn from(e: quick_xml::Error) -> Self {
        Self::ParserError(e.into(), Location::Unknown)
    }
}

impl From<ureq::Error> for HornedError {
    fn from(e: ureq::Error) -> Self {
        Self::ParserError(e.into(), Location::Unknown)
    }
}