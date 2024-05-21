//! Errors for the Horned-OWL library
use std::fmt::{Display, Formatter};
use std::ops::Range;

use pest::RuleType;
use thiserror::Error;

#[derive(Debug)]
pub enum Location {
    BytePosition(usize),
    ByteSpan(Range<usize>),
    Unknown,
}

impl From<usize> for Location {
    fn from(u: usize) -> Self {
        Location::BytePosition(u)
    }
}

impl From<Range<usize>> for Location {
    fn from(r: Range<usize>) -> Self {
        Location::ByteSpan(r)
    }
}

impl From<pest::error::InputLocation> for Location {
    fn from(l: pest::error::InputLocation) -> Self {
        match l {
            pest::error::InputLocation::Pos(x) => Location::BytePosition(x),
            pest::error::InputLocation::Span((x, y)) => Location::ByteSpan(x..y),
        }
    }
}

impl<'i> From<pest::Span<'i>> for Location {
    fn from(span: pest::Span<'i>) -> Self {
        Location::ByteSpan(span.start()..span.end())
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BytePosition(u) => write!(f, "Byte Position: {}", u),
            Self::ByteSpan(r) => write!(f, "Byte Span: {} to {}", r.start, r.end),
            Self::Unknown => write!(f, "Unknown"),
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
    pub fn invalid_at<S: Into<String>, L: Into<Location>>(s: S, l: L) -> HornedError {
        HornedError::ValidityError(s.into(), l.into())
    }
    pub fn invalid<S: Into<String>>(s: S) -> HornedError {
        HornedError::ValidityError(s.into(), Location::Unknown)
    }
}

impl From<quick_xml::Error> for HornedError {
    fn from(e: quick_xml::Error) -> Self {
        Self::ParserError(e.into(), Location::Unknown)
    }
}

impl From<rio_xml::RdfXmlError> for HornedError {
    fn from(e: rio_xml::RdfXmlError) -> Self {
        Self::ParserError(e.into(), Location::Unknown)
    }
}

impl<R: RuleType + 'static> From<pest::error::Error<R>> for HornedError {
    fn from(e: pest::error::Error<R>) -> Self {
        let location = e.location.clone().into();
        Self::ParserError(e.into(), location)
    }
}
